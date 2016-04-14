//   Copyright 2014 Commonwealth Bank of Australia
//
//   Licensed under the Apache License, Version 2.0 (the "License");
//   you may not use this file except in compliance with the License.
//   You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
//   Unless required by applicable law or agreed to in writing, software
//   distributed under the License is distributed on an "AS IS" BASIS,
//   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//   See the License for the specific language governing permissions and
//   limitations under the License.

package au.com.cba.omnia.answer

import scala.util.control.NonFatal

import java.sql.Connection

import scalikejdbc.{DB => SDB, _}

import scalaz._, Scalaz._, scalaz.syntax.{MonadSyntax, PlusSyntax}

import au.com.cba.omnia.omnitool.{Result, Ok, ResultantOps, RelMonad, ToResultantMonadOps}
import au.com.cba.omnia.omnitool.ResultantMonadSyntax._
import au.com.cba.omnia.omnitool.%~>._

import au.com.cba.omnia.answer.{DB => DBResult}

/** Configuration required to run a `DB` instance. 
  *   @param jdbcUrl  : jdbc url to the database
  *   @param user     : user name for the database
  *   @param password : password for the database
  *   @param driver   : Optional driver name like `oracle.jdbc.OracleDriver`. In the absence
                        of a driver name, the driver to use is derived from the jdbcUrl provided.
  */
case class DBConfig(jdbcUrl: String, user: String, password: String, driver: Option[String] = None) {
  /** Unique name for this config. This unique name is used internally as the connection pool name.*/
  val name = s"$jdbcUrl/$user"
}


/** Builds a DB monad for any Result-related monad R */
class DBT[R[_]](implicit R: Result %~> R) {

  /** A datatype that operates on a scalikejdbc `DBSession` 
    * to produces a `Resultant` and provides a nice set of
    * combinators.
    * A quick way to run the DB action is call [[DB.run]]
    * with a jdbc connection.
    */
  case class DB[A](action: DBSession => R[A]) {

    /** Run the DB action with the provided connection. 
      *   @param connection: SQL Connection used to run this action
      *   @return Resultant `A` of this DB action.
      */
    def run(connection: Connection): R[A] = {
      R.point(SDB(connection)) >>= ((sdb: scalikejdbc.DB) =>
        (
          R.point(sdb.begin())  >>
          sdb.withinTx(action)  >>= ((a: A) =>
          R.point(sdb.commit()) .map(_ =>
          a
        )))    // Proposed new "multiple close parenthesis" convention for "for-like" code
          .onException { R.point(sdb.rollback()) }   //TODO:  logger.warn(s"...");
      )
    }

    /** Run the DB action with the provided configuration and return the Resultant.
      *   @param config: database configuration to run the DB action against
      *   @return Resultant `A` of DB action
      */
    def run(config: DBConfig): R[A] =
      DB.DBR.connection(config).bracket(
        conn => R.point(conn.close())
      )(conn => run(conn))
  }

  object DB extends DBOps

  /** Operations for DBT[R].DB */
  trait DBOps extends DbROps[DB] {

    // A generalised "self-composing" L %~> DB instance for all L with L %~> R, instantiated twice below.
    def lowerRel[L[_]](implicit LRelR: L %~> R) = new (L %~> DB) {
      def rPoint[A](v: => L[A]): DB[A]              = DB[A](_ => LRelR.rPoint(v))
      def rBind[A, B](dba: DB[A])(f: L[A] => DB[B]) = DB[B](c => LRelR.rBind(dba.action(c))(a => f(a).action(c)))
    }
 
    implicit val DbRel:     DB %~> DB = %~>.SelfR[DB]                  // For DbROps[DB]
    implicit val monad: Result %~> DB = lowerRel[Result](R)            // For ResultantOps[DB]
    implicit val RRel:       R %~> DB = lowerRel[R](RelMonad.SelfR[R])

    /** Run with the action as first argument.  Only for DBT[R].DB, not for DBT[R].DB %~> M. */
    def run[A](conf: DBConfig)(action: DB[A]): R[A] = action.run(conf)

    /** An implicit conversion that is handy, but alas generally doesn't work in for-comprehensions. */
    implicit def fromDB[A](self: au.com.cba.omnia.answer.DB[A]): DB[A] = DbRel.rPoint(self)
  }

  /** Convenient operations that you can do when you have `RelMonad[DB, _]`. */
  trait DbROps[M[_]] extends ResultantOps[M] with ToResultantMonadOps {
    implicit val DbRel:     DB %~> M
    implicit val monad: Result %~> M  // Supplies `map` via Monad[M], which is used below.

    val DBR = this                   // Allows using operations via `DBR.query` etc.

    def ask[A](f: DBSession => A): M[A] =
      DbRel.rPoint(DB(s => R.point(f(s))))

    /** Runs the specified sql query expecting a single row as response.
      * If no row matches the query it returns `None`. If the result is not single, an `Error` 
      * is returned.
      */
    def querySingle[A : Extractor](sql: SQL[Nothing, NoExtractor]): M[Option[A]] =
      ask(implicit session => sql.map(implicitly[Extractor[A]].extract).single.apply())

    /** Runs the specified sql query and get the first row as response.
      * If no row matches the query it returns `None`.
      */
    def queryFirst[A : Extractor](sql: SQL[Nothing, NoExtractor]): M[Option[A]] =
      query[A](sql).map(_.headOption)

    /** Runs the specified sql query */
    def query[A : Extractor](sql: SQL[Nothing, NoExtractor]): M[Traversable[A]] =
      ask(implicit session => sql.map(implicitly[Extractor[A]].extract).traversable().apply())

    /** Create a new SQL connection or get a pooled connection. Library users are responsible
      * for adding the appropriate jdbc drivers as a dependency.
      *   @param config: database configuration for the connection
      *   @return The SQL connection 
      */
    def connection(config: DBConfig): R[java.sql.Connection] = R.rPoint (Result.safe {
      if (!ConnectionPool.isInitialized(config.name)) {
        lazy val derivedDriver = config.jdbcUrl.split(":").take(3) match {
          case Array("jdbc", "sqlserver", _) => "com.microsoft.sqlserver.jdbc.SQLServerDriver"
          case Array("jdbc", "hsqldb"   , _) => "org.hsqldb.jdbcDriver"
          case Array("jdbc", "oracle"   , _) => "oracle.jdbc.OracleDriver"
          case Array("jdbc", "mysql"    , _) => "com.mysql.jdbc.Driver"
          case Array("jdbc", unknown    , _) => throw new Exception(s"Unsupported jdbc driver: $unknown")
          case _                             => throw new Exception(s"Invalid jdbc url: ${config.jdbcUrl}")
        }
        Class.forName(config.driver.fold(derivedDriver)(identity))
        ConnectionPool.add(config.name, config.jdbcUrl, config.user, config.password)
      }
      ConnectionPool.borrow(config.name)
    })
  }
}    // End of DBT[R]

/** Extended by the answer package object to provide answer.DB class and object */
trait DBPackage {
  val  DBTResult = new DBT[Result]()(RelMonad.SelfR[Result])
  type DB[A]     = DBTResult.DB[A]
  val  DB        = DBTResult.DB
}
