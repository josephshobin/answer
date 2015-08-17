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

import scalaz._, Scalaz._

import au.com.cba.omnia.omnitool.{Result, ResultantMonad, ResultantOps, ToResultantMonadOps, ResultantMonadSyntax}

import au.com.cba.omnia.answer.RelMonad._

/** Configuration required to run a `DB` instance.
  *
  * @param jdbcUrl  : jdbc url to the database
  * @param user     : user name for the database
  * @param password : password for the database
  * @param driver   : Optional driver name like `oracle.jdbc.OracleDriver`. In the absence
                      of a driver name, the driver to use is derived from the jdbcUrl provided.
  */
case class DBConfig(jdbcUrl: String, user: String, password: String, driver: Option[String] = None) {
  /** Unique name for this config. This unique name is used internally as the connection pool name.*/
  val name = s"$jdbcUrl/$user"
}

/**
  * A datatype that operates on a scalikejdbc `DBSession`
  * to produces a `Result` and provides a nice set of
  * combinators.
  *
  * A quick way to run the DB action is call [[DB.run ]]
  * with a jdbc connection.
  */
class DBT[R[_] : ResultantMonad] {

  val R = ResultantMonad[R]
  import ResultantMonadSyntax._

  /** The actual database monad for a particular `ResultantMonad` [[R]] */
  case class DB[A](action: DBSession => R[A]) { // self =>

  import scalaz.\&/.These

  /** Recovers from an error. */
  def recoverWithR[A](ra: R[A])(recovery: PartialFunction[These[String, Throwable], R[A]]): R[A] =
    R.rBind(ra)(r => r.fold(
      _     => R.rPoint(r),
      error => recovery.applyOrElse[These[String, Throwable], R[A]](error, _ => R.rPoint(r))
    ))

  /**
    * Like "finally", but only performs the final action if there was an error.
    *
    * If `action` fails that error is swallowed and only the initial error is returned.
    */
  def onExceptionR[A, B](ra: R[A])(action: R[B]): R[A] =
    recoverWithR(ra) { case e => action.rFlatMap(_ => R.rPoint(Result.these(e))) }

    val reader = new ReaderT[R, DBSession, A](action)  // Includes an implicit `DBSession => R[A]`

    /**
      * Run the DB action with the provided connection.
      *
      * @param connection: SQL Connection used to run this action
      *
      * @return result `R[A]` of this DB action.
      */
    def run(connection: Connection): R[A] = {
      val sdb = SDB(connection)
      val ra = (for {
        _       <- R.point(sdb.begin())
        actionR <- sdb.withinTx(action)
        _       <- R.point(println(s"precomit, actionR = ${actionR}"))
        _       <- R.point(sdb.commit())
        _       <- R.point(println(s"comitted, actionR = ${actionR}"))
      } yield actionR)
      onExceptionR(ra)(R.point(println(s"DB.run->onException")))  // Seems to cause "not active" failures??
      .onException(R.point(sdb.rollback()))  // Seems to cause "not active" failures??
      //  .recoverWith { case e => {println(e); error(e.toString)}}
    }

    /** Run the DB action with the provided configuration and return the Result.
      *
      * @param config: database configuration to run the DB action against
      *
      * @return Result `A` of DB action
      */
    def run(config: DBConfig): R[A] =
      DB.connection(config).bracket(
        conn => R.point{println(s"closing: ${this}"); conn.close()}  // close seems to cause "not active" errors?
      )(conn => run(conn))
//      DB.connection(config).flatMap { conn =>
//        LoanPattern.using(conn) { safeConn =>
//          run(safeConn)
//        }
//      }

    // //  Almost implementation of filter (was dummy)
    // def filter(f: A => Boolean): DB[A] =
    //   DB.monad.bind(this)(a =>
    //     if(f(a)) R.point(a)
    //     else R.fail("Filtered via DB")
    //   )
  }

  object DB extends ResultantOps[DB] with ToResultantMonadOps {

    /** Build a DB operation from a function. The resultant DB operation will not throw an exception. */
    def ask[A](f: DBSession => A): DB[A] =
      DB[A](s => R.point(f(s)))

    /**
      * Runs the specified sql query expecting a single row as response.
      *
      * If no row matches the query it returns `None`. If the result is not single, an `Error`
      * is returned.
      */
    def querySingle[A : Extractor](sql: SQL[Nothing, NoExtractor]): DB[Option[A]] =
      ask(implicit session => sql.map(implicitly[Extractor[A]].extract).single.apply())

    /**
      * Runs the specified sql query and get the first row as response.
      *
      * If no row matches the query it returns `None`.
      */
    def queryFirst[A : Extractor](sql: SQL[Nothing, NoExtractor]): DB[Option[A]] =
      query[A](sql).map(_.headOption)

    /** Runs the specified sql query */
    def query[A : Extractor](sql: SQL[Nothing, NoExtractor]): DB[Traversable[A]] =
      ask(implicit session => sql.map(implicitly[Extractor[A]].extract).traversable().apply())

    /** Create a new SQL connection or get a pooled connection. Library users are responsible
      * for adding the appropriate jdbc drivers as a dependency.
      *
      * @param config: database configuration for the connection
      *
      * @return The SQL connection
      */
    def connection(config: DBConfig): R[java.sql.Connection] = R.point {
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
    }

    /** DBT[R].DB is relative to R */
    implicit val relM: RelMonad[R, DB] = new RelMonad[R, DB] {
      def rPoint[A](v: => R[A]): DB[A] = DB[A](_ => v)
      def rBind[A, B](dbma: DB[A])(f: R[A] => DB[B]): DB[B] =
        DB[B](c => f(dbma.action(c)).action(c))
    }

    /** DBT[R].DB is relative to L if R is relative to L */
    implicit def relLowerR[L[_] : Monad](relL_R: RelMonad[L, R]): RelMonad[L, DB] = new RelMonad[L, DB] {
      val relReadTR = new ReaderTR[L, R, DBSession](relL_R).relMonad
      def rPoint[A](v: => L[A]) = DB(relReadTR.rPoint(v))
      def rBind[A, B](dba: DB[A])(f: L[A] => DB[B]) =
        DB[B](relReadTR.rBind(dba.reader)(la => f(la).reader))
    }

    /** DBT[R].DB is a resultant monad. */
    implicit val monad: ResultantMonad[DB] =
      resultantFromRelMonad(relLowerR[Result](relMonadFromResultant(R)))

    // These two conversions should be in ResultantMonad or similar
    def relMonadFromResultant[RM[_]](implicit RM: ResultantMonad[RM]) = new RelMonad[Result, RM] {
      def rPoint[A](v: => Result[A]): RM[A] = RM.rPoint(v)
      def rBind[A, B](rA: RM[A])(f: Result[A] => RM[B]): RM[B] = RM.rBind(rA)(f)
    }
    def resultantFromRelMonad[RR[_]](implicit RR: RelMonad[Result, RR]) = new ResultantMonad[RR] {
      def rPoint[A](v: => Result[A]): RR[A] = RR.rPoint(v)
      def rBind[A, B](rA: RR[A])(f: Result[A] => RR[B]): RR[B] = RR.rBind(rA)(f)
    }

  }
}
