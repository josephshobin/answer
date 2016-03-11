//   Copyright 2015 Commonwealth Bank of Australia
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

import java.sql.Connection

import scalikejdbc.{DB => SDB, _}

import scalaz._, Scalaz._

import au.com.cba.omnia.omnitool.{Result, Ok, ResultantMonad, ResultantMonadSyntax, ResultantOps, ToResultantMonadOps, RelMonad}
import ResultantMonad._
import ResultantMonadSyntax._


/** Convenient operations that you can do on a `RelMonad[DB, _]`. */
trait DbROps[M[_]] {
  implicit val monad: ResultantMonad[M] // Don't need this now

  implicit val DbRel: RelMonad[DB, M]

  def ask[A](f: DBSession => A): M[A] =
    DbRel.rPoint(DB(s => Result.safe(f(s))))

  /**
    * Runs the specified sql query expecting a single row as response.
    * 
    * If no row matches the query it returns `None`. If the result is not single, an `Error` 
    * is returned.
    */
  def querySingle[A : Extractor](sql: SQL[Nothing, NoExtractor]): M[Option[A]] =
    ask(implicit session => sql.map(implicitly[Extractor[A]].extract).single.apply())

  /**
    * Runs the specified sql query and get the first row as response.
    * 
    * If no row matches the query it returns `None`.
    */
  def queryFirst[A : Extractor](sql: SQL[Nothing, NoExtractor]): M[Option[A]] =
    query[A](sql).map(_.headOption)

  /** Runs the specified sql query */
  def query[A : Extractor](sql: SQL[Nothing, NoExtractor]): M[Traversable[A]] =
    ask(implicit session => sql.map(implicitly[Extractor[A]].extract).traversable().apply())

  /** Create a new SQL connection or get a pooled connection. Library users are responsible 
    * for adding the appropriate jdbc drivers as a dependency.
    *
    * @param config: database configuration for the connection
    *
    * @return The SQL connection 
    */
  def connection(config: DBConfig): Result[java.sql.Connection] = Result.safe {
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

}
