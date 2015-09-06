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

import au.com.cba.omnia.omnitool.{Result, Ok, ResultantMonad, ResultantOps, ToResultantMonadOps, RelMonad}

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
  * A quick way to run the DB action is call [[DB.run]]
  * with a jdbc connection.
  */
case class DB[A](action: DBSession => Result[A]) {
  /**
    * Run the DB action with the provided connection. 
    *
    * @param connection: SQL Connection used to run this action
    *
    * @return Result `A` of this DB action.
    */
  def run(connection: Connection): Result[A] = Result.safe {
    val sdb = SDB(connection) 
    sdb.begin()
    val result = sdb.withinTx(action)
    result match {
      case Ok(_) => sdb.commit()
      case _     => sdb.rollback()
    }
    result
  }.join

  /** Run the DB action with the provided configuration and return the Result.
    * 
    * @param config: database configuration to run the DB action against
    *
    * @return Result `A` of DB action
    */
  def run(config: DBConfig): Result[A] = 
    DB.connection(config).flatMap { conn =>
      LoanPattern.using(conn) { safeConn =>
        run(safeConn)
      }
    }
}

object DB extends ResultantOps[DB] with ToResultantMonadOps  with DbROps[DB] {

  implicit val DbRel = new RelMonad.SelfR[DB]() 
  
  implicit val monad: ResultantMonad[DB] = new ResultantMonad[DB] {
    def rPoint[A](v: => Result[A]): DB[A] = DB[A](_ => v)
    def rBind[A, B](ma: DB[A])(f: Result[A] => DB[B]): DB[B] =
      DB(c => f(ma.action(c)).action(c))
  }
}
