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
import scalaz.\&/.These

import au.com.cba.omnia.omnitool.{Result, ResultantMonad, ResultantOps}

/**
  * A datatype that operates on a scalikejdbc `DBSession` 
  * to produces a `Result` and provides a nice set of
  * combinators.
  * 
  * A quick way to run the DB action is call [[DB.run]]
  * with a jdbc connection.
  */
case class DB[A](action: DBSession => Result[A]) {
  /** Run the DB action with the provided connection. */
  def run(connection: Connection): Result[A] =
    SDB(connection).localTx(action)
}

object DB {
  /** Build a DB operation from a function. The resultant DB operation will not throw an exception. */
  def ask[A](f: DBSession => A): DB[A] =
    DB(s => Result.safe(f(s)))

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


  implicit def DBResultantMonad: ResultantMonad[DB] = new ResultantMonad[DB] {
    def rPoint[A](v: => Result[A]): DB[A] = DB[A](_ => v)
    def rBind[A, B](ma: DB[A])(f: Result[A] => DB[B]): DB[B] =
      DB(c => f(ma.action(c)).action(c))
  }

  implicit def ToResultantOps(v: DB.type): ResultantOps[DB.type, DB] =
    new ResultantOps[DB.type, DB](v)
}
