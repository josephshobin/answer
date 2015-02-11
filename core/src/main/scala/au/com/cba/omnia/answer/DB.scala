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

import au.com.cba.omnia.omnitool.Result


case class DB[A](action: DBSession => Result[A]) {
  /** Map across successful Db operations. */
  def map[B](f: A => B): DB[B] =
    andThen(f andThen Result.ok)

  /** Bind through successful DB operations. */
  def flatMap[B](f: A => DB[B]): DB[B] =
    DB(c => action(c).flatMap(f(_).action(c)))

  /** Chain an unsafe operation through a successful DB operation. */
  def safeMap[B](f: A => B): DB[B] =
    flatMap(a => DB.value(f(a)))

  /** Chain a context free result (i.e. requires no configuration) to this DB operation. */
  def andThen[B](f: A => Result[B]): DB[B] =
    flatMap(a => DB(_ => f(a)))

  /** Convert this to a safe DB operation that converts any exceptions to failed results. */
  def safe: DB[A] =
    DB(c => try { action(c) } catch { case NonFatal(t) => Result.exception(t) })

  /**
    * Runs the first Hdfs operation. If it fails, runs the second operation. Useful for chaining optional operations.
    *
    * Throws away any error from the first operation.
    */
  def or(other: => DB[A]): DB[A] =
    DB(c => action(c).fold(Result.ok, _ => other.action(c)))

  /** Alias for `or`. Provides nice syntax: `DB.create("bad") ||| DB.create("good")` */
  def |||(other: => DB[A]): DB[A] =
    or(other)

  /** Run the DB action with the provided connection. */
  def run(connection: Connection): Result[A] =
    SDB(connection).localTx(action)

    /** Recovers from an error. */
  def recoverWith(recovery: PartialFunction[These[String, Throwable], DB[A]]): DB[A] =
    DB(c => action(c).fold(
      res   => Result.ok(res),
      error => recovery.andThen(_.action(c)).applyOrElse(error, Result.these)
    ))

  /** Like "finally", but only performs the final action if there was an error. */
  def onException[B](action: DB[B]): DB[A] =
    this.recoverWith { case e => action >> DB.result(Result.these(e)) }

  /**
    * Applies the "during" action, calling "after" regardless of whether there was an error.
    * All errors are rethrown. Generalizes try/finally.
    */
  def bracket[B, C](after: A => DB[B])(during: A => DB[C]): DB[C] = for {
    a <- this
    r <- during(a) onException after(a)
    _ <- after(a)
  } yield r

  /** Like "bracket", but takes only a computation to run afterward. Generalizes "finally". */
  def ensuring[B](sequel: DB[B]): DB[A] = for {
    r <- onException(sequel)
    _ <- sequel
  } yield r
}

object DB {
    /** Build a DB operation from a result. */
  def result[A](v: Result[A]): DB[A] =
    DB(_ => v)

  /** Build a failed DB operation from the specified message. */
  def fail[A](message: String): DB[A] =
    result(Result.fail(message))

  /** Build a failed DB operation from the specified exception. */
  def exception[A](t: Throwable): DB[A] =
    result(Result.exception(t))

  /** Build a failed DB operation from the specified exception and message. */
  def error[A](message: String, t: Throwable): DB[A] =
    result(Result.error(message, t))

  /**
    * Fails if condition is not met
    *
    * Provided instead of [[scalaz.MonadPlus]] typeclass, as DB does not
    * quite meet the required laws.
    */
  def guard(ok: Boolean, message: String): DB[Unit] =
    result(Result.guard(ok, message))

  /**
    * Fails if condition is met
    *
    * Provided instead of [[scalaz.MonadPlus]] typeclass, as DB does not
    * quite meet the required laws.
    */
  def prevent(fail: Boolean, message: String): DB[Unit] =
    result(Result.prevent(fail, message))

  /**
    * Ensures a DB operation returning a boolean success flag fails if unsuccessfull
    *
    * Provided instead of [[scalaz.MonadPlus]] typeclass, as DB does not
    * quite meet the required laws.
    */
  def mandatory(action: DB[Boolean], message: String): DB[Unit] =
    action flatMap (guard(_, message))

  /**
    * Ensures a DB operation returning a boolean success flag fails if succeesfull
    *
    * Provided instead of [[scalaz.MonadPlus]] typeclass, as DB does not
    * quite meet the required laws.
    */
  def forbidden(action: DB[Boolean], message: String): DB[Unit] =
    action flatMap (prevent(_, message))

  /** Build a DB operation from a function. The resultant DB operation will not throw an exception. */
  def ask[A](f: DBSession => A): DB[A] =
    DB(s => Result.safe(f(s)))

  /**
    * Runs the specified query expecting a single row as response.
    * 
    * If no row matches the query it returns `None`.
    */
  def querySingle[A : Extractor](query: SQL[Nothing, NoExtractor]): DB[Option[A]] =
    ask(implicit session => query.map(implicitly[Extractor[A]].extract).single.apply())

  /** Runs the specified query. */
  def query[A : Extractor](query: SQL[Nothing, NoExtractor]): DB[List[A]] =
    ask(implicit session => query.map(implicitly[Extractor[A]].extract).list.apply())

  /** Build a DB operation from a value. The resultant DB operation will not throw an exception. */
  def value[A](v: => A): DB[A] =
    ask(_ => v)

  implicit def DBMonad: Monad[DB] = new Monad[DB] {
    def point[A](v: => A) = result(Result.ok(v))
    def bind[A, B](a: DB[A])(f: A => DB[B]) = a flatMap f
  }

}
