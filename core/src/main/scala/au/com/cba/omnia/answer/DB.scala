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

  def run(connection: Connection): Result[A] =
    SDB(connection).autoCommit(action)
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
  def query[A](f: DBSession => A): DB[A] =
    DB(c => Result.safe(f(c)))

  /** Build a DB operation from a value. The resultant DB operation will not throw an exception. */
  def value[A](v: => A): DB[A] =
    query(_ => v)
}
