package com.ambiata.mundane.control

import scala.util.control.NonFatal
import scalaz._, Scalaz._, \&/._
import scalaz.concurrent.Task
import scalaz.effect._

import au.com.cba.omnia.omnitool._

/**
 * Transformer version of Result.
 */
case class ResultT[F[_], A](run: F[Result[A]]) {
  def map[B](f: A => B)(implicit F: Functor[F]): ResultT[F, B] =
    ResultT(run.map(_.map(f)))

  def flatMap[B](f: A => ResultT[F, B])(implicit F: Monad[F]): ResultT[F, B] =
    ResultT(run.flatMap({
      case Ok(a)    => f(a).run
      case Error(e) => Result.these(e).pure[F]
    }))

  def flatMapError(f: These[String, Throwable] => ResultT[F, A])(implicit F: Monad[F]): ResultT[F, A] =
    ResultT(run.flatMap({
      case Ok(a)    => Result.ok(a).pure[F]
      case Error(e) => f(e).run
    }))

  def onResult[B](f: Result[A] => Result[B])(implicit F: Functor[F]): ResultT[F, B] =
    ResultT(run.map(f))

//  def mapError(f: These[String, Throwable] => These[String, Throwable])(implicit F: Functor[F]): ResultT[F, A] =
//    onResult(_.mapError(f))

  def isOk(implicit F: Functor[F]): F[Boolean] =
    toOption.map(_.isDefined)

//  def isError(implicit F: Functor[F]): F[Boolean] =
//    isOk.map(!_)

  def toOption(implicit F: Functor[F]): F[Option[A]] =
    toDisjunction.map(_.toOption)

  def toDisjunction(implicit F: Functor[F]): F[These[String, Throwable] \/ A] =
    run.map(_.toDisjunction)

  def getOrElse[AA >: A](otherwise: => AA)(implicit F: Functor[F]): F[AA] =
    toOption.map(_.getOrElse(otherwise))

  // def |||(otherwise: => ResultT[F, A])(implicit F: Monad[F]): ResultT[F, A] =
  //   ResultT[F, A](run.flatMap(result => if (result.isOk) result.point[F] else otherwise.run))

  def zip[B](other: ResultT[F, B])(implicit F: Monad[F]): ResultT[F, (A, B)] =
    flatMap(a => other.map(a -> _))
}

object ResultT extends LowPriorityResultT {
  def safe[F[_]: Monad, A](thunk: => A): ResultT[F, A] =
    ResultT[F, A](Result.safe(thunk).point[F])

  // def option[F[_]: Monad, A](thunk: => A): ResultT[F, Option[A]] =
  //   ResultT[F, Option[A]](Result.option(thunk).point[F])

  def ok[F[_]: Monad, A](value: A): ResultT[F, A] =
    ResultT[F, A](Result.ok(value).point[F])

  def unit[F[_]: Monad]: ResultT[F, Unit] =
    ResultT[F, Unit](Result.ok(()).point[F])

  def result[F[_]: Monad, A](result: Result[A]): ResultT[F, A] =
    ResultT[F, A](result.point[F])

  def exception[F[_]: Monad, A](t: Throwable): ResultT[F, A] =
    these[F, A](That(t))

  def fail[F[_]: Monad, A](message: String): ResultT[F, A] =
    these[F, A](This(message))

  def error[F[_]: Monad, A](message: String, t: Throwable): ResultT[F, A] =
    these[F, A](Both(message, t))

  def these[F[_]: Monad, A](both: These[String, Throwable]): ResultT[F, A] =
    ResultT[F, A](Result.these(both).point[F])

  // def fromDisjunctionF[F[_]: Functor, A](v: F[These[String, Throwable] \/ A]): ResultT[F, A] =
  //   ResultT[F, A](v.map(Result.fromDisjunction))

  // def fromDisjunction[F[_]: Monad, A](v: These[String, Throwable] \/ A): ResultT[F, A] =
  //   fromDisjunctionF(v.point[F])

//  def fromDisjunctionString[F[_]: Monad, A](v: String \/ A): ResultT[F, A] =
//    fromDisjunction(v.leftMap(This.apply))

//  def fromDisjunctionThrowable[F[_]: Monad, A](v: Throwable \/ A): ResultT[F, A] =
//    fromDisjunction(v.leftMap(That.apply))

  def fromOption[F[_]: Monad, A](v: Option[A], failure: String): ResultT[F, A] =
    v.cata(ResultT.ok[F, A], ResultT.fail(failure))

  def when[F[_]: Monad](v: Boolean, thunk: => ResultT[F, Unit]): ResultT[F, Unit] =
    if (v) thunk else unit

  def unless[F[_]: Monad](v: Boolean, thunk: => ResultT[F, Unit]): ResultT[F, Unit] =
    when(!v, thunk)

  def using[A: Resource, B <: A, C](a: ResultT[IO, B])(run: B => ResultT[IO, C]): ResultT[IO, C] =
    ResultT(a.run.bracket((aa: Result[B]) => aa match {
      case Error(e) => IO { () }
      case Ok(aaa) => implicitly[Resource[A]].close(aaa)
    })((aa: Result[B]) => aa match {
      case Error(e) => IO { Error(e) }
      case Ok(aaa) => run(aaa).run
    }))

  implicit def ResultTMonad[F[_]: Monad]: Monad[({ type l[a] = ResultT[F, a] })#l] =
    new Monad[({ type l[a] = ResultT[F, a] })#l] {
      def point[A](v: => A) = ok[F, A](v)
      def bind[A, B](m: ResultT[F, A])(f: A => ResultT[F, B]) = m.flatMap(f)
    }

  implicit def ResultTEqual[F[_], A](implicit E: Equal[F[Result[A]]]): Equal[ResultT[F, A]] =
    implicitly[Equal[F[Result[A]]]].contramap(_.run)

  implicit def ResultTResultantMonad[F[_]: Monad]: ResultantMonad[({ type l[a] = ResultT[F, a] })#l] = {
    val F = implicitly[Monad[F]]
    new ResultantMonad[({ type l[a] = ResultT[F, a] })#l] {
      def rPoint[A](v: => Result[A]) = ResultT(F.point(v))
      def rBind[A, B](m: ResultT[F, A])(f: Result[A] => ResultT[F, B]) =
        m match {
          case ResultT(fRes) => ResultT(
            F.bind(fRes)(x => f(x) match {
              case ResultT(fRes2) => fRes2
            })
          )
        }
    }
  }
}

trait LowPriorityResultT {
  implicit def ResultTMonadIO[F[_]: MonadIO]: MonadIO[({ type l[a] = ResultT[F, a] })#l] =
    new MonadIO[({ type l[a] = ResultT[F, a] })#l] {
      def point[A](v: => A) = ResultT.ok[F, A](v)
      def bind[A, B](m: ResultT[F, A])(f: A => ResultT[F, B]) = m.flatMap(f)
      def liftIO[A](ioa: IO[A]): ResultT[F, A] = ResultT(ioa.liftIO[F].map(Result.ok))
    }
}
