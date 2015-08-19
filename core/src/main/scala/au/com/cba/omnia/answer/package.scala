package au.com.cba.omnia

import scalaz._, Scalaz._, Free.Trampoline

import org.slf4j.{Logger, LoggerFactory}

import au.com.cba.omnia.omnitool.{Result, ResultantMonad}

/** ResultantMonad instance for Result, to intantiate DBT, defining [[answer.DB]].
  * Should be moved to omnitool and generalized to `RelSelf: RelMonad[R, R]`.
  */

object ResultantTrampoResult {

  val logger = LoggerFactory.getLogger("au.com.cba.omnia.answer")

  type TrampoResult[A] = Trampoline[Result[A]]
  val Trampoline = implicitly[Monad[Trampoline]]

  implicit def monad: ResultantMonad[TrampoResult] = new ResultantMonad[TrampoResult] {
    def rPoint[A](v: => Result[A]): Trampoline[Result[A]] = {
      Trampoline.point(v)
    }
    def rBind[A, B](m: Trampoline[Result[A]])(f: Result[A] => Trampoline[Result[B]]) = {
      logger.debug(s"rBind(m)(f): before Trampline.bind(m)(f)  m = ${m}")
      Trampoline.bind(m)(f)
    }
  }
}

import ResultantTrampoResult._

// Instaniate DBT, forming the monad: `DBSession => Trampoline[Result[_]]`
package object answer extends DBT[TrampoResult](logger)
