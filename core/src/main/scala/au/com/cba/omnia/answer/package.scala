package au.com.cba.omnia

import au.com.cba.omnia.omnitool.{Result, ResultantMonad}

/** ResultantMonad instance for Result, to intantiate DBT, defining [[answer.DB]].
  * Should be moved to omnitool and generalized to `RelSelf: RelMonad[R, R]`.
  */
object ResultantMonadResult {
  implicit def monad: ResultantMonad[Result] = new ResultantMonad[Result] {
    def rPoint[A](v: => Result[A]): Result[A] = v
    def rBind[A, B](ma: Result[A])(f: Result[A] => Result[B]): Result[B] = f(ma)
  }
}
import ResultantMonadResult._

package object answer extends DBT[Result]
