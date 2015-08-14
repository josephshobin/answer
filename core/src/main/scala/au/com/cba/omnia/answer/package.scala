package au.com.cba.omnia

import scalaz._, Scalaz._, Free.Trampoline

import com.ambiata.mundane.control._

import au.com.cba.omnia.omnitool.{Result, ResultantMonad}

/** ResultantMonad instance for Result, to intantiate DBT, defining [[answer.DB]].
  * Should be moved to omnitool and generalized to `RelSelf: RelMonad[R, R]`.
  */
// object ResultantMonadResult {
//   implicit def monad: ResultantMonad[Result] = new ResultantMonad[Result] {
//     def rPoint[A](v: => Result[A]): Result[A] = v
//     def rBind[A, B](ma: Result[A])(f: Result[A] => Result[B]): Result[B] = f(ma)
//   }
// }
// import ResultantMonadResult._
// import com.ambiata.mundane.control.ResultT.ResultTResultantMonad

object ResultantMonadResultT {

  type ResultTrampoline[A] = ResultT[Trampoline, A]
  val Trampoline = implicitly[Monad[Trampoline]]

  implicit def monad: ResultantMonad[ResultTrampoline] = new ResultantMonad[ResultTrampoline] {
      def rPoint[A](v: => Result[A]) = ResultT(Trampoline.point(v))
      def rBind[A, B](m: ResultTrampoline[A])(f: Result[A] => ResultTrampoline[B]) =
        m match {
          case ResultT(fRes) => ResultT(
            Trampoline.bind(fRes)(x => f(x) match {
              case ResultT(fRes2) => fRes2
            })
          )
        }
  }
//  type FF[T] = Free[Function0, T]
//  type RFF[A] = ResultT[FF, A]
//  implicit def monad2: ResultantMonad[RFF] = monad
}

import ResultantMonadResultT._

package object answer extends DBT[({ type l[a] = ResultT[Trampoline, a]})#l]
