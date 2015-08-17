package au.com.cba.omnia

import scalaz._, Scalaz._, Free.Trampoline, effect.IO

//import com.ambiata.mundane.control._

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

  type TrampoResult[A] = Trampoline[() => Result[A]]
  val Trampoline = implicitly[Monad[Trampoline]]

  implicit def monad: ResultantMonad[TrampoResult] = new ResultantMonad[TrampoResult] {
    def rPoint[A](v: => Result[A]) = {
      def vv() = v
      Trampoline.point(vv)
    }
    def rBind[A, B](mTramp: TrampoResult[A])(f: Result[A] => TrampoResult[B]) = { println(s"Tramp.rBind: preBind, mTramp = ${mTramp}")
      Trampoline.bind(mTramp)(mRes => { // println(s"Tramp.rBind: in bind, mTramp = ${mTramp} mRes = ${mRes}")
        f(mRes()) match {
          case fRes => {       // println(s"Tramp.rBind:  in bind, mTramp = ${mTramp} mRes = ${mRes} fRes = ${fRes} ")
            (fRes: TrampoResult[B]) }}
        // `: Trampoline[B]` as required by Trampoline.bind
      })
    }
  }

      //   }
//  type FF[T] = Free[Function0, T]
//  type RFF[A] = ResultT[FF, A]
//  implicit def monad2: ResultantMonad[RFF] = monad
}

import ResultantMonadResultT._

// Instaniate DBT - effectively forming  DBSession => Trampoline[Result[_]]
package object answer extends DBT[TrampoResult]

//  { type l[a] = ResultT[Trampoline, a]})#l]
