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

import scalaz.{Monad, ReaderT}

import au.com.cba.omnia.omnitool.{Result, ResultantMonad} // ResultantOps, ToResultantMonadOps, ResultantMonadSyntax}

trait RelMonad[R[_], M[_]] {
  def rPoint[A](v: => R[A]): M[A]
  def rBind[A, B](ma: M[A])(f: R[A] => M[B]): M[B]
}

object RelMonad {
  @inline def apply[M[_], R[_]](implicit RM: RelMonad[R, M]): RelMonad[R, M] = RM
}

object ConvertRelM {
  // These two conversions should be in ResultantMonad, RelMonad or similar
  def fromResultant[RM[_]](implicit RM: ResultantMonad[RM]) = new RelMonad[Result, RM] {
    def rPoint[A](v: => Result[A]): RM[A] = RM.rPoint(v)
    def rBind[A, B](rA: RM[A])(f: Result[A] => RM[B]): RM[B] = RM.rBind(rA)(f)
  }
  def toResultant[RR[_]](implicit RR: RelMonad[Result, RR]) = new ResultantMonad[RR] {
    def rPoint[A](v: => Result[A]): RR[A] = RR.rPoint(v)
    def rBind[A, B](rA: RR[A])(f: Result[A] => RR[B]): RR[B] = RR.rBind(rA)(f)
  }
}

/** RelMonad[M, N] instances can be extended via ReaderT to RelMonad[M, ReaderT[N, Rd, _]] */
class ReaderTR[M[_]: Monad, N[_]: Monad, Rd](relM_N: RelMonad[M, N]) extends RelMonad[M,
  ({ type rN[A]=ReaderT[N, Rd, A]})#rN   // aka ReadM (below)
] {
  val N = Monad[N]

  type ReadM[A] = ReaderT[N, Rd, A]
  val readM = Monad[ReadM]
  def mkReadM[A](f: Rd => N[A]) = new ReaderT[N, Rd, A](f)

  def rPoint[A](v: => M[A]): ReadM[A] = new ReaderT(_ => relM_N.rPoint(v))
  def rBind[A, B](rNA: ReadM[A])(f: M[A] => ReadM[B]): ReadM[B] =
    readM.join(mkReadM[ReadM[B]](rd =>
      relM_N.rBind(rNA(rd))(ma => N.point(f(ma)))
    ))
  def relMonad: RelMonad[M, ReadM] = this
}
