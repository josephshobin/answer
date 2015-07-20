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

package au.com.cba.omnia.answer.RelMonad

import scalaz.{Monad, ReaderT}

trait RelMonad[R[_], M[_]] {
  def rPoint[A](v: => R[A]): M[A]
  def rBind[A, B](ma: M[A])(f: R[A] => M[B]): M[B]
}

object RelMonad {
  @inline def apply[M[_], R[_]](implicit RM: RelMonad[R, M]): RelMonad[R, M] = RM
}

class ReaderTR[M[_]: Monad, N[_]: Monad, Rd](relM_N: RelMonad[M, N]) extends RelMonad[M, ({ type rN[A]=ReaderT[N, Rd, A] })#rN] {
  val N = Monad[N]

  type ReadM[A] = ReaderT[N, Rd, A]
  val readM = Monad[ReadM]
  def mkReadM[A](f: Rd => N[A]) = new ReaderT[N, Rd, A](f)

  def rPoint[A](v: => M[A]): ReaderT[N, Rd, A] = new ReaderT(_ => relM_N.rPoint(v))
  def rBind[A, B](rNA: ReaderT[N, Rd, A])(f: M[A] => ReaderT[N, Rd, B]): ReaderT[N, Rd, B] =
    readM.join(mkReadM[ReadM[B]](rd =>
      relM_N.rBind(rNA(rd))(ma => N.point(f(ma)))
    ))
  def relMonad: RelMonad[M, ReadM] = this
}
