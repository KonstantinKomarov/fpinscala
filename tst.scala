package fpinscala.exercises
package tst

import state.*
import monads.*

val M: Monad[[X] =>> State[Int, X]] = summon[Monad[[X] =>> State[Int, X]]]

val cnts = M.replicateM(3, State(s => (s, s + 1)))

val Mstr: Monad[[X] =>> State[String, X]] = summon[Monad[[X] =>> State[String, X]]]
val s: State[String, String] = State(s => (s + s, s + "*"))
val lst = List(s, s, s)

def zipWithIndex[A](as: List[A])(iv: Int = 0)
(using F: Monad.stateMonad[Int])
: List[(Int, A)] = 
    as.foldLeft(F.unit(List[(Int, A)]()))((acc, a) => for {
        xs <- acc
        n <- F.getState
        _ <- F.setState(n + 1)
    } yield (n, a) :: xs ).run(iv)._1.reverse

extension [S](m: Monad[[X] =>> State[S, X]])
    def getState: State[S, S] = State(s => (s, s))
    def setState(v: S): State[S, Unit] = State(_ => ((), v))