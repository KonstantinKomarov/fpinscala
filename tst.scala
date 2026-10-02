package fpinscala.exercises
package tst

sealed trait IO[A] {
	def flatMap[B](f: A => IO[B]): IO[B] =
		FlatMap(this, f)
	def map[B](f: A => B): IO[B] =
		flatMap(f andThen (Return(_)))
}
case class Return[A](a: A) extends IO[A]
case class Suspend[A](resume: () => A) extends IO[A]
case class FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]

@annotation.tailrec def run[A](io: IO[A]): A = io match {
	case Return(a) => a
	case Suspend(r) => r()
	case FlatMap(x, f) => x match {
		case Return(a) => run(f(a))
		case Suspend(r) => run(f(r()))
		case FlatMap(y, g) => run(y.flatMap(a => g(a).flatMap(f)))
	}
}

val f: Int => IO[Int] = (x: Int) => Return(x)
val g: Int => IO[Int] = List.fill(100000)(f).foldLeft(f) {
	(a, b) => x => Suspend(() => x).flatMap(a).flatMap(b)
}

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