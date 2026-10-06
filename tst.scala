package fpinscala.exercises
package tst

import fpinscala.exercises.localeffects.* 

sealed trait Proc[I, O]
case class Emit[I, O] (
	head: O,
	tail: Proc[I, O] = Halt[I, O]()
) extends Proc[I, O]
case class Await[I, O] (
	recv: Option[I] => Proc[I, O]
) extends Proc[I, O]
case class Halt[I, O]() extends Proc[I, O]

extension [I, O](p: Proc[I, O])
	def apply(src: LazyList[I]): LazyList[O] = p match
		case Halt()			=> LazyList.empty
		case Await(recv) => src match {
			case h #:: t 	=> recv(Some(h))(t)
			case _				=> recv(None)(LazyList.empty)
		}
		case Emit(h, t)	=> h #:: t(src)
	
def liftOne[I, O](f: I => O): Proc[I, O] = 
	Await {
		case Some(i) => Emit(f(i))
		case None => Halt()
	}

val p = liftOne((x: Int) => x * 2)



trait RunnableST[A] {
	def apply[S]: ST[S, A]
}

object Tst:
	val p = new RunnableST[(Int, Int)] {
		def apply[S] = for {
			r1 <- STRef(1)
			r2 <- STRef(2)
			x  <- r1.read
			y  <- r2.read
			_  <- r1.write(y + 1)
			_  <- r2.write(x + 1)
			a  <- r1.read
			b  <- r2.read 
		} yield (a, b)
	}

	// val pRef = new RunnableST[STRef[_, Int]] {
	// 	def apply[S] = for {
	// 		r1 <- STRef(1)
	// 	} yield r1
	// }

object tstST:
	def apply[S, A](a: => A): ST[S, A] = ST.apply(a)
	def runST[A](st: RunnableST[A]): A = ST.run[A]([s] => () => st.apply[s])

/*
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

*/