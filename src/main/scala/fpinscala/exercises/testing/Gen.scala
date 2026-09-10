package fpinscala.exercises.testing

import fpinscala.exercises.state.*
import fpinscala.exercises.parallelism.*
import fpinscala.exercises.parallelism.Par.Par
import Gen.*
import Prop.*
import java.util.concurrent.{Executors,ExecutorService}

trait Prop { self =>
  def check: Boolean

  def &&(p: Prop): Prop = new Prop {
    def check: Boolean = self.check && p.check
  }
}

object Prop:
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???

opaque type Gen[+A] = State[RNG, A]

object Gen:
  extension [A](self: Gen[A])
    // We should use a different method name to avoid looping (not 'run')
    def next(rng: RNG): (A, RNG) = self.run(rng)

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    require(start < stopExclusive, "start must be less than stopExclusive")

    // Gen[Int] = State[RNG, Int], поэтому возвращаем State напрямую
    State { (rng: RNG) =>
      val (n, rng2) = RNG.nonNegativeInt(rng)
      val range = stopExclusive - start
      (start + n % range, rng2)
    }
  }

  def unit[A](a: => A): Gen[A] = ???

  extension [A](self: Gen[A])
    def flatMap[B](f: A => Gen[B]): Gen[B] = ???

trait SGen[+A]