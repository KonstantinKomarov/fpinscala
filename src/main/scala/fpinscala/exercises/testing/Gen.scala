package fpinscala.exercises.testing

import fpinscala.exercises.state.*
import fpinscala.exercises.parallelism.*
import fpinscala.exercises.parallelism.Par.Par
import Gen.*
import Prop.*
import java.util.concurrent.{Executors,ExecutorService}

import Prop.Result.{Passed, Falsified, Proved}

opaque type Prop = (MaxSize, TestCases, RNG) => Result

object Prop:
  opaque type SuccessCount = Int
  object SuccessCount:
    extension(x: SuccessCount) def toInt: Int = x
    def fromInt(x: Int): SuccessCount = x

  opaque type TestCases = Int
  object TestCases:
    extension(x: TestCases) def toInt: Int = x
    def fromInt(x: Int): TestCases = x

  opaque type MaxSize = Int
  object MaxSize:
    extension(x: MaxSize) def toInt: Int = x
    def fromInt(x: Int): MaxSize = x
  
  opaque type FailedCase = String
  object FailedCase:
    extension(f: FailedCase) def toString: String = f
    def fromString(f: String): FailedCase = f
  
  enum Result:
    case Passed
    case Falsified(failure: FailedCase, successes: SuccessCount)
    case Proved

    def isFalsified: Boolean = this match
      case Falsified(_, _) => true
      case _1              => false 
    
  def apply(f: (TestCases, RNG) => Result): Prop = 
    (_, n, rng) => f(n, rng)

  extension [A](self: Prop)
    def &&(that: Prop): Prop = 
      (max, n, rng) => self.tag("and-left")(max, n, rng) match
        case Passed | Proved => that.tag("and-right")(max, n, rng)
        case x => x

    def ||(that: Prop): Prop = 
      (max, n, rng) => self.tag("or-left")(max, n, rng) match
        case Falsified(msg, _) => 
          that.tag("or-right").tag(msg.toString)(max, n, rng)
        case x => x
      
    def tag(msg: String): Prop = 
      (max, n, rng) => self(max, n, rng) match
        case Falsified(e, c) => Falsified(FailedCase.fromString(s"$msg($e)"), c)
        case x => x

    def check(
        maxSize: MaxSize = 100,
        testCases: TestCases = 100,
        rng: RNG = RNG.Simple(System.currentTimeMillis)
    ): Result =
      self(maxSize, testCases, rng)

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = 
    (max, n, rng) => {
      @annotation.tailrec
      def loop(i: Int, r: RNG, successes: SuccessCount): Result = 
        if i >= n.toInt then Passed
        else
          val (a, r2) = gen.run(r)
          if f(a) then loop(i + 1, r2, SuccessCount.fromInt(successes.toInt + 1))
          else Falsified(
            FailedCase.fromString(s"property failed for $a"),
            successes
          )
      loop(0, rng, SuccessCount.fromInt(0))
    }

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

  def unit[A](a: => A): Gen[A] = 
    State { (rng: RNG) => (a, rng) }

  def boolean: Gen[Boolean] =
    State{ (rng: RNG) =>
      val (n, rng2) = rng.nextInt
      (n % 2 == 0, rng2)
    }
  
  extension [A](self: Gen[A])
    def listOfN(n: Int): Gen[List[A]] =
      State { (rng: RNG) =>
        val (list, rng2) = (0 until n).foldLeft((List.empty[A], rng)) {
          case ((acc, r), _) =>
            val (a, r2) = self.run(r)
            (a :: acc, r2)
        }
        (list.reverse, rng2)
      }

    def flatMap[B](f: A => Gen[B]): Gen[B] = 
      State { (rng: RNG) => 
        val (a, rng2) = self.run(rng)
        f(a).run(rng2)
      }

    def flatMapViaStateFlatMap[B](f: A => Gen[B]): Gen[B] =
      State.flatMap(self)(f)

    def listOfN(size: Gen[Int]): Gen[List[A]] = 
      size.flatMap(n => self.listOfN(n))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = 
      boolean.flatMap(b => if b then g1 else g2)
  
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = 
    val g1Grade = g1._2.abs / (g1._2.abs + g2._2.abs)
    State(RNG.double).flatMap(d => if d < g1Grade then g1._1 else g2._1)
  
  extension [A](self: Gen[A])
    def unsized: SGen[A] = SGen(_ => self)

    def list(n: Int): Gen[List[A]] = self.listOfN(n)

    def nonEmptyList(n: Int): Gen[List[A]] = 
      self.listOfN(n max 1)

  def listOf[A](g: Gen[A]): SGen[List[A]] = 
    SGen(n => g.listOfN(n))

  def genIntList: Gen[List[Int]] = 
    Gen.choose(1, 10).flatMap(n => Gen.choose(-1000, 1000).listOfN(n))

  def lengthPreserved: Prop = 
    Prop.forAll(genIntList)(list => list.sorted.length == list.length)
  
  def elementsPreserved: Prop = 
    Prop.forAll(genIntList)(list => list.sorted.sorted == list.sorted)
  
  def isOrdered: Prop = 
    Prop.forAll(genIntList) { list =>
      list.sorted.sliding(2).forall {
        case List(a, b) => a <= b
        case _          => true
      }
    }
  
  def sortedProp: Prop = lengthPreserved && elementsPreserved && isOrdered

end Gen

opaque type SGen[+A] = Int => Gen[A]

object SGen:
  def apply[A](f: Int => Gen[A]): SGen[A] = f

  extension [A](self: SGen[A])
    def apply(n: Int): Gen[A] = self(n)

    def map[B](f: A => B): SGen[B] =
      SGen(n => self(n).map(f))
    
    def flatMap[B](f: A => SGen[B]): SGen[B] =
      SGen { n => 
        self(n).flatMap { a =>
          f(a)(n)
        }
      }
    
    // def listOf(g: Gen[A]): SGen[List[A]] = 
    //   SGen(n => g.listOfN(n))