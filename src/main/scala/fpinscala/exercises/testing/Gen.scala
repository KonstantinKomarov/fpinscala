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

  def forAll[A](eg: ExhaustiveGen[A])(f: A => Boolean): Prop = 
    (max, n, rng) =>
      eg.domain match
        case Some(list) =>
          @annotation.tailrec
          def loop(xs: LazyList[A], successes: SuccessCount): Result = 
            if xs.isEmpty then Proved
            else
              val x = xs.head
              val rest = xs.tail
                if f(x) then loop(rest, SuccessCount.fromInt(successes.toInt - 1))
                else Falsified(FailedCase.fromString(s"property failed for $x"), successes)
          loop(list, SuccessCount.fromInt(0))
        
        case None =>
          @annotation.tailrec
          def loop(i: Int, r: RNG, successes: SuccessCount): Result =
            if i >= n.toInt then Passed
            else
              val (a, r2) = eg.gen.next(r)
              if f(a) then loop(i + 1, r2, SuccessCount.fromInt(successes.toInt + 1))
              else Falsified(FailedCase.fromString(s"property failed for $a"), successes)
          loop(0, rng, SuccessCount.fromInt(0))
  def forAllSized[A](g: SGen[A])(f: A => Boolean): Prop = 
    (max, _, rng) => 
      @annotation.tailrec
      def loop(size: Int, r: RNG, successes: SuccessCount): Result = 
        if size > max.toInt then Proved
        else
          val (a, r2) = g(size).next(r)
          if f(a) then loop(size + 1, r2, SuccessCount.fromInt(successes.toInt + 1))
          else Falsified(
            FailedCase.fromString(s"property failed for size=$size, value=$a"),
            successes
          )
      loop(0, rng, SuccessCount.fromInt(0))

end Prop

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

  def booleanExhaustive: ExhaustiveGen[Boolean] = 
    ExhaustiveGen(boolean,Some(LazyList(true, false)))
  
  def chooseExhaustive(start: Int, stopExclusive: Int): ExhaustiveGen[Int] = {
    require(start < stopExclusive)
    val domain =
      if stopExclusive - start <= 256 then
        Some(LazyList.range(start, stopExclusive))
      else None
    ExhaustiveGen(choose(start, stopExclusive), domain)
  }

  extension [A](self: Gen[A])
    def toExhaustive: ExhaustiveGen[A] = ExhaustiveGen(self, None)

    def map[B](f: A => B): Gen[B] = 
      self.flatMap(a => Gen.unit(f(a)))

  def parInt: SGen[Par[Int]] =
    SGen { size => 
      if size <= 0 then
        Gen.choose(-100, 100).map(Par.unit)
      else
        val sub = parInt(size / 2)
        Gen.choose(0, 3).flatMap {
          case 0 => Gen.choose(-100, 100).map(Par.unit)
          case 1 => sub.map(p => Par.map(p)(_ + 1))
          case 2 => for { p1 <- sub; p2 <- sub } yield p1.map2(p2)( _ + _)
          case 3 => sub.map(p => Par.fork(p))
        }
    }
  
  extension [A](self: Gen[A])
    def map2[B, C](other: Gen[B])(f: (A, B) => C): Gen[C] = 
      State.map2(self)(other)(f)

    @annotation.targetName("product")
    def **[B](gb: Gen[B]): Gen[(A, B)] = 
      map2(gb)((_, _))

  object `**`:
    def unapply[A, B](p: (A, B)) = Some(p)

end Gen

def forkProp: Prop = 
  Prop.forAllSized(parInt) { p => 
    val es = Executors.newCachedThreadPool()
    try
      val r1 = p.run(es).get()
      val r2 = Par.fork(p).run(es).get()
      r1 == r2
    finally
      es.shutdown()
  }

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


case class ExhaustiveGen[+A](gen: Gen[A], domain: Option[LazyList[A]])

opaque type Cogen[-A] = (A, RNG) => RNG

object Cogen:
  def apply[A](f: (A, RNG) => RNG): Cogen[A] = f
  extension [A](self: Cogen[A])
    def perturb(a: A, rng: RNG): RNG = self(a, rng)
  
  def fn[B](g: Gen[B]): Gen[() => B] = 
    g.map(b => () => b)
  
  def fn1[A, B](g: Gen[B])(using cogen: Cogen[A]): Gen[A => B] =
    State { (rng: RNG) =>
      val f: A => B = (a: A) =>
        val perturbed = cogen.perturb(a, rng)
        g.run(perturbed)._1
      (f, rng)
    }
  
  given Cogen[Int] = Cogen { (a, rng) =>
    val mixed = rng.nextInt._1 ^ a.hashCode
    RNG.Simple(mixed.toLong)
  }