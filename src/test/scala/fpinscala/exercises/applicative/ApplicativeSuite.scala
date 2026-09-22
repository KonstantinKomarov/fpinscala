package fpinscala.exercises.applicative

import munit.FunSuite

type EitherString[X] = Either[String, X]
val eitherMonadInstance: Monad[EitherString] = new Monad[EitherString]:
	def unit[A](a: => A): EitherString[A] = Right(a)
	extension [A](eea: EitherString[A])
		override def flatMap[B](f: A => EitherString[B]): EitherString[B] = eea match
			case Right(a) => f(a)
			case Left(e) => Left(e) 

class ApplicativeSuite extends FunSuite:
	given optionApplicative: Applicative[Option] with
		def unit[A](a: => A): Option[A] = Some(a)
		override def apply[A, B](fab: Option[A => B])(fa: Option[A]): Option[B] =
			for 
				f <- fab
				a <- fa
			yield f(a)
	
	import optionApplicative.*

	test("sequence"):
		assertEquals(
			optionApplicative.sequence(
				List(Some(1), Some(2), Some(3))),
				Some(List(1, 2, 3))
			)
	
	test("replicateM"):
		assertEquals(
			optionApplicative.replicateM(3, Some(5)),
			Some(List(5, 5, 5))
		)
		assertEquals(optionApplicative.replicateM(3, None), None)
		assertEquals(optionApplicative.replicateM(0, (None: Option[Int])), Some(List.empty[Int]))
	
	test("product"):
		assertEquals(Some(1).product(Some("a")), Some(1, "a"))
		assertEquals((None: Option[Int]).product(Some("a")), None)
	
	trait AppA[F[_]]:
		def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
		def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = 
			map2(fab, fa)((f, a) => f(a))
	given AppA[Option] with
		def unit[A](a: => A): Option[A] = Some(a)
		def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] =
			for
				a <- fa
				b <- fb
			yield f(a, b)
		
	test("A.map2ViaApply == original map2"):
		val A = summon[AppA[Option]]
		def map2ViaApply[A2, B, C](fa: Option[A2], fb: Option[B])(f: (A2, B) => C): Option[C] =
			A.apply(
				A.apply(
					A.unit(
						(a: A2) => 
							(b: B) => 
								f(a, b)
					)
				)(fa)
			)(fb)
		assertEquals(
			map2ViaApply(Some(3), Some(4))(_ + _),
			A.map2(Some(3), Some(4))(_ + _)
		)
	
	trait AppB[F[_]]:
		def unit[A](a: => A): F[A]
		def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]
		def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = 
			apply(
				apply(
					unit(
						(a: A) => (b: B) => f(a, b)
					)
				)(fa)
			)(fb)
	given AppB[Option] with
		def unit[A](a: => A): Option[A] = Some(a)
		def apply[A, B](fab: Option[A => B])(fa: Option[A]): Option[B] = 
			for 
				f <- fab
				a <- fa
			yield f(a)

	test("B.applyViaMap2 == original apply"):
		val B = summon[AppB[Option]]
		def applyViaMap2[X, Y](fxy: Option[X => Y])(fx: Option[X]): Option[Y] =
			B.map2(fxy, fx)((f, x) => f(x))

	test("map3 with different types"):
		def calc(t: Option[Boolean]): Option[String] = 
			Some(2).map3(Some("abc"),	t)((n, s, b) =>
					if b then s * n else "not"
				)
 
		var t: Option[Boolean] = Some(true)
		assertEquals(calc(t), Some("abcabc"))

		t = Some(false)
		assertEquals(calc(t), Some("not"))

		t = None
		assertEquals(calc(t), None)

	test("map4"):
		assertEquals(
			Some(1).map4(Some(2), Some(3), Some(4))(_ + _ + _ + _),
			Some(10)
		)			

	test("sequence: infinit stream - infinit result"):
		// import lazyListApplicative.*
		val ones = LazyList.continually(1)
		val nats = LazyList.from(1)
		assertEquals(
			lazyListApplicative.sequence(List(ones, nats)).take(4).toList,
			List(List(1, 1), List(1, 2), List(1, 3), List(1, 4))
		)
	
	test("eitherMonad: laws"):
		val M = eitherMonadInstance
		val f: Int => EitherString[Int] = n =>
			if n > 0 then Right(n * 2) else Left(s"negative: $n")
		
		assertEquals(M.unit(5).flatMap(f), f(5))
	
	test("validationApplicative: collect errors"):
		import Validation.* 
		import Validation.given

		val v_1: Validation[String, Int] = Success(-1)
		val v0: Validation[String, Int] = Success(0)
		val v1: Validation[String, Int] = Success(1)

		assertEquals(v1.map2(v_1)(_ + _), v0)

def applicativeFromMonad[F[_]](M: Monad[F]): Applicative[F] = new Applicative[F]:
	def unit[A](a: => A): F[A] = M.unit(a)
	override def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = 
		M.flatMap(fab)(f => M.map(fa)(a => f(a)))
	extension [A](fa: F[A])
		override def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] =
			M.flatMap(fa)(a => M.map(fb)(b => f(a, b)))

import fpinscala.exercises.common.PropSuite
import fpinscala.exercises.monads.Monad
import fpinscala.answers.testing.exhaustive.Gen as tstGen
import fpinscala.answers.testing.exhaustive.Gen.**

class MonadIsApplicativeSuite extends PropSuite:
	val A: Applicative[Option] = applicativeFromMonad(Monad.optionMonad)
	import A.*

	private val genOptionInt: tstGen[Option[Int]] = 
		tstGen.boolean.flatMap(b =>
			if b then tstGen.unit(None: Option[Int])
			else tstGen.choose(-100, 100).map(Some(_))
		)
	
	private val genOptionFn: tstGen[Option[Int => Int]] = 
		tstGen.boolean.flatMap(b =>
			if b then tstGen.unit(None: Option[Int => Int])
			else tstGen.choose(0, 5).map { n =>
				Some((x: Int) => x + n)
			}
		)
	
	test("ApplicativeFromMonad: identity")(genOptionInt): v => 
		val id: Int => Int = identity
		assertEquals(apply(unit(identity))(v), v, "identity")
