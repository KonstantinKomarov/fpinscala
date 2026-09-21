package fpinscala.exercises.applicative

import munit.FunSuite

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
