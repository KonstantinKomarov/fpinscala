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