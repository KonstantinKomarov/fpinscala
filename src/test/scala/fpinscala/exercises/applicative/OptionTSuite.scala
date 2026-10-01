package fpinscala.answers.applicative

import munit.FunSuite

class OptionTSuite extends FunSuite:
	given Monad[Option] with
		def unit[A](a: => A): Option[A] = Some(a)
		extension [A](fa: Option[A])
			override def flatMap[B](f: A => Option[B]): Option[B] = fa.flatMap(f)
	
	given Monad[List] with
		def unit[A](a: => A): List[A] = List(a)
		extension [A](fa: List[A])
			override def flatMap[B](f: A => List[B]): List[B] = fa.flatMap(f)
	
	type OT[A] = OptionT[Option, A]
	type LT[A] = OptionT[List, A]

	val MO: Monad[OT] = summon
	val ML: Monad[LT] = summon

	test("unit") {
		val i = 42
		assertEquals(MO.unit(i).value, Some(Some(i)))
		assertEquals(ML.unit(i).value, List(Some(i)))
	} 

	test("List:") {
		import fpinscala.exercises.state.State

		val nextNat: State[Int, Int] = for {
			_ <- State.modify[Int](_ + 1)
			n <- State.get[Int]
		} yield n
		
		val l: List[Option[Int]] = List(Some(1), None, Some(3))
		val m: LT[Int] = OptionT(l) // Some(nextNat.replicateA(3).run(10).value._1)
		val f: Int => LT[Int] = i => OptionT(List(Some(i), Some(i * 10)))
		assertEquals(
			ML.flatMap(m)(f).value,
			l.map { a => a match {
				case None => List(None)
				case Some(a) => List(Some(a), Some(a * 10))
			}}
			.flatten
			// List(Some(1), Some(10), None, Some(3), Some(30))
		)
	}