package fpinscala.exercises.applicative

import munit.FunSuite

class ComposeMSuite extends FunSuite:

	type OptList[A] = Option[List[A]]

	given Monad[Option] with
		def unit[A](a: => A): Option[A] = Some(a)
		extension [A](fa: Option[A])
			override def flatMap[B](f: A => Option[B]): Option[B] = fa.flatMap(f)
	
	given Monad[List] with
		def unit[A](a: => A): List[A] = List(a)
		extension [A](fa: List[A])
			override def flatMap[B](f: A => List[B]): List[B] = fa.flatMap(f)

	val M: Monad[OptList] = Monad.composeM[Option, List]
	
	test("flatMap: traverse inner and concatinate") {
		val m: OptList[Int] = Some(List(1, 2, 3))

		val f: Int => OptList[Int] = 
			i => Some(List(i + 1, i * 100))
		
		assertEquals(
			M.flatMap(m)(f),
			m.map{ lst => lst.map { i => List(i + 1, i * 100) }.flatten }
		)
	}