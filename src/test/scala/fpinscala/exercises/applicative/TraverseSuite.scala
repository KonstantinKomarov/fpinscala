package fpinscala.exercises.applicative

import munit.FunSuite

class TraverseSuite extends FunSuite:
	given listTraverse: Traverse[List] with
		extension [A](fa: List[A])
			override def map[B](f: A => B): List[B] = fa.map(f)
			override def traverse[G[_]: Applicative, B](f: A => G[B]): G[List[B]] =
				val APP = summon[Applicative[G]]
				fa.foldRight(APP.unit(List.empty[B])) {
					(a, acc) => APP.map2(f(a))(acc)(_ :: _)
				}

	test("map") {
		val xs = List(1, 2, 3)
		assertEquals(listTraverse.map(xs)(_ * 10), List(10, 20, 30))
	}

	test("foldLeft, toList, reverse") {
		val lst = List(1, 2, 3)
		assertEquals(listTraverse.foldLeft(lst)(0)(_ + _), 6)
		assertEquals(listTraverse.foldLeft(lst)(List.empty[Int])((acc, a) => a :: acc), lst.reverse)

		assertEquals(listTraverse.toList(lst), lst)

		assertEquals(listTraverse.reverse(lst), lst.reverse)
	}

	private val optionApplicative: Applicative[Option] = new Applicative[Option]:
		def unit[A](a: => A): Option[A] = Some(a)
		override def apply[A, B](fab: Option[A => B])(fa: Option[A]): Option[B] =
			fab.flatMap(f => fa.map(f))
	
	private val listApplicative: Applicative[List] = new Applicative[List]:
		def unit[A](a: => A): List[A] = List(a)
		override def apply[A, B](fab: List[A => B])(fa: List[A]): List[B] =
			for {
				f <- fab
				a <- fa
			} yield f(a)

	test("fuse: Option and List") {
		val xs = List(1, 2, 3)
		val (o, l) = xs.fuse[Option, Option, Int](
			a => if a > 0 then Some(a) else None,
			a => Some(a * 10)
		)(using optionApplicative, optionApplicative)
		
		assertEquals(o, Some(List(1, 2, 3)))
		assertEquals(l, Some(List(10, 20, 30)))
	}