package fpinscala.exercises.streamingio.Proc

import munit.FunSuite

class ProcessSuite extends FunSuite:
	test("filter over take") {
		val evens = filter((x: Int) => x % 2 == 0)
		val first10 = take[Int](10)(LazyList.from(1))
		val result = evens(first10).toList
		assertEquals((result.length, result.forall(_ % 2 == 0)), (5, true))
	}

	test("count: count*count ") {
		val c1 = LazyList.from(1)
		val c2 = lift((_: Unit) => 1)(LazyList.continually(()))
		val amount = 400
		assertEquals(
			take[Int](amount)(count[Int](c1)).toList, 
			take[Int](amount)(count[Int](c2)).toList
		)
		assertEquals(
			take[Int](amount)(count0[Int](c1)).toList, 
			take[Int](amount)(count0[Int](c2)).toList
		)
	}