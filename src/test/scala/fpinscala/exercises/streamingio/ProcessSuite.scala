package fpinscala.exercises.streamingio.Proc

import munit.FunSuite

class ProcessSuite extends FunSuite:
	test("filter over take") {
		val evens = filter((x: Int) => x % 2 == 0)
		val first10 = take[Int](10)(LazyList.from(1))
		val result = evens(first10).toList
		assertEquals((result.length, result.forall(_ % 2 == 0)), (5, true))
	}