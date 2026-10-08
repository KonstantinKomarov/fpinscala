package fpinscala.exercises.streamingio.Proc

import munit.FunSuite

class ProcessSuite extends FunSuite:
	test("filter over take") {
		val evens = filter((x: Int) => x % 2 == 0)
		val first10 = take[Int](10)(LazyList.from(1))
		val result = evens(first10).toList
		assertEquals((result.length, result.forall(_ % 2 == 0)), (5, true))
	}

	test("drop + take = slice") {
		val ll = LazyList.from(1).take(6)
		val step1 = drop[Int](2)(ll)
		val step2 = take[Int](3)(step1)
		assertEquals(step2.toList, ll.drop(2).take(3).toList)
	}

	test("[take + drop]While") {
		val xs = LazyList(2, 4, 6, 1, 3, 5, 8)
		assertEquals(
			takeWhile[Int](_ % 2 == 0)(xs),
			xs.take(3)
		)
		assertEquals(
			dropWhile[Int](_ % 2 == 0)(xs),
			xs.drop(3)
		)
	}

	test("count: count*count ") {
		val c1 = LazyList.from(1)
		val c2 = lift((_: Unit) => 1)(LazyList.continually(()))
		val amount = 400
		assertEquals(
			take[Int](amount)(count[Int](c1)), 
			take[Int](amount)(count[Int](c2))
		)
		assertEquals(
			take[Int](amount)(count0[Int](c1)).toList, 
			take[Int](amount)(count0[Int](c2)).toList
		)
	}

	private def meanScanLeft(ll: LazyList[Double]): LazyList[Double] =
			ll.scanLeft((0.0, 0)) { (acc ,i) =>
				val (sum, n) = acc
				(sum + i, n + 1)
			}.tail.map { case (sum, n) => sum / n }

	test("mean") {
		val ll = LazyList.from(1).take(4).map((i: Int) => (i: Double))
		val expected = meanScanLeft(ll) 
		assertEquals(mean(ll), expected)
	}

	test("mean: over filter"){
		val evens = filter((x: Double) => x % 2 == 0)
		val ll 		= LazyList.from(1).take(6).map((i: Int) => (i: Double))
		val expected = meanScanLeft(
			ll.filter(x => x % 2 == 0)
		)
		assertEquals(mean(evens(ll)), expected)
	}