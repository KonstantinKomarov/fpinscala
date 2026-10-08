package fpinscala.exercises.streamingio

import munit.FunSuite
import fpinscala.answers.monoids.Monoid
import SimplePulls.Pull
import SimplePulls.Pull.*
import SimplePulls.Pull.{Result, Output, FlatMap}

class SimplePullsSuite extends FunSuite:
	
	// def fromList[O](xs: List[O]): Pull[O, Unit] = 
	// 	xs match
	// 		case Nil			=> Result(())
	// 		case h :: t 	=> Output(h) >> fromList(t)

	def run[O, R](p: Pull[O, R]): (R, List[O]) = 
		val (r, bldr) = p.fold(List.newBuilder[O])((b, o) => b += o)
		(r, bldr.result())
		
	test("drop: save final countdown") {
		val p: Pull[Int, String] = fromList(List(3, 2, 1)) >> Result("done")
		val (r, outs) = run(p.drop(1))
		assertEquals(r, "done")
		assertEquals(outs, List(2, 1))
	}

	test("takeWhile: takes prefix, returns reminder with first non-complient") {
		val p = fromList(List(1, 2, 3, 4, 5, 1, 2))
		val tw = p.takeWhile(_ < 5)
		val (remaining, outs) = run(tw)
		assertEquals(outs, List(1, 2, 3, 4))
		assertEquals(remaining.toList, List(5, 1, 2))
	}

	test("dropWhile, takeWhile together") {
		val xs = List(2, 4, 6, 1, 3, 5, 8)
		val p = fromList(xs)
		val (taken, prefix) = run(p.takeWhile(_ % 2 == 0))
		val (rest, _)				= run(p.dropWhile(_ % 2 == 0))
		assertEquals(prefix, List(2, 4, 6))
		assertEquals(taken.toList, List(1, 3, 5, 8))
		assertEquals(rest.toList, List(1, 3, 5, 8))
	}

	import fpinscala.answers.monoids.Monoid.*

	// val listConcat: Monoid[List[Int]] = new Monoid[List[Int]]:
	// 	def combine(a: List[Int], b: List[Int]): List[Int] = a ++ b
	// 	def empty: List[Int] = Nil

	test("tally: with monoid on list collects lists") {
		val p = fromList(List(1, 2, 3))
		val t = p.mapOutput(x => List(x)).tally[List[Int]](using listMonoid)
		assertEquals(t.toList, List(List(1), List(1, 2), List(1, 2, 3)))
	}

	test("tallyViaMapAccumulate: equals tally") {
		val l = List("x", "y", "z")
		val xs = fromList(l)
		val a = xs.tally[String](using stringMonoid).toList
		val b = xs.tallyViaMapAccumulate[String](using stringMonoid).toList
		assertEquals(a, b)
		assertEquals(b, l.scanLeft("")(_ ++ _).tail)
	}