package fpinscala.exercises.localeffects

import munit.FunSuite

class STArraySuite extends FunSuite:
  trait RunnableST[A] {
    def apply[S]: ST[S, A]
  }
  
  def run[A](r: RunnableST[A]): A = 
    ST.run[A]([s] => () => r.apply[s])
  
  test("fill") {
    val prog: RunnableST[List[Int]] = new RunnableST[List[Int]] {
      def apply[S] = for {
        arr <- STArray.fromList[S, Int](List.fill(5)(0))
        _   <- arr.fill(Map(0 -> 1, 2 -> 3))
        xs  <- arr.freeze
      } yield xs
    }
    assertEquals(run(prog), List(1, 0, 3, 0, 0))
  }

class LocalEffectsSuite extends FunSuite:
  test("1") {
    assertEquals(1, 2)
  }
