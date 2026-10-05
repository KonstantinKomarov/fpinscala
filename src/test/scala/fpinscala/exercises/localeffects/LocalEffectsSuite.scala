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

  test("qs"){
    val prog: RunnableST[(Int, List[Int])] = new RunnableST[(Int, List[Int])]:
      def apply[S] = for 
        arr <- STArray.fromList[S, Int](List(3, 1, 4, 1, 5, 9, 2, 6))
        sz  <- arr.size
        p   <- Immutable.partition(arr, 0, sz - 1, sz / 2 )
        xs  <- arr.freeze
      yield (p, xs)
    
    val (p, xs) = ST.run[(Int, List[Int])]([s] => () => prog.apply[s])
    assertEquals(xs(p), 4)
    assert(xs.take(p).forall(_ < 4), s"left side: ${xs.take(p)}")
    assert(xs.drop(p + 1).forall(_ >= 4), s"right side: ${xs.drop(p + 1)}")
  }

  test("histogram") {
    val words = List("a", "b", "a", "c", "b", "a")
    val prog: RunnableST[Map[String, Int]] = 
      new RunnableST[Map[String, Int]]:
        def apply[S] = for
          m <- STMap.empty[S, String, Int]
          _ <- words.foldLeft(ST[S, Unit](())) { (acc, w) =>
            acc.flatMap( _ =>
              m.read(w).flatMap {
                case Some(n) => m.write(w, n + 1)
                case None    => m.write(w, 1)
              }
            )
          }
          x <- m.freeze
        yield x
    assertEquals(run(prog), Map("a" -> 3, "b" -> 2, "c" -> 1))
  }

class LocalEffectsSuite extends FunSuite:
  test("1") {
    assertEquals(1, 2)
  }
