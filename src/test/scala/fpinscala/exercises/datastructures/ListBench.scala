//> using dep org.openjdk.jmh:jmh-core:1.37
//> using dep org.openjdk.jmh:jmh-generator-annprocess:1.37
//> using jmh

package fpinscala.exercises.datastructures

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import fpinscala.exercises.datastructures.List.{Cons, Nil}
import scala.util.Random

@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class ListBenchmark {

  val size = 10000
  val half = size / 2
  val seed = 42L
  private val rng = new Random(seed)

  // ---- генераторы ----
  private def genIntList(n: Int): List[Int] = {
    @annotation.tailrec
    def loop(i: Int, acc: List[Int]): List[Int] =
      if (i >= n) acc else loop(i + 1, Cons(rng.nextInt(), acc))
    loop(0, Nil)
  }

  private def genDoubleList(n: Int): List[Double] = {
    @annotation.tailrec
    def loop(i: Int, acc: List[Double]): List[Double] =
      if (i >= n) acc else loop(i + 1, Cons(rng.nextDouble(), acc))
    loop(0, Nil)
  }

  private def genListOfLists(num: Int, len: Int): List[List[Int]] = {
    @annotation.tailrec
    def loop(i: Int, acc: List[List[Int]]): List[List[Int]] =
      if (i >= num) acc else loop(i + 1, Cons(genIntList(len), acc))
    loop(0, Nil)
  }

  // ---- данные ----
  val intList: List[Int] = genIntList(size)
  val intList2: List[Int] = genIntList(half)
  val doubleList: List[Double] = genDoubleList(size)
  val listOfLists: List[List[Int]] = genListOfLists(100, 100)

  // ---- бенчмарки (все из ListSuite) ----

  @Benchmark def tail(): List[Int] = List.tail(intList)
  @Benchmark def setHead(): List[Int] = List.setHead(intList, 0)
  @Benchmark def drop(): List[Int] = List.drop(intList, half)
  @Benchmark def dropWhile(): List[Int] = List.dropWhile(intList, _ <= half)
  @Benchmark def init(): List[Int] = List.init(intList)
  @Benchmark def length(): Int = List.length(intList)
  @Benchmark def foldLeft(): Int = List.foldLeft(intList, 0, _ + _)   // ← исправлено: числовой
  @Benchmark def sumViaFoldLeft(): Int = List.sumViaFoldLeft(intList)
  @Benchmark def productViaFoldLeft(): Double = List.productViaFoldLeft(doubleList)
  @Benchmark def lengthViaFoldLeft(): Int = List.lengthViaFoldLeft(intList)
  @Benchmark def reverse(): List[Int] = List.reverse(intList)

  @Benchmark def foldLeftViaFoldRight(): String = {
    val strList = List.doubleToString(doubleList)
    List.foldRightViaFoldLeft(strList, "!", _ + _)
  }

  @Benchmark def appendViaFoldRight(): List[Int] =
    List.appendViaFoldRight(intList, intList2)
  @Benchmark def appendViaFoldLeft(): List[Int] =
    List.appendViaFoldLeft(intList, intList2)

  @Benchmark def concat(): List[Int] = List.concat(listOfLists)
  @Benchmark def concatViaFoldLeft(): List[Int] = List.concatViaFoldLeft(listOfLists)

  @Benchmark def incrementEach(): List[Int] = List.incrementEach(intList)
  @Benchmark def doubleToString(): List[String] = List.doubleToString(doubleList)

  @Benchmark def map(): List[Int] = List.map(intList, _ * 2)
  @Benchmark def mapViaFoldLeft(): List[Int] = List.mapViaFoldLeft(intList, _ * 2)

  @Benchmark def filter(): List[Int] = List.filter(intList, _ % 2 == 0)
  @Benchmark def filterViaFoldLeft(): List[Int] = List.filterViaFoldLeft(intList, _ % 2 == 0)

  @Benchmark def flatMap(): List[Int] = List.flatMap(intList, a => List(a, a))
  @Benchmark def filterViaFlatMap(): List[Int] = List.filterViaFlatMap(intList, _ % 2 == 0)

  @Benchmark def addPairwise(): List[Int] = List.addPairwise(intList, intList2)
  @Benchmark def zipWith(): List[Int] = List.zipWith(intList, intList2, _ * _)

  @Benchmark def hasSubsequence(): Boolean = List.hasSubsequence(intList, intList2)

  // (опционально) если нужен строковый foldLeft для сравнения:
  @Benchmark def foldLeftString(): String =
    List.foldLeft(intList, "", _ + _.toString)   // будет медленным
}