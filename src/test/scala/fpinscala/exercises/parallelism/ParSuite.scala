package fpinscala.exercises.parallelism

import java.util.concurrent.*
import fpinscala.answers.testing.exhaustive.Gen
import fpinscala.exercises.common.PropSuite
import Par.*

class ParSuite extends PropSuite {

  // Генератор небольших целых чисел (для индексов и значений)
  private val genSmallInt: Gen[Int] = Gen.choose(0, 10)

  // Один ExecutorService для всех тестов
  private val es: ExecutorService = Executors.newCachedThreadPool()

  // Генератор списка Par[Int] заданной длины
  private def genParList(n: Int): Gen[List[Par[Int]]] =
    Gen.listOfN(n, genSmallInt.map(unit))

  // Генератор для choiceN: индекс и список, где длина списка > индекс
  private val genChoiceN: Gen[(Par[Int], List[Par[Int]])] =
    for {
      idx   <- genSmallInt
      list  <- genParList(idx + 1)   // гарантируем, что индекс корректен
    } yield (unit(idx), list)

  test("Par.choiceN selects the correct element by index")(genChoiceN):
    case (pIdx, choices) =>
      val expected = choices(pIdx.run(es).get).run(es).get
      val result   = choiceN(pIdx)(choices).run(es).get
      assertEquals(result, expected)

  // Генератор для choice: булев Par и два Par[Int]
  private val genChoice: Gen[(Par[Boolean], Par[Int], Par[Int])] =
    for {
      b     <- Gen.boolean
      tVal  <- genSmallInt
      fVal  <- genSmallInt
    } yield (unit(b), unit(tVal), unit(fVal))

  test("Par.choice selects correct branch based on boolean")(genChoice):
    case (pBool, t, f) =>
      val expected = if (pBool.run(es).get) t.run(es).get else f.run(es).get
      val result   = choice(pBool)(t, f).run(es).get
      assertEquals(result, expected)

  // Освобождаем ресурсы после всех тестов (опционально)
  override def afterAll(): Unit =
    es.shutdown()
}