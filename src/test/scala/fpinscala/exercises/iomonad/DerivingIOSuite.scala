package fpinscala.exercises.iomonad

import munit.FunSuite

class DerivingIOSuite extends FunSuite:
	import Free.{Return, Suspend, FlatMap}

	enum Ask[A]:
		case AskName extends Ask[String]
	
	val M: Monad[[X] =>> Free[Ask, X]] = summon
	
	type Id[A] = A
	given Monad[Id] with
		def unit[A](a: => A): Id[A] = a
		extension [A](fa: Id[A])
			override def flatMap[B](f: A => Id[B]): Id[B] = f(fa)
	
	val askToId: [X] => Ask[X] => Id[X] = [X] => (a: Ask[X]) =>
		a match
			case Ask.AskName => "Bob"
		

	test("freeMonad: associativity, flatMap") {
		val m: Free[Ask, Int] = Suspend(Ask.AskName).flatMap(_ => Return(1))
		val f: Int => Free[Ask, Int] = i => Return(i + 1)
		val g: Int => Free[Ask, Int] = i => Return(i * 10)
		
		val lhs: Id[Int] = 
			M.flatMap(M.flatMap(m)(f))(g).runFree[Id](askToId)
		val rhs: Id[Int] =
			M.flatMap(m)(i => M.flatMap(f(i))(g)).runFree[Id](askToId)
		assertEquals(lhs, rhs)			
	}

	test("runTrampline: exception") {
		val log = scala.collection.mutable.ArrayBuffer.empty[String]
		val io: Free[Function0, Int] = 
			Suspend ( () => { log += "1"; 1 } )
				.flatMap(_ => Suspend ( () => { log += "2"; throw new RuntimeException("fail") } ) )
				.flatMap((x: Int) => Suspend ( () => { log += "3"; x } ) )
		intercept[RuntimeException](io.runTrampoline)
		assertEquals(log.toList, List("1", "2"))
	}