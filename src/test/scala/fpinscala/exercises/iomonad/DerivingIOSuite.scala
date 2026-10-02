package fpinscala.exercises.iomonad

import munit.FunSuite

class DerivingIOSuite extends FunSuite:
	import Free.{Return, Suspend, FlatMap}

	enum Ask[A]:
		case AskName extends Ask[String]
	
	val M: Monad[[X] =>> Free[Ask, X]] = summon
				
	test("freeMonad: associativity, flatMap") {
		type Id[A] = A
		given Monad[Id] with
			def unit[A](a: => A): Id[A] = a
			extension [A](fa: Id[A])
				override def flatMap[B](f: A => Id[B]): Id[B] = f(fa)

		val askToId: [X] => Ask[X] => Id[X] = [X] => (a: Ask[X]) =>
			a match
				case Ask.AskName => "Bob"

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

	test("run vs runTrampoline") {
		given Monad[Function0] with
			def unit[A](a: => A): Function0[A] = () => a
			extension [A](fa: Function0[A])
				override def flatMap[B](f: A => Function0[B]): Function0[B] = 
					() => f(fa())()
		
		val prog: List[Free[Function0, Int]] = List(
			Return(42),
			Suspend(() => 1).flatMap(x => Return(x + 1)),
			Suspend(() => 1)
				.flatMap(x => Suspend(() => x + 1))
				.flatMap(y => Suspend(() => y * 10)),
			Suspend(() => 0)
				.flatMap(_ => Suspend(() => 0))
				.flatMap(_ => Return(7))
		)

		prog.foreach { io => 
			assertEquals(
				io.run(),
				io.runTrampoline,
				s"mismatch for prog: $io"
			)
		}
	}

	import IO3.{Console, Free}

	test("translate: saves flatMap's structure") {
		given Monad[Option] with
			def unit[A](a: => A): Option[A] = Some(a)
			extension [A](fa: Option[A])
				override def flatMap[B](f: A => Option[B]): Option[B] = fa.flatMap(f)

		val collected = scala.collection.mutable.ArrayBuffer.empty[String]
		val recordNat: [X] => Console[X] => Option[X] = [X] => (c: Console[X]) => c match
			case Console.ReadLine			=> Some(None)
			case Console.PrintLine(l)	=> collected += l; Some(())
		val prog: Free[Console, Unit] =
			Console.printLn("a")
				.flatMap(_ => Console.printLn("b"))
				.flatMap(_ => Console.printLn("c"))
		assertEquals(prog.translate[Option](recordNat).run, Some(()))
		assertEquals(collected.toList, List("a", "b", "c"))
	}

	test("unsafeRunConsole") {
		val origOut = System.out
		val baos = new java.io.ByteArrayOutputStream()
		System.setOut(new java.io.PrintStream(baos))
		try
			val prog: Free[Console, Unit] = 
				Console.printLn("hello")
					.flatMap(_ => Console.printLn("world"))
			prog.unsafeRunConsole
			assertEquals(baos.toString.trim, "hello\nworld")
		finally 
			System.setOut(origOut)
	}
