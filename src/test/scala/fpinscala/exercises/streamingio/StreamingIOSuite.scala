package fpinscala.exercise.streamingio

import munit.FunSuite

class StreamingIOSuite extends FunSuite:
	
	import java.nio.file.{Files, Path}
	def withTempFile[A](lines: List[String])(f: Path => A): A = 
		val path = Files.createTempFile("lines", ".txt")
		try 
			Files.write(path, lines.mkString("\n").getBytes)
			f(path)
		finally Files.delete(path)

	import java.util.concurrent.Executors
	import fpinscala.answers.iomonad.IO
	def runIO[A](io: IO[A]): A = 
		val pool = Executors.newFixedThreadPool(4)
		try io.unsafeRunSync(pool)
		finally pool.shutdown()

	import fpinscala.exercises.streamingio.ImperativeAndLazyIO.*
	test("linesGt40k") {
		withTempFile(List.fill(40000)("x")) { path => 
			assertEquals(runIO(linesGt40k(path.toString)), false)
		}
		withTempFile(List.fill(40001)("x")) { path =>
			assertEquals(runIO(linesGt40k(path.toString)), true)
		}
	}