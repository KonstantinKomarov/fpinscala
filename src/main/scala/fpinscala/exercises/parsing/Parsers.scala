package fpinscala.exercises.parsing

trait Parsers[Parser[+_]]:
  
  def string(s: String): Parser[String]

  def char(c: Char): Parser[Char] = 
    string(c.toString).map(_.charAt(0))

  def succeed[A](a: A): Parser[A]

  extension [A](p: Parser[A])
    def map[B](f: A => B): Parser[B] =
      p.flatMap(f andThen succeed)

    def flatMap[B](f: A => Parser[B]): Parser[B]

    infix def or(p2: => Parser[A]): Parser[A]
    def |(p2: => Parser[A]): Parser[A] = p.or(p2)

    def listOfN(n: Int): Parser[List[A]] = 
      if n <= 0 then succeed(Nil)
      else p.map2(p.listOfN(n - 1))(_ :: _)
    
    def many: Parser[List[A]] =
      p.map2(p.many)(_ :: _) | succeed(Nil)

    def product[B](p2: => Parser[B]): Parser[(A, B)] = 
      p.flatMap(a => p2.map(b => (a, b)))

    def map2[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] = 
      p.product(p2).map((a, b) => f(a, b))

    def slice: Parser[String]

    def many1: Parser[List[A]] = 
      p.map2(p.many)(_ :: _)
    
    def **[B](p2: Parser[B]): Parser[(A, B)] = p.product(p2)

  case class ParserOps[A](p: Parser[A])

  object Laws

case class Location(input: String, offset: Int = 0):

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  def remaining: String = ???

  def slice(n: Int) = input.substring(offset, offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.linesIterator.drop(line-1).next()
    else ""

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()):
  def push(loc: Location, msg: String): ParseError = ???

  def label(s: String): ParseError = ???

class Examples[Parser[+_]](P: Parsers[Parser]):
  import P.*

  val nonNegativeInt: Parser[Int] = ???

  val nConsecutiveAs: Parser[Int] = ???
