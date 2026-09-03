package fpinscala.exercises.datastructures

/** `List` data type, parameterized on a type, `A`. */
enum List[+A]:
  /** A `List` data constructor representing the empty list. */
  case Nil
  /** Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
    which may be `Nil` or another `Cons`.
   */
  case Cons(head: A, tail: List[A])

object List: // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.

  def product(doubles: List[Double]): Double = doubles match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn // Scala gives a hint here via a warning, so let's disable that
  val result = List(1,2,3,4,5) match
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

  def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B = // Utility functions
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x,y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(ns, 1.0, _ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = 
    l match {
      case Nil => sys.error("")
      case Cons(_, t) => t
    }

  def setHead[A](l: List[A], h: A): List[A] = 
    l match {
      case Nil => sys.error("")
      case Cons(_, t) => Cons(h, t)
    }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = 
    if n <= 0 then l
    else l match
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
  
  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = 
    l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  def init[A](l: List[A]): List[A] = 
    init2(l)

  def init2[A](l: List[A]): List[A] = 
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def build(cur: List[A]): List[A] = cur match
      case Nil => sys.error("init2 by empty list")
      case Cons(_, Nil) => List(buf.toList*)
      case Cons(head, tail) => buf += head; build(tail)
    build(l)

  def init1[A](l: List[A]): List[A] = 
    l match
      case Nil => sys.error("init of emtpy list")
      case Cons(_, Nil) => Nil
      case Cons(head, tail) => Cons(head, init(tail))
  
  def length[A](l: List[A]): Int = 
    foldRight(l, 0, (_, acc) => acc + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], acc: B, f: (B, A) => B): B = l match
    case Nil => acc
    case Cons(head, tail) => foldLeft(tail, f(acc, head), f)

  def sumViaFoldLeft(ns: List[Int]): Int = 
    foldLeft(ns, 0, (acc, a) => acc + a)

  def productViaFoldLeft(ns: List[Double]): Double = 
    foldLeft(ns, 1.0, (acc, a) => acc * a)

  def lengthViaFoldLeft[A](l: List[A]): Int = 
    foldLeft(l, 0, (acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] = 
    foldLeft(l, List[A](), (acc, a) => Cons(a, acc))

  def foldLeftViaFoldRight[A, B](l: List[A], acc: B, f: (B, A) => B): B = 
    foldRight(l, (b: B) => b, (i, g) => b => g(f(b, i)))(acc)
  
  def foldRightViaFoldLeft[A, B](l: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(l, (b: B) => b, (g, i) => b => g(f(i, b)))(acc)

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = 
    foldRight(l, r, Cons(_, _))

  def appendViaFoldLeft[A](l: List[A], r: List[A]): List[A] = 
    foldLeft(l, (lst: List[A]) => lst, (accF, a) => (tail: List[A]) => accF(Cons(a, tail)))(r)

  def concat[A](l: List[List[A]]): List[A] = 
    foldRight(l, Nil: List[A], append)

  def concatViaFoldLeft[A](l: List[List[A]]): List[A] = 
    foldLeft(l, (xs: List[A]) => xs, (accF, xs) => (tail: List[A]) => accF(append(xs, tail)))(Nil: List[A])

  def incrementEach(l: List[Int]): List[Int] = 
    foldRight(l, Nil: List[Int], (i, acc) => Cons(i + 1, acc))

  def doubleToString(l: List[Double]): List[String] = 
    foldLeft(l, (xs: List[String]) => xs, (accF, d) => (tail: List[String]) => accF(Cons(d.toString, tail)))(Nil)

  def map[A,B](l: List[A], f: A => B): List[B] = 
    foldRight(l, Nil: List[B], (i, acc) => Cons(f(i), acc))
  
  def mapViaFoldLeft[A,B](l: List[A], f: A => B): List[B] =  
    foldLeft(l, (xs: List[B]) => xs, (accF, a) => (tail: List[B]) => accF(Cons(f(a), tail)))(Nil)

  def filter[A](as: List[A], f: A => Boolean): List[A] = 
    foldRight(as, Nil: List[A], (i, acc) => if f(i) then Cons(i, acc) else acc)
  
  def filterViaFoldLeft[A](as: List[A], f: A => Boolean): List[A] = 
    foldLeft(as, (xs: List[A]) => xs, (accF, a) =>
      if f(a) then (tail: List[A]) => accF(Cons(a, tail))
      else accF
    )(Nil)

  def flatMap[A,B](as: List[A], f: A => List[B]): List[B] = 
    foldLeft(as, (xs: List[B]) => xs,
      (accF, a) => (tail: List[B]) => accF(append(f(a), tail)) 
    )(Nil)

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] = 
    flatMap(as, a => if f(a) then List(a) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = 
    foldLeft(
      a,
      ((xs: List[Int]) => xs, b),
      {
        case ((accF, restB), x) => 
          restB match {
            case Nil => (accF, Nil)
            case Cons(y, ys) =>
              ((tail: List[Int]) => accF(Cons(x + y, tail)), ys)
        }
      }
    )._1(Nil)

  // def zipWith - TODO determine signature
  def zipWith[A, B, C](a: List[A], b: List[B], f: (A, B) => C): List[C] = 
    List.foldLeft(
      a,
      ((xs: List[C]) => xs, b),
      {
        case ((accF, restB), x) =>
          restB match {
            case Nil => (accF, Nil)
            case Cons(y, ys) =>
              ((tail: List[C]) => accF(Cons(f(x, y), tail)), ys)
          }
      }
    )._1(Nil)

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = 
    @annotation.tailrec
    def check(supRem: List[A], subRem: List[A]): Boolean = 
      (supRem, subRem) match {
        case (_, Nil) => true
        case (Nil, _) => false
        case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => check(t1, t2)
        case (Cons(h1, t1), _) => check(t1, subRem) 
      }
    check(sup, sub)