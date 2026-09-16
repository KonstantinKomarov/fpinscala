package fpinscala.exercises.monoids

import fpinscala.exercises.parallelism.Nonblocking.*

trait Monoid[A]:
  def combine(a1: A, a2: A): A
  def empty: A

object Monoid:

  val stringMonoid: Monoid[String] = new:
    def combine(a1: String, a2: String) = a1 + a2
    val empty = ""

  def listMonoid[A]: Monoid[List[A]] = new:
    def combine(a1: List[A], a2: List[A]) = a1 ++ a2
    val empty = Nil

  lazy val intAddition: Monoid[Int] = new:
    def combine(a1: Int, a2: Int) = a1 + a2
    val empty = 0

  lazy val intMultiplication: Monoid[Int] = new:
    def combine(a1: Int, a2: Int) = a1 * a2
    val empty = 1

  lazy val booleanOr: Monoid[Boolean] = new:
    def combine(a1: Boolean, a2: Boolean) = a1 || a2
    val empty = false

  lazy val booleanAnd: Monoid[Boolean] = new:
    def combine(a1: Boolean, a2: Boolean) = a1 && a2
    val empty = true

  def optionMonoid[A]: Monoid[Option[A]] = new:
    def combine(a1: Option[A], a2: Option[A]) = a1 orElse a2
    val empty = None 

  def dual[A](m: Monoid[A]): Monoid[A] = new:
    def combine(x: A, y: A): A = m.combine(y, x)
    val empty = m.empty

  def endoMonoid[A]: Monoid[A => A] = new: 
    def combine(f1: A => A, f2: A => A) = f1 andThen f2
    val empty = identity

  import fpinscala.exercises.testing.{Prop, Gen}
  import Gen.`**`

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    val associative = Prop 
      .forAll(gen ** gen ** gen):
        case x ** y ** z =>
          m.combine(m.combine(x, y), z)  == m.combine(x, m.combine(y, z))
      .tag("associativity")
    val identity = Prop
      .forAll(gen):
        case x => 
          m.combine(m.empty, x) == x && m.combine(x, m.empty) == x 
      .tag("identity")
    associative && identity

  def combineAll[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.empty)(m.combine)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.empty)((acc, a) => m.combine(acc, f(a)))

  def foldRight[A, B](as: List[A])(acc: B)(f: (A, B) => B): B =
    foldMap(as, dual(endoMonoid))(f.curried)(acc)

  def foldLeft[A, B](as: List[A])(acc: B)(f: (B, A) => B): B =
    foldMap(as, endoMonoid)(a => b => f(b, a))(acc)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if as.isEmpty then m.empty
    else if as.length == 1 then f(as(0))
    else 
      val (l, r) = as.splitAt(as.length / 2)
      m.combine(foldMapV(l, m)(f), foldMapV(r, m)(f))

  import fpinscala.exercises.parallelism.Nonblocking.Par

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]]:
    def combine(pa: Par[A], pb: Par[A]): Par[A] = pa.map2(pb)(m.combine)
    def empty: Par[A] = Par.unit(m.empty)

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = 
    Par.parMap(v)(f).flatMap: bs =>
      foldMapV(bs, par(m))(b => Par.lazyUnit(b))

  case class Segment(isOrdered: Boolean, first: Int, last: Int)
  val optSegmentMonoid: Monoid[Option[Segment]] = new Monoid[Option[Segment]]:
    def combine(a: Option[Segment], b: Option[Segment]): Option[Segment] = 
      (a, b) match
        case (None, _) => a
        case (_, None) => b
        case (Some(a), Some(b)) =>
          Some(Segment(
            a.isOrdered && b.isOrdered && a.last <= b.first,
            a.first,
            b.last
          ))
    def empty: Option[Segment] = None

  def ordered(ints: IndexedSeq[Int]): Boolean =
    foldMapV(ints, optSegmentMonoid)(i => Some(Segment(true, i, i)))
    .forall(_.isOrdered)

  enum WC:
    case Stub(chars: String)
    case Part(lStub: String, words: Int, rStub: String)

  lazy val wcMonoid: Monoid[WC] = new:
    import WC.*
    def combine(a: WC, b: WC): WC = (a, b) match 
      case (Stub(c1), Stub(c2)) =>
        Stub(c1 + c2)
      case (Stub(c), Part(l, w, r)) =>
        Part(c + l, w, r)
      case (Part(l, w, r), Stub(c)) =>
        Part(l, w, r + c)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
        val commWords = if (r1 + l2).nonEmpty then 1 else 0
        Part(l1, w1 + w2 + commWords, r2)
    def empty: WC = Stub("")

  def count(s: String): Int = ???

  given productMonoid[A, B](using ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] with
    def combine(x: (A, B), y: (A, B)) = ???
    val empty = ???

  given functionMonoid[A, B](using mb: Monoid[B]): Monoid[A => B] with
    def combine(f: A => B, g: A => B) = ???
    val empty: A => B = a => ???

  given mapMergeMonoid[K, V](using mv: Monoid[V]): Monoid[Map[K, V]] with
    def combine(a: Map[K, V], b: Map[K, V]) = ???
    val empty = ???

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    ???

end Monoid
