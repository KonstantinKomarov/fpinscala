package fpinscala.exercises.laziness

import LazyList.*

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] = 
    @annotation.tailrec
    def build(ll: LazyList[A], acc: List[A]): List[A] = ll match
      case Cons(h, t) => build(t(), h() :: acc)
      case Empty => acc.reverse
    build(this, Nil)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty

  @annotation.tailrec
  final def drop(n: Int): LazyList[A] = this match
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  
  def takeWhile(p: A => Boolean): LazyList[A] = this match
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty

  def forAll(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) && b)

  def takeWhile_1(p: A => Boolean): LazyList[A] = 
    foldRight(empty)((a, acc) => if p(a) then cons(a, acc) else empty)

  def headOption: Option[A] = 
    foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): LazyList[B] = 
    foldRight(empty[B])((a, acc) => cons(f(a), acc))
  
  def filter(f: A=> Boolean): LazyList[A] = 
    foldRight(empty[A])((a, acc) => if f(a) then cons(a, acc) else acc)

  def append[A2>:A](other: LazyList[A2]): LazyList[A2] = 
    foldRight(other)((a, acc) => cons(a, acc))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] = 
    foldRight(empty[B])((a, acc) => f(a).append(acc))

  def mapViaUnfold[B](f: A => B): LazyList[B] = 
    unfold(this):
      case Cons(h, t) => Some((f(h()), t()))
      case Empty => None
  
  def takeViaUnfold(n: Int): LazyList[A] = 
    unfold((this, n)):
      case (Cons(h, t), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n - 1)))
      case _ => None
  
  def takeWhileViaUnfold(p: A => Boolean): LazyList[A] =
    unfold(this):
      case(Cons(h, t)) if p(h()) => Some((h(), t()))
      case _ => None
  
  def zipAll[B](other: LazyList[B]): LazyList[(Option[A], Option[B])] =
    unfold((this, other)):
      case (Empty, Empty) => None
      case (Cons(h1, t1), Empty) => Some((Some(h1()) -> None) -> (t1() -> Empty))
      case (Empty, Cons(h2, t2)) => Some((None -> Some(h2())) -> (Empty -> t2()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()) -> Some(h2())) -> (t1() -> t2()))

  def zipWith[B, C](other: LazyList[B])(f: (A, B) => C): LazyList[C] =
    unfold((this, other)):
      case (Cons(h1, t1), Cons(h2, t2)) => 
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None

  def zipWithAll[B, C](other: LazyList[B])(f: (Option[A], Option[B]) => C): LazyList[C] = 
    unfold((this, other)):
      case (Empty, Empty) => None
      case (Cons(h1, t1), Empty) => Some(f(Some(h1()), Option.empty[B]) -> (t1() -> empty[B]))
      case (Empty, Cons(h2, t2)) => Some(f(Option.empty[A], Some(h2())) -> (empty[A] -> t2()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2())) 

  def zipAllViaZipWithAll[B](ll: LazyList[B]): LazyList[(Option[A], Option[B])] =
    zipWithAll(ll)((_,_))

  def startsWith[B](pref: LazyList[B]): Boolean = 
    zipAll(pref).takeWhile(_(1).isDefined).forAll((a, b) => a == b)

  def tails: LazyList[LazyList[A]] = 
    unfold(this):
      case Empty => None
      case Cons(h, t) => Some((Cons(h, t), t()))
    .append(LazyList(empty))

  def hasSubsequence[A](s: LazyList[A]): Boolean =
    tails.exists(_.startsWith(s))

  def scanRight[B](init: B)(f: (A, => B) => B): LazyList[B] = 
    foldRight((init, LazyList(init))): (a, acc) =>
      val prevAcc = acc
      val newAcc = f(a, prevAcc._1)
      (newAcc, cons(newAcc, prevAcc._2))
    ._2

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = 
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty 
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] = 
    lazy val single: LazyList[A] = LazyList.cons(a, single)
    single

  def from(n: Int): LazyList[Int] = 
    cons(n, from(n + 1))

  lazy val fibs: LazyList[Int] = 
    def build(cur: Int, nxt: Int): LazyList[Int] =
      cons(cur, build(nxt, cur + nxt))
    build(0, 1)

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = 
    f(state) match
      case Some((h,s)) => cons(h, unfold(s)(f))
      case None => empty
    
  lazy val fibsViaUnfold: LazyList[Int] = 
    unfold((0, 1)):
      case (cur, nxt) =>
        Some((cur, (nxt, (cur + nxt))))

  def fromViaUnfold(n: Int): LazyList[Int] = 
    unfold(n)(n => Some((n, n + 1)))

  def continuallyViaUnfold[A](a: A): LazyList[A] = 
    unfold(())(_ => Some((a, ())))

  lazy val onesViaUnfold: LazyList[Int] = 
    unfold(())(_ => Some((1, ())))
