package fpinscala.exercises.state


trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  def nonNegativeInt(rng: RNG): (Int, RNG) = 
    val (i, r) = rng.nextInt
    (if i < 0 then -(i + 1) else i, r) 

  def double(rng: RNG): (Double, RNG) = 
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)

  def intDouble(rng: RNG): ((Int,Double), RNG) = 
    val (i, r1) = int(rng)
    val (d, r2) = double(r1)
    ((i, d), r2)

  def doubleInt(rng: RNG): ((Double,Int), RNG) = 
    val ((i, d), r1) = intDouble(rng)
    ((d, i), r1)

  def double3(rng: RNG): ((Double,Double,Double), RNG) = 
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = 
    @annotation.tailrec
    def loop(cnt: Int)(rng: RNG)(xs: List[Int]): (List[Int], RNG) = cnt match
      case c if c <= 0 =>
        (xs, rng)
      case _ =>
        val (x, r) = int(rng)
        loop(cnt - 1)(r)(x :: xs)
    loop(count)(rng)(List())

  def _double: Rand[Double] = 
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    rng0 =>
      val (a, rng1) = ra(rng0)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = 
    map2(ra, rb)((_,_))
  
  def randIntDouble: Rand[(Int, Double)] = 
    both(int, double)
  
  def randDoubleInt: Rand[(Double, Int)] = 
    both(double, int)

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] = 
    rs.foldRight(unit(Nil: List[A]))((rnd, acc) => map2(rnd, acc)(_ :: _))
  
  def _ints(count: Int): Rand[List[Int]] = 
    sequence(List.fill(count)(int))

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] = 
    rng0 =>
      val (a, rng1) = r(rng0)
      f(a)(rng1)

  def nonNegativeLessThan(n: Int): Rand[Int] = 
    flatMap(nonNegativeInt): i =>
      val mod = i % n
      if i + (n - 1) - mod >= 0 then unit(mod) else nonNegativeLessThan(n)

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] = 
    flatMap(r)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    flatMap(ra)(a => map(rb)(b => f(a, b)))

opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] =
      flatMap(a => unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      for 
        a <- underlying
        b <- sb
      yield f(a, b)

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      s =>
        val (a, s1) = underlying(s)
        f(a)(s1)
    
  def unit[S, A](a: A): State[S, A] = 
    s => (a, s)

  def apply[S, A](f: S => (A, S)): State[S, A] = f

  def sequence[S, A](acts: List[State[S, A]]): State[S, List[A]] = 
    acts.foldRight(unit[S, List[A]](Nil))((act, acc) => act.map2(acc)(_ :: _))

  def traverse[S, A, B](as: List[A])(f: A => State[S, B]): State[S, List[B]] = 
    as.foldRight(unit[S, List[B]](Nil))((a, acc) => f(a).map2(acc)(_ :: _))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- State.traverse(inputs)(inp => State.modify(update(inp)))
    s <- State.get
  } yield (s.coins, s.candies)

  val update = (inp: Input) => (s: Machine) => (inp, s) match
    case (_, Machine(_, 0, _)) => s
    case (Input.Coin, Machine(false, _, _)) => s
    case (Input.Turn, Machine(true, _, _)) => s
    case (Input.Coin, Machine(true, candies, coins)) =>
      Machine(false, candies, coins + 1)
    case (Input.Turn, Machine(false, candies, coins)) =>
      Machine(true, candies - 1, coins)

