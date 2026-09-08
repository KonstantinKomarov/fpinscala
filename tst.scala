val ones: Stream[Int] = Stream.cons(1, ones)
def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))
val threes = from(3)
val tst = threes.takeWhile(_ < 5)