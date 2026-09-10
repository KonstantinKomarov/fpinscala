val ones: LazyList[Int] = LazyList.cons(1, ones)
def from(n: Int): LazyList[Int] = LazyList.cons(n, from(n + 1))
val threes = from(3)
val tst = threes.takeWhile(_ < 5)