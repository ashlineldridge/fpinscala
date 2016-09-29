import fpinscala.state._

import scala.annotation.tailrec

type Rand[+A] = State[RNG, A]

def int: Rand[Int] = State(_.nextInt)

def ints(count: Int): Rand[List[Int]] = {
  @tailrec
  def go(is: List[Int], r: RNG): (List[Int], RNG) =
    if (is.length >= count) (is, r)
    else {
      val (i, r2) = r.nextInt
      go(i :: is, r2)
    }
  State(rng => go(List(), rng))
}

val ns: Rand[List[Int]] =
  int.flatMap(x =>
    int.flatMap(y =>
      ints(x).map(xs =>
        xs.map(_ % y))))

val ns2: Rand[List[Int]] = for {
  x <- int
  y <- int
  xs <- ints(x)
} yield xs.map(_ % y)

val s = State.simulateMachine2(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn))
s.run(Machine(true, 10, 5))
