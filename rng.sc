trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt

    (n, nextRNG)
  }
}

def nonNegativeInt(rng: RNG): (Int, RNG) = {
  val (n, nextRNG) = rng.nextInt
  (if (n < 0) -(n + 1) else n, nextRNG)
}

def double(rng: RNG): (Double, RNG) = {
  val (n, nextRNG) = nonNegativeInt(rng)
  (n / (Int.MaxValue.toDouble + 1), nextRNG)
}

def intDouble(rng: RNG): ((Int,Double), RNG) = {
  val (i, nextRNG) = rng.nextInt
  val (d, nextRNG2) = double(nextRNG)

  ((i, d), nextRNG2)
}

def doubleInt(rng: RNG): ((Double,Int), RNG) = {
  val (d, nextRNG) = double(rng)
  val (i, nextRNG2) = nextRNG.nextInt

  ((d, i), nextRNG2)
}

def double3(rng: RNG): ((Double,Double,Double), RNG) = {
  val (d, nextRNG) = double(rng)
  val (d2, nextRNG2) = double(nextRNG)
  val (d3, nextRNG3) = double(nextRNG2)

  ((d,d2,d3), nextRNG3)
}

def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
  f(z)
    .map{ case(a, s) => Stream.cons(a, unfold(s)(f)) }
    .getOrElse(Stream.empty)
}

def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
  val s = unfold(rng)((rng) => {
    val (i, nextRNG) = rng.nextInt
    Some(((i, nextRNG), nextRNG))
  }).take(count)
  (s.map(_._1).toList, s.last._2)
}

type Rand[+A] = RNG => (A, RNG)

val int: Rand[Int] = _.nextInt

def unit[A](a: A): Rand[A] =
  rng => (a, rng)

def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
  rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

def nonNegativeEven: Rand[Int] =
  map(nonNegativeInt)(i => i - i % 2)

def doubleViaMap: Rand[Double] = {
  map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))
}

def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
  rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a,b), rng3)
  }
}

def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
  map2(ra, rb)((_, _))


def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
  rng => {
    val seq = fs.foldLeft((rng, Nil:List[A]))((s, f) => {
      val (a, nextRNG) = f(s._1)
      (nextRNG, a::s._2)
    })
    (seq._2, seq._1)
  }
}

def intsViaSequence(count: Int): Rand[List[Int]] = {
  sequence(List.fill(count)(int))
}

def sequenceBetter[A](fs: List[Rand[A]]): Rand[List[A]] =
  fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
  rng => {
    val (a, nextRNG) = f(rng)
    g(a)(nextRNG)
  }
}

def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
  flatMap(nonNegativeInt)(i => rng => {
    val mod = i % n
    if (i + (n-1) - mod >= 0)
      (mod, rng)
    else nonNegativeLessThan(n)(rng)
  })(rng)
}

def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
  flatMap(s)(a => unit(f(a)))(rng)
}

def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
  flatMap(ra)(a => map(rb)(b => f(a,b)))
}

val rng = SimpleRNG(42)
doubleViaMap(rng)
both(nonNegativeEven, doubleViaMap)(rng)
intsViaSequence(5)(rng)
nonNegativeLessThan(60)(rng)




