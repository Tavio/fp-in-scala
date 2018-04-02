case class State[S,+A](run: S => (A,S)) {
  def unit[A](a: A): State[S, A] =
    State(s => (a, s))

  def map[B](f: A => B): State[S, B] = {
    flatMap(a => unit(f(a)))
  }

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
    flatMap(a => sb.map(b => f(a,b)))
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State(s => {
      val (a, nextState) = run(s)
      f(a).run(nextState)
    })
  }
}

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

type Rand[A] = State[RNG, A]

object Gen {

  def int: Rand[Int] = {
    new Rand(_.nextInt)
  }

  def nonNegativeInt: Rand[Int] = {
    int.map(i => if (i < 0) -(i + 1) else i)
  }

  def double: Rand[Double] = {
    int.map(_ / (Int.MaxValue.toDouble + 1))
  }

  def intDouble: Rand[(Int, Double)] = {
    int.map2(double)((_,_))
  }

  def doubleInt: Rand[(Double, Int)] = {
    double.map2(int)((_,_))
  }

  def double3: Rand[(Double, Double, Double)] = {
    double.map2(double)((_,_)).map2(double)((p, d) => (p._1, p._2, d))
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    new Rand(rng => {
      val seq = fs.foldLeft((rng, Nil:List[A]))((s, f) => {
        val (a, nextRNG) = f.run(s._1)
        (nextRNG, a::s._2)
      })
      (seq._2, seq._1)
    })
  }

  def ints(count: Int): Rand[List[Int]] = {
    sequence(List.fill(count)(int))
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    new Rand(rng =>
      nonNegativeInt.flatMap(i => new Rand(rng => {
        val mod = i % n
        if (i + (n-1) - mod >= 0)
          (mod, rng)
        else nonNegativeLessThan(n).run(rng)
      })).run(rng))
  }
}

val rng = SimpleRNG(42)
Gen.intDouble.run(rng)
Gen.ints(5).run(rng)
Gen.nonNegativeLessThan(10).run(rng)

val ns: Rand[List[Int]] =
  Gen.nonNegativeLessThan(20).flatMap(x =>
    Gen.int.flatMap(y =>
      Gen.ints(x).map(xs =>
        xs.map(_ % y))))

val nsForComprehension = for {
  x <- Gen.nonNegativeLessThan(20)
  y <- Gen.int
  xs <- Gen.ints(x)
} yield xs.map(_ % y)

nsForComprehension.run(rng)._1