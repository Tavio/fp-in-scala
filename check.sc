import scala.annotation.tailrec

sealed trait Stream[+A] {
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def toListTailRec: List[A] = {
    @tailrec
    def loop(s: Stream[A], res: List[A]):List[A] = s match{
      case Empty => res
      case Cons(h, t) => loop(t(), h()::res)
    }

    loop(this, List()).reverse
  }

  def toListFastStillPure: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h,t) =>
        buf += h()
        go(t())
      case _ => buf.toList
    }
    go(this)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
    case Cons(h, t) => Stream.cons(h(), Stream.empty)
    case _ => Stream.empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n >= 1 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h,t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
    case _ => Stream.empty
  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((h, t) => {
      p(h) && t
    })
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = {
    foldRight(Stream.empty : Stream[A])((h, t) => {
      if(p(h)) Stream.cons(h, t) else Stream.empty
    })
  }

  //  def headOption[A]: Option[A] = this match {
  //    case Empty => None
  //    case Cons(h, _) => Some(h())
  //  }
  //
  //  def headOptionViaFoldRight[A]: Option[A] = {
  //    foldRight(None: Option[A])((h, _) => Some(h) )
  //  }

  def map[B](f: A => B): Stream[B] = {
    foldRight(Empty: Stream[B])((h, t) => {
      Stream.cons(f(h), t)
    })
  }

  def filter(p: A => Boolean): Stream[A] = {
    foldRight(Empty: Stream[A])((h, t) => {
      if(p(h)) Stream.cons(h, t) else t
    })
  }

  def append[B>:A](b: => Stream[B]): Stream[B] = {
    foldRight(b)((h, t) => Stream.cons(h, t))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(Empty: Stream[B])((h, t) => {
      f(h).append(t)
    })
  }

  def mapViaUnfold[B](f: A => B): Stream[B] = {
    Stream.unfold(this)(s => s match {
      case Cons(h,t) => Some((f(h()), t()))
      case Empty => None
    })
  }

  def takeViaUnfold(n: Int): Stream[A] = {
    Stream.unfold((this, n))(s => s match {
      case (Cons(h,t), 1) => Some(h(), (Stream.empty, 0))
      case (Cons(h,t), n) if n > 1 => Some(h(), (t(), n - 1))
      case _ => None
    })
  }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] = {
    Stream.unfold(this) {
      case Cons(h, t) if f(h()) => Some(h(), t())
      case _ => None
    }
  }

  def zipWith[B, C](bs: Stream[B])(f: (A, B) => B): Stream[B] = {
    Stream.unfold((this, bs)){
      case (Cons(h, t), Cons(h2, t2)) => Some(f(h(), h2()), (t(), t2()))
      case _ => None
    }
  }

  def zip[B](bs: Stream[B]): Stream[(A, B)] = {
    Stream.unfold((this, bs)) {
      case (Cons(h, t), Cons(h2, t2)) => Some((h(), h2()), (t(), t2()))
      case _ => None
    }
  }

  def zipAll[B](bs: Stream[B]): Stream[(Option[A],Option[B])] = {
    Stream.unfold((this, bs)){
      case (Cons(h, t), Cons(h2, t2)) => Some((Some(h()), Some(h2())), (t(), t2()))
      case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), Stream.empty))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Stream.empty, t2()))
      case _ => None
    }
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def startsWith[A](s: Stream[A]): Boolean = {
    zipAll(s).takeWhile(! _._2.isEmpty).forAll(a => a._1 == a._2)
  }

  def tails: Stream[Stream[A]] = {
    Stream.unfold(this) {
      case (s@Cons(h,t)) => Some(s, t())
      case _ => None
    }
  }

  def hasSubsequence[A](s: Stream[A]): Boolean = {
    tails exists (_ startsWith s)
  }

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = {
    foldRight(z, Stream(z))((a, p0) => {
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, Stream.cons(b2, p1._2))
    })._2
  }

  def find(f: A => Boolean): Option[A] = this match {
    case Cons(h, t) => if(f(h())) Some(h()) else t().find(f)
    case Empty => None
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z)
      .map{ case(a, s) => Stream.cons(a, unfold(s)(f)) }
      .getOrElse(Stream.empty)
  }

  def from(n: Int): Stream[Int] = {
    Stream.unfold(n)(i => Some((i, i + 1)))
  }
}

case class State[S,+A](run: S => (A,S)) {

  def map[B](f: A => B): State[S, B] = {
    flatMap(a => State.unit(f(a)))
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

object State {
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))
  }

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
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

type Rand[+A] = State[RNG,A]

object Rand {
  def nextInt: Rand[Int] = {
    State(_.nextInt)
  }

  def nonNegativeInt: Rand[Int] = {
    nextInt.map(n => if (n < 0) -(n + 1) else n)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    nonNegativeInt.flatMap(i => {
      val mod = i % n
      if (i + (n-1) - mod >= 0)
        State.unit(mod)
      else nonNegativeLessThan(n)
    })
  }

  def boolean: Rand[Boolean] = {
    nonNegativeLessThan(10).map(_ < 5)
  }

  def double: Rand[Double] = {
    nonNegativeInt.map(i => i / (Int.MaxValue.toDouble + 1))
  }
}

case class Gen[A](sample: Rand[A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(f(_).sample))
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(i => Gen(State.sequence(List.fill(i)(sample))))
  }

  def unsized: SGen[A] = {
    SGen((_ => this))
  }
}

case class SGen[A](g: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = g(n)
}

object SGen {
  def listOf[A](g: Gen[A]): SGen[List[A]] = {
    SGen(i => g.listOfN(Gen.unit(i)))
  }

  def listOf1[A](g: Gen[A]): SGen[List[A]] = {
    SGen(i => g.listOfN(Gen.unit(i max 1)))
  }
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(Rand.nonNegativeLessThan(stopExclusive).map(i => {
      (i.toFloat / stopExclusive * (stopExclusive - start) + start).toInt
    }))
  }

  def unit[A](a: => A): Gen[A] = {
    Gen(State.unit(a))
  }

  def boolean: Gen[Boolean] =
    Gen(Rand.boolean)

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    boolean.flatMap(bool => {
      if(bool) g1 else g2
    })
  }

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    val pg1 = g1._2 / (g1._2 + g2._2)

    Gen(Rand.double.flatMap(d => if(d < pg1) g1._1.sample else g2._1.sample))
  }
}


type SuccessCount = Int
type FailedCase = String
type TestCases = Int
type MaxSize = Int

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  def isFalsified = false
}

case class Falsified(failure: FailedCase,
                     successes: SuccessCount) extends Result {
  def isFalsified = true
}

case class Prop(run: (MaxSize,TestCases,RNG) => Result) {
  def &&(p: Prop): Prop = Prop {
    (m, n, rng) => {
      val result = run(m, n, rng)
      result match {
        case Passed => p.run(m, n, rng)
        case Falsified(_,_) => result
      }
    }
  }

  def tag(msg: String) = Prop {
    (max,n,rng) => run(max,n,rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x => x
    }
  }
}

def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
  forAll2(i => g(i))(f)

def forAll2[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
  (max,n,rng) =>
    val casesPerSize = (n + (max - 1)) / max
    val props: Stream[Prop] =
      Stream.from(0).take((n min max) + 1).map(i => forAll("", g(i))(f))
    val prop: Prop =
      props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
      }).toList.reduce(_ && _)
  prop.run(max,n,rng)
}

def forAll[A](label: String, as: Gen[A])(f: A => Boolean): Prop = Prop {
  (_, n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
    case (a, i) => try {
      if (f(a)) Passed else Falsified(a.toString, i)
    } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
  }.find((r:Result) => r.isFalsified).getOrElse(Passed)
}

def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = {
  Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
}

def buildMsg[A](s: A, e: Exception): String =
  s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

def run(p: Prop,
        maxSize: Int = 100,
        testCases: Int = 100,
        rng: RNG = new SimpleRNG(System.currentTimeMillis)): Unit =
  p.run(maxSize, testCases, rng) match {
    case Falsified(msg, n) =>
      println(s"! Falsified after $n passed tests:\n $msg")
    case Passed =>
      println(s"+ OK, passed $testCases tests.")
  }





val rng = new SimpleRNG(52)
val smallInt = Gen.choose(-10,10)
val maxProp = forAll(SGen.listOf1(smallInt)) { ns =>
  val max = ns.max
  !ns.exists(_ > max)
}

val sortedProp = forAll(SGen.listOf1(Gen.choose(-1000, 1000))) { ns =>
  val sorted = ns.sorted
  !sorted.zip(sorted.tail).exists(pair => pair._1 > pair._2)
}

run(maxProp)
run(sortedProp)