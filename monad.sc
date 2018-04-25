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

type Rand[A] = State[RNG, A]

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]

  def map[A,B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))
  def sequence[A](lma: List[F[A]]): F[List[A]] = {
    lma.foldRight(unit(List[A]()))((ma, mla) => map2(ma,mla)(_ :: _))
  }
  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] = {
    la.foldRight(unit(List[B]()))((ma, mlb) => map2(f(ma), mlb)(_ :: _))
  }
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = {
    sequence(List.fill(n)(ma))
  }
  def product[A,B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
    ms.foldRight(unit(List[A]()))((m, macc) => flatMap(f(m))(b => {
      if(b) map2(unit(m), macc)(_::_)
      else macc
    }))
  }
  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = {
    a => flatMap(f(a))(g(_))
  }
  def flatMapViaCompose[A,B](ma: F[A])(f: A => F[B]): F[B] = {
    compose((_ : Unit) => ma, f)(())
  }
  def join[A](mma: F[F[A]]): F[A]  = {
    flatMap(mma)(ma => ma)
  }
  def flatMapViaJoin[A,B](ma: F[A])(f: A => F[B]): F[B] = {
    join(map(ma)(f))
  }
}

object Monad {
//  val genMonad = new Monad[Gen] {
//    def unit[A](a: => A): Gen[A] = Gen.unit(a)
//    def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
//      ma flatMap f
//  }

  val optionMonad = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)
    def flatMap[A,B](ma: Option[A])(f: A => Option[B]): Option[B] =
      ma flatMap f
  }

  val streamMonad = new Monad[Stream] {
    def unit[A](a: => A) = Stream(a)
    def flatMap[A,B](ma: Stream[A])(f: A => Stream[B]) = ma flatMap f
  }

  val listMonad = new Monad[List] {
    def unit[A](a: => A) = List(a)
    def flatMap[A,B](ma: List[A])(f: A => List[B]) = ma flatMap f
  }

  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = Id(a)
    def flatMap[A,B](ma: Id[A])(f: A => Id[B]) = ma flatMap f
  }
}

def Try[A](a: => A): Option[A] =
  try Some(a)
  catch { case e: Exception => None }

Monad.optionMonad.sequence(List(Some(42), Some(11)))
Monad.optionMonad.traverse(List("42", "hahaha", "11"))(i => Try(i.toInt))
Monad.optionMonad.traverse(List("42", "11"))(i => Try(i.toInt))
Monad.optionMonad.replicateM(5, None)
Monad.listMonad.replicateM(2, List(1,2,3))

Monad.optionMonad.filterM(List(1,2,3,4,5))(i => if(i > 1) Some(true) else Some(false))
Monad.optionMonad.filterM(List(1,2,3,4,5))(i => if(i > 1) Some(true) else None)

def powerset[A](as: List[A]): List[List[A]] = {
  Monad.listMonad.filterM(as)(_ => List(true, false))
}

powerset(List(1,2))

Monad.optionMonad.compose((i: Int)=> Some[Int](i * 2), (i:Int) => Some(i))(42)
Monad.optionMonad.compose((i:Int) => Some(i), (i: Int)=> Some[Int](i * 2))(42)

Monad.listMonad.join(List(List(1,2,3), List(4,5,6)))

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A =>Id[B]): Id[B] = f(value)
}


type IntState[A] = State[Int, A]

object IntStateMonad extends Monad[IntState] {
  def unit[A](a: => A): IntState[A] = State(s => (a, s))
  def flatMap[A,B](st: IntState[A])(f: A => IntState[B]): IntState[B] =
    st flatMap f
}

def stateMonad[S] = new Monad[({type f[x] = State[S,x]})#f] {
  def unit[A](a: => A): State[S,A] = State(s => (a, s))
  def flatMap[A,B](st: State[S,A])(f: A => State[S,B]): State[S,B] =
    st flatMap f
  def getState[S]: State[S, S] = State.get
  def setState[S](s: => S): State[S, Unit] = State.set(s)
}



val F = stateMonad[Int]
def zipWithIndex[A](as: List[A]): List[(Int,A)] =
  as.foldLeft(F.unit(List[(Int, A)]()))((acc,a) => for {
    xs <- acc
    n <- F.getState
    _ <- F.setState(n + 1)
  } yield (n, a) :: xs).run(0)._1.reverse

zipWithIndex(List(1,2,3))


case class Reader[R, A](run: R => A) {
  def flatMap[B](f: A => Reader[R,B]): Reader[R,B] = {
    Reader((r: R) => f(run(r)).run(r))
  }
}
object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    def unit[A](a: => A): Reader[R,A] = {
      Reader(_ => a)
    }
    def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] = {
      st.flatMap(f)
    }
  }
  def ask[R]: Reader[R, R] = Reader(r => r)
}

val R = Reader.readerMonad[String]

