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
}

val ones: Stream[Int] = Stream.cons(1, ones)

def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

def fibs(a: Int = 0, b: Int = 1): Stream[Int] = {
  Stream.cons(a, fibs(b, a + b))
}

def fibsViaUnfold: Stream[Int] = {
  Stream.unfold((0, 1))(value => {
    Some((value._1, (value._2, value._1 + value._2)))
  })
}

def fromViaUnfold(n: Int): Stream[Int] = {
  Stream.unfold(n)(i => Some((i, i + 1)))
}

def constantViaUnfold[A](a: A): Stream[A] = {
  Stream.unfold(a)(_ => Some(a, a))
}

def onesViaUnfold: Stream[Int] = {
  constantViaUnfold(1)
}

Stream(1,3,4,5,6,7,8,9,10,11,12,13,14,15).startsWith(Stream(1,2))

Stream(1,2,3).tails.toList.map(_.toList)