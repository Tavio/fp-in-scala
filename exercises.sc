import scala.annotation.tailrec

def fib(n: Int): Int = {
  @tailrec
  def go(a: Int, b: Int, i: Int, d: Int): Int = {
    if(d == 0) return a
    if (i == d) return b
    else go(b, a + b, i + 1, d)
  }
  go(0, 1, 1, n - 1)
}

def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
  @tailrec
  def loop(n: Int): Boolean = {
    if(n == as.size - 1) true
    else if(!ordered(as(n), as(n + 1))) false
    else loop(n + 1)
  }
  loop(0)
}


def partial1[A,B,C](a: A, f: (A,B) => C): B => C = {
  b => f(a, b)
}

partial1(5, (x: Int, y: Int) => x + y)(3)


def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
  a => b => f(a, b)
}

def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
  (a, b) => f(a)(b)
}

def compose[A,B,C](f: B => C, g: A => B): A => C = {
  a => f(g(a))
}

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](l: List[A]): Option[List[A]] = l match {
    case Nil => None
    case Cons(_, tail) => Some(tail)
  }

  def setHead[A](l: List[A], a: A): List[A] = l match {
    case Nil => Cons(a, Nil)
    case Cons(h, tail) => Cons(a, tail)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    @tailrec
    def go(l: List[A], n: Int): List[A] = {
      if(n == 0) l
      else {
        List.tail(l) match {
          case None => Nil
          case Some(tail) => go(tail, n - 1)
        }
      }
    }
    go(l, n)
  }

  def dropWhile[A](l: List[A])( f: A => Boolean): List[A] = {
    @tailrec
    def go(l: List[A], f: A => Boolean): List[A] = {
      l match {
        case Nil => Nil
        case x@Cons(h, tail) =>
          if(f(h)) go(tail, f) else x
      }
    }
    go(l, f)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(h,t) => f(h, foldRight(t, z)(f))
  }

  def product2(l: List[Int]): Double = {
    foldRight(l, 1.0)(_ * _)
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, sum) => sum + 1)
  }

  @tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z, h))(f)
  }

  def reverse[A](l: List[A]): List[A] = {
    List.foldLeft(l, Nil:List[A])((acc, a) => Cons(a, acc))
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  def foldLeft2[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(as, (b: B) => b)((a, acc) => (b: B) => acc(f(b, a)))(z)
  }

  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(as, (b: B) => b)((acc, a) => (b: B) => acc(f(a, b)))(z)
  }

  def append[A](as: List[A], as2: List[A]): List[A] = {
    foldRight(as, as2)((a, acc) => Cons(a, acc))
  }

  def concat[A](as: List[List[A]]): List[A] = {
    foldRight(as, Nil:List[A])((a, acc) => append(a, acc))
  }
}