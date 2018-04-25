trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

val stringMonoid = new Monoid[String] {
  def zero = ""
  def op(s1: String, s2: String) = s1 + s2
}

def listMonoid[A] = new Monoid[List[A]] {
  def zero = Nil
  def op(l1: List[A], l2: List[A]) = l1 ++ l2
}

val intAddition = new Monoid[Int] {
  def zero = 0
  def op(i1: Int, i2: Int) = i1 + i2
}

val intMultiplication = new Monoid[Int] {
  def zero = 0
  def op(i1: Int, i2: Int) = i1 * i2
}

val booleanOr = new Monoid[Boolean] {
  def zero = false
  def op(b1: Boolean, b2: Boolean) = b1 || b2
}

val booleanAnd = new Monoid[Boolean] {
  def zero = true
  def op(b1: Boolean, b2: Boolean) = b1 && b2
}

def optionMonoid[A] = new Monoid[Option[A]] {
  def zero = None
  def op(o1: Option[A], o2: Option[A]): Option[A] = {
    o1.orElse(o2)
  }
}

def endoMonoid[A] = new Monoid[A => A] {
  def zero = (a: A) => a
  def op(f1: A => A, f2: A => A) = {
    f1.andThen(f2)
  }
}

def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
  def op(x: A, y: A): A = m.op(y, x)
  val zero = m.zero
}

val o1 = Some(11)
val o2 = Some(42)
val o3 = None
optionMonoid.op(optionMonoid.op(o1, o2), o3)
optionMonoid.op(o1, optionMonoid.op(o2, o3))

val f1 = (d: Double) => d / 2
val f2 = (d: Double) => d / 4
val f3 = (d: Double) => d / 8

endoMonoid.op(endoMonoid.op(f1, f2), f3)(4.0)
endoMonoid.op(f1, endoMonoid.op(f2, f3))(4.0)
endoMonoid.zero(4.0)

def concatenate[A](as: List[A], m: Monoid[A]): A =
  as.foldLeft(m.zero)(m.op)

def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = {
  as.foldLeft(m.zero)((acc, a) => m.op(acc, f(a)))
}

def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
  if(v.length == 0) m.zero
  else if(v.length == 1) m.op(m.zero, f(v.head))
  else if(v.length == 2) m.op(f(v.head), f(v.tail.head))
  else {
    val (left, right) = v.splitAt(v.length/2)
    m.op(foldMapV(left, m)(f), foldMapV(right, m)(f))
  }
}

List(1,2,3,4).toVector.splitAt(2)
List(1,2).toVector.splitAt(1)

sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

val wcMonoid: Monoid[WC] = new Monoid[WC] {
  val zero = Stub("")
  def op(a: WC, b: WC) = (a, b) match {
    case (Stub(c), Stub(d)) => Stub(c + d)
    case (Stub(c), Part(l, w, r)) => Part(c + l, w, r)
    case (Part(l, w, r), Stub(c)) => Part(l, w, r + c)
    case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
      Part(l1, w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2, r2)
  }
}

def count(s: String): Int = {
  def wc(c: Char): WC =
    if (c.isWhitespace)
      Part("", 0, "")
    else
      Stub(c.toString)
  def unstub(s: String) = s.length min 1
  foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
    case Stub(s) => unstub(s)
    case Part(l, w, r) => unstub(l) + w + unstub(r)
  }
}

trait Foldable[F[_]] {
  def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B = {
    foldMap(as)(f.curried)(endoMonoid[B])(z)
  }
  def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B = {
    foldMap(as)(a => (b: B) => f(b,a))(dual(endoMonoid[B]))(z)
  }
  def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B = {
    foldLeft(as)(mb.zero)((acc, a) => mb.op(acc, f(a)))
  }
  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](fa: F[A]): List[A] = {
    foldRight(fa)(List[A]())(_ :: _)
  }
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
}
object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    foldMapV(as, mb)(f)
}
object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldLeft[A,B](as: Tree[A])(z: B)(f: (B,A) => B): B = {
    def go(acc: B, t: Tree[A]): B = t match {
      case Leaf(a) => f(acc, a)
      case Branch(l, r) => go(go(acc, l), r)
    }

    go(z, as)
  }

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) = {
    def go(acc: B, t: Tree[A]): B = t match {
      case Leaf(a) => f(a, acc)
      case Branch(l, r) => go(go(acc, r), l)
    }

    go(z, as)
  }
}

object OptionFoldable extends Foldable[Option] {
  override def foldRight[A,B](o: Option[A])(z: B)(f: (A,B) => B): B = {
    o.map(a => f(a, z)).getOrElse(z)
  }

  override def foldLeft[A,B](o: Option[A])(z: B)(f: (B,A) => B): B = {
    o.map(a => f(z, a)).getOrElse(z)
  }
}

def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
  def zero: A => B = (a: A) => B.zero

  def op(f1: A => B, f2: A => B): A => B = {
    (a: A) => B.op(f1(a), f2(a))
  }
}

def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
  new Monoid[Map[K, V]] {
    def zero = Map[K,V]()
    def op(a: Map[K, V], b: Map[K, V]) =
      (a.keySet ++ b.keySet).foldLeft(zero) { (acc,k) =>
        acc.updated(k, V.op(a.getOrElse(k, V.zero),
          b.getOrElse(k, V.zero)))
      }
  }

def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
  foldMapV(as, mapMergeMonoid[A, Int](intAddition))(a => Map(a -> 1))
}

bag(Vector("a", "rose", "is", "a", "rose"))