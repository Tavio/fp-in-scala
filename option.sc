import scala.util.Try

sealed trait MyOption[+A] {
  def map[B](f: A => B): MyOption[B] = this match {
    case MyNone => MyNone
    case MySome(a) => MySome(f(a))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case MyNone => default
    case MySome(a) => a
  }

  def flatMap[B](f: A => MyOption[B]): MyOption[B] = {
    map(f).getOrElse(MyNone)
  }

  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = {
    this.map(MySome(_)).getOrElse(ob)
  }

  def filter(f: A => Boolean): MyOption[A] = {
    flatMap((a) => if(f(a)) MySome(a) else MyNone)
  }
}

case class MySome[+A](get: A) extends MyOption[A]
case object MyNone extends MyOption[Nothing]

def mean(xs: Seq[Double]): MyOption[Double] =
  if (xs.isEmpty) MyNone
  else MySome(xs.sum / xs.length)

def variance(xs: Seq[Double]): MyOption[Double] =
  mean(xs).flatMap((m) => {
    mean(xs.map((x) => math.pow(x - m, 2)))
  })

def map2[A,B,C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] = {
  a.map(f.curried).flatMap(b map _)
}

def sequence[A](a: List[MyOption[A]]): MyOption[List[A]] = {
  def loop(a: List[MyOption[A]], result: MyOption[List[A]]): MyOption[List[A]] = a match {
    case Nil => result
    case MySome(h)::t => loop(t, result.map(h :: _))
    case MyNone::_ => MyNone
  }

  loop(a, MySome(List()))
}

def sequence2[A](a: List[MyOption[A]]): MyOption[List[A]] = a match {
  case Nil => MySome(Nil)
  case h::t => h flatMap(hh => sequence2(t) map(hh :: _))
}

sequence2(List(MySome(4), MyNone, MySome(11)))

def traverse[A, B](a: List[A])(f: A => MyOption[B]): MyOption[List[B]] = a match {
  case Nil => MySome(Nil)
  case h::t => f(h).flatMap(hh => traverse(t)(f).map(hh :: _))
}

def Try[A](a: => A): MyOption[A] =
  try MySome(a)
  catch { case e: Exception => MyNone }

traverse(List("1", "2", "haha", "3"))(i => Try(i.toInt))
