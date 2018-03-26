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
