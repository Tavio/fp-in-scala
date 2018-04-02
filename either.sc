sealed trait MyEither[+E, +A] {
  def map[B](f: A => B): MyEither[E, B] = this match {
    case MyRight(a) => MyRight(f(a))
    case MyLeft(e) => MyLeft(e)
  }

  def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] = this match {
    case MyRight(a) => f(a)
    case MyLeft(e) => MyLeft(e)
  }

  def orElse[EE >: E,B >: A](b: => MyEither[EE, B]): MyEither[EE, B] = this match {
    case MyRight(a) => MyRight(a)
    case MyLeft(e) => b
  }

  def map2[EE >: E, B, C](b: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] = {
    this.map(f.curried).flatMap(b map _)
  }
}

case class MyLeft[+E](value: E) extends MyEither[E, Nothing]
case class MyRight[+A](value: A) extends MyEither[Nothing, A]


def Try[A](a: => A): MyEither[Exception, A] =
  try MyRight(a)
  catch { case e: Exception => MyLeft(e) }

def parseInsuranceRateQuote(age: String,
                            numberOfSpeedingTickets: String): MyEither[Exception,Double] =
  for {
    a <- Try { age.toInt }
    tickets <- Try { numberOfSpeedingTickets.toInt }
  } yield insuranceRateQuote(a, tickets)

def insuranceRateQuote(i: Int, i1: Int) = 2.0

def sequence[E, A](es: List[MyEither[E, A]]): MyEither[E, List[A]] = es match {
  case Nil => MyRight(Nil)
  case e::t => e.flatMap(ee => sequence(t).map(ee :: _))
}

sequence(List(MyRight(1), MyLeft("haha"), MyRight(2)))

def traverse[E, A, B](as: List[A])(f: A => MyEither[E, B]): MyEither[E, List[B]] = as match {
  case Nil => MyRight(Nil)
  case a::t => f(a).flatMap(b => traverse(t)(f).map(b :: _))
}

traverse(List("1","2","haha", "3"))(s => Try(s.toInt))