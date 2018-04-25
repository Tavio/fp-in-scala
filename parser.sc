trait Parsers[ParseError, Parser[+_]] { self =>

  implicit def string(s: String): Parser[String]
  implicit def number(d: Double): Parser[Double]
  implicit def regex(r: scala.util.matching.Regex): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
  ParserOps[String] = ParserOps(f(a))

  val whitespace = "\\s".r

  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  def product[A,B](p1: Parser[A], p2: => Parser[B]): Parser[(A,B)] = {
    flatMap(p1)(a => map(p2)(b => (a, b)))
  }
  def map[A,B](p:Parser[A])(f: A => B): Parser[B] = {
    flatMap(p)(a => succeed(f(a)))
  }
  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)
  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = {
    flatMap(p)(a => map(p2)(b => f(a, b)))
    // map(product(p, p2))(f.tupled)
  }
  def many[A](p: Parser[A]): Parser[List[A]] = {
    or(map2(p, many(p))(_ :: _), succeed(List()))
  }
  def many1[A](p: Parser[A]): Parser[List[A]] = {
    map2(p, many(p))(_ :: _)
  }
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    if (n <= 0) succeed(List())
    else map2(p, listOfN(n - 1, p))(_ :: _)
  }
  def maybe[A](p: Parser[A]): Parser[Option[A]] = {
    p.map(Some(_)) or succeed(None)
  }
  def trimmed[A](p: Parser[A]): Parser[A] = {
    product(product(whitespace.slice.many, p), whitespace.slice.many).flatMap({
      case ((spacesBefore, a), spacesAfter) => succeed(a)
    })
  }
  def surroundedBy[A,B](delim1: Parser[B], delim2: Parser[B])(p:Parser[A]): Parser[A] = {
    product(product(product(product(delim1, whitespace.slice.many), p), whitespace.slice.many), delim2).flatMap({
      case ((((b1, spaces1), a), spaces2), b2) => succeed(a)
    })
  }
  def manySeparatedBy[A,B](sep: Parser[B])(p: Parser[A]): Parser[List[A]] = {
    many(product(sep.trimmed, p).map(_._2))
  }
  def separatedBy[A,B,C](p: Parser[A], p2: Parser[B])(sep: Parser[C]): Parser[(A,B)] = {
    product(product(product(product(p, whitespace.slice.many), sep), whitespace.slice.many), p2).flatMap({
      case ((((a, spaces1), sep), spaces2), b) => succeed((a, b))
    })
  }
  def as[A,B](a: Parser[A])(b: B): Parser[B] = map(a)(_ => b)



  def run[A](p: Parser[A])(input: String): Either[ParseError,A]
  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]
  def slice[A](p: Parser[A]): Parser[String]

  case class ParserOps[A](p: Parser[A]) {

    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    def **[B>:A](p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def many: Parser[List[A]] = self.many(p)
    def many1: Parser[List[A]] = self.many1(p)
    def slice: Parser[String] = self.slice(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def map2[B,C](p2: Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p2)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def listOfN(n: Int): Parser[List[A]] = self.listOfN(n, p)
    def trimmed: Parser[A] = self.trimmed(p)
    def surroundedBy[B](sep1: Parser[B], sep2: Parser[B]): Parser[A] = self.surroundedBy(sep1, sep2)(p)
    def manySeparatedBy[B](sep: Parser[B]): Parser[List[A]] = self.manySeparatedBy(sep)(p)
    def separatedBy[B,C](sep: Parser[B])(p2: Parser[C]): Parser[(A, C)] = self.separatedBy(p, p2)(sep)
    def as[B](b:B): Parser[B] = self.as(p)(b)
  }

  // import Parsers._
  val numA: Parser[Int] = char('a').many.slice.map(_.size)

  val x = char('a').many.map2(char('b').many1)((as, bs) => (as.size, bs.size))
  val y = char('a').many.slice.map(_.size) ** char('b').many1.slice.map(_.size  )

  // Exercise 9.6:
  "[0-9]+".r.flatMap(s => char('a').listOfN(s.toInt))

  trait JSON {

    object JSON {
      case object JNull extends JSON
      case class JNumber(get: Double) extends JSON
      case class JString(get: String) extends JSON
      case class JBool(get: Boolean) extends JSON
      case class JArray(get: IndexedSeq[JSON]) extends JSON
      case class JObject(get: Map[String, JSON]) extends JSON

      def jsonNull = "null".as(JNull)
      def jsonNumber = "[0-9]+(?:\\.[0-9]+)?".r.trimmed.map((s: String) => JNumber(s.toDouble))
      def jsonTrue = "true".as(JBool(true))
      def jsonFalse = "true".as(JBool(false))
      def jsonString =  "[^\"]".r.many.surroundedBy("", "").map((s:List[String]) => JString(s.mkString("")))

      def jsonValue: Parser[JSON] = jsonNull | jsonNumber | jsonTrue | jsonFalse | jsonString | jsonArray

      def jsonArray = jsonValue.manySeparatedBy(",").surroundedBy("[", "]").map((js: List[JSON]) => JArray(js.toVector))
      def jsonPair = jsonString.separatedBy(":")(jsonValue)
      def jsonObject = {
        jsonPair.manySeparatedBy(",").surroundedBy("{","}").map(pairs => {
          JObject(pairs.map {case (k,v) => (k.get, v)}.toMap)
        })

      }
    }
  }
}

