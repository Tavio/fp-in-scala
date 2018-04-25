import java.util.concurrent._

trait MyFuture[A] {
  def get: A

  def get(timeout: Long, unit: TimeUnit): A

  def cancel(evenIfRunning: Boolean): Boolean

  def isDone: Boolean

  def isCancelled: Boolean
}

type Par[A] = ExecutorService => Future[A]

object Par {
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = {
    (es: ExecutorService) => UnitFuture(f(a(es).get, b(es).get))
  }

  def map3[A,B,C,D](a: Par[A], b: Par[B], c:Par[C])(f: (A,B,C) => D): Par[D] = {
    (es: ExecutorService) => {
      val g = (a: A, b: B) => f(a, b, c(es).get)
      map2(a, b)(g)(es)
    }
  }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A,B](f: A => B): A => Par[B] = {
    a => {
      lazyUnit(f(a))
    }
  }

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))

  def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = {
    (es: ExecutorService) => UnitFuture(f(pa(es).get)(es).get)
  }

  def join[A](p: Par[Par[A]]): Par[A] = {
    es => (p(es).get())(es)
  }

  def flatMapViaJoin[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = {
    join(map(pa)(f))
  }

  def joinViaFlatMap[A](p: Par[Par[A]]): Par[A] = {
    flatMap(p)(pa => pa)
  }

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = {
    map(parList)(l => l.sorted)
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldRight(unit(List[A]()))((p, acc) => map2(p, acc)(_ :: _))
  }

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
    as.foldRight(unit(List[A]()))((a, pAcc) => map(pAcc)(acc => {
      if(f(a)) a :: acc else acc
    }))
  }

  def parFilterBookAnswer[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] = l.map(asyncF(a => if(f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten)
  }

  def mapReduce[A, B](l: List[A])(f: A => B)(g: List[B] => Par[B]): Par[B] = {
    flatMap(Par.sequence(l.map(Par.asyncF(f))))(g(_))
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)
}

private case class UnitFuture[A](get: A) extends Future[A] {
  def isDone = true
  def get(timeout: Long, units: TimeUnit) = get
  def isCancelled = false
  def cancel(evenIfRunning: Boolean): Boolean = false
}

def sum(ints: IndexedSeq[Int]): Par[Int] =
  if (ints.length <= 1)
    Par.unit(ints.headOption getOrElse 0)
  else {
    val (l,r) = ints.splitAt(ints.length/2)
    Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
  }

def countWords(paragraphs: List[String]): Par[Int] = {
  Par.mapReduce(paragraphs)(_.size)((l: List[Int]) => sum(l.toVector))
}