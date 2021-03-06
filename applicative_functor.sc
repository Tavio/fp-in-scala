trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
}

trait Applicative[F[_]] extends Functor[F] {
  // primitive combinators
  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B]
  def unit[A](a: => A): F[A]

  // derived combinators
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    apply(apply(unit(a => f.curried(a)))(fa))(fb)
  }
  def applyViaMap2[A,B](fab: F[A => B])(fa: F[A]): F[B] = {
    map2(fa, fab)((a, f) => f(a))
  }
  def map[A,B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))
  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] = {
    traverse(fas)(fa => fa)
  }

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = {
    sequence(List.fill(n)(fa))
  }

  def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] = {
    map2(fa,fb)((_, _))
  }
}

