case class State[S,+A](run: S => (A,S)) {
  def unit[A](a: A): State[S, A] =
    State(s => (a, s))

  def map[B](f: A => B): State[S, B] = {
    flatMap(a => unit(f(a)))
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

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input
case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {

  def insertCoin(a: (Int, Int)): State[Machine, (Int, Int)] = {
    State((m: Machine) => {
      if (m.locked && m.candies > 0) ((a._1, a._2 + 1), Machine(false, a._1, a._2 + 1))
      else ((a._1, a._2), m)
    })
  }

  def turnKnob(a: (Int, Int)): State[Machine, (Int, Int)] = {
    State((m: Machine) => {
      if (m.locked || m.candies <= 0) ((a._1, a._2), m)
      else ((a._1 - 1, a._2), Machine(true, a._1 - 1, a._2))
    })
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val initialState: State[Machine, (Int, Int)] = State(m => ((m.candies, m.coins), m))
    inputs.foldLeft(initialState) {
      case (s, Coin) => s.flatMap(insertCoin)
      case (s, Turn) => s.flatMap(turnKnob)
    }
  }
}

val m = Machine(true, 10, 0)
Candy.simulateMachine(List(Coin, Turn, Turn, Coin, Coin, Coin, Turn)).run(m)
