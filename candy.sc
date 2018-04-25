case class State[S,+A](run: S => (A,S)) {

  def map[B](f: A => B): State[S, B] = {
    flatMap(a => State.unit(f(a)))
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
}

object State {
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))
  }

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
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
    State((m: Machine) => {
      inputs.foldLeft(State.unit((m.candies, m.coins)): State[Machine, (Int, Int)]) {
        case (s, Coin) => s.flatMap(insertCoin)
        case (s, Turn) => s.flatMap(turnKnob)
      }.run(m)
    })
  }

  def simulateMachineBookAnswer(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- State.sequence(inputs.map(i => State.modify((s: Machine) => (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    })))
    s <- State.get
  } yield (s.coins, s.candies)
}

val m = Machine(true, 10, 0)
Candy.simulateMachine(List(Coin, Turn, Turn, Coin, Coin, Coin, Turn)).run(m)
Candy.simulateMachineBookAnswer(List(Coin, Turn, Turn, Coin, Coin, Coin, Turn)).run(m)

val inputs = List(Coin, Turn)

val seq = State.sequence(inputs.map(i => State.modify((s: Machine) => (i, s) match {
  case (_, Machine(_, 0, _)) => s
  case (Coin, Machine(false, _, _)) => s
  case (Turn, Machine(true, _, _)) => s
  case (Coin, Machine(true, candy, coin)) =>
    Machine(false, candy, coin + 1)
  case (Turn, Machine(false, candy, coin)) =>
    Machine(true, candy - 1, coin)
})))

seq.flatMap(_ => {
  State.get.map(s => {
    (s.coins, s.candies)
  })
})
