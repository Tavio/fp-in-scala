import scala.annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = {
    @tailrec
    def loop(t: List[Tree[A]], size: Int):Int = t match {
      case Nil => size
      case (_: Leaf[A])::t => loop(t, size + 1)
      case (b: Branch[A])::t => loop(b.left::b.right::t, size + 1)
    }

    loop(List(t), 0)
  }

  def max(t: Tree[Int]): Int = {
    @tailrec
    def loop(t: List[Tree[Int]], max: Int):Int = t match {
      case Nil => max
      case (l: Leaf[Int])::t => loop(t, max.max(l.value))
      case (b: Branch[Int])::t => loop(b.left::b.right::t, max)
    }

    loop(List(t), Int.MinValue)
  }

  def depth[A](t: Tree[A]): Int = {
    case class DepthMarker(value: Int)

    @tailrec
    def loop(t: List[Any], currDepth: Int, maxDepth: Int):Int = t match {
      case Nil => maxDepth
      case (_: Leaf[Int])::t => loop(DepthMarker(currDepth)::t, currDepth, maxDepth)
      case (b: Branch[Int])::t => loop(b.left::b.right::DepthMarker(currDepth)::t, currDepth + 1,maxDepth)
      case (DepthMarker(value))::t => loop(t, value, maxDepth.max(value))
    }

    loop(List(t), 1, Int.MinValue)
  }

  def map[A,B](t: Tree[A])(f: A => B):Tree[B] = t match{
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def mapTailRec[A,B](t: Tree[A])(f: A => B):Tree[B] = {
    def loop(t: Tree[A], m: Tree[B])(f: A => B): Tree[B] = {

    }



  }
}


Tree.max(Branch(Branch(Leaf(1), Leaf(5)), Leaf(3)))
Tree.depth(Branch(Branch(Leaf(1), Leaf(5)), Leaf(3)))
Tree.depth(Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Branch(Leaf(7), Leaf(5))))))


Tree.map(Branch(Branch(Leaf(1), Leaf(5)), Leaf(3)))(_ * 2)