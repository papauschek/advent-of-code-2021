import java.nio.file.{Files, Path}

object Advent18 {

  def main(args: Array[String]): Unit = {
    val lines = Files.readString(Path.of("./data/input18.txt")).linesIterator.toList
    val trees = lines.map(parseTree)

    val magnitudes = for {
      tree1 <- trees
      tree2 <- trees
    } yield add(tree1, tree2).magnitude

    println(magnitudes.max) // 4712
  }

  private def add(tree1: Tree, tree2: Tree): Tree = {
    var newTree: Tree = new Pair(tree1, tree2)
    while (reduce(newTree).isDefined) {
      newTree = reduce(newTree).getOrElse(newTree)
    }
    newTree
  }

  private def reduce(tree: Tree): Option[Tree] = {
    tree.explodingPair() match {
      case Some(pair) =>
        val magnitudes = tree.traverseMagnitudes
        val leftIndex = magnitudes.indexOf(pair.left)
        val maybeLeftMagnitude = magnitudes.lift.apply(leftIndex - 1)
        val maybeRightMagnitude = magnitudes.lift.apply(leftIndex + 2)
        var newTree = maybeLeftMagnitude.map(magnitude => tree.update(magnitude, new Magnitude(magnitude.magnitude + pair.left.magnitude))).getOrElse(tree)
        newTree = maybeRightMagnitude.map(magnitude => newTree.update(magnitude, new Magnitude(magnitude.magnitude + pair.right.magnitude))).getOrElse(newTree)
        newTree = newTree.update(pair, new Magnitude(0))
        Some(newTree)
      case _ =>
        tree.traverseMagnitudes.find(_.magnitude >= 10) match {
          case Some(magnitude) =>
            val half = magnitude.magnitude / 2.0
            val newPair = new Pair(new Magnitude(half.floor.toLong), new Magnitude(half.ceil.toLong))
            Some(tree.update(magnitude, newPair))
          case _ =>
            None
        }
    }
  }

  private def parseTree(text: String): Tree = {
    text.head match {
      case '[' =>
        val left = parseTree(text.substring(1))
        val right = parseTree(text.substring(left.toString.length + 2))
        new Pair(left, right)
      case magnitude =>
        new Magnitude(magnitude.asDigit)
    }
  }

  trait Tree {
    def magnitude: Long
    def explodingPair(depth: Int = 0): Option[Pair] = None
    def traverseMagnitudes: List[Magnitude]
    def update(existing: Tree, replacement: Tree): Tree
  }

  class Magnitude(val magnitude: Long) extends Tree {
    def traverseMagnitudes: List[Magnitude] = this :: Nil
    def update(existing: Tree, replacement: Tree): Tree = if (existing == this) replacement else this
    override def toString: String = magnitude.toString
  }

  class Pair(val left: Tree, val right: Tree) extends Tree {

    def magnitude: Long = left.magnitude * 3 + right.magnitude * 2

    def traverseMagnitudes: List[Magnitude] = left.traverseMagnitudes ++ right.traverseMagnitudes

    def update(existing: Tree, replacement: Tree): Tree = {
      if (existing == this) {
        replacement
      } else {
        val newLeft = left.update(existing, replacement)
        val newRight = right.update(existing, replacement)
        if (newLeft != left || newRight != right) {
          new Pair(newLeft, newRight)
        } else {
          this
        }
      }
    }

    override def explodingPair(depth: Int = 0): Option[Pair] = {
      if (depth == 4) {
        Some(this)
      } else {
        left.explodingPair(depth + 1).orElse(right.explodingPair(depth + 1))
      }
    }

    override def toString: String = s"[$left,$right]"

  }
}
