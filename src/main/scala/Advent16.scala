import java.nio.file.{Files, Path}

object Advent16 {

  def main(args: Array[String]): Unit = {
    val hexString = Files.readString(Path.of("./data/input16.txt")).linesIterator.toList.head
    val (trees, _) = parseTree(toBitString(hexString))
    println(trees.head)
    println(trees.head.evaluate)
  }

  private def parseTree(bitString: String, maxCount: Int = Int.MaxValue): (List[Tree], String) = {
    if (maxCount == 0) {
      (Nil, bitString)
    } else if (bitString.forall(_ == '0')) {
      (Nil, "")
    } else {
      val version = Integer.parseInt(bitString.substring(0, 3), 2)
      val typeId = Integer.parseInt(bitString.substring(3, 6), 2)

      val (tree, restBits) = typeId match {
        case 4 => parseLiteralValue(bitString, version)
        case _ => parseOperator(bitString, version, typeId)
      }

      if (restBits.isEmpty) {
        (tree :: Nil, "")
      } else {
        val (trees, rest) = parseTree(restBits, maxCount - 1)
        (tree :: trees, rest)
      }
    }
  }

  private def parseOperator(bitString: String, version: Int, typeId: Int): (Operator, String) = {
    val isTotalLength = bitString(6) == '0'
    if (isTotalLength) {
      val length = Integer.parseInt(bitString.substring(7, 22), 2)
      val (trees, _) = parseTree(bitString.substring(22, length + 22))
      val operatorRest = bitString.substring(length + 22)
      (Operator(version, typeId, trees), operatorRest)
    } else {
      val packetCount = Integer.parseInt(bitString.substring(7, 18), 2)
      val (trees, rest) = parseTree(bitString.substring(18), maxCount = packetCount)
      (Operator(version, typeId, trees), rest)
    }
  }

  private def parseLiteralValue(bitString: String, version: Int): (LiteralValue, String) = {
    val valuePackets = bitString.substring(6).grouped(5).toList
    val leadingPackets = valuePackets.takeWhile(_.head == '1')
    val lastPacket = valuePackets(leadingPackets.length)
    val combinedPackets = (leadingPackets :+ lastPacket).map(_.drop(1)).mkString
    val value = java.lang.Long.parseLong(combinedPackets, 2)
    val restBits = bitString.substring((combinedPackets.length / 4) * 5 + 6)
    (LiteralValue(version, value), restBits)
  }

  private def toBitString(hex: String): String = {
    hex.flatMap(char => ("000" + Integer.toBinaryString(Integer.parseInt(char.toString, 16))).takeRight(4))
  }

  trait Tree {
    def version: Int
    def typeId: Int
    def versionSum: Int
    def evaluate: Long
  }

  case class LiteralValue(version: Int, value: Long) extends Tree {
    def typeId: Int = 4
    def versionSum: Int = version
    def evaluate: Long = value
  }

  case class Operator(version: Int, typeId: Int, trees: List[Tree]) extends Tree {

    def versionSum: Int = version + trees.map(_.versionSum).sum

    def evaluate: Long = {
      typeId match {
        case 0 => trees.map(_.evaluate).sum
        case 1 => trees.tail.foldLeft(trees.head.evaluate)((x, tree) => tree.evaluate * x)
        case 2 => trees.tail.foldLeft(trees.head.evaluate)((x, tree) => tree.evaluate.min(x))
        case 3 => trees.tail.foldLeft(trees.head.evaluate)((x, tree) => tree.evaluate.max(x))
        case 5 => if (trees(0).evaluate > trees(1).evaluate) 1 else 0
        case 6 => if (trees(0).evaluate < trees(1).evaluate) 1 else 0
        case 7 => if (trees(0).evaluate == trees(1).evaluate) 1 else 0
        case _ => throw new IllegalArgumentException(typeId.toString)
      }
    }

  }

}
