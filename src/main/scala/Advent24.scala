import java.nio.file.{Files, Path}

object Advent24 {

  def main(args: Array[String]): Unit = {
    val instructions = Files.readString(Path.of("./data/input24.txt")).linesIterator.toList.map(parseInstruction)

    val state = getStateWithInputs(State(instructions), inputCount = 3)
    println(state)
    println(state.findValid())
  }

  private def getStateWithInputs(state: State, inputCount: Int): State = {
    (0 until inputCount).foldLeft(state){
      (state, _) => state.inputStep(9)
    }
  }

  case class State(instructions: List[Instruction],
                   inputCount: Int = 0,
                   vars: Vector[Int] = Vector.fill(4)(0)) {

    def findValid(inputs: List[Int] = Nil): List[Int] = {
      if (instructions.isEmpty) {
        if (hasValidOutput) {
          println("VALID: " + inputs.mkString(", "))
          inputs
        } else {
          Nil
        }
      } else {
        var input = 9
        var result = List.empty[Int]
        while (result.isEmpty && input >= 1) {
          val afterState = copy(instructions.tail, inputCount = inputCount + 1, vars = vars.updated(instructions.head.a, input))
          val nextState = afterState.stepUntilInput
          val nextInputs = input :: inputs
          result = nextState.findValid(nextInputs)
          input = input - 1
        }
        result
      }
    }

    def stepUntilInput: State = {
      var next = this
      while (!next.instructions.isEmpty && !next.needsInput) {
        next = next.step
      }
      next
    }

    def inputStep(input: Int): State = {
      require(needsInput)
      copy(instructions.tail, inputCount = inputCount + 1, vars = vars.updated(instructions.head.a, input)).stepUntilInput
    }

    def step: State = {
      val remaining = instructions.tail
      instructions.head match {
        //case Inp(a) => copy(remaining, inputs = inputs.tail, vars = vars.updated(a, inputs.head))
        case Add(a, b) => copy(remaining, vars = vars.updated(a, vars(a) + vars(b)))
        case Mul(a, b) => copy(remaining, vars = vars.updated(a, vars(a) * vars(b)))
        case Div(a, b) => copy(remaining, vars = vars.updated(a, vars(a) / vars(b)))
        case Mod(a, b) => copy(remaining, vars = vars.updated(a, vars(a) % vars(b)))
        case Eql(a, b) => copy(remaining, vars = vars.updated(a, (vars(a) == vars(b)).compare(false)))
        case AddConst(a, b) => copy(remaining, vars = vars.updated(a, vars(a) + b))
        case MulConst(a, b) => copy(remaining, vars = vars.updated(a, vars(a) * b))
        case DivConst(a, b) => copy(remaining, vars = vars.updated(a, vars(a) / b))
        case ModConst(a, b) => copy(remaining, vars = vars.updated(a, vars(a) % b))
        case EqlConst(a, b) => copy(remaining, vars = vars.updated(a, (vars(a) == b).compare(false)))
      }
    }

    override def toString: String = {
      s"State(${vars.mkString("|")}, $inputCount, $instructions)"
    }

    def isValid: Boolean = instructions.isEmpty && vars(3) == 0

    def hasValidOutput: Boolean = vars(3) == 0

    def needsInput: Boolean = instructions.head.isInstanceOf[Inp]

  }

  private def parseInstruction(line: String): Instruction = {
    val parts = line.split(' ').toList
    val var1 = varToIndex(parts(1))
    if (varToIndex.contains(parts.last)) {
      val var2 = varToIndex(parts.last)
      parts.head match {
        case "inp" => Inp(var1)
        case "add" => Add(var1, var2)
        case "mul" => Mul(var1, var2)
        case "div" => Div(var1, var2)
        case "mod" => Mod(var1, var2)
        case "eql" => Eql(var1, var2)
      }
    } else {
      val const = parts.last.toInt
      parts.head match {
        case "add" => AddConst(var1, const)
        case "mul" => MulConst(var1, const)
        case "div" => DivConst(var1, const)
        case "mod" => ModConst(var1, const)
        case "eql" => EqlConst(var1, const)
      }
    }
  }

  private val varToIndex = Map("w" -> 0, "x" -> 1, "y" -> 2, "z" -> 3)

  trait Instruction {
    def a: Int
  }

  case class Inp(a: Int) extends Instruction
  case class Add(a: Int, b: Int) extends Instruction
  case class Mul(a: Int, b: Int) extends Instruction
  case class Div(a: Int, b: Int) extends Instruction
  case class Mod(a: Int, b: Int) extends Instruction
  case class Eql(a: Int, b: Int) extends Instruction
  case class AddConst(a: Int, b: Int) extends Instruction
  case class MulConst(a: Int, b: Int) extends Instruction
  case class DivConst(a: Int, b: Int) extends Instruction
  case class ModConst(a: Int, b: Int) extends Instruction
  case class EqlConst(a: Int, b: Int) extends Instruction

}
