package Brainfuck

/**
 * User: Iñigo Sola Núñez
 * Date: 09/09/12
 */
class BrainfuckAlgorithm extends Function[State, State] {

  def apply(oldState: State): State = {
    val newState = new State(oldState)
    newState.next()
    newState.readInput match {
      case '>' => newState.cellPtr += 1
      case '<' => newState.cellPtr -= 1
      case '+' => newState.memory.update(newState.cellPtr, (newState.memory(newState.cellPtr) + 1).toByte)
      case '-' => newState.memory.update(newState.cellPtr, (newState.memory(newState.cellPtr) - 1).toByte)
      case '.' => newState.output += newState.memory(newState.cellPtr).toChar
      case ',' => newState.memory.update(newState.cellPtr, Console.readChar().toByte)
      case '[' => if (newState.memory(newState.cellPtr) == 0) {
        var count = 1
        while (!newState.endOfInput && (newState.readInput != ']' || count > 0)) {
          newState.next()
          if (newState.readInput == '[') {
            count += 1
          } else if (newState.readInput == ']') {
            count -= 1
          }
        }
      }
      case ']' => var count = 1
      while (!newState.beginOfInput && (newState.readInput != '[' || count > 0)) {
        newState.prev()
        if (newState.readInput == ']') {
          count += 1
        } else if (newState.readInput == '[') {
          count -= 1
        }
      }
      if (!newState.beginOfInput) newState.prev()
      case _ => // nothing to do
    }
    newState
  }

}