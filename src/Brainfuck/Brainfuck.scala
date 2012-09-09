package Brainfuck

/**
 * User: Iñigo Sola Núñez
 * Date: 09/09/12
 */
object Brainfuck extends Function[String, String] {

  def apply(input: String): String = {
    val brainfuck = new BrainfuckAlgorithm
    var state = new State(input)
    while (!state.endOfInput) state = brainfuck(state)
    state.output
  }

}
