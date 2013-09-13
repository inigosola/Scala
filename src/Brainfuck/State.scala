package Brainfuck

class State(val input: String) {

  var output: String = ""
  var memory: Array[Byte] = new Array[Byte](30000)
  var cellPtr: Int = 0
  var lookahead: Int = -1

  def this(state: State) {
    this(state.input)
    output = state.output
    memory = state.memory
    cellPtr = state.cellPtr
    lookahead = state.lookahead
  }

  def next() {
    if (lookahead < input.size - 1) lookahead += 1
  }

  def prev() {
    if (lookahead >= 0) lookahead -= 1
  }

  def readInput: Char = input(lookahead)

  def endOfInput: Boolean = lookahead >= input.size - 1

  def beginOfInput: Boolean = lookahead < 0

}
