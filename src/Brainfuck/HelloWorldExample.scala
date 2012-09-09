package Brainfuck

/**
 * User: Iñigo Sola Núñez
 * Date: 09/09/12
 */
object HelloWorldExample extends App {
  override def main(args: Array[String]) {
    val input = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."
    val output = Brainfuck(input)
    println(output)
  }
}
