package TemporalLogic

import parser.Analizador

/**
 * User: Iñigo Sola Núñez
 * Date: 09/09/12
 */
object ParseExample extends App {
  override def main(args: Array[String]) {
    val input = "a*-b*000(c+d)*aRc*--d"
    val formula = Analizador.analizar(input)
    println(formula)
  }
}
