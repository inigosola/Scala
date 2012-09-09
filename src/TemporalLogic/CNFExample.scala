package TemporalLogic

import algorithms.CNF
import parser.Analizador

/**
 * User: Iñigo Sola Núñez
 * Date: 09/09/12
 */
object CNFExample extends App {
  override def main(args: Array[String]) {
    val input = "-[]a*-b*-000(c+<>d)*aRc*--d*-([]bU0c)"
    val formula = Analizador.analizar(input)
    val cnf = CNF(formula)
    println(cnf)
  }
}
