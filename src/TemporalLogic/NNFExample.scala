package TemporalLogic

import algorithms.NNF
import parser.Analizador

/**
 * User: Iñigo Sola Núñez
 * Date: 09/09/12
 */
object NNFExample extends App {
  override def main(args: Array[String]) {
    val input = "-[]a*-b*-000(c+d)*aRc*--d*-(bUc)"
    val formula = Analizador.analizar(input)
    val nnf = NNF(formula)
    println(nnf)
  }
}
