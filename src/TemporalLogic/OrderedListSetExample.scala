package TemporalLogic

import algorithms.CNF
import formula.And
import parser.Analizador
import util.OrderedListSet

/**
 * User: Iñigo Sola Núñez
 * Date: 09/09/12
 */
object OrderedListSetExample extends App {

  override def main(args: Array[String]) {
    val input = "-[]a*-b*-000(c+d)*aRc*--d*-(bUc)"
    val formula = Analizador.analizar(input)
    val cnf = CNF(formula)
    val olset = cnf match {
      case And(op) => OrderedListSet(op.toSeq:_*)
      case formula => OrderedListSet(formula)
    }
    println(olset)
  }

}
