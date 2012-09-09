package TemporalLogic.parser

import scala.util.parsing.combinator._
import TemporalLogic.formula._
import TemporalLogic.util.OrderedListSet
import TemporalLogic.algorithms.Folding

object Analizador extends JavaTokenParsers {

  def p: Parser[String] = """[a-z1-9]+""".r

  def E: Parser[Formula] = (
    T ~ opt(AO) ^^ {
      case f1 ~ None => f1 //.folding
      case f1 ~ Some(f2) if f2._1 == "*" => And(OrderedListSet(f1, f2._2)) //.folding
      case f1 ~ Some(f2) => new Or(f1, f2._2) //.folding
    }
    )

  def AO: Parser[(String, Formula)] = (
    ("*" | "&") ~ T ~ opt(AO) ^^ {
      case o ~ f ~ None => ("*", f)
      case o ~ f ~ Some(ao) if ao._1 == "*" => (o, And(OrderedListSet(f, ao._2)))
      case o ~ f ~ Some(ao) => (o, new Or(f, ao._2))
    }
      | ("+" | "|") ~ T ~ opt(AO) ^^ {
      case o ~ f ~ None => ("+", f)
      case o ~ f ~ Some(ao) if ao._1 == "+" => (o, new Or(f, ao._2))
      case o ~ f ~ Some(ao) => (o, And(OrderedListSet(f, ao._2)))
    }
    )

  def T: Parser[Formula] = (
    F ~ opt(RU) ^^ {
      case f1 ~ None => f1
      case f1 ~ Some(f2) if f2._1 == "R" => R(f1, f2._2)
      case f1 ~ Some(f2) => U(f1, f2._2)
    }
    )

  def RU: Parser[(String, Formula)] = (
    "R" ~ F ^^ {
      case o ~ f => (o, f)
    }
      | "U" ~ F ^^ {
      case o ~ f => (o, f)
    }
    )

  def F: Parser[Formula] = (
    ("-" | "~") ~> F ^^ {
      Negacion(_)
    }
      | S
    )

  def S: Parser[Formula] = (
    ("[]" | "G") ~> S ^^ {
      Siempre(_)
    }
      | ("<>" | "F") ~> S ^^ {
      AlgunaVez(_)
    }
      | ("0" | "X") ~> S ^^ {
      SiguienteVez(_)
    }
      | RR
    )

  def RR: Parser[Formula] = (
    "(" ~> I <~ ")"
      | ("-" | "~") ~> P ^^ {
      Negacion(_)
    }
      | P
    )

  def I: Parser[Formula] = (
    E ~ opt(("->" | "<->" | "=>" | "<=>") ~ E) ^^ {
      case e ~ None => e
      case e1 ~ Some(x ~ e2) if x.length == 2 =>
        new Or(Negacion(e1), e2)
      case e1 ~ Some(x ~ e2) if x.length == 3 =>
        new Or(new And(e1, e2), new And(Negacion(e1), Negacion(e2)))
    }
  )

  def P: Parser[Formula] = p ^^ {
    case "true" => BooleanLogic(true)
    case "false" => BooleanLogic(false)
    case p => Proposition(p)
  }

  def analizar(formula: String): Formula = {
    val f = parseAll(E, formula).get
    Folding(f, Folding.ALL)
  }
}
