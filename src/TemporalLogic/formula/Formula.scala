package TemporalLogic.formula

import TemporalLogic.algorithms.Folding
import TemporalLogic.util.OrderedListSet

sealed abstract class Formula {
  def folding(): Formula = this

  def negar(): Formula = Negacion(this)

  override def toString: String

  def iguales(f: Formula): Boolean = Folding(this, Folding.ALL) == Folding(f, Folding.ALL)

  def simplificar(): Formula = this
}

case class Proposition(name: String) extends Formula {
  override def toString: String = name
}

case class Negacion(f: Formula, n: Int = 1) extends Formula {

  override def toString = ("-" * n) + f

  override def folding() = this match {
    case Negacion(Negacion(f2, 1), 1) => f2.folding()
    case _ => Negacion(f.folding(), n)
  }

  override def simplificar(): Formula = {
    if (f.isInstanceOf[BooleanLogic]) {
      if (f.asInstanceOf[BooleanLogic].v == true) {
        BooleanLogic(false)
      } else {
        BooleanLogic(true)
      }
    } else {
      Negacion(f.simplificar())
    }
  }

  override def negar() = f
}

case class SiguienteVez(f: Formula, n: Int = 1) extends Formula {

  override def toString: String = ("S" * n) + f

  override def folding(): SiguienteVez = f match {
    case SiguienteVez(f2, n2) => SiguienteVez(f2, n2 + n).folding()
    case _ => SiguienteVez(f.folding(), n)
  }

  override def negar() = SiguienteVez(f.negar(), n)

  def reduce(): Formula = if (n == 1) f else SiguienteVez(f, n - 1)

  override def simplificar(): Formula = SiguienteVez(f.simplificar(), n)
}

case class Siempre(f: Formula, n: Int = 1) extends Formula {

  override def toString = ("[]" * n) + f //"[]" + (if (n>1) ("["+n+"]") else ("")) + f

  override def folding(): Siempre = f match {
    case Siempre(f2, n2) => Siempre(f2, 1).folding()
    case _ => Siempre(f.folding(), n)
  }

  override def negar() = AlgunaVez(f.negar(), n)

  override def simplificar(): Formula = {
    if (f.isInstanceOf[BooleanLogic]) {
      f
    } else {
      Siempre(f.simplificar(), n)
    }
  }
}

case class AlgunaVez(f: Formula, n: Int = 1) extends Formula {

  override def toString: String = ("<>" * n) + f

  override def folding(): AlgunaVez = f match {
    case AlgunaVez(f2, n2) => AlgunaVez(f2, n2 + n).folding()
    case _ => AlgunaVez(f.folding(), n)
  }

  override def negar() = Siempre(f.negar(), n)

  override def simplificar(): Formula = if (f.isInstanceOf[BooleanLogic]) {
    f
  } else {
    AlgunaVez(f.simplificar(), n)
  }

}

case class Or(operandos: OrderedListSet[Formula]) extends Formula {

  def this(opes: Formula*) = this(OrderedListSet[Formula](opes: _*))

  override def toString: String = "(" + (operandos mkString " + ") + ")"

  override def folding(): Or = {
    if (operandos.exists(_.isInstanceOf[Or])) {
      var noOrs: OrderedListSet[Formula] = operandos.filter(!_.isInstanceOf[Or])
      val siOrs: OrderedListSet[Formula] = operandos.filter(_.isInstanceOf[Or])

      siOrs.foreach(noOrs ++= _.asInstanceOf[Or].operandos)
      Or(noOrs).folding()
    } else {
      var o: OrderedListSet[Formula] = OrderedListSet()
      operandos.foreach(o += _.folding())
      Or(o)
    }
  }

  override def negar() = {
    val operandosNegados = operandos.map(_.negar())
    And(operandosNegados)
  }

  override def simplificar(): Formula = {
    if (operandos.contains(BooleanLogic(true))) {
      BooleanLogic(true);
    } else if (operandos.contains(BooleanLogic(false))) {
      Or(operandos.-(BooleanLogic(false))).simplificar()
    } else {
      var or: Or = Or(operandos.map(_.simplificar()))
      if (or != this)
        or.simplificar()
      else
        this
    }
  }
}

case class And(operandos: OrderedListSet[Formula]) extends Formula {

  def this(opes: Formula*) = this(OrderedListSet[Formula](opes: _*))

  override def toString: String = "(" + (operandos mkString " * ") + ")"

  override def folding(): And = {
    if (operandos.exists(_.isInstanceOf[And])) {
      var noOrs: OrderedListSet[Formula] = operandos.filter(!_.isInstanceOf[And])
      val siOrs: OrderedListSet[Formula] = operandos.filter(_.isInstanceOf[And])

      siOrs.foreach(noOrs ++= _.asInstanceOf[And].operandos)
      And(noOrs).folding()
    } else {
      var o: OrderedListSet[Formula] = OrderedListSet()
      operandos.foreach(o += _.folding())
      And(o)
    }
  }

  override def negar() = {
    val operandosNegados = operandos.map(_.negar())
    Or(operandosNegados)
  }

  override def simplificar(): Formula = {
    if (operandos.contains(BooleanLogic(false))) {
      BooleanLogic(false);
    } else if (operandos.contains(BooleanLogic(true))) {
      And(operandos.-(BooleanLogic(true))).simplificar()
    } else {
      var and: And = And(operandos.map(_.simplificar()))
      if (and != this)
        and.simplificar()
      else
        this
    }
  }
}

case class R(op1: Formula, op2: Formula) extends Formula {

  override def toString: String = "(" + op1 + " R " + op2 + ")"

  override def negar() = U(op1.negar(), op2.negar())

  override def simplificar(): Formula = (op1, op2) match {
    case (_, op: BooleanLogic) => op
    case (BooleanLogic(true), _) => op2
    case (_, _) => R(op1.simplificar(), op2.simplificar())
  }
}

case class U(op1: Formula, op2: Formula) extends Formula {

  override def toString: String = "(" + op1 + " U " + op2 + ")"

  override def negar() = R(op1.negar(), op2.negar())

  override def simplificar(): Formula = (op1, op2) match {
    case (_, op: BooleanLogic) => op
    case (BooleanLogic(false), _) => op2
    case (_, _) => U(op1.simplificar(), op2.simplificar())
  }
}

case class BooleanLogic(v: Boolean) extends Formula {
  override def toString: String = v.toString

  override def negar() = BooleanLogic(!v)
}
