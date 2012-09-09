package TemporalLogic.algorithms

import TemporalLogic.formula._
import TemporalLogic.util.OrderedListSet

object DtNF {

  def apply(formula: Formula): Formula = Folding(NNF(formula) match {

    case AlgunaVez(f, n) if n > 1 =>
      DtNF(AlgunaVez(f, 1))

    case Siempre(f, n) if n > 1 =>
      DtNF(Siempre(f, 1))

    case U(fl, fr) if Folding(fl, Folding.ALL) == Folding(fr, Folding.ALL) =>
      DtNF(fl)

    case R(fl, fr) if Folding(fl, Folding.ALL) == Folding(fr, Folding.ALL) =>
      DtNF(fl)

    case U(f, U(fl, fr)) if Folding(f, Folding.ALL) == Folding(fl, Folding.ALL) =>
      DtNF(U(fl, fr))

    case R(f, R(fl, fr)) if Folding(f, Folding.ALL) == Folding(fl, Folding.ALL) =>
      DtNF(R(fl, fr))

    case Or(operandos) if operandos.exists(_.isInstanceOf[And]) =>
      val and: And = operandos.find(_.isInstanceOf[And]).get.asInstanceOf[And]
      var opes: OrderedListSet[Formula] = operandos - and
      val otro: Formula = opes.head
      opes = opes - otro
      val c = And(and.operandos.map(x => new Or(otro, x)))
      if (opes.isEmpty)
        DtNF(Folding(c, Folding.AND_OR))
      else
        DtNF(Folding(Or(opes + c), Folding.AND_OR))

    case SiguienteVez(Or(operandos), n) =>
      DtNF(Or(operandos.map(SiguienteVez(_, n))))

    case SiguienteVez(And(operandos), n) =>
      DtNF(And(operandos.map(SiguienteVez(_, n))))

    case U(f, Or(operandos)) =>
      DtNF(Or(operandos.map(U(f, _))))

    case R(f, And(operandos)) =>
      DtNF(And(operandos.map(R(f, _))))

    case U(And(operandos), f) =>
      DtNF(And(operandos.map(U(_, f))))

    case R(Or(operandos), f) =>
      DtNF(Or(operandos.map(R(_, f))))

    case AlgunaVez(Or(operandos), n) =>
      DtNF(Or(operandos.map(AlgunaVez(_, n))))

    case Siempre(And(operandos), n) =>
      DtNF(And(operandos.map(Siempre(_, n))))

    case Negacion(f, n) =>
      Negacion(DtNF(f), n)

    case Siempre(f, n) =>
      Siempre(DtNF(f), n)

    case AlgunaVez(f, n) =>
      AlgunaVez(DtNF(f), n)

    case SiguienteVez(f, n) =>
      SiguienteVez(DtNF(f), n)

    case Or(operandos) =>
      Or(operandos.map(DtNF(_)))

    case And(operandos) =>
      And(operandos.map(DtNF(_)))

    case U(op1, op2) =>
      U(DtNF(op1), DtNF(op2))

    case R(op1, op2) =>
      R(DtNF(op1), DtNF(op2))

    case fo => fo
  }, Folding.ALL)

}