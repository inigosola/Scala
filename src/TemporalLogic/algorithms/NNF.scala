package TemporalLogic.algorithms

import TemporalLogic.formula._

object NNF {

  def apply(formula: Formula): Formula = {
    var fo = Folding(formula, Folding.ALL)
    fo = fo match {

      case Negacion(f, n) if n % 2 == 0 =>
        NNF(f)

      case Negacion(f, n) if n > 1 && n % 2 == 1 =>
        NNF(Negacion(f, 1))

      case a@Negacion(SiguienteVez(f, n), 1) =>
        NNF(SiguienteVez(Negacion(f), n))

      case a@Negacion(AlgunaVez(f, n), 1) =>
        NNF(Siempre(Negacion(f), n))

      case a@Negacion(Siempre(f, n), 1) =>
        NNF(AlgunaVez(Negacion(f), n))

      case a@Negacion(f: Or, 1) =>
        NNF(f.negar())

      case a@Negacion(f: And, 1) =>
        NNF(f.negar())

      case a@Negacion(U(fl, fr), 1) =>
        NNF(R(fl.negar(), fr.negar()))

      case a@Negacion(R(fl, fr), 1) =>
        NNF(U(fl.negar(), fr.negar()))

      case a@Negacion(f, n) if n > 1 =>
        NNF(Negacion(f, n % 2))

      case Negacion(f, 1) =>
        Negacion(NNF(f), 1)

      case Siempre(f, n) =>
        Siempre(NNF(f), n)

      case AlgunaVez(f, n) =>
        AlgunaVez(NNF(f), n)

      case SiguienteVez(f, n) =>
        SiguienteVez(NNF(f), n)

      case Or(operandos) =>
        Or(operandos.map(NNF(_)))

      case And(operandos) =>
        And(operandos.map(NNF(_)))

      case U(op1, op2) =>
        U(NNF(op1), NNF(op2))

      case R(op1, op2) =>
        R(NNF(op1), NNF(op2))

      case _ => formula
    } //, Folding.ALL)
    Folding(fo, Folding.ALL)
  }
}