package TemporalLogic.algorithms

import TemporalLogic.formula._
import TemporalLogic.util.OrderedListSet

object Folding {
  val AND = 0
  val OR = 1
  val AND_OR = 2
  val ALL = 3

  def apply(formula: Formula, typeFolding: Int): Formula = typeFolding match {
    case AND =>
      andFolding(formula)
    case OR =>
      orFolding(formula)
    case AND_OR =>
      allFolding(formula)
    case _ => booleanFolding(allFolding(formula))
  }

  def andOrFoldign(formula: Formula): Formula = formula match {

    case And(operandos) =>
      if (operandos.exists(_.isInstanceOf[And])) {
        var noAnds: OrderedListSet[Formula] = operandos.filter(!_.isInstanceOf[And])
        val siAnds: OrderedListSet[Formula] = operandos.filter(_.isInstanceOf[And])

        siAnds.foreach(noAnds ++= _.asInstanceOf[And].operandos)
        andOrFoldign(And(noAnds))
      } else {
        var o: OrderedListSet[Formula] = OrderedListSet()
        operandos.foreach(o += andOrFoldign(_))
        And(o)
      }

    case Or(operandos) =>
      if (operandos.exists(_.isInstanceOf[Or])) {
        var noOrs: OrderedListSet[Formula] = operandos.filter(!_.isInstanceOf[Or])
        val siOrs: OrderedListSet[Formula] = operandos.filter(_.isInstanceOf[Or])

        siOrs.foreach(noOrs ++= _.asInstanceOf[Or].operandos)
        andOrFoldign(Or(noOrs))
      } else {
        var o: OrderedListSet[Formula] = OrderedListSet()
        operandos.foreach(o += andOrFoldign(_))
        Or(o)
      }

    case Negacion(f, n) => Negacion(andOrFoldign(f), n)

    case SiguienteVez(f, n) => SiguienteVez(andOrFoldign(f), n)

    case Siempre(f, n) => Siempre(andOrFoldign(f), n)

    case AlgunaVez(f, n) => AlgunaVez(andOrFoldign(f), n)

    case R(fl, fr) => R(andOrFoldign(fl), andOrFoldign(fr))

    case U(fl, fr) => U(andOrFoldign(fl), andOrFoldign(fr))

    case _ => formula
  }

  def andFolding(formula: Formula): Formula = formula match {
    case And(operandos) =>
      if (operandos.exists(_.isInstanceOf[And])) {
        var noAnds: OrderedListSet[Formula] = operandos.filter(!_.isInstanceOf[And])
        val siAnds: OrderedListSet[Formula] = operandos.filter(_.isInstanceOf[And])

        siAnds.foreach(noAnds ++= _.asInstanceOf[And].operandos)
        andFolding(And(noAnds))
      } else {
        var o: OrderedListSet[Formula] = OrderedListSet()
        operandos.foreach(o += andFolding(_))
        And(o)
      }

    case Or(operandos) =>
      var o: OrderedListSet[Formula] = OrderedListSet()
      operandos.foreach(o += andFolding(_))
      Or(o)

    case Negacion(f, n) => Negacion(andFolding(f), n)

    case SiguienteVez(f, n) => SiguienteVez(andFolding(f), n)

    case Siempre(f, n) => Siempre(andFolding(f), n)

    case AlgunaVez(f, n) => AlgunaVez(andFolding(f), n)

    case R(fl, fr) => R(andFolding(fl), andFolding(fr))

    case U(fl, fr) => U(andFolding(fl), andFolding(fr))

    case _ => formula
  }

  def orFolding(formula: Formula): Formula = formula match {
    case Or(operandos) =>
      if (operandos.exists(_.isInstanceOf[Or])) {
        var noOrs: OrderedListSet[Formula] = operandos.filter(!_.isInstanceOf[Or])
        val siOrs: OrderedListSet[Formula] = operandos.filter(_.isInstanceOf[Or])

        siOrs.foreach(noOrs ++= _.asInstanceOf[Or].operandos)
        orFolding(Or(noOrs))
      } else {
        var o: OrderedListSet[Formula] = OrderedListSet()
        operandos.foreach(o += orFolding(_))
        Or(o)
      }

    case And(operandos) =>
      var o: OrderedListSet[Formula] = OrderedListSet()
      operandos.foreach(o += orFolding(_))
      And(o)

    case Negacion(f, n) => Negacion(orFolding(f), n)

    case SiguienteVez(f, n) => SiguienteVez(orFolding(f), n)

    case Siempre(f, n) => Siempre(orFolding(f), n)

    case AlgunaVez(f, n) => AlgunaVez(orFolding(f), n)

    case R(fl, fr) => R(orFolding(fl), orFolding(fr))

    case U(fl, fr) => U(orFolding(fl), orFolding(fr))

    case _ => formula
  }

  def allFolding(formula: Formula): Formula = formula match {

    case And(operandos) =>
      if (operandos.exists(_.isInstanceOf[And])) {
        var c = OrderedListSet[Formula]()
        val ite = operandos.iterator
        while (ite.hasNext) {
          var f = ite.next()
          if (f.isInstanceOf[And]) {
            c ++= f.asInstanceOf[And].operandos
          } else {
            c += f
          }
        }
        allFolding(And(c))
      } else {
        var o: OrderedListSet[Formula] = OrderedListSet()
        operandos.foreach(o += allFolding(_))
        And(o)
      }

    case Or(operandos) =>
      if (operandos.exists(_.isInstanceOf[Or])) {
        var c = OrderedListSet[Formula]()
        val ite = operandos.iterator
        while (ite.hasNext) {
          var f = ite.next()
          if (f.isInstanceOf[Or]) {
            c ++= f.asInstanceOf[Or].operandos
          } else {
            c += f
          }
        }
        allFolding(Or(c))
      } else {
        var o: OrderedListSet[Formula] = OrderedListSet()
        operandos.foreach(o += allFolding(_))
        Or(o)
      }

    case Negacion(Negacion(f, n1), n2) => allFolding(Negacion(f, n1 + n2))

    case Negacion(f, n) => Negacion(allFolding(f), n)

    case SiguienteVez(SiguienteVez(f, n1), n2) => allFolding(SiguienteVez(f, n1 + n2))

    case SiguienteVez(f, n) => SiguienteVez(allFolding(f), n)

    case Siempre(Siempre(f, n1), n2) => allFolding(Siempre(f, n1 + n2))

    case Siempre(f, n) => Siempre(allFolding(f), n)

    case AlgunaVez(AlgunaVez(f, n1), n2) => allFolding(AlgunaVez(f, n1 + n2))

    case AlgunaVez(f, n) => AlgunaVez(allFolding(f), n)

    case R(fl, fr) => R(allFolding(fl), allFolding(fr))

    case U(fl, fr) => U(allFolding(fl), allFolding(fr))

    case _ => formula
  }

  def booleanFolding(f: Formula): Formula = f match {
    case And(o) if o.exists(_.isInstanceOf[BooleanLogic]) =>
      val (nb, b) = o.partition(!_.isInstanceOf[BooleanLogic])
      if (b.exists(_.asInstanceOf[BooleanLogic].v == false)) {
        BooleanLogic(false)
      } else {
        And(nb)
      }
    case Or(o) if o.exists(_.isInstanceOf[BooleanLogic]) =>
      val (nb, b) = o.partition(!_.isInstanceOf[BooleanLogic])
      if (b.exists(_.asInstanceOf[BooleanLogic].v == true)) {
        BooleanLogic(true)
      } else {
        Or(nb)
      }
    case U(BooleanLogic(true), u) => AlgunaVez(u)
    case R(BooleanLogic(false), r) => Siempre(r)
    case Negacion(BooleanLogic(b), n) => BooleanLogic(if (n % 2 == 0) b else !b)
    case AlgunaVez(b@BooleanLogic(_), _) => b
    case _ => f
  }

}