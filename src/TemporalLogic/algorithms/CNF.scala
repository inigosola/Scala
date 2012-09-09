package TemporalLogic.algorithms

import collection.mutable.Map
import TemporalLogic.formula._
import TemporalLogic.util.{OrderedListSet, Util}

object CNF {

  def apply(formula: Formula, format: Boolean = false, ids: Set[String] = Set()): Formula = {
    val dtnf = DtNF(formula)
    val (f, _, r) = apply2(dtnf, Map(), OrderedListSet())
    val f2 = if (r.isEmpty) Folding(f, Folding.ALL) else Folding(And(r + f), Folding.ALL)
    if (format) putIds(f2, ids) else f2
  }

  def apply2(f: Formula, done: Map[Formula, Formula],
             r: OrderedListSet[Formula]): (Formula, Map[Formula, Formula], OrderedListSet[Formula]) = DtNF(NNF(f)) match {

    case fe@U(fl, fr) if !done.contains(fe) =>
      var r2: OrderedListSet[Formula] = OrderedListSet()
      var p1 = fl
      var p2 = fr
      if (!simple(fl)) {
        p1 = Proposition(Util.newId)
        val cnf1 = CNF(Siempre(new Or(Negacion(p1), fl)))
        r2 += cnf1
      }
      if (!simple(fr)) {
        p2 = Proposition(Util.newId)
        val cnf2 = CNF(Siempre(new Or(Negacion(p2), fr)))
        r2 += cnf2
      }
      val fc = U(p1, p2)
      done += (fe -> fc)
      (fc, done, r ++ r2)

    case fe@U(fl, fr) if done.contains(fe) =>
      val fc = done.get(fe).get.asInstanceOf[U]
      (fc, done, r)

    case fe@R(fl, fr) if !done.contains(fe) =>
      var r2: OrderedListSet[Formula] = OrderedListSet()
      var p1 = fl
      var p2 = fr
      if (!simple(fl)) {
        p1 = Proposition(Util.newId)
        val cnf1 = CNF(Siempre(new Or(Negacion(p1), fl)))
        r2 += cnf1
      }
      if (!simple(fr)) {
        p2 = Proposition(Util.newId)
        val cnf2 = CNF(Siempre(new Or(Negacion(p2), fr)))
        r2 += cnf2
      }
      val fc = R(p1, p2)
      done += (fe -> fc)
      (fc, done, r ++ r2)

    case fe@R(fl, fr) if done.contains(fe) =>
      val fc = done.get(fe).get.asInstanceOf[R]
      (fc, done, r)

    case Or(opes) if opes.exists(siempreNoSimple(_)) =>
      val fe: Siempre = opes.find(siempreNoSimple(_)).get.asInstanceOf[Siempre]
      if (!done.contains(fe)) {
        val p: Proposition = Proposition(Util.newId)
        val fc = Siempre(p)
        done += (fe -> fc)
        val cnf1 = CNF(Siempre(new Or(Negacion(p), fe.f)))
        val opes2: OrderedListSet[Formula] = (opes - fe) + fc
        apply2(Or(opes2), done, r + cnf1)
      } else {
        val fc: Siempre = done.get(fe).get.asInstanceOf[Siempre]
        val opes2: OrderedListSet[Formula] = (opes - fe) + fc
        apply2(Siempre(Or(opes2)), done, r)
      }

    case Or(opes) if opes.exists(siguienteSiempreNoSimple(_)) =>
      val fe1: SiguienteVez = opes.find(siguienteSiempreNoSimple(_)).get.asInstanceOf[SiguienteVez]
      val fe2: Siempre = fe1.f.asInstanceOf[Siempre]
      if (!done.contains(fe2)) {
        val p: Proposition = Proposition(Util.newId)
        val fc = Siempre(p)
        done += (fe2 -> fc)
        val cnf1 = CNF(Siempre(new Or(Negacion(p), fe2.f)))
        val fc2 = SiguienteVez(fc, fe1.n)
        val opes2: OrderedListSet[Formula] = (opes - fe1) + fc2
        apply2(Or(opes2), done, r + cnf1)
      } else {
        val fc: Siempre = done.get(fe2).get.asInstanceOf[Siempre]
        val fc2 = SiguienteVez(fc, fe1.n)
        val opes2: OrderedListSet[Formula] = (opes - fe1) + fc2
        apply2(Or(opes2), done, r)
      }

    case fe@AlgunaVez(fo, 1) if !done.contains(fe) =>
      if (!simple(fo)) {
        val p = Proposition(Util.newId)
        val fc = AlgunaVez(p)
        done += (fe -> fc)
        (fc, done, r + CNF(Siempre(new Or(Negacion(p), fo))))
      } else {
        (fe, done, r)
      }

    case fe@AlgunaVez(fo, n) if done.contains(fe) =>
      val fc = done.get(fe).get.asInstanceOf[AlgunaVez]
      (AlgunaVez(fc), done, r)

    case Siempre(fo, n) =>
      val (f2, m, r2) = apply2(fo, done, r)
      (Siempre(f2, n), m, r2)

    case fe@Negacion(fo, n) =>
      val (f2, m, r2) = apply2(fo, done, r)
      (Negacion(f2, n), m, r2)

    case SiguienteVez(fo, n) =>
      val (f2, m, r2) = apply2(fo, done, r)
      (SiguienteVez(f2, n), m, r2)

    case And(operandos) =>
      var tupla = (f, done, r)
      var fs: OrderedListSet[Formula] = OrderedListSet()

      for (p <- operandos) {
        tupla = apply2(p, tupla._2, tupla._3)
        fs = fs + tupla._1
      }
      (And(fs), tupla._2, tupla._3)

    case Or(operandos) =>
      var tupla = (f, done, r)
      var fs: OrderedListSet[Formula] = OrderedListSet()

      for (p <- operandos) {
        tupla = apply2(p, tupla._2, tupla._3)
        fs = fs + tupla._1
      }
      (Or(fs), tupla._2, tupla._3)

    case fo => (fo, done, r)
  }

  private def siempreNoSimple(f: Formula) = f match {
    case Siempre(formula, _) if !simple(formula) => true
    case _ => false
  }

  private def siguienteSiempreNoSimple(f: Formula) = f match {
    case SiguienteVez(formula, _) if siempreNoSimple(formula) => true
    case _ => false
  }

  private def simple(f: Formula): Boolean = f match {
    case Proposition(_) => true

    case BooleanLogic(_) => true

    case Negacion(Proposition(_), _) => true

    case Negacion(BooleanLogic(_), _) => true

    /*
    case Negacion(p,_) => simple(p)

    case Or(opers) =>
      var r = true
      for (o <- opers)
        r = r && simple(o)
      r

    case And(opers) =>
      var r = true
      for (o <- opers)
        r = r && simple(o)
      r
    */
    case _ => false
  }

  private def putIds(formula: Formula, identificadores: Set[String] = Set()): Formula = {
    val ids: Set[String] = Util.getIds(formula).filter(_(0) != '_') ++ identificadores
    val tempIds: Set[String] = Util.getIds(formula).filter(_(0) == '_')
    val ite = tempIds.iterator
    var idsMap = ids.zip(ids).toMap
    while (ite.hasNext) {
      val e = ite.next()
      idsMap.+=((e, Util.newId(idsMap.values.toSet)))
    }
    putIds(formula, idsMap)
  }

  private def putIds(formula: Formula, ids: collection.immutable.Map[String, String]): Formula = formula match {
    case Proposition(p) => Proposition(ids(p))
    case Or(o) => Or(o.map(putIds(_, ids)))
    case And(a) => And(a.map(putIds(_, ids)))
    case U(u1, u2) => U(putIds(u1, ids), putIds(u2, ids))
    case R(r1, r2) => R(putIds(r1, ids), putIds(r2, ids))
    case Siempre(s, n) => Siempre(putIds(s, ids), n)
    case SiguienteVez(s, n) => SiguienteVez(putIds(s, ids), n)
    case Negacion(neg, n) => Negacion(putIds(neg, ids), n)
    case AlgunaVez(a, n) => AlgunaVez(putIds(a, ids), n)
    case _ => formula
  }

  /* private def putIds(formula:Formula,
                     ids:collection.immutable.Map[String,String]):(Formula,collection.immutable.Map[String,String]) = formula match {
    case Proposition(p) if p(0)=='_' =>
      var id:String = null
      if (!ids.contains(p)) {
        id = Util.newId(ids.keys.toSet)
      } else {
        id = ids(p)
      }
      (Proposition(id), ids+((p,id)))
    case Or(o) =>
      var ite = o.iterator
      var i = ids
      var s = OrderedListSet[Formula]()
      while (ite.hasNext) {
        var e:Formula = ite.next
        var (f1,i1) = putIds(e,i)
        s+=f1
        i++=i1
      }
      (Or(s),i)
    case And(a) =>
      var ite = a.iterator
      var i = ids
      var s = OrderedListSet[Formula]()
      while (ite.hasNext) {
        var e = ite.next
        var (f1,i1) = putIds(e,i)
        s+=f1
        i++=i1
      }
      (And(s),i)
    case U(u1,u2) =>
      var (f1,i1) = putIds(u1, ids)
      var (f2,i2) = putIds(u2, i1)
      (U(f1, f2), i2)
    case R(r1,r2) =>
      var (f1,i1) = putIds(r1, ids)
      var (f2,i2) = putIds(r2, i1)
      (R(f1, f2), i2)
    case Siempre(s,n) =>
      var (f,i) = putIds(s,ids)
      (Siempre(f,n), i)
    case AlgunaVez(s,n) =>
      var (f,i) = putIds(s,ids)
      (AlgunaVez(f,n), i)
    case Negacion(s,n) =>
      var (f,i) = putIds(s,ids)
      (Negacion(f,n), i)
    case SiguienteVez(s,n) =>
      var (f,i) = putIds(s,ids)
      (SiguienteVez(f,n), i)
    case _ => (formula, ids)
  }*/

}