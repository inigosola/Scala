package TemporalLogic.util

import collection.TraversableLike
import TemporalLogic.formula._

object Util {
  val ID_INICIAL = 0
  private val prefix_id: String = "_"
  private var id: Int = ID_INICIAL

  def newId: String = {
    id += 1
    prefix_id + id
  }

  def newId(clauses: OrderedListSet[Formula]): String = newId(getIds(clauses))

  def newId(cs: Set[String]): String = {
    var id = "a"
    while (cs.contains(id)) {
      id = nextId(id)
    }
    id
  }

  def getIds(f: Formula): Set[String] = getIds(OrderedListSet[Formula](f))

  def getIds(c: OrderedListSet[Formula]): Set[String] = c.mkString(" ").replaceAll("R|U|S|<>|\\[\\]|\\(|\\)|\\+|-|\\*|0", " ").trim.split( """\s+""").toSet

  def nextChar(c: Char) = if (c == 'z') 'a' else (c.toInt + 1).toChar

  def nextId(s: String) = {
    val c = nextChar(s.last)
    if (c == 'a') s :+ c else s.substring(0, s.length - 1) :+ c
  }

  def resetID() {
    id = ID_INICIAL
  }

  def isFalse(f: Formula): Boolean = f match {
    case BooleanLogic(false) => true
    case Siempre(BooleanLogic(false), _) => true
    case _ => false
  }

  def alws_nows(f: TraversableLike[Formula, Iterable[Formula]]): (Iterable[Formula], Iterable[Formula]) = f.partition(_.isInstanceOf[Siempre])

  def alw(f: TraversableLike[Formula, Iterable[Formula]]): Iterable[Formula] = alws_nows(f)._1

  def now(f: TraversableLike[Formula, Iterable[Formula]]): Iterable[Formula] = alws_nows(f)._2

  def alw_setFormula(c: Formula): (Boolean, OrderedListSet[Formula]) = c match {
    case Siempre(Or(o), _) => (true, o)
    case Siempre(o, _) => (true, OrderedListSet(o))
    case Or(o) => (false, o)
    case null => (false, OrderedListSet())
    case _ => (false, OrderedListSet(c))
  }

  def alg_setFormula(c: Formula): (Boolean, OrderedListSet[Formula]) = c match {
    case AlgunaVez(Or(o), _) => (true, o)
    case AlgunaVez(o, _) => (true, OrderedListSet(o))
    case Or(o) => (false, o)
    case null => (false, OrderedListSet())
    case _ => (false, OrderedListSet(c))
  }

  def cycling(D: List[OrderedListSet[Formula]]): Boolean = {
    (D.length > 0) && (D.take(D.length - 1).contains(D.last))
  }

  def existEvent(o: OrderedListSet[Formula], e: Formula): Boolean = o.exists(existEvent(_, e))

  def existEvent(f: Formula, e: Formula): Boolean = f match {
    case x if x == e => true
    case Or(opes) if opes.exists(_ == e) => true
    case _ => false
  }

  def cycling(g: List[OrderedListSet[Formula]], selfun: FairCollection[Formula]): (Boolean, Int) = {
    var ciclo: Boolean = false
    val js = nowEquals(g)
    var i = 0
    var j = 0
    while (i < js.length && !ciclo) {
      j = js(i)
      val gp = g drop j

      val ts = gp.map(event(_)).reduceLeft(_ intersect _)

      ciclo = ts.forall(t => (j until g.length - 1).flatMap(h => selfun.get(h)).toSet.contains(t))
      i += 1
    }
    (ciclo, j)
  }

  def getNcontainsEvent(lo: List[OrderedListSet[Formula]], f: Formula): Set[Formula] = lo.flatMap(getNcontainsEvent(_, f)).toSet

  def getNcontainsEvent(o: OrderedListSet[Formula], f: Formula): OrderedListSet[Formula] = o.filter(existEvent(_, f))

  def nowEquals(g: List[OrderedListSet[Formula]]): List[Int] = {
    val ultimo = now(g.last)
    val resto = g.init
    var indices = List[Int]()
    var indice = 0
    do {
      var indiceEncontrado = resto.indexWhere(now(_) == ultimo, indice)
      if (indiceEncontrado != -1) {
        indices :+= indiceEncontrado
        indice = indiceEncontrado
      }
      indice += 1
    } while (indice < resto.length)
     indices
  }

  def drop(clausulas: OrderedListSet[Formula]): OrderedListSet[Formula] = clausulas.map(drop(_)).reduceLeft(_ ++ _)

  def drop(c: Formula): OrderedListSet[Formula] = c match {
    case Siempre(s, _) => OrderedListSet(s)
    case _ => OrderedListSet(c)
  }

  def event(lc: List[OrderedListSet[Formula]]): OrderedListSet[Formula] = lc.map(event(_)).reduceLeft(_ ++ _)

  def event(clausulas: Iterable[Formula]): OrderedListSet[Formula] = {
    if (clausulas.size == 0)
      OrderedListSet[Formula]()
    else
      clausulas.map(event(_)).reduceLeft(_ ++ _)
  }

  def event(c: Formula): OrderedListSet[Formula] = c match {
    case u: U => OrderedListSet(u)
    case a: AlgunaVez => OrderedListSet(a)
    case Siempre(s, _) => event(s)
    case Or(opes) => opes.map(event(_)).reduceLeft(_ ++ _)
    case _ => OrderedListSet()
  }

  def getPartes[A](l: List[A]) = (0 until l.length).map(l.slice(_, l.length))

  def eventN(f: Formula, cs: List[OrderedListSet[Formula]]): List[OrderedListSet[Formula]] = cs.map(eventN(f, _))

  def eventN(f: Formula, c: OrderedListSet[Formula]): OrderedListSet[Formula] = c.map(eventN(f, _)).reduceLeft(_ ++ _)

  def eventN(f: Formula, c: Formula): OrderedListSet[Formula] = c match {
    case `f` => OrderedListSet(f)
    case o@Or(opes) if opes contains f => OrderedListSet(o)
    case _ => OrderedListSet[Formula]()
  }

  def containsN(f: Formula, c: OrderedListSet[Formula]): Boolean = c.map(containsN(f, _)).reduceLeft(_ | _)

  def containsN(f: Formula, c: Formula): Boolean = c match {
    case `f` => true
    case o@Or(opes) => opes contains f
    case _ => false
  }

  def getNeventuality(e: Formula, g: List[OrderedListSet[Formula]]) = g.map(containsN(e, _))

  def eventForAll(g: List[OrderedListSet[Formula]]): OrderedListSet[Formula] = g.map(event(_)).reduceLeft(_ & _)

}
