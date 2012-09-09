package TemporalLogic.util

class FairCollection[T](var collection: OrderedListSet[T] = OrderedListSet[T]()) {

  var selfun: List[OrderedListSet[T]] = List[OrderedListSet[T]]()

  var selected: OrderedListSet[T] = OrderedListSet[T]() //selfun.head

  def update(i: Int, u: OrderedListSet[T]) {
    val (tratados, nuevos) = u partition (selected contains _)
    collection = nuevos ++ tratados
    if ((selfun.isEmpty || (selfun.last & collection).isEmpty) && !collection.isEmpty) {
      put(i, OrderedListSet[T](balanceSelection(u.toList)))
    } else {
      put(i, OrderedListSet[T]())
    }
  }

  def get: OrderedListSet[T] = if (selfun.isEmpty) OrderedListSet[T]() else selfun.last

  def get(i: Int): OrderedListSet[T] = selfun(i)

  def put(i: Int, s: OrderedListSet[T]) {
    if (i == selfun.length) {
      selfun :+= s
    } else {
      selfun = selfun.updated(i, s)
    }
    selected ++= s
  }

  def propagate() {
    put(selfun.length, selfun.last)
  }

  override def toString: String = selfun.mkString("{ ", ", ", "}")

  private def balanceSelection(fs: List[T]): T = {
    var s: T = fs.head
    var is: Int = selfun.size
    var i: Int = 0
    var found: Boolean = false
    while (!found && i < fs.size) {
      var f = fs(i)
      var z = selfun.lastIndexOf(OrderedListSet(f))
      z match {
        case -1 =>
          found = true
          s = f
        case li if li <= is =>
          is = li
          s = f
        case _ =>
      }
      i += 1
    }
    s
  }

}