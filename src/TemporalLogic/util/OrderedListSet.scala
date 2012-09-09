package TemporalLogic.util

import collection.SetLike
import collection.generic.{CanBuildFrom, ImmutableSetFactory, GenericCompanion, GenericSetTemplate}

@SerialVersionUID(2L)
class OrderedListSet[A] extends Set[A]
with GenericSetTemplate[A, OrderedListSet]
with SetLike[A, OrderedListSet[A]] {

  override def companion: GenericCompanion[OrderedListSet] = OrderedListSet

  override def size: Int = 0

  override def empty = OrderedListSet.empty[A]

  def iterator: Iterator[A] = Iterator.empty

  override def foreach[U](f: A => U) {}

  def contains(e: A): Boolean = get0(e)

  override def +(e: A): OrderedListSet[A] = updated0(e)

  override def +(elem1: A, elem2: A, elems: A*): OrderedListSet[A] = this + elem1 + elem2 ++ elems

  def -(e: A): OrderedListSet[A] = removed0(e)

  protected def get0(key: A): Boolean = false

  protected def updated0(key: A): OrderedListSet[A] =
    new OrderedListSet.OrderedListSet1(key)

  protected def removed0(key: A): OrderedListSet[A] = this

  def modify(keyFind: A, key: A): OrderedListSet[A] = new OrderedListSet.OrderedListSet1[A](key)

  protected val indexes: List[Int] = List[Int]()

  protected val nextIndex: Int = 1

  def pairIterator: Iterator[(A, Int)] = Iterator.empty

  protected def writeReplace(): AnyRef = new OrderedListSet.SerializationProxy(this)

  def pairForeach[U](f: ((A, Int)) => U): Unit = {}

  def resetIndexes: OrderedListSet[A] = this

  override def toString() = {
    var result = ""
    val ite = pairIterator
    while (ite.hasNext) {
      var (a, b) = ite.next()
      result += b.toString + ". " + a.toString + "\n"
    }
    if (result.isEmpty) result else result.substring(0, result.length - 1)
  }
}

object OrderedListSet extends ImmutableSetFactory[OrderedListSet] {
  /** $setCanBuildFromInfo */
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, OrderedListSet[A]] = setCanBuildFrom[A]

  override def empty[A]: OrderedListSet[A] = EmptyOrderedListSet.asInstanceOf[OrderedListSet[A]]

  private object EmptyOrderedListSet extends OrderedListSet[Any] {
  }

  class OrderedListSet1[A](private[OrderedListSet] var key: A) extends OrderedListSet[A] {

    override def size = 1

    override val indexes = List[Int](1)

    override val nextIndex = indexes.head + 1

    override def get0(key: A): Boolean = (key == this.key)

    override def updated0(key: A): OrderedListSet[A] =
      if (this.key == key) {
        this
      } else {
        val m = new EEOrderedListSet[A](List[A](this.key), indexes, nextIndex)
        m.updated0(key)
      }

    override def removed0(key: A): OrderedListSet[A] = if (key == this.key) OrderedListSet.empty[A] else this

    override def modify(keyFind: A, key: A): OrderedListSet[A] = {
      if (this.key == keyFind) {
        new OrderedListSet.OrderedListSet1[A](key)
      } else {
        (new OrderedListSet.OrderedListSet1[A](this.key)).updated0(key)
      }
    }

    override def iterator = Iterator(key)

    override def pairIterator: Iterator[(A, Int)] = Iterator((key, indexes.head))

    override def foreach[U](f: A => U): Unit = f(key)

    override def pairForeach[U](f: ((A, Int)) => U) = f(key, indexes.head)
  }


  class EEOrderedListSet[A](private var elems: List[A],
                            override protected[OrderedListSet] val indexes: List[Int],
                            override protected[OrderedListSet] val nextIndex: Int)
    extends OrderedListSet[A] {

    override def size = elems.size

    override def get0(key: A): Boolean = elems.contains(key)

    override def updated0(key: A): OrderedListSet[A] = {
      if (elems contains key) {
        this
      } else {
        new EEOrderedListSet(elems.:+(key), indexes.:+(nextIndex), nextIndex + 1)
      }
    }

    override def removed0(key: A): OrderedListSet[A] = {
      val r = elems findIndexOf (_ == key)
      if (r != -1) {
        val e = elems filterNot (_ == key)
        val (i1, i2) = indexes splitAt r
        val i = i1 ++ i2.tail
        new EEOrderedListSet(e, i, nextIndex)
      } else {
        this
      }
    }

    override def modify(keyFind: A, key: A): OrderedListSet[A] = {
      if (elems contains keyFind) {
        if (!elems.contains(key)) {
          val i = elems.indexOf(keyFind)
          val e = elems.updated(i, key)
          new OrderedListSet.EEOrderedListSet[A](e, indexes, nextIndex)
        } else {
          this.removed0(key)
        }
      } else {
        this.updated0(key)
      }
    }

    override def iterator = elems.iterator

    override def pairIterator: Iterator[(A, Int)] = (elems zip indexes).iterator

    override def foreach[U](f: A => U): Unit = elems.foreach(f)

    override def pairForeach[U](f: ((A, Int)) => U): Unit = (elems zip indexes).foreach(f)

    override def resetIndexes: OrderedListSet[A] = {
      new OrderedListSet.EEOrderedListSet(elems, 1 to elems.length toList, elems.length + 1)
    }
  }

  @SerialVersionUID(2L) private class SerializationProxy[A, B](@transient private var orig: OrderedListSet[A]) {
    private def writeObject(out: java.io.ObjectOutputStream) {
      val s = orig.size
      out.writeInt(s)
      for (e <- orig) {
        out.writeObject(e)
      }
    }

    private def readObject(in: java.io.ObjectInputStream) {
      orig = empty
      val s = in.readInt()
      for (i <- 0 until s) {
        val e = in.readObject().asInstanceOf[A]
        orig = orig + e
      }
    }

    private def readResolve(): AnyRef = orig
  }

}
