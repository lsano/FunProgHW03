object intsets
{
  val t1 = new NonEmpty(3, new Empty, new Empty)
  val t2 = t1.incl(5)
  val t3 = t2.incl(6)
  val t4 = t3.incl(7)
  val filterTest = t4.filter((x:Int) => x == 5 )
  val filterTest2 = t4.filter(x => x == 6)
  val filterTest3 = t4.filter(x => x == 7)
  val filterTest4 = t4.filter(x => x == 3)

  val unionTest = t4.union(new NonEmpty(10, new Empty, new Empty))

}
abstract class IntSet
{
  def incl(x:Int): IntSet
  def contains(x:Int):Boolean
  def union(other:IntSet):IntSet
  def filter(p:Int => Boolean):IntSet
  def filterAcc(p:Int => Boolean, acc:IntSet):IntSet
}
class Empty extends IntSet
{
  def contains(x:Int):Boolean = false
  def incl(x:Int): IntSet = new NonEmpty(x, new Empty, new Empty)
  override def toString = "."
  def union(other:IntSet):IntSet = other
  def filter(p:Int=>Boolean):IntSet = new Empty
  def filterAcc(p:Int=>Boolean, acc:IntSet):IntSet = acc
}
class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet
{
  def contains(x: Int): Boolean =
  {
    if (x < elem) left.contains(x)
    else if (x > elem) right.contains(x)
    else true
  }
  def incl(x:Int): IntSet =
  {
    if (x < elem) new NonEmpty(elem, left.incl(x), right)
    else if (x > elem) new NonEmpty(elem, left, right.incl(x))
    else this
  }
  override def toString = "{" + left + elem + right + "}"
  def union(other:IntSet): IntSet =
    (left.union(right)  union other) incl elem
  def filter(p:Int=>Boolean):IntSet =
  {
    filterAcc(p, new Empty)
  }
  def filterAcc(p:Int => Boolean, acc:IntSet) : IntSet =
  {
    if(p(elem)) left.filterAcc(p, right.filterAcc(p,acc.incl(elem)))
    else left.filterAcc(p,right.filterAcc(p,acc))

  }

}