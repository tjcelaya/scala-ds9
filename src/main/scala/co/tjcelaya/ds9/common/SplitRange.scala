package co.tjcelaya.ds9.common

/**
  * Created by tj on 3/11/17.
  */
case class SplitRange[V: Ordering](lower: V, mid: V, upper: V) {
  val iO = implicitly[Ordering[V]]
  def contains(other: V): Boolean = {
    val lC = iO.gteq(other, lower)
    val uC = iO.lteq(other, upper)
    lC && uC
  }
}
