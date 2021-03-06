package co.tjcelaya.ds9.common

/**
  * Created by tj on 3/11/17.
  */
case class SplitRange[V: Ordering](lower: V, mid: V, upper: V) {
  private val iO = implicitly[Ordering[V]]

  def contains(other: V): Boolean = iO.gteq(other, lower) && iO.lteq(other, upper)
}
