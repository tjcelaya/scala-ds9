package co.tjcelaya.ds9.common

/**
  * Created by tomascelaya on 7/2/17.
  */
trait Interval[T] {
  def start: Option[T]

  def end: Option[T]

  def withStart(start: T): Interval[T]

  def withEnd(end: T): Interval[T]
}

object Interval {
  def unapply[T](interval: Interval[T]): Option[(Option[T], Option[T])] = {
    Some(interval.start, interval.end)
  }
}


case class OptionInterval[T](startVal: Option[T] = None,
                             endVal: Option[T] = None)
  extends Interval[T] {

  override def start: Option[T] = startVal

  override def end: Option[T] = endVal

  override def withStart(newStart: T): Interval[T] = copy(startVal = Some(newStart))

  override def withEnd(newEnd: T): Interval[T] = copy(endVal = Some(newEnd))
}