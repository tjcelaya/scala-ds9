package co.tjcelaya.ds9.common

/**
  * Created by tomascelaya on 7/2/17.
  */
trait Monotonic[A] extends Any {
  def increment(): A
}

//final class IncrementingInt(val i: Int) extends AnyVal with Monotonic[Int] {
//  override def increment(): Int = i + 1
//}
//
//
//final class IncrementingZonedDateTime extends Monotonic[ZonedDateTime] {
//  override def increment(): ZonedDateTime = ZonedDateTime.now()
//}
