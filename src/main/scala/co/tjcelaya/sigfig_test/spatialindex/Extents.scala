package co.tjcelaya.sigfig_test.spatialindex

import co.tjcelaya.sigfig_test.spatialindex.exceptions.InvalidQueryException

/**
  * Created by tj on 3/11/17.
  */
object Extents {
  def minVal[T](v: T): T = (v match {
    case _: Int => Int.MinValue
    case _: Double => Double.MinValue
    case _ => throw new InvalidQueryException()
  }).asInstanceOf[T]

  def maxVal[T](v: T): T = (v match {
    case _: Int => Int.MaxValue
    case _: Double => Double.MaxValue
    case _ => throw new InvalidQueryException()
  }).asInstanceOf[T]
}
