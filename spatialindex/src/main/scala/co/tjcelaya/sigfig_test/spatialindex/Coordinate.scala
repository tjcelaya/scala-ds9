package co.tjcelaya.sigfig_test.spatialindex

/**
  * Created by tj on 3/7/17.
  */
trait Coordinate extends Traversable[Coordinate] {
  def apply(dim: Int): Int

  def compare(that: Coordinate, dim: Int): Int = this (dim) - that(dim)

  def rank: Int
}

case class SeqCoordinate(coordinates: Int*)
  extends Coordinate {
  override def apply(dim: Int): Int = {
    coordinates(dim % coordinates.size)
  }

  override def rank: Int = coordinates.size

  override def foreach[U](f: (Coordinate) => U): Unit = coordinates.foreach _

  override def toString(): String = "(" + coordinates.mkString(",") + ")"
}
