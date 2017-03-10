package co.tjcelaya.sigfig_test.spatialindex

/**
  * Created by tj on 3/7/17.
  */
abstract class Coordinate[V](implicit ev: V => Ordered[V]) {
  def apply(dim: Int): V

  def axisDistance(that: Coordinate[V], dim: Int): Int =
    this (dim).asInstanceOf[Int] - that(dim).asInstanceOf[Int]

  def rank: Int = toSeq.size

  def distance(that: Coordinate[V]): Double

  def toSeq: Seq[V]
}

case class SeqCoordinate(coordinates: Int*)
  extends Coordinate[Int] {

  override def apply(dim: Int): Int = {
    val effectiveDim = dim % coordinates.size
    val v = coordinates(effectiveDim)
    v
  }

  override def distance(that: Coordinate[Int]): Double = {
    this.coordinates
      .zip(that.toSeq)
      .foldLeft[Double](0) { (acc: Double, d: (Int, Int)) => acc + Math.pow(d._1 - d._2, 2) }
  }

  override def rank: Int = coordinates.size

  // override def foreach[U](f: (Coordinate) => U): Unit = coordinates.foreach _

  override def toString(): String = "(" + coordinates.mkString(",") + ")"

  override def toSeq: Seq[Int] = coordinates
}
