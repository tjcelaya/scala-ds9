package co.tjcelaya.sigfig_test.spatialindex

/**
  * Created by tj on 3/7/17.
  */
abstract class Coordinate[V: Distanced] {
  def apply(dim: Int): V

  def axisDistance(that: Coordinate[V], dim: Int): Number =
    implicitly[Distanced[V]].distance(that(dim), this (dim))

  def rank: Int = toSeq.size

  def distance(that: Coordinate[V]): Double

  def toSeq: Seq[V]
}

case class SeqCoordinate[V: Distanced](coordinates: V*)
  extends Coordinate[V] {

  override def apply(dim: Int): V = {
    val effectiveDim = dim % coordinates.size
    val v = coordinates(effectiveDim)
    v
  }

  override def distance(that: Coordinate[V]): Double = {
    val iD = implicitly[Distanced[V]]
    this.coordinates
      .zip(that.toSeq)
      .foldLeft[Double](0) {
      (acc: Double, d: (V, V)) =>
        acc + Math.pow(iD.distance(d._2, d._1).doubleValue(), 2)
    }
  }

  override def rank: Int = coordinates.size

  // override def foreach[U](f: (Coordinate) => U): Unit = coordinates.foreach _

  override def toString(): String = "(" + coordinates.mkString(",") + ")"

  override def toSeq: Seq[V] = coordinates
}
