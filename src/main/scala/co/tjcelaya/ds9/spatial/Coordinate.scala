package co.tjcelaya.ds9.spatial

import co.tjcelaya.ds9.common.Rank

/**
  * Created by tj on 3/7/17.
  */
abstract class Coordinate[V: Distanced] {
  def apply(rank: Rank): V

  def axisDistance(that: Coordinate[V], r: Rank): Number =
    implicitly[Distanced[V]].distance(that(r), this (r))

  def rank: Rank = new Rank(toSeq.size)

  def distance(that: Coordinate[V]): Double

  def toSeq: Seq[V]
}

case class SeqCoordinate[V: Distanced](coordinates: V*)
  extends Coordinate[V] {

  override def apply(rank: Rank): V = {
    val effectiveDim = rank.v % coordinates.size
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

  override def rank: Rank = new Rank(coordinates.size)

  // override def foreach[U](f: (Coordinate) => U): Unit = coordinates.foreach _

  override def toString(): String = "(" + coordinates.mkString(",") + ")"

  override def toSeq: Seq[V] = coordinates
}
