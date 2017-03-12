package co.tjcelaya.ds9.spatialindex

import co.tjcelaya.ds9.spatialindex.{Distanced, Extrema}

import scala.language.implicitConversions

/**
  * Created by tj on 3/8/17.
  */
package object implicits {

  implicit object IntDistanced extends Distanced[Int] {
    override def distance(thus: Int, that: Int) = (that - thus).asInstanceOf[Number]
  }

  implicit object IntExtrema extends Extrema[Int] {
    override def minVal: Int = Int.MinValue

    override def maxVal: Int = Int.MaxValue
  }

  implicit object IntOrdering extends Ordering[Int] {
    override def compare(x: Int, y: Int): Int = x - y
  }

}
