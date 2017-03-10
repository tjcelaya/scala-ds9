package co.tjcelaya.sigfig_test.spatialindex

import scala.language.implicitConversions

/**
  * Created by tj on 3/8/17.
  */
package object implicits {
  implicit def intToComparable(i: Int): Ordered[Int] = new Ordered[Int] {
    override def compare(that: Int): Int = that - this.asInstanceOf[Int]
  }
}
