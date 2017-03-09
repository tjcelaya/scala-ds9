package co.tjcelaya.sigfig_test.spatialindex

import scala.language.implicitConversions
import scala.runtime.RichInt

/**
  * Created by tj on 3/8/17.
  */
package object implicits {
  implicit def intToComparable(i: Int): Ordered[Int] = new RichInt(i)
}
