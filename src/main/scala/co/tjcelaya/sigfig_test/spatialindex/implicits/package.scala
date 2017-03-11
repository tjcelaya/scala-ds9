package co.tjcelaya.sigfig_test.spatialindex

import scala.language.implicitConversions

/**
  * Created by tj on 3/8/17.
  */
package object implicits {

  implicit object IntDistanced extends Distanced[Int] {
    override def distance(thus: Int, that: Int) = (that - thus).asInstanceOf[Number]
  }

}
