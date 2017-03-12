package co.tjcelaya.ds9.spatialindex

import co.tjcelaya.ds9.common.Rank
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by tj on 3/11/17.
  */
class KdNodeSpec extends FlatSpec with Matchers {

  import co.tjcelaya.ds9.spatialindex.implicits._
  type C = SeqCoordinate[Int]
  val C = SeqCoordinate // object equivalent of `type C = SeqCoordinate`

  it should "be a thing" in {
    val n = LeafKdNode[Int](C(1, 2), new Rank(0))
  }
}
