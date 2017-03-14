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

  it should "calculate dimensional bounds" in {
    val n = LeafKdNode[Int](C(1, 2), new Rank(0))
    val bounds0 = n.dimensionBounds(Seq(), new Rank(0))
    val bounds1 = n.dimensionBounds(Seq(), new Rank(1))

    bounds0.mid shouldEqual 1
    bounds1.mid shouldEqual 2
  }

  it should "calculate dimensional bounds with a greater parent" in {
    val child = LeafKdNode[Int](C(2, 2), new Rank(1))

    val root = LesserKdNode[Int](
      C(5, 5),
      new Rank(0),
      child)

    val bounds0 = child.dimensionBounds(Seq(root), new Rank(0))
    val bounds1 = child.dimensionBounds(Seq(root), new Rank(1))

    bounds0.upper shouldEqual 5
    bounds1.upper shouldEqual 5
  }

  // THIS ONE TOO
  it should "calculate dimensional bounds with a lesser parent" in {
    val child = LeafKdNode[Int](C(8, 8), new Rank(1))

    val root = GreaterKdNode[Int](
      C(5, 5),
      new Rank(0),
      child)

    val bounds0 = child.dimensionBounds(Seq(root), new Rank(0))
    val bounds1 = child.dimensionBounds(Seq(root), new Rank(1))

    bounds0.lower shouldEqual 5
    bounds1.lower shouldEqual 5
  }


  Seq(C(2, 2), C(5, 5), C(9, 9)).permutations.foreach({ coordList =>
    val (c1 :: c2 :: c3 :: Nil) = coordList

    it should s"be bounded by all parents using the following insertion order $c1, $c2, $c3" in {
      // val grandChild = LeafKdNode[Int](C(5, 5), new Rank(0))
      // val child = LesserKdNode[Int](C(9, 9), new Rank(1), grandChild)
      // val root = GreaterKdNode[Int](C(2, 2), new Rank(0), child)

      val t0 = KdTree[Int]().insert(C(2, 2)).insert(C(9, 9)).insert(C(5, 5))
      val root = t0.rootNode.get
      val child = root.maybeNext.get
      val grandChild = child.maybePrev.get

      val bounds0 = grandChild.dimensionBounds(Seq(child, root), new Rank(0))
      val bounds1 = grandChild.dimensionBounds(Seq(child, root), new Rank(1))

      bounds0.lower shouldEqual 2
      bounds0.upper shouldEqual 9

      bounds1.lower shouldEqual 2
      bounds1.upper shouldEqual 9
    }
  })
}
