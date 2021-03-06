package co.tjcelaya.ds9.spatial

import co.tjcelaya.ds9.spatial.exceptions.DuplicateCoordinateException
import org.scalatest.{FlatSpec, Matchers}

class KdTreeSpec extends FlatSpec with Matchers {

  import co.tjcelaya.ds9.spatial.implicits._

  type C = SeqCoordinate[Int]
  val C = SeqCoordinate // object equivalent of `type C = SeqCoordinate`

  it should "be empty by default" in {
    val emptyTree = KdTree[Int]()
    emptyTree.isEmpty shouldEqual true
    val x = 1

    var size = 0
    emptyTree.foreach(node => {
      size = size + 1
    })
    size shouldEqual 0
  }

  it should "accept inserts for lesser values" in {
    val minCoords = C(1, 1)
    val t0 = KdTree[Int]()
    val t1 = t0.insert(C(3, 3))
    val t2 = t1.insert(C(2, 2))
    val t3 = t2.insert(minCoords)

    t3.size shouldEqual 3

    t3.rootNode.get
      .maybePrev.get
      .maybePrev.get
      .coordinates shouldEqual minCoords
  }

  it should "accept inserts for greater values" in {
    val maxCoords = C(9, 9)
    val t = KdTree[Int]()
      .insert(C(5, 5))
      .insert(C(6, 6))
      .insert(maxCoords)

    t.size shouldEqual 3

    t.rootNode.get
      .maybeNext.get
      .maybeNext.get
      .coordinates shouldEqual maxCoords
  }

  it should "accept a mix of values" in {
    val prevExpected = C(2, 6)
    val nextExpected = C(7, 9)
    val t = KdTree[Int]()
      .insert(C(5, 5))
      .insert(prevExpected)
      .insert(nextExpected)

    t.size shouldEqual 3

    t.rootNode.get
      .maybePrev.get
      .coordinates shouldEqual prevExpected

    t.rootNode.get
      .maybeNext.get
      .coordinates shouldEqual nextExpected
  }

  it should "accept more inserts" in {
    val t = KdTree[Int]()
      .insert(C(30, 40))
      .insert(C(5, 25))
      .insert(C(10, 12))
      .insert(C(70, 70))
      .insert(C(50, 30))
      .insert(C(35, 45))

    t.size shouldEqual 6
  }

  it should "prevent duplicates" in {
    assertThrows[DuplicateCoordinateException] {
      val t = KdTree[Int]()
        .insert(C(1, 2))
        .insert(C(1, 2))
    }
  }

  it should "answer a nearest neighbor query" in {
    val c0 = C(5, 5)

    val c1 = C(1, 2)
    val c2 = C(4, 3)
    val search = C(2, 2)

    val t = Seq(C(5, 5), C(4, 3), c1).foldLeft[KdTree[Int]](KdTree())(_.insert(_))

    t.query(search).coordinates shouldEqual c1
  }

  // the following code dynamically generates tests from a grid 10 x 10 grid
  // uses a test per assertion to make it easier to guage progress

  private val points = Seq(C(2, 3), C(5, 4), C(9, 6), C(4, 7), C(8, 1), C(7, 2))

  // foldLeft accepts an initial val (an empty tree here)
  // and a function from ((acc: $INITIAL_VAL_T, $T_OF_SEQ) => updated $INITIAL_VAL_T)
  private val t = points.foldLeft[KdTree[Int]](KdTree[Int]())(_.insert(_))

  // generate a grid to thoroughly test ideal point selection
  Range(1, 10)
    .flatMap(x => Range(1, 10).map(y => C(x, y)))
    .foreach { (c: C) =>
      val ideals: Seq[SeqCoordinate[Int]] = points
        .groupBy(_.distance(c))
        .toSeq
        .minBy(_._1)
        ._2

      it should s"find the correct nearest for $c (which should be one of ${ideals.mkString(",")}" in {

        val res = t.query(c).coordinates

        ideals.contains(res) shouldEqual true
      }
    }

}
