package example

import co.tjcelaya.sigfig_test.spatialindex._
import co.tjcelaya.sigfig_test.spatialindex.exceptions.DuplicateCoordinateException
import org.scalatest._
import sext._

class KdTreeSpec extends FlatSpec with Matchers {
  type C = SeqCoordinate
  val C = SeqCoordinate // object equivalent of `type C = SeqCoordinate`

  it should "be empty by default" in {
    val emptyTree = new KdTree()
    emptyTree.isEmpty shouldEqual true

    var size = 0
    emptyTree.foreach(node => {
      size = size + 1
    })
    size shouldEqual 0
  }

  it should "accept inserts for lesser values" in {
    val minCoords = C(1, 1)
    val t0 = new KdTree()
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
    val t = new KdTree()
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
    val t = new KdTree()
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
    val t = new KdTree()
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
      val t = new KdTree()
        .insert(C(1, 2))
        .insert(C(1, 2))
    }
  }

  it should "answer a nearest neighbor query" in {
    val c0 = C(5, 5)
    val c1 = C(1, 2)
    val c2 = C(4, 3)
    val search = C(2, 2)

    val t = new KdTree().insert(c0).insert(c1).insert(c2)

    t.query(search).coordinates shouldEqual c1
  }

  // the following code dynamically generates tests from a grid overlaid on the

  val points = Seq(
    C(2, 3),
    C(5, 4),
    C(9, 6),
    C(4, 7),
    C(8, 1),
    C(7, 2)
    // foldLeft accepts an initial val (an empty tree here)
    // and a function from ((acc: $INITIAL_VAL_T, $T_OF_SEQ) => updated $INITIAL_VAL_T)
  )
  val t = points.foldLeft[KdTree](KdTree())(_.insert(_))

  val minSearch = C(2, 2)
  val maxSearch = C(100, 100)
  val anotherSearch = C(4, 4)
  val weirderSearch = C(7, 6)

  // generate a grid to thoroughly test ideal point selection
  Range(1, 10)
    .flatMap(x => Range(1, 10).map(y => C(x, y)))
    //          Seq(
    //        minSearch,
    //        maxSearch,
    //        anotherSearch,
    //        weirderSearch
    //      )
    .foreach { (c: C) =>
    val (ideal, _) = points.zip(points.map(_.distance(c))).minBy(_._2)
    it should s"find the correct nearest for $c (which should be $ideal)" in {
      t.query(c).coordinates shouldEqual ideal
    }
  }
}
