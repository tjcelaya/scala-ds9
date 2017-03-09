package example

import co.tjcelaya.sigfig_test.spatialindex._
import co.tjcelaya.sigfig_test.spatialindex.exceptions.DuplicateCoordinateException
import org.scalatest._
import sext._

class KdTreeSpec extends FlatSpec with Matchers {
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
    val minCoords = SeqCoordinate(1, 1)
    val t0 = new KdTree()
    val t1 = t0.insert(SeqCoordinate(3, 3))
    val t2 = t1.insert(SeqCoordinate(2, 2))
    val t3 = t2.insert(minCoords)

    t3.size shouldEqual 3

    t3.rootNode.get
      .maybePrev.get
      .maybePrev.get
      .coordinates shouldEqual minCoords
  }

  it should "accept inserts for greater values" in {
    val maxCoords = SeqCoordinate(9, 9)
    val t = new KdTree()
      .insert(SeqCoordinate(5, 5))
      .insert(SeqCoordinate(6, 6))
      .insert(maxCoords)

    t.size shouldEqual 3

    t.rootNode.get
      .maybeNext.get
      .maybeNext.get
      .coordinates shouldEqual maxCoords
  }

  it should "accept a mix of values" in {
    val prevExpected = SeqCoordinate(2, 6)
    val nextExpected = SeqCoordinate(7, 9)
    val t = new KdTree()
      .insert(SeqCoordinate(5, 5))
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
      .insert(SeqCoordinate(30, 40))
      .insert(SeqCoordinate(5, 25))
      .insert(SeqCoordinate(10, 12))
      .insert(SeqCoordinate(70, 70))
      .insert(SeqCoordinate(50, 30))
      .insert(SeqCoordinate(35, 45))

    t.size shouldEqual 6
  }

  it should "prevent duplicates" in {
    assertThrows[DuplicateCoordinateException] {
      val t = new KdTree()
        .insert(SeqCoordinate(1, 2))
        .insert(SeqCoordinate(1, 2))
    }
  }

  it should "accept a nearest neighbor query" in {
    val c0 = SeqCoordinate(5, 5)
    val c1 = SeqCoordinate(1, 2)
    val c2 = SeqCoordinate(4, 3)
    val search = SeqCoordinate(2, 2)

    val t = new KdTree().insert(c0).insert(c1).insert(c2)

    t.query(search).coordinates shouldEqual c1
  }
}
