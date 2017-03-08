package example

import co.tjcelaya.sigfig_test.spatialindex._
import org.scalatest._

class SpatialIndexSpec extends WordSpec with Matchers {
  "The KdTree" should {
    "be empty by default" in {
      val emptyTree = KdTree(None)
      emptyTree.isEmpty shouldEqual true

      var size = 0
      emptyTree.foreach(node => {
        size = size + 1
      })
      size shouldEqual 0
    }

    "accept inserts for lesser values" in {
      val minCoords = SeqCoordinate(1, 1)
      val t0 = KdTree(Some(KdNode(SeqCoordinate(5, 5))))
        .insert(KdNode(SeqCoordinate(2, 2)))
        .insert(KdNode(minCoords))

      t0.rootNode.get
        .maybePrev.get
        .maybePrev.get
        .coordinates shouldEqual minCoords
    }

    "accept inserts for greater values" in {
      val maxCoords = SeqCoordinate(9, 9)
      val t0 = KdTree(Some(KdNode(SeqCoordinate(5, 5))))
        .insert(KdNode(SeqCoordinate(6, 6)))
        .insert(KdNode(maxCoords))

      t0.rootNode.get
        .maybeNext.get
        .maybeNext.get
        .coordinates shouldEqual maxCoords
    }

    "accept a mix of values" in {
      val prevExpected = SeqCoordinate(2, 6)
      val nextExpected = SeqCoordinate(7, 9)
      val t0 = KdTree(Some(KdNode(SeqCoordinate(5, 5))))
        .insert(KdNode(prevExpected))
        .insert(KdNode(nextExpected))

      t0.rootNode.get
        .maybePrev.get
        .coordinates shouldEqual prevExpected

      t0.rootNode.get
        .maybeNext.get
        .coordinates shouldEqual nextExpected
    }

    "prevent duplicates" in {
      assertThrows[DuplicateCoordinateException] {
        val t0 = KdTree(Some(KdNode(1, 2)))
          .insert(KdNode(1, 2))
      }
    }

    "accept a nearest neighbor query" in {
      val t0 = KdTree(Some(KdNode(5, 5)))
        .insert(KdNode(1, 4))
        .insert(KdNode(4, 3))

      t0.query(SeqCoordinate(2, 2)).get.coordinates shouldEqual SeqCoordinate(4, 3)
    }
  }
}
