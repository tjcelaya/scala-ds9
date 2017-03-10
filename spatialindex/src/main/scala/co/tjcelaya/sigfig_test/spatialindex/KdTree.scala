package co.tjcelaya.sigfig_test.spatialindex

import co.tjcelaya.sigfig_test.spatialindex.exceptions.InvalidQueryException

import scala.language.implicitConversions

/**
  * Created by tj on 3/7/17.
  */
object KdTree {
  implicit def toTraversable(kdTree: KdTree): TraversableKdTree = new TraversableKdTree(kdTree)
}

case class KdTree(rootNode: Option[KdNode[Int]] = None) {
  type IntCoordinate = Coordinate[Int]
  type IntNode = KdNode[Int]

  def root: Option[IntNode] = rootNode

  def insert(c: IntCoordinate): KdTree = rootNode match {
    case None => copy(rootNode = Some(LeafKdNode(c, 0)))
    case Some(r) =>
      val newRoot = this.rootNode.get.insertAtDepth(c, 0)
      copy(rootNode = Some(newRoot))
  }

  def copy(rootNode: Option[IntNode] = None) = new KdTree(rootNode)

  def query(from: IntCoordinate): IntNode = {
    if (rootNode.isEmpty)
      throw InvalidQueryException()

    var stop = false
    var descending = true
    var currentNode: IntNode = rootNode.get
    var currentBest: Option[IntNode] = None
    var currentBestDistance: Option[Double] = None
    var pathStash = List[IntNode]()
    var exhausted = Set[IntNode]()
    var depth = 0

    while (!stop) {
      val sto = stop
      val des = descending
      var curN = currentNode
      var curB = currentBest
      var curBD = currentBestDistance
      var pat = pathStash
      val dep = depth
      val (_, _, axisDist) = currentNode.compareOnAxis(from, depth)
      val totalDist = currentNode.coordinates.distance(from)

      if (descending) {
        if (axisDist == 0 && currentNode.coordinates == from) {
          currentBest = Some(currentNode)
          stop = true
        } else if (axisDist < 0 && currentNode.maybePrev.isDefined) {
          pathStash = currentNode :: pathStash
          currentNode = currentNode.maybePrev.get
          depth = depth + 1
        } else if (0 <= axisDist && currentNode.maybeNext.isDefined) {
          pathStash = currentNode :: pathStash
          currentNode = currentNode.maybeNext.get
          depth = depth + 1
        } else {
          descending = false
          if (currentBest.isEmpty && currentNode.isInstanceOf[LeafKdNode[_]]) {
            currentBest = Some(currentNode)
            currentBestDistance = Some(totalDist)
          }
        }
      } else /* ascending */ {
        if (
          currentBestDistance.isEmpty
            ||
            (currentBestDistance.isDefined && totalDist < currentBestDistance.get)
        ) {
          currentBest = Some(currentNode)
          currentBestDistance = Some(totalDist)
        } else if (
          currentBestDistance.isDefined
            && Math.abs(axisDist) < currentBestDistance.get
            && !exhausted.contains(currentNode)
        ) {
          exhausted = exhausted + currentNode

          if (axisDist <= 0 && currentNode.maybeNext.isDefined) {
            pathStash = currentNode :: pathStash
            currentNode = currentNode.maybeNext.get
            depth = depth + 1
            descending = true
          } else if (0 < axisDist && currentNode.maybePrev.isDefined) {
            pathStash = currentNode :: pathStash
            currentNode = currentNode.maybePrev.get
            depth = depth + 1
            descending = true
          } else if (pathStash.nonEmpty) {
            currentNode = pathStash.head
            pathStash = pathStash.tail
            if (depth == 0) {
              throw new Exception()
            }
            depth = depth - 1
          }
        } else if (pathStash.nonEmpty) {
          currentNode = pathStash.head
          pathStash = pathStash.tail
          if (depth == 0) {
            throw new Exception()
          }
          depth = depth - 1
        } else {
          stop = true
        }
      }
    }

    currentBest.get
  }

  def toStringPadded: String = rootNode.get.toStringPadded(0)

}

class TraversableKdTree(override val rootNode: Option[KdNode[Int]] = None)
  extends KdTree(rootNode = None)
    with Traversable[KdNode[Int]] {

  def this(kdTree: KdTree) = {
    this(kdTree.root)
  }

  def foreach[U](f: (KdNode[Int]) => U): Unit = {
    if (rootNode.isEmpty)
      return

    def recur(current: IntNode): Unit = current match {
      case n: LeafKdNode[Int] =>
        f(n)
      case n @ (previshNode: LesserKdNode[Int]) =>
        recur(previshNode.prev)
        f(n)
      case n @ (nextishNode: GreaterKdNode[Int]) =>
        f(n)
        recur(nextishNode.next)
      case n @ (balancedNode: BalancedKdNode[Int]) =>
        recur(balancedNode.prev)
        f(n)
        recur(balancedNode.next)
    }

    recur(rootNode.get)
  }

  def printCoordinates: String = {
    val cs = this.map(_.coordinates)
    val xs = cs.map(_.apply(0))
    val ys = cs.map(_.apply(1))
    val minMax = (acc: Option[(Int, Int)], nex: Int) =>
      acc match {
        case None => Some(nex, nex)
        case Some((mi, ma)) => Some(mi min nex, ma max nex)
      }

    val Some((minX, maxX)) = xs.foldLeft(Option.empty[(Int, Int)])(minMax)
    val Some((minY, maxY)) = ys.foldLeft(Option.empty[(Int, Int)])(minMax)
    s"bounds minX: $minX, minY: $minY, maxX: $maxX maxY $maxY"
  }
}
