package co.tjcelaya.sigfig_test.spatialindex

import co.tjcelaya.sigfig_test.spatialindex.exceptions.InvalidQueryException

import scala.collection.mutable
import scala.collection.mutable.Stack
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
    var currentBest: IntNode = null
    var currentBestDistance: Double = currentNode.coordinates.distance(from)
    var pathStash = List[IntNode]()
    var depth = 0

    while (!stop) {
      val dist = currentNode.coordinates.distance(from)
      val sto = stop
      val des = descending
      var curN = currentNode
      var curB = currentBest
      var curBD = currentBestDistance
      var pat = pathStash
      val dep = depth

      if (dist < currentBestDistance) {
        currentBest = currentNode
        currentBestDistance = dist
        curB = currentBest
      }

      if (descending) {
        val (_, _, cmp) = currentNode.compareOnAxis(from, depth)
        if (cmp == 0 && currentNode.coordinates == from) {
          currentBest = currentNode
          stop = true
        } else if (cmp < 0
          && currentNode.maybePrev.isDefined
          && currentNode.maybePrev.get.coordinates.distance(from) < currentBestDistance
        ) {
            pathStash = currentNode :: pathStash
            currentNode = currentNode.maybePrev.get
            depth = depth + 1
        } else if (0 <= cmp
          && currentNode.maybeNext.isDefined
          && currentNode.maybeNext.get.coordinates.distance(from) < currentBestDistance
        ) {
            pathStash = currentNode :: pathStash
            currentNode = currentNode.maybeNext.get
            depth = depth + 1
        } else {
          println(s"changing query traversal direction at $currentNode")
          descending = false
        }
      } else /* ascending */ {
        currentNode = pathStash.head
        curN = currentNode
        pathStash = pathStash.tail
        pat = pathStash

        stop = true
      }
    }
    currentBest
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

  def printCoordinates: Unit = {
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
    println(s"bounds minX: $minX, minY: $minY, maxX: $maxX maxY $maxY ")
  }
}
