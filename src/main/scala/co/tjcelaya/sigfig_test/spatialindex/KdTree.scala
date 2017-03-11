package co.tjcelaya.sigfig_test.spatialindex

import co.tjcelaya.sigfig_test.spatialindex.exceptions.{InvalidQueryException, InvalidStateException}

import scala.language.implicitConversions

/**
  * Created by tj on 3/7/17.
  */
object KdTree {
  def apply[V : Distanced](): KdTree[V] = new KdTree(None)

  implicit def toTraversable[V : Distanced](kdTree: KdTree[V]): TraversableKdTree[V] = new
      TraversableKdTree(kdTree)
}

class KdTree[V : Distanced](rootNode: Option[KdNode[V]] = None) {
  type TypedCoordinate = Coordinate[V]
  type TypedNode = KdNode[V]

  def root: Option[TypedNode] = rootNode

  def insert(c: TypedCoordinate): KdTree[V] = rootNode match {
    case None => copy(rootNode = Some(LeafKdNode(c, 0)))
    case Some(r) =>
      val newRoot = this.rootNode.get.insertAtDepth(c, 0)
      copy(rootNode = Some(newRoot))
  }

  def copy(rootNode: Option[TypedNode] = None) = new KdTree(rootNode)

  def query(from: TypedCoordinate): TypedNode = {
    if (rootNode.isEmpty) {
      throw InvalidQueryException()
    }

    import Math.abs
    case class NNResult[T](best: Option[(T, Double)], seen: Set[T] = Set())

    def qR(search: TypedCoordinate,
           depth: Int,
           node: Option[TypedNode],
           champ: NNResult[TypedNode]): NNResult[TypedNode] = {
      // val cA = depth % search.rank
      // val coords = node.coordinates
      // val aD = search axisDistance(coords, cA)
      // val aaD = Math.abs(aD)
      // val tD = search distance coords
      // if (tD < 1) {
      //   throw new Exception()
      // }

      NNResult(None, Set())
    }

    val r = qR(from, 0, rootNode, NNResult(None, Set[TypedNode]())).best
    if (r.isEmpty) null else r.get._1
  }

  def toStringPadded: String = rootNode.get.toStringPadded(0)
}

/**
  * KdTree was originally traversable but this broke debugging output, so this class was
  * created and combined with an implicit conversion on KdTree
  *
  * @param rootNode
  */
class TraversableKdTree[V : Distanced](val rootNode: Option[KdNode[V]] = None)
  extends KdTree[V](rootNode = rootNode)
    with Traversable[KdNode[V]] {

  def this(kdTree: KdTree[V]) = {
    this(kdTree.root)
  }

  def foreach[U](f: (KdNode[V]) => U): Unit = {
    if (rootNode.isEmpty)
      return

    def recur(current: TypedNode): Unit = current match {
      case n: LeafKdNode[V] =>
        f(n)
      case n: LesserKdNode[V] =>
        recur(n.prev)
        f(n)
      case n: GreaterKdNode[V] =>
        f(n)
        recur(n.next)
      case n: BalancedKdNode[V] =>
        recur(n.prev)
        f(n)
        recur(n.next)
    }

    recur(rootNode.get)
  }

  def printCoordinates: String = {
    val cs = this.map(_.coordinates)
    val xs = cs.map(_.apply(0))
    val ys = cs.map(_.apply(1))
    val iD = implicitly[Distanced[V]]
    val minMax = (acc: Option[(V, V)], nex: V) =>
      acc match {
        case None => Some(nex, nex)
        case Some((mi, ma)) =>
          Some((
          if (iD.distance(mi, nex).shortValue() < 0) nex else mi,
          if (iD.distance(ma, nex).shortValue() > 0) nex else ma
        ))
      }

    val Some((minX, maxX)) = xs.foldLeft(Option.empty[(V, V)])(minMax)
    val Some((minY, maxY)) = ys.foldLeft(Option.empty[(V, V)])(minMax)
    s"bounds minX: $minX, minY: $minY, maxX: $maxX maxY $maxY"
  }
}
