package co.tjcelaya.ds9.spatialindex

import co.tjcelaya.ds9.common.{Rank, SplitRange}
import co.tjcelaya.ds9.spatialindex.exceptions._

import scala.language.implicitConversions

/**
  * Created by tj on 3/7/17.
  */
object KdTree {
  def apply[V: Distanced : Extrema : Ordering](): KdTree[V] = new KdTree(None)

  implicit def toTraversable[V: Distanced : Extrema : Ordering](kdTree: KdTree[V]): TraversableKdTree[V] = new
      TraversableKdTree(kdTree)
}

class KdTree[V: Distanced : Extrema : Ordering](rootNode: Option[KdNode[V]] = None) {
  type TypedSplitRange = SplitRange[V]
  val TypedSplitRange = SplitRange
  type TypedCoordinate = Coordinate[V]
  type TypedNode = KdNode[V]

  def root: Option[TypedNode] = rootNode

  def insert(c: TypedCoordinate): KdTree[V] = rootNode match {
    case None => copy(rootNode = Some(LeafKdNode(c, new Rank(0))))
    case Some(r) =>
      val newRoot = this.rootNode.get.insertAtDepth(c, 0)
      copy(rootNode = Some(newRoot))
  }

  def copy(rootNode: Option[TypedNode] = None) = new KdTree(rootNode)

  def query(from: TypedCoordinate): TypedNode = {
    if (rootNode.isEmpty) {
      throw InvalidQueryException()
    }

    case class NNResult[V](best: Option[(V, Double)] = None, seen: Set[V] = Set())
    val emptyChamp = NNResult[TypedNode](None, Set[TypedNode]())


    def sphereWithinBounds(space: Seq[TypedSplitRange], coordinate: TypedCoordinate): Boolean = {
      (0 to coordinate.rank.v).forall((i: Int) => {
        val r = new Rank(i)
        space(r.v).contains(coordinate(r))
      })
    }

    def considerUpdate(node: TypedNode, distance: Double, champ: NNResult[TypedNode]): NNResult[TypedNode] =
      champ match {
        case NNResult(None, _) =>
          champ.copy(best = Some((node, distance.doubleValue)))
        case NNResult(Some((cbN, cbD)), _) if distance.doubleValue() < cbD =>
          champ.copy(best = Some((node, distance.doubleValue)))
        case _ => champ
      }

    def qR(search: TypedCoordinate,
           depth: Int,
           maybeParent: Option[TypedNode],
           node: TypedNode,
           champ: NNResult[TypedNode]): NNResult[TypedNode] = {
      val (axisRank: Rank, cmp: Number) = node.compareOnAxis(search, depth)
      val coords = node.coordinates
      val tD = node.coordinates.distance(search)
      val aaD = Math.abs(cmp.doubleValue)

      val leafUpdate =
        if (node.isInstanceOf[LeafKdNode[_]]) {
          considerUpdate(node, tD, champ)
        } else {
          champ
        }

      val preferPrev = cmp.shortValue <= 0

      val nearerUpdate =
        if (preferPrev && node.maybePrev.isDefined) {
          qR(search, depth + 1, Some(node), node.maybePrev.get, leafUpdate)
        } else if (!preferPrev && node.maybeNext.isDefined) {
          qR(search, depth + 1, Some(node), node.maybeNext.get, leafUpdate)
        } else {
          leafUpdate
        }

      val hb = node.hyperBounds(maybeParent)
      val shouldCheckFurther = false

      val furtherUpdate =
        if (shouldCheckFurther) {
          if (preferPrev && node.maybeNext.isDefined) {
            qR(search, depth + 1, Some(node), node.maybeNext.get, leafUpdate)
          } else if (!preferPrev && node.maybePrev.isDefined) {
            qR(search, depth + 1, Some(node), node.maybePrev.get, leafUpdate)
          } else {
            nearerUpdate
          }
        } else {
          nearerUpdate
        }

      val seenUpdate = furtherUpdate.copy(seen = furtherUpdate.seen + node)

      val fallbackUpdate =
        if (seenUpdate.best.isEmpty) {
          seenUpdate.copy(best = Some((node, tD)))
        } else {
          seenUpdate
        }


      fallbackUpdate
    }

    val r = qR(from, 0, None, rootNode.get, emptyChamp).best
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
class TraversableKdTree[V: Distanced : Extrema : Ordering](val rootNode: Option[KdNode[V]] = None)
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
    val xs = cs.map(_.apply(new Rank(0)))
    val ys = cs.map(_.apply(new Rank(1)))
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
