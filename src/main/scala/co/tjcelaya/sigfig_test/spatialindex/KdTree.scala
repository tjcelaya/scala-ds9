package co.tjcelaya.sigfig_test.spatialindex

import co.tjcelaya.sigfig_test.spatialindex.exceptions.{InvalidQueryException, InvalidStateException}

import scala.language.implicitConversions

/**
  * Created by tj on 3/7/17.
  */
object KdTree {
  def apply(): KdTree = new KdTree

  implicit def toTraversable(kdTree: KdTree): TraversableKdTree = new TraversableKdTree(kdTree)
}

class KdTree(rootNode: Option[KdNode[Int]] = None) {
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

  def queryR(from: IntCoordinate): IntNode = {
    import Math.abs
    case class NNResult[T](best: Option[(T, Double)], seen: Set[T] = Set())

    def qR(search: IntCoordinate,
           depth: Int,
           node: IntNode,
           nnRes: NNResult[IntNode]): NNResult[IntNode] = {
      val cA = depth % search.rank
      val coords = node.coordinates
      val aD = search axisDistance(coords, cA)
      val aaD = Math.abs(aD)
      val tD = search distance coords
      if (tD < 1) {
        throw new Exception()
      }

      (node, nnRes) match {

        // insertion traversal phase

        case (lfN: LeafKdNode[Int], NNResult(None, _)) =>
          // reached a leaf and don't have a best yet, set it
          nnRes copy (best = Some(lfN, tD))
        case (ltN: LesserKdNode[Int], eR @ NNResult(None, _)) if aD < 0 =>
          qR(search, depth + 1, ltN.prev, eR) match {
            case sR @ NNResult(Some(_), _) => sR
            case _ => eR
          }
        case (gtN: GreaterKdNode[Int], eR @ NNResult(None, _)) if 0 <= aD =>
          qR(search, depth + 1, gtN.next, eR) match {
            case sR @ NNResult(Some(_), _) => sR
            case _ => eR
          }
        case (blN: BalancedKdNode[Int], eR @ NNResult(None, _)) if aD < 0 =>
          qR(search, depth + 1, blN.prev, eR) match {
            case sR @ NNResult(Some((_, nbD)), _) if aaD < nbD =>
              qR(search, depth + 1, blN.next, sR)
            case sR @ NNResult(Some((_, nbD)), _) => sR
            case _ => eR
          }
        case (blN: BalancedKdNode[Int], eR @ NNResult(None, _)) if 0 <= aD =>
          qR(search, depth + 1, blN.next, eR) match {
            case sR @ NNResult(Some((_, nbD)), _) if aaD < nbD =>
              qR(search, depth + 1, blN.prev, sR)
            case sR @ NNResult(Some((_, nbD)), _) => sR
            case _ => eR
          }

        case (lfN: LeafKdNode[Int], NNResult(Some((cbN, cbD)), _)) if tD < cbD =>
          nnRes copy (best = Some(lfN, tD))


        // attempting alternates after reaching the node we'd insert at

        case (ltN: LesserKdNode[Int], cR @ NNResult(Some((_, cbD)), _)) if aaD < cbD =>
          qR(search, depth + 1, ltN.prev, nnRes) match {
            case nR @ NNResult(Some((_, nbD)), _) if nbD < tD => nR
            case _ => cR
          }
        case (gtN: GreaterKdNode[Int], cR @ NNResult(Some((_, cbD)), _)) if aaD < cbD =>
          qR(search, depth + 1, gtN.next, nnRes) match {
            case sR @ NNResult(Some((_, nbD)), _) if nbD < tD => sR
            case _ => cR
          }

        case (n: IntNode, NNResult(None, _)) =>
          qR(search, depth, n, nnRes.copy(best = Some(n, tD)))

        case (n: IntNode, NNResult(Some((_, cbD)), _)) if tD < cbD =>
          nnRes.copy(best = Some((n, tD)))

        case _ =>
          println(s"giving up from $node")
          nnRes
        // val checkNext = qR(search, depth + 1, blN.prev, nnRes)
      }
    }

    val r = qR(from, 0, rootNode.get, NNResult(None, Set[IntNode]())).best
    if (r.isEmpty) null else r.get._1
  }

  /**
    * Apologies, should've stuck with the functional style, this needs a lot of cleaning up...
    *
    * @param from
    * @return
    */
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

/**
  * KdTree was originally traversable but this broke debugging output, so this class was
  * created and combined with an implicit conversion on KdTree
  *
  * @param rootNode
  */
class TraversableKdTree(val rootNode: Option[KdNode[Int]] = None)
  extends KdTree(rootNode = rootNode)
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
      case n: LesserKdNode[Int] =>
        recur(n.prev)
        f(n)
      case n: GreaterKdNode[Int] =>
        f(n)
        recur(n.next)
      case n: BalancedKdNode[Int] =>
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



//        case (blN: BalancedKdNode[Int], eR @ NNResult(None, _)) =>
//          val childDepth = depth + 1
//          val prevAxisDist = search.axisDistance(blN.prev.coordinates, childDepth)
//          val nextAxisDist = search.axisDistance(blN.next.coordinates, childDepth)
//          (
//            (
//              if (aD < 0)
//                qR(search, depth + 1, blN.prev, nnRes)
//              else eR,
//              if (0 <= aD)
//                qR(search, depth + 1, blN.next, nnRes)
//              else eR
//            ) match {
//              case (NNResult(None, _), NNResult(None, _)) => eR
//              case (p @ NNResult(Some((_, _)), _), NNResult(None, _)) => p
//              case (NNResult(None, _), n @ NNResult(Some((_, _)), _)) => n
//              case (p @ NNResult(Some((_, prevND)), _), n @ NNResult(Some((_, nextND)), _)) =>
//                if (prevND < nextND) p else n
//            }) match {
//             // subtree result is better than us, use it
//            case sR @ NNResult(Some((_, nbD)), _) if nbD < tD || nbD == tD => sR
//
//             // we are closer than either subtree
//            case sR @ NNResult(Some((_, nbD)), _) if tD < nbD => sR.copy(best = Some((node, tD)))
//          }
