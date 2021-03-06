package co.tjcelaya.ds9.spatial

import co.tjcelaya.ds9.common.{Distanced, Rank, SplitRange}
import co.tjcelaya.ds9.spatial.exceptions.{DuplicateCoordinateException, InvalidKdDropException, InvalidKdSetException}

/**
  * Created by tj on 3/7/17.
  */

abstract class KdNode[V: Distanced : Extrema : Ordering] {
  type TypedKdNode = KdNode[V]
  type TypedCoordinate = Coordinate[V]
  type TypedSplitRange = SplitRange[V]
  val coordinates: TypedCoordinate
  val rank: Rank

  def maybePrev: Option[TypedKdNode] = this match {
    case LesserKdNode(_, _, p) => Some(p)
    case BalancedKdNode(_, _, p, _) => Some(p)
    case _ => None
  }

  def setPrev(n: TypedKdNode): TypedKdNode = this match {
    case LeafKdNode(lC, lA) => LesserKdNode(lC, lA, n)
    case LesserKdNode(lC, lA, _) => LesserKdNode(lC, lA, n)
    case GreaterKdNode(gC, gA, gN) => BalancedKdNode(gC, gA, n, gN)
    case BalancedKdNode(bC, bA, _, bN) => BalancedKdNode(bC, bA, n, bN)
    case _ => throw InvalidKdSetException(this, n, "prev")
  }

  def dropPrev: TypedKdNode = this match {
    case LesserKdNode(lC, lA, _) => LeafKdNode(lC, lA)
    case BalancedKdNode(bC, bA, _, bN) => GreaterKdNode(bC, bA, bN)
    case _ => throw InvalidKdDropException(this, "prev")
  }

  def maybeNext: Option[TypedKdNode] = this match {
    case GreaterKdNode(_, _, n) => Some(n)
    case BalancedKdNode(_, _, _, n) => Some(n)
    case _ => None
  }

  def setNext(n: TypedKdNode): TypedKdNode = this match {
    case LeafKdNode(lC, lA) => GreaterKdNode(lC, lA, n)
    case LesserKdNode(lC, lA, lP) => BalancedKdNode(lC, lA, lP, n)
    case GreaterKdNode(gC, gA, _) => GreaterKdNode(gC, gA, n)
    case BalancedKdNode(bC, bA, bP, _) => BalancedKdNode(bC, bA, bP, n)
    case _ => throw InvalidKdSetException(this, n, "next")
  }

  def dropNext: TypedKdNode = this match {
    case GreaterKdNode(gC, gA, _) => LeafKdNode(gC, gA)
    case BalancedKdNode(bC, bA, bP, _) => LesserKdNode(bC, bA, bP)
    case _ => throw InvalidKdDropException(this, "next")
  }

  def insertAtDepth(c: TypedCoordinate, depth: Int): TypedKdNode = {
    val compared = compareOnAxis(c, depth)
    val cmpAxis = compared._1
    val cmp = compared._2.shortValue()
    val insertRank = (depth + 1) % coordinates.rank.v

    if (cmp == 0 && this.coordinates == c) {
      throw DuplicateCoordinateException()
    } else if (cmp <= 0) {
      if (this.maybePrev.isDefined) {
        val updatedSubtree = this.maybePrev.get.insertAtDepth(c, depth + 1)
        this.setPrev(updatedSubtree)
      } else {
        this.setPrev(LeafKdNode(c, new Rank(insertRank)))
      }
    } else {
      if (this.maybeNext.isDefined) {
        val updatedSubtree = this.maybeNext.get.insertAtDepth(c, depth + 1)
        this.setNext(updatedSubtree)
      } else {
        this.setNext(LeafKdNode(c, new Rank(insertRank)))
      }
    }
  }

  def compareOnAxis(c: TypedCoordinate, depth: Int): (Rank, Number) = {
    val cmpAxis = new Rank(depth % c.rank.v)
    val cmp = c.axisDistance(this.coordinates, cmpAxis)
    (cmpAxis, cmp)
  }

  def hyperBounds(parents: Seq[TypedKdNode]): Seq[TypedSplitRange] = {
    (0 until coordinates.rank.v).map(r => dimensionBounds(parents, new Rank(r)))
  }

  def dimensionBounds(parents: Seq[TypedKdNode], rank: Rank): TypedSplitRange = {
    val iD = implicitly[Distanced[V]]
    val iE = implicitly[Extrema[V]]
    val iO = implicitly[Ordering[V]]

    parents.foldLeft[TypedSplitRange](
      SplitRange(iE.minVal, coordinates(rank), iE.maxVal)
    ) {
      (acc: TypedSplitRange, rankedAncestor: TypedKdNode) =>

        val parentSplit: V = rankedAncestor.coordinates(rank)

        if (iO.compare(parentSplit, coordinates(rank)) <= 0) {
          if (iO.gt(parentSplit, acc.lower)) {
            acc.copy(lower = parentSplit)
          } else {
            acc
          }
        } else {
          if (iO.lt(parentSplit, acc.upper)) {
            acc.copy(upper = parentSplit)
          } else {
            acc
          }
        }
    }
  }
}

case class LeafKdNode[V: Distanced : Extrema : Ordering](coordinates: Coordinate[V],
                                                         rank: Rank)
  extends KdNode[V]

case class LesserKdNode[V: Distanced : Extrema : Ordering](coordinates: Coordinate[V],
                                                           rank: Rank,
                                                           prev: KdNode[V])
  extends KdNode[V]

case class GreaterKdNode[V: Distanced : Extrema : Ordering](coordinates: Coordinate[V],
                                                            rank: Rank,
                                                            next: KdNode[V])
  extends KdNode[V]

case class BalancedKdNode[V: Distanced : Extrema : Ordering](coordinates: Coordinate[V],
                                                             rank: Rank,
                                                             prev: KdNode[V],
                                                             next: KdNode[V])
  extends KdNode[V]
