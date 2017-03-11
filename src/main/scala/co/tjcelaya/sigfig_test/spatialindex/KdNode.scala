package co.tjcelaya.sigfig_test.spatialindex

import co.tjcelaya.sigfig_test.spatialindex.exceptions.{DuplicateCoordinateException, InvalidKdDropException, InvalidKdSetException, InvalidQueryException}

/**
  * Created by tj on 3/7/17.
  */

abstract class KdNode[V: Distanced] {
  type TypedKdNode = KdNode[V]
  type TypedCoordinate = Coordinate[V]
  type TypedSplitRange = SplitRange[V]
  val TypedSplitRange = SplitRange
  val coordinates: TypedCoordinate

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
    val insertAxis = (depth + 1) % coordinates.rank

    if (cmp == 0 && this.coordinates == c) {
      throw DuplicateCoordinateException()
    } else if (cmp <= 0) {
      if (this.maybePrev.isDefined) {
        val updatedSubtree = this.maybePrev.get.insertAtDepth(c, depth + 1)
        this.setPrev(updatedSubtree)
      } else {
        this.setPrev(LeafKdNode(c, insertAxis))
      }
    } else {
      if (this.maybeNext.isDefined) {
        val updatedSubtree = this.maybeNext.get.insertAtDepth(c, depth + 1)
        this.setNext(updatedSubtree)
      } else {
        this.setNext(LeafKdNode(c, insertAxis))
      }
    }
  }

  def compareOnAxis(c: TypedCoordinate, depth: Int): (Int, Number) = {
    val cmpAxis = depth % c.rank
    val cmp = c.axisDistance(this.coordinates, cmpAxis)
    (cmpAxis, cmp)
  }

  def toStringPadded(depth: Int): String = {
    (" " * depth) + (this match {
      case self: LeafKdNode[_] => s"${self.coordinates}"
      case self: LesserKdNode[_] => s"${self.coordinates}p:\n ${self.prev.toStringPadded(depth + 1)}"
      case self: GreaterKdNode[_] => s"${self.coordinates}n:\n ${self.next.toStringPadded(depth + 1)}"
      case self: BalancedKdNode[_] =>
        s"${self.coordinates}p:\n " +
          s"${self.prev.toStringPadded(depth + 1)}n:\n " +
          s"${self.next.toStringPadded(depth + 1)}"
    }).split("\n").mkString((" " * (1 + depth)) + "\n")
  }

  def axisBounds(parent: Option[TypedKdNode], axis: Int = 0): TypedSplitRange = parent match {
    //    case None =>
    //      TypedSplitRange(V.minVal, coordinates(axis), V.maxVal)
    case _ =>
      throw new Exception()
  }

  def hyperBounds(parent: Option[TypedKdNode]): Seq[TypedSplitRange] = {
    coordinates.rank.to(0).map(axisBounds(parent, _))
  }

}





case class LeafKdNode[V: Distanced](coordinates: Coordinate[V],
                                    axis: Int)
  extends KdNode[V]

case class LesserKdNode[V: Distanced](coordinates: Coordinate[V],
                                      axis: Int,
                                      prev: KdNode[V])
  extends KdNode[V]

case class GreaterKdNode[V: Distanced](coordinates: Coordinate[V],
                                       axis: Int,
                                       next: KdNode[V])
  extends KdNode[V]

case class BalancedKdNode[V: Distanced](coordinates: Coordinate[V],
                                        axis: Int,
                                        prev: KdNode[V],
                                        next: KdNode[V])
  extends KdNode[V]
