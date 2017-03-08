package co.tjcelaya.sigfig_test.spatialindex

import com.github.mdr.ascii.graph.Graph

/**
  * Created by tj on 3/7/17.
  */
case class KdTree(rootNode: Option[KdNode])
  extends Traversable[KdNode] {

  def root: Option[KdNode] = rootNode

  def insert(n: LeafKdNode): KdTree = rootNode match {
    case None => copy(rootNode = Some(n))
    case Some(r) =>
      copy(rootNode = Some(r.insertAtDepth(n, 0)))
  }

  def query(from: Coordinate): Option[KdNode] = rootNode match {
    case None => None
    case Some(r) => ???
      //copy(rootNode = Some(r.insertAtDepth(n, 0)))
  }

  def foreach[U](f: (KdNode) => U): Unit = {
    if (rootNode.isEmpty)
      return

    def recur(current: KdNode): Unit = current match {
      case n: LeafKdNode =>
        f(n)
      case n @ (previshNode: LesserKdNode) =>
        recur(previshNode.prev)
        f(n)
      case n @ (nextishNode: GreaterKdNode) =>
        f(n)
        recur(nextishNode.next)
      case n @ (balancedNode: BalancedKdNode) =>
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

  def toStringPadded: String = rootNode.get.toStringPadded(0)
  def toGraph: String = rootNode.get.toGraph
}