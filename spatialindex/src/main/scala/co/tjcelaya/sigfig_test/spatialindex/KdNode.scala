package co.tjcelaya.sigfig_test.spatialindex

import com.github.mdr.ascii.graph.Graph
import sun.util.logging.resources.logging

/**
  * Created by tj on 3/7/17.
  */


object KdNode {
  def apply(coordinate: Coordinate) = LeafKdNode(coordinate)

  def apply(xs: Int*) = LeafKdNode(SeqCoordinate(xs: _*))

  var logging = false
}

sealed trait KdNode {
  val coordinates: Coordinate
  val axis: Int

  def maybePrev: Option[KdNode] = this match {
    case LesserKdNode(_, _, p) => Some(p)
    case BalancedKdNode(_, _, p, _) => Some(p)
    case _ => None
  }

  def maybeNext: Option[KdNode] = this match {
    case GreaterKdNode(_, _, n) => Some(n)
    case BalancedKdNode(_, _, _, n) => Some(n)
    case _ => None
  }

  def insertAtDepth(n: LeafKdNode, depth: Int): KdNode = {
    import KdNode.logging
    val cmpAxis = depth % this.coordinates.rank
    val insertAxis = cmpAxis + 1
    val cmp = n.coordinates.compare(this.coordinates, cmpAxis)
    if (logging) {
      println((" " * depth) + s" ${n.coordinates} vs ${this.coordinates} using dim $cmpAxis => $cmp")
    }

    if (logging) {} // println(this.toGraph)}

    this match {
      case self: KdNode if self.coordinates == n.coordinates =>
        throw DuplicateCoordinateException()

      case self: LeafKdNode if cmp < 0 =>
        if (logging) {
          println("inserting lesser of leaf")
        }
        LesserKdNode(self.coordinates, self.axis, n.copy(axis = insertAxis))
      case self: LeafKdNode =>
        if (logging) {
          println("inserting greater of leaf")
        }
        GreaterKdNode(self.coordinates, self.axis, n.copy(axis = insertAxis))

      case self: BalancedKdNode =>
        if (cmp < 0) {
          if (logging) {
            println("inserting at depth prev of balanced")
          }
          self.copy(prev = self.prev.insertAtDepth(n, depth + 1))
        } else {
          if (logging) {
            println("inserting at depth next of balanced")
          }
          self.copy(next = self.next.insertAtDepth(n, depth + 1))
        }
      case self: LesserKdNode =>
        if (cmp < 0) {
          if (logging) {
            println("inserting at depth prev of lesser")
          }
          self.copy(prev = self.prev.insertAtDepth(n, depth + 1))
        } else {
          if (logging) {
            println("balancing next of lesser")
          }
          BalancedKdNode(self.coordinates, self.axis, self.prev, n.copy(axis = insertAxis))
        }
      case self: GreaterKdNode =>
        if (cmp < 0) {
          if (logging) {
            println("balancing next of lesser")
          }
          BalancedKdNode(self.coordinates, self.axis, n.copy(axis = depth + 1), self.next)
        } else {
          if (logging) {
            println("balancing next of lesser")
          }
          self.copy(next = self.next.insertAtDepth(n, depth + 1))
        }
    }
  }

  def toStringPadded(depth: Int): String = {
    (" " * depth) + (this match {
      case self: LeafKdNode => s"${self.coordinates}"
      case self: LesserKdNode => s"${self.coordinates}p:\n ${self.prev.toStringPadded(depth + 1)}"
      case self: GreaterKdNode => s"${self.coordinates}n:\n ${self.next.toStringPadded(depth + 1)}"
      case self: BalancedKdNode =>
        s"${self.coordinates}p:\n ${self.prev.toStringPadded(depth + 1)}n:\n ${
          self.next.toStringPadded(depth
            + 1)
        }"
    }).split("\n").mkString((" " * (1 + depth)) + "\n")
  }

  def toGraph: String = {
    var nodes = Set[String]()
    var edges = List[(String, String)]()
    KdTree(Some(this)).foreach(n => {
      def buildGraphNode(node: KdNode) = {
        s"${node.coordinates} ${node.axis}" + (node match {
          case n: BalancedKdNode => "\np n"
          case n: LesserKdNode => "\np"
          case n: GreaterKdNode => "\nn"
          case _ => ""
        })
      }

      nodes = nodes + buildGraphNode(n)
      n match {
        case n: BalancedKdNode =>
          val pE = Tuple2(buildGraphNode(n), buildGraphNode(n.prev))
          val nE = Tuple2(buildGraphNode(n), buildGraphNode(n.next))
          edges = pE :: nE :: edges
        case n: LesserKdNode =>
          edges = Tuple2(buildGraphNode(n), buildGraphNode(n.prev)) :: edges
        case n: GreaterKdNode =>
          edges = Tuple2(buildGraphNode(n), buildGraphNode(n.next)) :: edges
        case _ =>
      }
    })

    val g = Graph[String](nodes, edges)
    g.toString()
  }

}

case class LeafKdNode(coordinates: Coordinate,
                      axis: Int = 0)
  extends KdNode {
}

case class LesserKdNode(coordinates: Coordinate,
                        axis: Int,
                        prev: KdNode)
  extends KdNode {
}

case class GreaterKdNode(coordinates: Coordinate,
                         axis: Int,
                         next: KdNode)
  extends KdNode {
}

case class BalancedKdNode(coordinates: Coordinate,
                          axis: Int,
                          prev: KdNode,
                          next: KdNode)
  extends KdNode {
}

case class DuplicateCoordinateException() extends Exception
