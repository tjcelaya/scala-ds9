package co.tjcelaya.sigfig_test.spatialindex

/**
  * Created by tj on 3/8/17.
  */
package object exceptions {
  case class DuplicateCoordinateException() extends Exception
  case class InvalidKdSetException[V : Distanced](self: KdNode[V], node: KdNode[V], side: String)
  extends Exception {
    override def toString: String = s"${super.toString}: from $self on $side with $node"
  }
  case class InvalidKdDropException[V : Distanced](self: KdNode[V], side: String) extends Exception
  case class InvalidQueryException() extends Exception
  case class InvalidStateException(msg: String) extends Exception(msg)
}
