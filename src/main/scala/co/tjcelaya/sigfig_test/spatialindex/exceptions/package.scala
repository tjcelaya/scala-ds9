package co.tjcelaya.sigfig_test.spatialindex

/**
  * Created by tj on 3/8/17.
  */
package object exceptions {
  case class DuplicateCoordinateException() extends Exception
  case class InvalidKdSetException(self: KdNode[_], node: KdNode[_], side: String) extends Exception {
    override def toString: String = s"${super.toString}: from $self on $side with $node"
  }
  case class InvalidKdDropException(self: KdNode[_], side: String) extends Exception
  case class InvalidQueryException() extends Exception
}
