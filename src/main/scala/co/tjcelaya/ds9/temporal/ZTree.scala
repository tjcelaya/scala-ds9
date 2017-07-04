package co.tjcelaya.ds9.temporal

import co.tjcelaya.ds9.common.{Interval, OptionInterval}

/**
  * Created by tomascelaya on 7/2/17.
  */

case class ZTree[T, U](content: U,
                                      bounds: Interval[T],
                                      children: List[ZTree[T, U]] = Nil) {
  type Tree = ZTree[T, U]

  def isClosed: Boolean = bounds.end.nonEmpty

  def hasOpenChild: Boolean = children.nonEmpty

  def push(content: U, t: T): Tree = {
    val node = ZTree(content, OptionInterval(Some(t), None))
    children match {
      case Nil => copy(children = List(node))
      case c :: _ => copy(children = node :: children)
    }
  }

  def pop(t: T): Tree = {
    if (bounds.end.nonEmpty) {
      throw new IllegalStateException("can't pop if finished")
    }

    (bounds, children) match {
      case (Interval(_, None), Nil) =>
        copy(bounds = bounds.withEnd(t))
      case (Interval(_, None), (c@ZTree(_, Interval(_, None), _)) :: (cs@_)) =>
        copy(children = c.copy(bounds = c.bounds.withEnd(t)) :: cs)
      case (Interval(_, None), (c@ZTree(_, Interval(_, Some(_)), _)) :: (cs@_)) =>
        copy(bounds = bounds.withEnd(t))
    }
  }

  def terminate(cleanly: Boolean): Tree = {
    this
  }

}