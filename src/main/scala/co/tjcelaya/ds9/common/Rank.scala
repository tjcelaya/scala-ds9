package co.tjcelaya.ds9.common

/**
  * Created by tj on 3/11/17.
  */
final class Rank(val i: Int) extends AnyVal {

  def v: Int = if (0 <= i) i else throw new RankInitializationException(i)

  def increment(maximum: Rank): Rank = new Rank((v + 1) % maximum.v)

  def decrement(maximum: Rank): Rank = new Rank((if (0 < v) v else maximum.v) - 1)
}

case class RankInitializationException(i: Int) extends Exception(s"$i must be non-negative")