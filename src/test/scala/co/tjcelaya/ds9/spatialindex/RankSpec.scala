package co.tjcelaya.ds9.spatialindex

import co.tjcelaya.ds9.common.Rank
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by tj on 3/13/17.
  */
class RankSpec extends FlatSpec with Matchers {

  it should "increment rank correctly" in {
    val r0 = new Rank(0)
    val r1 = r0.increment(new Rank(3))
    val r2 = r1.increment(new Rank(3))
    val r3 = r2.increment(new Rank(3))
    val r4 = r3.increment(new Rank(3))

    r1.v shouldEqual 1
    r2.v shouldEqual 2
    r3.v shouldEqual 0
    r4.v shouldEqual 1
  }

  it should "decrement rank correctly" in {
    val r0 = new Rank(0)
    val r1 = r0.decrement(new Rank(3))
    val r2 = r1.decrement(new Rank(3))
    val r3 = r2.decrement(new Rank(3))
    val r4 = r3.decrement(new Rank(3))

    r1.v shouldEqual 2
    r2.v shouldEqual 1
    r3.v shouldEqual 0
    r4.v shouldEqual 2
  }
}
