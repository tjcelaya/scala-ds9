package co.tjcelaya.ds9.temporal

import co.tjcelaya.ds9.common.OptionInterval
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by tomascelaya on 7/2/17.
  */
class ZTreeSpec extends FlatSpec with Matchers {
  it should "follow the expected workflow" in {
    var i = 0
    val zt0 = new ZTree[Int, String]("words", OptionInterval(Some(i), None))

    zt0.isClosed shouldBe false
    zt0.hasOpenChild shouldBe false
    i += 1
    val zt1 = zt0.push("more words", i)
    zt1.children.length shouldBe 1
    zt1.children.head.isClosed shouldBe false
    zt1.children.head.children.isEmpty shouldBe true
    i += 1
    val zt2 = zt1.pop(i)
    zt2.children.head.isClosed shouldBe true
    zt2.isClosed shouldBe false
    i += 1
    val zt3 = zt2.pop(i)
    zt3.isClosed shouldBe true
  }

}
