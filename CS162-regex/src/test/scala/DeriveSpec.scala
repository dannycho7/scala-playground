package edu.ucsb.cs.cs162.regex.derivative

import org.scalatest._
import edu.ucsb.cs.cs162.regex._

class DeriveSpec extends FlatSpec with Matchers {
  //----------------------------------------------------------------------------
  // Fixtures and helpers.
  // ---------------------------------------------------------------------------

  val b = Chars('b')
  val c = Chars('c')
  val d = Chars('d')

  val r1 = Chars('a') | Chars('b').+
  val r2 = Chars('x', 'y').* ~ r1
  val r3 = Chars('y', 'x').+ ~ r1
  val r4 = (r1^0) ~ r2
  val r5 = r1 & r2
  val r6 = Chars('a', 'c')^5
  val r7 = (b | d) <> (0, 4)
  val r8 = !r1
  val r9 = c >= 3
  val r10 = d <= 5
  val r11 = r1.?
  val r12 = (c | d).*

  import Regex._

  //----------------------------------------------------------------------------
  // Tests.
  // ---------------------------------------------------------------------------

  behavior of "Recursive Derive matches"
  it should "recognize strings in the language 1" in {
    Derive.matches(r1, "a") should equal(true)
    Derive.matches(r1, "b") should equal(true)
    Derive.matches(r1, "bbbbb") should equal(true)
  }

  it should "recognize strings in the language 2" in {
    Derive.matches(r2, "a") should equal(true)
    Derive.matches(r2, "xyxxa") should equal(true)
  }

  it should "recognize strings in the language 3" in {
    Derive.matches(r3, "yxya") should equal(true)
    Derive.matches(r3, "xbb") should equal(true)
  }

  it should "recognize strings in the language 4" in {
    Derive.matches(r4, "a") should equal(true)
    Derive.matches(r4, "xyxbb") should equal(true)
  }

  it should "recognize strings in the language 5" in {
    Derive.matches(r5, "a") should equal(true)
    Derive.matches(r5, "bb") should equal(true)
  }

  it should "recognize strings in the language 6" in {
    Derive.matches(r6, "acccc") should equal(true)
    Derive.matches(r6, "acaca") should equal(true)
  }

  it should "recognize strings in the language 7" in {
    Derive.matches(r7, "") should equal(true)
    Derive.matches(r7, "b") should equal(true)
    Derive.matches(r7, "dbbd") should equal(true)
  }

  it should "recognize strings in the language 8" in {
    Derive.matches(r8, "rygh") should equal(true)
    Derive.matches(r8, "ba") should equal(true)
  }

  it should "recognize strings in the language 9" in {
    Derive.matches(r9, "cccccccc") should equal(true)
    Derive.matches(r9, "ccc") should equal(true)
  }

  it should "recognize strings in the language 10" in {
    Derive.matches(r10, "ddddd") should equal(true)
    Derive.matches(r10, "d") should equal(true)
    Derive.matches(r10, "") should equal(true)
  }

  it should "recognize strings in the language 11" in {
    Derive.matches(r11, "") should equal(true)
    Derive.matches(r11, "a") should equal(true)
    Derive.matches(r11, "bb") should equal(true)
  }

  it should "recognize strings in the language 12" in {
    Derive.matches(r12, "") should equal(true)
    Derive.matches(r12, "cddc") should equal(true)
  }

  it should "not recognize strings not in the language 1" in {
    Derive.matches(r1, "") should equal(false)
    Derive.matches(r1, "ab") should equal(false)
    Derive.matches(r1, "ba") should equal(false)
  }

  it should "not recognize strings not in the language 2" in {
    Derive.matches(r2, "xab") should equal(false)
  }

  it should "not recognize strings not in the language 3" in {
    Derive.matches(r3, "") should equal(false)
    Derive.matches(r3, "a") should equal(false)
    Derive.matches(r3, "ba") should equal(false)
  }

  it should "not recognize strings not in the language 4" in {
    Derive.matches(r2, "xab") should equal(false)
  }

  it should "not recognize strings not in the language 5" in {
    Derive.matches(r5, "") should equal(false)
    Derive.matches(r5, "ab") should equal(false)
    Derive.matches(r5, "ba") should equal(false)
  }

  it should "not recognize strings in the language 6" in {
    Derive.matches(r6, "cacccc") should equal(false)
    Derive.matches(r6, "acbca") should equal(false)
  }

  it should "not recognize strings in the language 7" in {
    Derive.matches(r7, "z") should equal(false)
    Derive.matches(r7, "bbdddb") should equal(false)
  }

  it should "not recognize strings in the language 8" in {
    Derive.matches(r8, "a") should equal(false)
    Derive.matches(r8, "bbb") should equal(false)
  }

  it should "not recognize strings in the language 9" in {
    Derive.matches(r9, "") should equal(false)
    Derive.matches(r9, "cc") should equal(false)
  }

  it should "not recognize strings in the language 10" in {
    Derive.matches(r10, "dddddddd") should equal(false)
    Derive.matches(r10, "x") should equal(false)
  }

  it should "not recognize strings in the language 11" in {
    Derive.matches(r10, "ab") should equal(false)
    Derive.matches(r10, "aa") should equal(false)
  }

  it should "not recognize strings in the language 12" in {
    Derive.matches(r12, "aj") should equal(false)
  }

  behavior of "matches"

  it should "recognize strings in the language 1" in {
    (new DerivativeMachine(r1)).eval("a") should equal(true)
    (new DerivativeMachine(r1)).eval("b") should equal(true)
    (new DerivativeMachine(r1)).eval("bbbbb") should equal(true)
  }

  it should "recognize strings in the language 2" in {
    (new DerivativeMachine(r2)).eval("a") should equal(true)
    (new DerivativeMachine(r2)).eval("xyxxa") should equal(true)
  }

  it should "recognize strings in the language 3" in {
    (new DerivativeMachine(r3)).eval("yxya") should equal(true)
    (new DerivativeMachine(r3)).eval("xbb") should equal(true)
  }

  it should "recognize strings in the language 4" in {
    (new DerivativeMachine(r4)).eval("a") should equal(true)
    (new DerivativeMachine(r4)).eval("xyxbb") should equal(true)
  }

  it should "recognize strings in the language 5" in {
    (new DerivativeMachine(r5)).eval("a") should equal(true)
    (new DerivativeMachine(r5)).eval("bb") should equal(true)
  }

  it should "recognize strings in the language 6" in {
    (new DerivativeMachine(r6)).eval("acccc") should equal(true)
    (new DerivativeMachine(r6)).eval("acaca") should equal(true)
  }

  it should "recognize strings in the language 7" in {
    (new DerivativeMachine(r7)).eval("") should equal(true)
    (new DerivativeMachine(r7)).eval("b") should equal(true)
    (new DerivativeMachine(r7)).eval("dbbd") should equal(true)
  }

  it should "recognize strings in the language 8" in {
    (new DerivativeMachine(r8)).eval("rygh") should equal(true)
    (new DerivativeMachine(r8)).eval("ba") should equal(true)
  }

  it should "recognize strings in the language 9" in {
    (new DerivativeMachine(r9)).eval("cccccccc") should equal(true)
    (new DerivativeMachine(r9)).eval("ccc") should equal(true)
  }

  it should "recognize strings in the language 10" in {
    (new DerivativeMachine(r10)).eval("ddddd") should equal(true)
    (new DerivativeMachine(r10)).eval("d") should equal(true)
    (new DerivativeMachine(r10)).eval("") should equal(true)
  }

  it should "recognize strings in the language 11" in {
    (new DerivativeMachine(r11)).eval("") should equal(true)
    (new DerivativeMachine(r11)).eval("a") should equal(true)
    (new DerivativeMachine(r11)).eval("bb") should equal(true)
  }

  it should "recognize strings in the language 12" in {
    (new DerivativeMachine(r12)).eval("") should equal(true)
    (new DerivativeMachine(r12)).eval("cddc") should equal(true)
  }

  it should "not recognize strings not in the language 1" in {
    (new DerivativeMachine(r1)).eval("") should equal(false)
    (new DerivativeMachine(r1)).eval("ab") should equal(false)
    (new DerivativeMachine(r1)).eval("ba") should equal(false)
  }

  it should "not recognize strings not in the language 2" in {
    (new DerivativeMachine(r2)).eval("xab") should equal(false)
  }

  it should "not recognize strings not in the language 3" in {
    (new DerivativeMachine(r3)).eval("") should equal(false)
    (new DerivativeMachine(r3)).eval("a") should equal(false)
    (new DerivativeMachine(r3)).eval("ba") should equal(false)
  }

  it should "not recognize strings not in the language 4" in {
    (new DerivativeMachine(r2)).eval("xab") should equal(false)
  }

  it should "not recognize strings not in the language 5" in {
    (new DerivativeMachine(r5)).eval("") should equal(false)
    (new DerivativeMachine(r5)).eval("ab") should equal(false)
    (new DerivativeMachine(r5)).eval("ba") should equal(false)
  }

  it should "not recognize strings in the language 6" in {
    (new DerivativeMachine(r6)).eval("cacccc") should equal(false)
    (new DerivativeMachine(r6)).eval("acbca") should equal(false)
  }

  it should "not recognize strings in the language 7" in {
    (new DerivativeMachine(r7)).eval("z") should equal(false)
    (new DerivativeMachine(r7)).eval("bbdddb") should equal(false)
  }

  it should "not recognize strings in the language 8" in {
    (new DerivativeMachine(r8)).eval("a") should equal(false)
    (new DerivativeMachine(r8)).eval("bbb") should equal(false)
  }

  it should "not recognize strings in the language 9" in {
    (new DerivativeMachine(r9)).eval("") should equal(false)
    (new DerivativeMachine(r9)).eval("cc") should equal(false)
  }

  it should "not recognize strings in the language 10" in {
    (new DerivativeMachine(r10)).eval("dddddddd") should equal(false)
    (new DerivativeMachine(r10)).eval("x") should equal(false)
  }

  it should "not recognize strings in the language 11" in {
    (new DerivativeMachine(r11)).eval("ab") should equal(false)
    (new DerivativeMachine(r11)).eval("aa") should equal(false)
  }

  it should "not recognize strings in the language 12" in {
    (new DerivativeMachine(r12)).eval("aj") should equal(false)
  }
}
