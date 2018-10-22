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

  import Regex._

  //----------------------------------------------------------------------------
  // Tests.
  // ---------------------------------------------------------------------------

  behavior of "matches"

  it should "recognize strings in the language 1" in {
    (new DerivativeMachine(r1)).eval("a") should equal(true)
    (new DerivativeMachine(r1)).eval("b") should equal(true)
    (new DerivativeMachine(r1)).eval("bbbbb") should equal(true)
  }

  it should "recognize strings in the language 2" in {
    (new DerivativeMachine(r2)).eval("a") should equal(true)
    (new DerivativeMachine(r1)).eval("b") should equal(true)
    (new DerivativeMachine(r1)).eval("bbbbb") should equal(true)
    (new DerivativeMachine(r2)).eval("xa") should equal(true)
    (new DerivativeMachine(r2)).eval("xbbbb") should equal(true)
    (new DerivativeMachine(r2)).eval("ybbbbb") should equal(true)
    (new DerivativeMachine(r2)).eval("xyxxa") should equal(true)
    (new DerivativeMachine(r2)).eval("yxbbbb") should equal(true)
    (new DerivativeMachine(r2)).eval("xybbbbb") should equal(true)
  }

  it should "recognize strings in the language 3" in {
    (new DerivativeMachine(r3)).eval("yxya") should equal(true)
    (new DerivativeMachine(r3)).eval("xb") should equal(true)
    (new DerivativeMachine(r3)).eval("xbbbbb") should equal(true)
  }

  it should "recognize strings in the language 4" in {
    (new DerivativeMachine(r2)).eval("a") should equal(true)
    (new DerivativeMachine(r1)).eval("b") should equal(true)
    (new DerivativeMachine(r1)).eval("bbbbb") should equal(true)
    (new DerivativeMachine(r2)).eval("xa") should equal(true)
    (new DerivativeMachine(r2)).eval("xbbbb") should equal(true)
    (new DerivativeMachine(r2)).eval("ybbbbb") should equal(true)
    (new DerivativeMachine(r2)).eval("xyxxa") should equal(true)
    (new DerivativeMachine(r2)).eval("yxbbbb") should equal(true)
    (new DerivativeMachine(r2)).eval("xybbbbb") should equal(true)
  }

  it should "recognize strings in the language 5" in {
    (new DerivativeMachine(r5)).eval("a") should equal(true)
    (new DerivativeMachine(r5)).eval("b") should equal(true)
    (new DerivativeMachine(r5)).eval("bbbbb") should equal(true)
  }

  it should "not recognize strings not in the language 1" in {
    (new DerivativeMachine(r1)).eval("") should equal(false)
    (new DerivativeMachine(r1)).eval("ab") should equal(false)
    (new DerivativeMachine(r1)).eval("ba") should equal(false)
  }

  it should "not recognize strings not in the language 2" in {
    (new DerivativeMachine(r2)).eval("") should equal(false)
    (new DerivativeMachine(r2)).eval("ab") should equal(false)
    (new DerivativeMachine(r2)).eval("ba") should equal(false)
  }

  it should "not recognize strings not in the language 3" in {
    (new DerivativeMachine(r3)).eval("") should equal(false)
    (new DerivativeMachine(r3)).eval("a") should equal(false)
    (new DerivativeMachine(r3)).eval("bbb") should equal(false)
    (new DerivativeMachine(r3)).eval("ab") should equal(false)
    (new DerivativeMachine(r3)).eval("ba") should equal(false)
  }

  it should "not recognize strings not in the language 4" in {
    (new DerivativeMachine(r4)).eval("") should equal(false)
    (new DerivativeMachine(r4)).eval("ab") should equal(false)
    (new DerivativeMachine(r4)).eval("ba") should equal(false)
  }

  it should "not recognize strings not in the language 5" in {
    (new DerivativeMachine(r5)).eval("") should equal(false)
    (new DerivativeMachine(r5)).eval("ab") should equal(false)
    (new DerivativeMachine(r5)).eval("ba") should equal(false)
  }
}
