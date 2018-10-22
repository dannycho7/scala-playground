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

  // more tests...

  it should "not recognize strings not in the language 1" in {
    (new DerivativeMachine(r1)).eval("") should equal(false)
    (new DerivativeMachine(r1)).eval("ab") should equal(false)
    (new DerivativeMachine(r1)).eval("ba") should equal(false)
  }

  // more tests...
}
