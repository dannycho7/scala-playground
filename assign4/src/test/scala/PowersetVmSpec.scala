package edu.ucsb.cs.cs162.regex.vm

import org.scalatest._
import edu.ucsb.cs.cs162.regex._
import edu.ucsb.cs.cs162.regex.vm.compiler._

class PowersetVmSpec extends FlatSpec with Matchers {
  //----------------------------------------------------------------------------
  // Fixtures and helpers.
  // ---------------------------------------------------------------------------

  import Regex._

  //----------------------------------------------------------------------------
  // Tests.
  // ---------------------------------------------------------------------------

  behavior of "eval"

  // Replace {language 1} with a more descriptive name for what you're testing.
  // Feel free to add more tests, or write many shorter ones.

  it should "parse strings in {language 1}" in {
    val bSet = Chars('b')
    val dSet = Chars('d')

    // Regex
    val regex = Concatenate(bSet, Concatenate(dSet, dSet))
    val program = Compiler.compile(regex)
    
    new PowersetVm(program).eval("bdd") shouldEqual new RecursiveBacktrackingVm(program).eval("bdd")
  }

  // more tests...

  it should "not parse strings not in {language 1}" in { pending }

  // more tests...
}
