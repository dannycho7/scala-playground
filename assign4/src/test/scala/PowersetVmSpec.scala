package edu.ucsb.cs.cs162.regex.vm

import org.scalatest._
import edu.ucsb.cs.cs162.regex._
import edu.ucsb.cs.cs162.regex.vm.compiler._
import edu.ucsb.cs.cs162.regex.parse_tree._

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

  it should "parse strings not in {language 2}" in {
    val b = Chars('b')
    val d = Chars('d')

    val re2 = Concatenate(Union(b, d), KleeneStar(Union(b, d)))
    val program = Compiler.compile(re2)

    new PowersetVm(program).eval("b") shouldEqual Some(ConcatNode(LeftNode(CharLeaf('b')), StarNode(Seq())))
    new PowersetVm(program).eval("d") shouldEqual Some(ConcatNode(RightNode(CharLeaf('d')), StarNode(Seq())))
    new PowersetVm(program).eval("bd") shouldEqual Some(ConcatNode(LeftNode(CharLeaf('b')), StarNode(Seq(RightNode(CharLeaf('d'))))))
    new PowersetVm(program).eval("db") shouldEqual Some(ConcatNode(RightNode(CharLeaf('d')), StarNode(Seq(LeftNode(CharLeaf('b'))))))
    new PowersetVm(program).eval("bbbd") shouldEqual Some(ConcatNode(LeftNode(CharLeaf('b')), StarNode(Seq(RightNode(CharLeaf('d')), LeftNode(CharLeaf('b')), LeftNode(CharLeaf('b'))))))
    new PowersetVm(program).eval("dddb") shouldEqual Some(ConcatNode(RightNode(CharLeaf('d')), StarNode(Seq(LeftNode(CharLeaf('b')), RightNode(CharLeaf('d')), RightNode(CharLeaf('d'))))))
    new PowersetVm(program).eval("bdbbd") shouldEqual Some(ConcatNode(LeftNode(CharLeaf('b')), StarNode(Seq(RightNode(CharLeaf('d')), LeftNode(CharLeaf('b')), LeftNode(CharLeaf('b')), RightNode(CharLeaf('d'))))))
  }

  // more tests...

  it should "not parse strings not in {language 1}" in {}

  // more tests...
}
