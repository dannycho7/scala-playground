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

  val b = Chars('b')
  val d = Chars('d')

  // Regex
  val re1 = Concatenate(b, Concatenate(d, d))
  val program1 = Compiler.compile(re1)
  val re2 = Concatenate(Union(b, d), KleeneStar(Union(b, d)))
  val program2 = Compiler.compile(re2)

  // Replace {language 1} with a more descriptive name for what you're testing.
  // Feel free to add more tests, or write many shorter ones.

  it should "parse strings in {language 1}" in {
    new PowersetVm(program1).eval("bdd") shouldEqual new RecursiveBacktrackingVm(program1).eval("bdd")
  }

  it should "parse strings not in {language 2}" in {
    new PowersetVm(program2).eval("b") shouldEqual Some(ConcatNode(LeftNode(CharLeaf('b')), StarNode(Seq())))
    new PowersetVm(program2).eval("d") shouldEqual Some(ConcatNode(RightNode(CharLeaf('d')), StarNode(Seq())))
    new PowersetVm(program2).eval("bd") shouldEqual Some(ConcatNode(LeftNode(CharLeaf('b')), StarNode(Seq(RightNode(CharLeaf('d'))))))
    new PowersetVm(program2).eval("db") shouldEqual Some(ConcatNode(RightNode(CharLeaf('d')), StarNode(Seq(LeftNode(CharLeaf('b'))))))
    new PowersetVm(program2).eval("bbbd") shouldEqual Some(ConcatNode(LeftNode(CharLeaf('b')), StarNode(Seq(RightNode(CharLeaf('d')), LeftNode(CharLeaf('b')), LeftNode(CharLeaf('b'))))))
    new PowersetVm(program2).eval("dddb") shouldEqual Some(ConcatNode(RightNode(CharLeaf('d')), StarNode(Seq(LeftNode(CharLeaf('b')), RightNode(CharLeaf('d')), RightNode(CharLeaf('d'))))))
    new PowersetVm(program2).eval("bdbbd") shouldEqual Some(ConcatNode(LeftNode(CharLeaf('b')), StarNode(Seq(RightNode(CharLeaf('d')), LeftNode(CharLeaf('b')), LeftNode(CharLeaf('b')), RightNode(CharLeaf('d'))))))
  }

  // more tests...

  it should "not parse strings not in {language 1}" in {
    new PowersetVm(program1).eval("") shouldEqual new RecursiveBacktrackingVm(program1).eval("")
    new PowersetVm(program1).eval("bddd") shouldEqual new RecursiveBacktrackingVm(program1).eval("bddd")
  }

  // more tests...
}
