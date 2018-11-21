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
  val re3 = Concatenate(Union(b, d), Capture("suffix", KleeneStar(Chars('a' -> 'g'))))
  val program3 = Compiler.compile(re3)
  val re4 = Union(b, b)
  val program4 = Compiler.compile(re4)
  val re5 = KleeneStar(ε)
  val program5 = Compiler.compile(re5)
  val re6 = KleeneStar(KleeneStar(d))
  val program6 = Compiler.compile(re6)
  val re7 = Union(b, d)
  val program7 = Compiler.compile(re7)
  val re8 = Union(b, ε)
  val program8 = Compiler.compile(re8)
  val re9 = Concatenate(Capture("cap_1", KleeneStar(d)), Capture("cap_2", KleeneStar(d)))
  val program9 = Compiler.compile(re9)

  // Replace {language 1} with a more descriptive name for what you're testing.
  // Feel free to add more tests, or write many shorter ones.

  it should "(program 1) parse strings in the language with Concat" in {
    new PowersetVm(program1).eval("bdd") shouldEqual new RecursiveBacktrackingVm(program1).eval("bdd")
  }

  it should "(program 2) parse strings in a complex regex" in {
    new PowersetVm(program2).eval("b") shouldEqual Some(ConcatNode(LeftNode(CharLeaf('b')), StarNode(Seq())))
    new PowersetVm(program2).eval("d") shouldEqual Some(ConcatNode(RightNode(CharLeaf('d')), StarNode(Seq())))
    new PowersetVm(program2).eval("bd") shouldEqual Some(ConcatNode(LeftNode(CharLeaf('b')), StarNode(Seq(RightNode(CharLeaf('d'))))))
    new PowersetVm(program2).eval("db") shouldEqual Some(ConcatNode(RightNode(CharLeaf('d')), StarNode(Seq(LeftNode(CharLeaf('b'))))))
    new PowersetVm(program2).eval("bbbd") shouldEqual Some(ConcatNode(LeftNode(CharLeaf('b')), StarNode(Seq(RightNode(CharLeaf('d')), LeftNode(CharLeaf('b')), LeftNode(CharLeaf('b'))))))
    new PowersetVm(program2).eval("dddb") shouldEqual Some(ConcatNode(RightNode(CharLeaf('d')), StarNode(Seq(LeftNode(CharLeaf('b')), RightNode(CharLeaf('d')), RightNode(CharLeaf('d'))))))
    new PowersetVm(program2).eval("bdbbd") shouldEqual Some(ConcatNode(LeftNode(CharLeaf('b')), StarNode(Seq(RightNode(CharLeaf('d')), LeftNode(CharLeaf('b')), LeftNode(CharLeaf('b')), RightNode(CharLeaf('d'))))))
  }

  it should "(program 3) parse strings in a regex with a capture node and extract out captures" in {
    val tree1 = new PowersetVm(program3).eval("b")
    tree1 shouldEqual new RecursiveBacktrackingVm(program3).eval("b")
    (new Extractor(tree1.get)).extract("suffix") shouldEqual List("")

    val tree2 = new PowersetVm(program3).eval("babcdfgfgf")
    tree2 shouldEqual new RecursiveBacktrackingVm(program3).eval("babcdfgfgf")
    (new Extractor(tree2.get)).extract("suffix") shouldEqual List("abcdfgfgf")
  }

  it should "(program 4) parse strings in an ambiguous regex" in {
    new PowersetVm(program4).eval("b") shouldEqual new RecursiveBacktrackingVm(program4).eval("b")
  }

  it should "(program 5 & 6) parse strings while terminating with CheckProgress due to a nullable regex within KleeneStar" in {
    new PowersetVm(program5).eval("") shouldEqual new RecursiveBacktrackingVm(program5).eval("")
    new PowersetVm(program6).eval("") shouldEqual new RecursiveBacktrackingVm(program6).eval("")
    new PowersetVm(program6).eval("d") shouldEqual new RecursiveBacktrackingVm(program6).eval("d")
  }

  it should "(program 7) parse strings while pushing left or right leaves" in {
    new PowersetVm(program7).eval("b") shouldEqual new RecursiveBacktrackingVm(program7).eval("b")
    new PowersetVm(program7).eval("d") shouldEqual new RecursiveBacktrackingVm(program7).eval("d")
  }

  it should "(program 8) parse strings while taking an empty leaf" in {
    new PowersetVm(program8).eval("") shouldEqual new RecursiveBacktrackingVm(program8).eval("")
  }

  it should "(program 9) parse strings and prioritize the left of concat for capture" in {
    val tree1 = new PowersetVm(program9).eval("")
    tree1 shouldEqual new RecursiveBacktrackingVm(program9).eval("")
    println((new Extractor(tree1.get)).extract("cap_1"))
    (new Extractor(tree1.get)).extract("cap_1") shouldEqual List("")
    (new Extractor(tree1.get)).extract("cap_2") shouldEqual List("")

    val tree2 = new PowersetVm(program9).eval("dddd")
    tree2 shouldEqual new RecursiveBacktrackingVm(program9).eval("dddd")
    (new Extractor(tree2.get)).extract("cap_1") shouldEqual List("dddd")
    (new Extractor(tree2.get)).extract("cap_2") shouldEqual List("")
  }

  // more tests...

  it should "(program 1) not parse strings not in the regex with Concat" in {
    new PowersetVm(program1).eval("") shouldEqual None
    new PowersetVm(program1).eval("bddd") shouldEqual None
  }

  it should "(program 2) not parse strings not in a complex regex" in {
    new PowersetVm(program2).eval("") shouldEqual None
    new PowersetVm(program2).eval("e") shouldEqual None
    new PowersetVm(program2).eval("bdg") shouldEqual None
  }

  it should "(program 3) not parse strings not in a regex with a capture node" in {
    new PowersetVm(program3).eval("") shouldEqual None
    new PowersetVm(program3).eval("ab") shouldEqual None
    new PowersetVm(program3).eval("bafajlskdjfklsjy") shouldEqual None
  }
}
