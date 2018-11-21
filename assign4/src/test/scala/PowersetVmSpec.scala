package edu.ucsb.cs.cs162.regex.vm

import org.scalatest._
import edu.ucsb.cs.cs162.range_set._
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
  val c = Chars('c')
  val d = Chars('d')

  val bSet = CharSet('b')
  val cSet = CharSet('c')
  val dSet = CharSet('d')

  // Regex
  val program_empty_string = IndexedSeq(PushEmpty, Accept)
  val program_empty_language = IndexedSeq(Reject, Accept)
  val re1 = Concatenate(b, Concatenate(d, d))
  val program1 = IndexedSeq(MatchSet(bSet), PushChar, MatchSet(dSet), PushChar, MatchSet(dSet), PushChar, PushConcat, PushConcat, Accept)
  val re2 = Concatenate(Union(b, d), KleeneStar(Union(b, d)))
  val program2 = IndexedSeq(Fork(1,5), MatchSet(bSet), PushChar, PushLeft, Jump(4), MatchSet(dSet), PushChar, PushRight, InitStar, Fork(1,11), Fork(1,5), MatchSet(bSet), PushChar, PushLeft, Jump(4), MatchSet(dSet), PushChar, PushRight, PushStar, Jump(-10), PushConcat, Accept)
  val re3 = Concatenate(Union(b, d), Capture("suffix", KleeneStar(Chars('a' -> 'g'))))
  val program3 = IndexedSeq(Fork(1,5), MatchSet(bSet), PushChar, PushLeft, Jump(4), MatchSet(dSet), PushChar, PushRight, InitStar, Fork(1,5), MatchSet(CharSet('a' -> 'g')), PushChar, PushStar, Jump(-4), PushCapture("suffix"), PushConcat, Accept)
  val re4 = Union(b, b)
  val program4 = IndexedSeq(Fork(1,5), MatchSet(bSet), PushChar, PushLeft, Jump(4), MatchSet(bSet), PushChar, PushRight, Accept)
  val re5 = KleeneStar(ε)
  val program5 = IndexedSeq(InitStar, CheckProgress, Fork(1,4), PushEmpty, PushStar, Jump(-4), Accept)
  val re6 = KleeneStar(KleeneStar(d))
  val program6 = IndexedSeq(InitStar, CheckProgress, Fork(1,9), InitStar, Fork(1,5), MatchSet(dSet), PushChar, PushStar, Jump(-4), PushStar, Jump(-9), Accept)
  val re7 = Union(b, d)
  val program7 = IndexedSeq(Fork(1,5), MatchSet(bSet), PushChar, PushLeft, Jump(4), MatchSet(dSet), PushChar, PushRight, Accept)
  val re8 = Union(b, ε)
  val program8 = IndexedSeq(Fork(1,5), MatchSet(bSet), PushChar, PushLeft, Jump(3), PushEmpty, PushRight, Accept)
  val re9 = Concatenate(Capture("cap_1", KleeneStar(d)), Capture("cap_2", KleeneStar(d)))
  val program9 = IndexedSeq(InitStar, Fork(1,5), MatchSet(dSet), PushChar, PushStar, Jump(-4), PushCapture("cap_1"), InitStar, Fork(1,5), MatchSet(dSet), PushChar, PushStar, Jump(-4), PushCapture("cap_2"), PushConcat, Accept)
  val re10 = KleeneStar(∅)
  val program10 = IndexedSeq(InitStar, Fork(1,4), Reject, PushStar, Jump(-3), Accept)
  val re11 = b
  val program11 = IndexedSeq(MatchSet(bSet), PushChar, Accept)
  val re12 = KleeneStar(b)
  val program12 = IndexedSeq(InitStar, Fork(1,5), MatchSet(bSet), PushChar, PushStar, Jump(-4), Accept)
  val re13 = Union(b, d)
  val program13 = IndexedSeq(Fork(1,5), MatchSet(bSet), PushChar, PushLeft, Jump(4), MatchSet(dSet), PushChar, PushRight, Accept)
  val re14 = Concatenate(Union(Union(b, c), Union(c, d)), d)
  val program14 = IndexedSeq(Fork(1,11), Fork(1,5), MatchSet(bSet), PushChar, PushLeft, Jump(4), MatchSet(cSet), PushChar, PushRight, PushLeft, Jump(10), Fork(1,5), MatchSet(cSet), PushChar, PushLeft, Jump(4), MatchSet(dSet), PushChar, PushRight, PushRight, MatchSet(dSet), PushChar, PushConcat, Accept)

  it should "(ε) parse strings in ε" in {
    new PowersetVm(program_empty_string).eval("") shouldEqual Some(EmptyLeaf)
  }

  it should "(ε) not parse strings not in ε" in {
    new PowersetVm(program_empty_string).eval("a") shouldEqual None
    new PowersetVm(program_empty_string).eval("bdd") shouldEqual None
  }

  it should "(∅) not parse any strings" in {
    new PowersetVm(program_empty_language).eval("a") shouldEqual None
    new PowersetVm(program_empty_language).eval("bdd") shouldEqual None
  }

  it should "(program 1) parse strings in the language with Concat" in {
    new PowersetVm(program1).eval("bdd") shouldEqual Some(ConcatNode(CharLeaf('b'), ConcatNode(CharLeaf('d'), CharLeaf('d'))))
  }

  it should "(program 1) not parse strings not in the regex with Concat" in {
    new PowersetVm(program1).eval("") shouldEqual None
    new PowersetVm(program1).eval("bddd") shouldEqual None
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

  it should "(program 2) not parse strings not in a complex regex" in {
    new PowersetVm(program2).eval("") shouldEqual None
    new PowersetVm(program2).eval("e") shouldEqual None
    new PowersetVm(program2).eval("bdg") shouldEqual None
  }

  it should "(program 3) parse strings in a regex with a capture node and extract out captures" in {
    val tree1 = new PowersetVm(program3).eval("b")
    (new Extractor(tree1.get)).extract("suffix") shouldEqual List("")

    val tree2 = new PowersetVm(program3).eval("babcdfgfgf")
    (new Extractor(tree2.get)).extract("suffix") shouldEqual List("abcdfgfgf")
  }

  it should "(program 3) not parse strings not in a regex with a capture node" in {
    new PowersetVm(program3).eval("") shouldEqual None
    new PowersetVm(program3).eval("ab") shouldEqual None
    new PowersetVm(program3).eval("bafajlskdjfklsjy") shouldEqual None
  }

  it should "(program 4) parse strings in an ambiguous regex" in {
    new PowersetVm(program4).eval("b") shouldEqual Some(LeftNode(CharLeaf('b')))
  }

  it should "(program 5 & 6) parse strings while terminating with CheckProgress due to a nullable regex within KleeneStar" in {
    new PowersetVm(program5).eval("") shouldEqual Some(StarNode(Seq()))
    new PowersetVm(program6).eval("") shouldEqual Some(StarNode(Seq()))
    new PowersetVm(program6).eval("d") shouldEqual Some(StarNode(Seq(StarNode(Seq(CharLeaf('d'))))))
  }

  it should "(program 5 & 6) not parse strings not in the regex while terminating with CheckProgress due to a nullable regex within KleeneStar" in {
    new PowersetVm(program5).eval("b") shouldEqual None
  }

  it should "(program 7) parse strings while pushing left or right leaves" in {
    new PowersetVm(program7).eval("b") shouldEqual Some(LeftNode(CharLeaf('b')))
    new PowersetVm(program7).eval("d") shouldEqual Some(RightNode(CharLeaf('d')))
  }

  it should "(program 8) parse strings while taking an empty leaf" in {
    new PowersetVm(program8).eval("") shouldEqual Some(RightNode(EmptyLeaf))
  }

  it should "(program 9) parse strings and prioritize the left of concat for capture" in {
    val tree1 = new PowersetVm(program9).eval("")
    (new Extractor(tree1.get)).extract("cap_1") shouldEqual List("")
    (new Extractor(tree1.get)).extract("cap_2") shouldEqual List("")

    val tree2 = new PowersetVm(program9).eval("dddd")
    (new Extractor(tree2.get)).extract("cap_1") shouldEqual List("dddd")
    (new Extractor(tree2.get)).extract("cap_2") shouldEqual List("")
  }

  it should "(program 10) parse strings while taking the KleeneStar of the empty language" in {
    new PowersetVm(program10).eval("") shouldEqual Some(StarNode(Seq()))
  }

  it should "(program 10) not parse strings not in the regex while taking the KleeneStar of the empty language" in {
    new PowersetVm(program10).eval("s") shouldEqual None
  }

  it should "(program 11) parse strings in chars regex" in {
    new PowersetVm(program11).eval("b") shouldEqual Some(CharLeaf('b'))
  }

  it should "(program 11) not parse strings not in chars regex" in {
    new PowersetVm(program11).eval("") shouldEqual None
    new PowersetVm(program11).eval("s") shouldEqual None
  }

  it should "(program 12) parse strings in KleeneStar regex" in {
    new PowersetVm(program12).eval("") shouldEqual Some(StarNode(Seq()))
    new PowersetVm(program12).eval("b") shouldEqual Some(StarNode(Seq(CharLeaf('b'))))
    new PowersetVm(program12).eval("bbbb") shouldEqual Some(StarNode(Seq(CharLeaf('b'), CharLeaf('b'), CharLeaf('b'), CharLeaf('b'))))
  }

  it should "(program 12) not parse strings not in KleeneStar regex" in {
    new PowersetVm(program12).eval("a") shouldEqual None
    new PowersetVm(program12).eval("bbba") shouldEqual None
  }

  it should "(program 13) parse strings in Union regex" in {
    new PowersetVm(program13).eval("b") shouldEqual Some(LeftNode(CharLeaf('b')))
    new PowersetVm(program13).eval("d") shouldEqual Some(RightNode(CharLeaf('d')))
  }

  it should "(program 13) not parse strings not in Union regex" in {
    new PowersetVm(program13).eval("") shouldEqual None
    new PowersetVm(program13).eval("c") shouldEqual None
    new PowersetVm(program13).eval("bd") shouldEqual None
  }

  it should "(program 14) parse strings in nested ambiguous regex" in { 
    new PowersetVm(program14).eval("cd") shouldEqual Some(ConcatNode(LeftNode(RightNode(CharLeaf('c'))), CharLeaf('d'))) // does not return one starting with right node
  }
}
