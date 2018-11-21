package edu.ucsb.cs.cs162.regex.vm.compiler

import org.scalatest._
import edu.ucsb.cs.cs162.range_set._
import edu.ucsb.cs.cs162.regex._
import edu.ucsb.cs.cs162.regex.vm._


class CompileSpec extends FlatSpec with Matchers {
  //----------------------------------------------------------------------------
  // Fixtures and helpers.
  // ---------------------------------------------------------------------------

  import Regex._

  //----------------------------------------------------------------------------
  // Tests.
  // ---------------------------------------------------------------------------

  behavior of "compile"

  val c = Chars('c')
  val d = Chars('d')
  val e = Chars('e')

  val cSet = CharSet('c')
  val dSet = CharSet('d')
  val eSet = CharSet('e')

  it should "correctly compile the empty language" in {
    Compiler.compile(∅) shouldEqual IndexedSeq(Reject, Accept)
  }

  it should "correctly compile ε" in {
    Compiler.compile(ε) shouldEqual IndexedSeq(PushEmpty, Accept)
  }

  it should "correctly compile α" in {
    Compiler.compile(α) shouldEqual IndexedSeq(MatchSet(α.chars), PushChar, Accept)
  }

  it should "correctly compile chars 1" in {
    pending
  }

  it should "correctly compile chars 2" in {
    pending
  }

  it should "correctly compile concatenation" in  {
    Compiler.compile(Concatenate(c, d)) shouldEqual IndexedSeq(
      MatchSet(cSet),
      PushChar,
      MatchSet(dSet),
      PushChar,
      PushConcat,
      Accept
    )
  }

  // More concatenation tests

  it should "correctly compile union" in  {
    Compiler.compile(Union(c, d)) shouldEqual IndexedSeq(
      Fork(1, 5),
      MatchSet(cSet),
      PushChar,
      PushLeft,
      Jump(4),
      MatchSet(dSet),
      PushChar,
      PushRight,
      Accept
    )
  }

  // More Union tests

  it should "correctly compile KleeneStar" in  {
    Compiler.compile(KleeneStar(Union(c, d))) shouldEqual IndexedSeq(
      InitStar,
      Fork(1, 11),
      Fork(1, 5),
      MatchSet(cSet),
      PushChar,
      PushLeft,
      Jump(4),
      MatchSet(dSet),
      PushChar,
      PushRight,
      PushStar,
      Jump(-10),
      Accept
    )
  }

  it should "correctly compile KleeneStar of nullable regexes" in  {
    Compiler.compile(KleeneStar(Union(c, ε))) shouldEqual IndexedSeq(
      InitStar,
      CheckProgress,
      Fork(1, 10),
      Fork(1, 5),
      MatchSet(cSet),
      PushChar,
      PushLeft,
      Jump(3),
      PushEmpty,
      PushRight,
      PushStar,
      Jump(-10),
      Accept
    )
  }

  it should "correctly compile KleeneStar of KleeneStar regexes" in  {
    Compiler.compile(KleeneStar(KleeneStar(c))) shouldEqual IndexedSeq(
      InitStar,
      CheckProgress,
      Fork(1, 9),
      InitStar,
      Fork(1, 5),
      MatchSet(cSet),
      PushChar,
      PushStar,
      Jump(-4),
      PushStar,
      Jump(-9),
      Accept
    )
  }

  it should "correctly compile complex regexes 1" in {
    Compiler.compile(Union(Concatenate(c, e), d)) shouldEqual IndexedSeq(
      Fork(1, 8),
      MatchSet(cSet),
      PushChar,
      MatchSet(eSet),
      PushChar,
      PushConcat,
      PushLeft,
      Jump(4),
      MatchSet(dSet),
      PushChar,
      PushRight,
      Accept
    )
  }

  it should "correctly compile complex regexes 2 that has a capture" in {
    Compiler.compile(Union(Concatenate(c, e), Capture("num_d", KleeneStar(d)))) shouldEqual IndexedSeq(
      Fork(1, 8),
      MatchSet(cSet),
      PushChar,
      MatchSet(eSet),
      PushChar,
      PushConcat,
      PushLeft,
      Jump(9),
      InitStar,
      Fork(1, 5),
      MatchSet(dSet),
      PushChar,
      PushStar,
      Jump(-4),
      PushCapture("num_d"),
      PushRight,
      Accept
    ) 
  }

  // TODO: 
  it should "correctly compile complex regexes 3 that has nested unions with the proper PushLeft and PushRight" in {
    pending
    // Compiler.compile(Union(Concatenate(c, e), Capture("num_d", KleeneStar(d)))) shouldEqual
  }

  it should "throw an assertion error by a regex with Complement" in {
    an [AssertionError] should be thrownBy Compiler.compile(Complement(c))
  }
  
  it should "throw an assertion error by a regex with Intersect" in {
    an [AssertionError] should be thrownBy Compiler.compile(Intersect(c, d))
  }

  it should "throw an assertion error by a complex invalid regex" in {
    an [AssertionError] should be thrownBy Compiler.compile(KleeneStar(Union(Intersect(c, d), Complement(e))))
  }
}
