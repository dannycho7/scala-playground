package edu.ucsb.cs.cs162.regex

import org.scalatest._

class RegexSpec extends FlatSpec with Matchers {
  //----------------------------------------------------------------------------
  // Fixtures and helpers.
  // ---------------------------------------------------------------------------

  import Regex._

  val b = Chars('b')
  val c = Chars('c')
  val d = Chars('d')

  val r = Chars('a') | Chars('b').+
  val r1 = Chars('x', 'y').* ~ r
  val r2 = Chars('y', 'x').+ ~ r
  //----------------------------------------------------------------------------
  // Tests.
  // ---------------------------------------------------------------------------

  behavior of "a regex"

  it should "be buildable using `~`" in {
    (r1 ~ r2) should equal (Concatenate(r1, r2))
    // simplifications
    (r ~ ∅) should equal(∅)
    (∅ ~ r) should equal(∅)
    (r ~ ε) should equal(r)
    (ε ~ r) should equal(r)
  }


  it should "be buildable using `|`" in {
    (r1 | r2) should equal(Union(r1, r2))
    // simplifications
    (r | ∅) should equal(r)
    (∅ | r) should equal(r)
    (Chars('a' -> 'c') | Chars('c' -> 'f')) should equal(Chars('a'->'f'))
    (r.* |   ε) should equal(r.*)
    (ε   | r.*) should equal(r.*)
    (α.* |   r) should equal(α.*)
    (r |   α.*) should equal(α.*)
    (r | r)     should equal(r)
  }

  it should "be buildable using `*`" in {
    r.* should equal(KleeneStar(r))
    // simplifications
    ∅.* should equal(ε)
    ε.* should equal(ε)
    (r.*).* should equal(r.*)
  }

  it should "be buildable using `!`" in {
    !r should equal(Complement(r))
    // Simplifications
    !(!r) should equal(r)
    !(∅) should equal(α.*)
    !ε should equal(α.+)
  }

  it should "be buildable using `&`" in {
    (r1 & r2) should equal(Intersect(r1, r2))
    // Simplifications
    (∅ & r) should equal(∅)
    (r & ∅) should equal(∅)
    (Chars('a'->'d') & Chars('c'->'f')) should equal (Chars('c'->'d'))
    (α.* & r) should equal(r)
    (r & α.*) should equal(r)
    (r & r) should equal(r)
  }

  it should "be buildable using `^`" in {
    (r^5) should equal(r ~ r ~ r ~ r ~ r)
  }

  it should "should not allow negative numbers for `^`" in {
    an [AssertionError] should be thrownBy (r^(-5))
  }

  it should "should be the empty string for r^0" in {
    (r^0) should equal(ε)
  }

  it should "be buildable using `>=`" in {
    (r >= 3) should equal(r ~ r ~ r ~ r.*)
  }

  it should "should not allow negative numbers for `>=`" in {
    an [AssertionError] should be thrownBy (r >= -5)
  }

  it should "be buildable using `<=`" in {
    (r <= 3) should equal(ε | r | (r ~ r) | (r ~ r ~ r))
  }

  it should "should not allow negative numbers for `<=`" in {
    an [AssertionError] should be thrownBy (r <= -5)
  }

  it should "be buildable using `<=>`" in {
    (r <>(2, 3)) should equal((r ~ r ~ r.*) & (ε | r | (r ~ r) | (r ~ r ~ r)))
  }

  it should "should not allow negative numbers for `<=>`" in {
    an [AssertionError] should be thrownBy (r <> (-5, -2))
    an [AssertionError] should be thrownBy (r <> (-5, 2))
    an [AssertionError] should be thrownBy (r <> (5, -2))
  }

  it should "be buildable using convenience methods 1" in {
    (b ~ c) should equal (Concatenate(b, c))
  }

  it should "be buildable using convenience methods 2" in {
    (b | (b ~ c)) should equal (Union(b, Concatenate(b, c)))
  }

  it should "be buildable using convenience methods 3" in {
    b.* should equal (KleeneStar(b))
  }

  it should "be buildable using convenience methods 4" in {
    !b should equal (Complement(b))
  }

  it should "be buildable using convenience methods 5" in {
    (b & (b ~ c)) should equal (Intersect(b, Concatenate(b, c)))
  }

  it should "be buildable using convenience methods 6" in {
    b.+ should equal (Concatenate(b, KleeneStar(b)))
  }

  it should "be buildable using convenience methods 7" in {
    b.? should equal (Union(ε, b))
  }

  it should "be buildable using convenience methods 8" in {
    b^3 should equal (Concatenate(Concatenate(b, b), b))
  }

  it should "be buildable using convenience methods 9" in {
    (b >= 2) should equal (Concatenate(Concatenate(b, b), KleeneStar(b)))
  }

  it should "be buildable using convenience methods 10" in {
    (b <= 2) should equal (Union(Union(ε, b), Concatenate(b, b)))
  }

  it should "be buildable using convenience methods 11" in {
    (b <> (1, 3)) should equal (Intersect(Concatenate(b, KleeneStar(b)), Union(Union(Union(ε, b), Concatenate(b, b)), Concatenate(Concatenate(b, b), b))))
  }

  it should "be buildable from strings" in {
    "ab".charset ~ "cd".concatenate should equal (Concatenate(Chars('a', 'b'),
      Concatenate(Chars('c'), Chars('d'))))
  }

  it should "pretty-print correctly" in {
    (b.? | (c >= 1)).prettyPrint should equal (s"""Union\n├─ Union\n│  ├─ ε\n│  └─ b\n└─ Concatenate\n   ├─ c\n   └─ KleeneStar\n      └─ c\n""")
  }


  behavior of "nullable"

  val nullable_r1 = ε
  val nullable_r2 = Chars('b', 'c').*
  val nullable_r3 = ∅ | Chars('b', 'c').*
  val nullable_r4 = r <> (0, 3)
  val nullable_r5 = r <= 10
  val nullable_r6 = r^0
  val nullable_r7 = !Chars('b')

  it should "recognize a nullable regex 1" in {
    nullable_r1.nullable should equal(ε)
  }

  it should "recognize a nullable regex 2" in {
    nullable_r2.nullable should equal(ε)
  }

  it should "recognize a nullable regex 3" in {
    nullable_r3.nullable should equal(ε)
  }

  it should "recognize a nullable regex 4" in {
    nullable_r4.nullable should equal(ε)
  }

  it should "recognize a nullable regex 5" in {
    nullable_r5.nullable should equal(ε)
  }

  it should "recognize a nullable regex 6" in {
    nullable_r6.nullable should equal(ε)
  }

  it should "recognize a nullable regex 7" in {
    nullable_r7.nullable should equal(ε)
  }

  val non_nullable_r1 = ∅
  val non_nullable_r2 = Chars('b', 'c').+
  val non_nullable_r3 = ∅ & Chars('b', 'c').*
  val non_nullable_r4 = Chars('b')
  val non_nullable_r5 = r^5
  val non_nullable_r6 = r <> (1, 3)
  val non_nullable_r7 = r >= 4

  it should "recognize a non-nullable regex 1" in {
    non_nullable_r1.nullable should equal(∅)
  }
  it should "recognize a non-nullable regex 2" in {
    non_nullable_r2.nullable should equal(∅)
  }

  it should "recognize a non-nullable regex 3" in {
    non_nullable_r3.nullable should equal(∅)
  }

  it should "recognize a non-nullable regex 4" in {
    non_nullable_r4.nullable should equal(∅)
  }

  it should "recognize a non-nullable regex 5" in {
    non_nullable_r5.nullable should equal(∅)
  }

  it should "recognize a non-nullable regex 6" in {
    non_nullable_r6.nullable should equal(∅)
  }

  it should "recognize a non-nullable regex 7" in {
    non_nullable_r7.nullable should equal(∅)
  }
}
