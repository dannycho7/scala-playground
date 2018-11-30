package edu.ucsb.cs.cs162.regex

import org.scalatest._
import edu.ucsb.cs.cs162.regex.derivative._

class RegexSpec extends FlatSpec with Matchers with OptionValues {
  //----------------------------------------------------------------------------
  // Fixtures and helpers.
  // ---------------------------------------------------------------------------

  import Regex._

  val charA = Chars('a')
  val b = Chars('b')
  val c = Chars('c')
  val d = Chars('d')
  val e = Chars('e')
  val f = Chars('f')

  val r = Chars('a') | Chars('b').+
  val r1 = Chars('x', 'y').* ~ r
  val r2 = Chars('y', 'x').+ ~ r
  //----------------------------------------------------------------------------
  // Tests.
  // ---------------------------------------------------------------------------

  behavior of "a regex"

  it should "be buildable using `~`" in {
    (r1 ~ r2) should equal (Chars('x', 'y').* ~ r ~ Chars('y', 'x').+ ~ r)
    // simplifications
    (r ~ ∅) should equal(∅)
    (∅ ~ r) should equal(∅)
    (r ~ ε) should equal(r)
    (ε ~ r) should equal(r)
  }


  it should "be buildable using `|`" in {
    (r1 | r2) should equal(Union(r2, r1)) // also testing normalization due to lengths of r1 and r2
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
    (r1 & r2) should equal(Intersect(r2, r1)) // also testing normalization due to lengths of r1 and r2
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

  it should "be buildable using `<>`" in {
    (r <>(2, 3)) should equal((r ~ r ~ r.*) & (ε | r | (r ~ r) | (r ~ r ~ r)))
  }

  it should "should not allow negative numbers for `<>`" in {
    an [AssertionError] should be thrownBy (r <> (-5, -2))
    an [AssertionError] should be thrownBy (r <> (-5, 2))
    an [AssertionError] should be thrownBy (r <> (5, -2))
    an [AssertionError] should be thrownBy (r <> (5, 2))
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
    b^3 should equal (Concatenate(b, Concatenate(b, b)))
  }

  it should "be buildable using convenience methods 9" in {
    (b >= 2) should equal (Concatenate(b, Concatenate(b, KleeneStar(b))))
  }

  it should "be buildable using convenience methods 10" in {
    (b <= 2) should equal (Union(ε, Union(b, Concatenate(b, b))))
  }

  it should "be buildable using convenience methods 11" in {
    (b <> (1, 3)) should equal (Intersect(Concatenate(b, KleeneStar(b)), Union(ε, Union(b, Union(Concatenate(b, b), Concatenate(b, Concatenate(b, b)))))))
  }

  it should "be buildable from strings" in {
    "ab".charset ~ "cd".concatenate should equal (Concatenate(Chars('a', 'b'),
      Concatenate(Chars('c'), Chars('d'))))
  }

  it should "pretty-print correctly" in {
    (b.? | (c >= 1)).prettyPrint should equal ("""Union
                                                 |├─ ε
                                                 |└─ Union
                                                 |   ├─ b
                                                 |   └─ Concatenate
                                                 |      ├─ c
                                                 |      └─ KleeneStar
                                                 |         └─ c
                                                 |""".stripMargin)
  }

  it should "normalize correctly 1" in {
    val re = ((charA ~ b) ~ (c ~ d)) ~ (e ~ f)

    val norm = Concatenate(charA, Concatenate(b, Concatenate(c,
      Concatenate(d, Concatenate(e, f)))))

    re should equal (norm)
  }

  it should "normalize correctly 2" in {
    val re = (((b | ε) & charA) | !charA | charA.*) | ((charA ~ b) |
      charA | ε)

    val norm = Union(ε, Union(charA, Union(Concatenate(charA, b),
      Union(KleeneStar(charA), Union(Complement(charA), Intersect(charA,
        Union(ε, b)))))))

    re should equal (norm)
  }

  behavior of "nullable"

  val nullable_r1 = ε
  val nullable_r2 = Chars('b', 'c').*
  val nullable_r3 = ∅ | nullable_r2
  val nullable_r4 = r <> (0, 3)
  val nullable_r5 = r <= 10
  val nullable_r6 = r^0
  val nullable_r7 = !Chars('b')
  val nullable_r8 = ε ~ nullable_r7
  val nullable_r9 = ε & nullable_r7
  val nullable_r10 = Chars('b', 'c', 'd').?

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

  it should "recognize a nullable regex 8" in {
    nullable_r8.nullable should equal(ε)
  }

  it should "recognize a nullable regex 9" in {
    nullable_r9.nullable should equal(ε)
  }

  it should "recognize a nullable regex 10" in {
    nullable_r10.nullable should equal(ε)
  }

  val non_nullable_r1 = ∅
  val non_nullable_r2 = Chars('b', 'c').+
  val non_nullable_r3 = ∅ & Chars('b', 'c').*
  val non_nullable_r4 = Chars('b')
  val non_nullable_r5 = r^5
  val non_nullable_r6 = r <> (1, 3)
  val non_nullable_r7 = r >= 4
  val non_nullable_r8 = !ε
  val non_nullable_r9 = !ε ~ nullable_r1
  val non_nullable_r10 = !ε | non_nullable_r9

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

  it should "recognize a non-nullable regex 8" in {
    non_nullable_r8.nullable should equal(∅)
  }

  it should "recognize a non-nullable regex 9" in {
    non_nullable_r9.nullable should equal(∅)
  }

  it should "recognize a non-nullable regex 10" in {
    non_nullable_r10.nullable should equal(∅)
  }

  behavior of "ambiguity type checker"

  it should "not allow unambiguous to be called on non-constructive regexes" in {
    val a = Chars('a')
    val b = Chars('b')

    an [AssertionError] should be thrownBy (Complement(b)).unambiguous
    an [AssertionError] should be thrownBy (Capture("a_val", a)).unambiguous
    an [AssertionError] should be thrownBy (Intersect(a, b)).unambiguous
    an [AssertionError] should be thrownBy (Union(Concatenate(Complement(b), Capture("a_val", a)), b)).unambiguous
  }

  it should "find the ambiguous subexpression and a witness string in an ambiguous regex (default test case)" in {
    val a = Chars('a')
    val b = Chars('b')
    val r = a ~ (b | ε) ~ (b | ε)
    val (ambiguousSubexpr, witness) = r.unambiguous.value
    ambiguousSubexpr shouldEqual (b | ε) ~ (b | ε)
    new DerivativeMachine(ambiguousSubexpr).eval(witness) shouldEqual true
  }

  it should "prioritize the left subexpression in an ambiguous Concatenate regex with ambiguity in both subexpressions" in {
    val a = Chars('a')
    val b = Chars('b')
    val r = Concatenate(Concatenate((b | ε), (b | ε)), (b | b.*))

    val (ambiguousSubexpr, witness) = r.unambiguous.value
    ambiguousSubexpr shouldEqual Concatenate((b | ε), (b | ε))
    new DerivativeMachine(ambiguousSubexpr).eval(witness) shouldEqual true
  }

  it should "prioritize the left subexpression in an ambiguous Union regex with ambiguity in both subexpressions" in {
    val a = Chars('a')
    val b = Chars('b')
    val r = Union(Concatenate((b | ε), (b | ε)), (b | b.*))

    val (ambiguousSubexpr, witness) = r.unambiguous.value
    ambiguousSubexpr shouldEqual Concatenate((b | ε), (b | ε))
    new DerivativeMachine(ambiguousSubexpr).eval(witness) shouldEqual true
  }

  it should "find the right subexpression in an ambiguous Union regex with ambiguity only in the right subexpression" in {
    val a = Chars('a')
    val b = Chars('b')
    val r = Union(a, (b | b.*))

    val (ambiguousSubexpr, witness) = r.unambiguous.value
    ambiguousSubexpr shouldEqual (b | b.*)
    new DerivativeMachine(ambiguousSubexpr).eval(witness) shouldEqual true
  }

  it should "find the right subexpression in an ambiguous Concatenate regex with ambiguity only in the right subexpression" in {
    val a = Chars('a')
    val b = Chars('b')
    val r = Concatenate(a, (b | b.*))

    val (ambiguousSubexpr, witness) = r.unambiguous.value
    ambiguousSubexpr shouldEqual (b | b.*)
    new DerivativeMachine(ambiguousSubexpr).eval(witness) shouldEqual true
  }

  it should "find the ambiguous subexpression and a witness string in an ambiguous KleeneStar of an unambiguous regex" in {
    val a = Chars('a')
    val b = Chars('b')
    val r = KleeneStar(Union(b~b, b~b~b))
    val (ambiguousSubexpr, witness) = r.unambiguous.value
    ambiguousSubexpr shouldEqual r
    new DerivativeMachine(ambiguousSubexpr).eval(witness) shouldEqual true
  }

  it should "find the ambiguous subexpression and a witness string in an ambiguous KleeneStar of an ambiguous regex" in {
    val a = Chars('a')
    val b = Chars('b')
    val r = KleeneStar(Union(b, b))
    val (ambiguousSubexpr, witness) = r.unambiguous.value
    ambiguousSubexpr shouldEqual Union(b, b)
    new DerivativeMachine(ambiguousSubexpr).eval(witness) shouldEqual true
  }

  it should "find the ambiguous subexpression and a witness string in an ambiguous KleeneStar of a nullable regex" in {
    val a = Chars('a')
    val b = Chars('b')
    val r = KleeneStar(Union(b, ε))
    val (ambiguousSubexpr, witness) = r.unambiguous.value
    ambiguousSubexpr shouldEqual KleeneStar(Union(b, ε))
    new DerivativeMachine(ambiguousSubexpr).eval(witness) shouldEqual true
  }

  it should "find the ambiguous subexpression and a witness string in a complex ambiguous regex" in {
    val a = Chars('a')
    val b = Chars('b')
    val d_z_set = Chars('d' -> 'z')
    val r = KleeneStar(Concatenate(Union(a, d_z_set), Union(b, b)))
    val (ambiguousSubexpr, witness) = r.unambiguous.value
    ambiguousSubexpr shouldEqual Union(b, b)
    new DerivativeMachine(ambiguousSubexpr).eval(witness) shouldEqual true
  }

  it should "return None if the regex is unambiguous (default test case)" in {
    val a = Chars('a')
    val b = Chars('b')
    val r = a ~ (b | ε)
    r.unambiguous shouldEqual None
  }

  it should "return None if the base regex is unambiguous" in {
    val a = Chars('a')
    ∅.unambiguous shouldEqual None
    ε.unambiguous shouldEqual None
    a.unambiguous shouldEqual None
  }

  it should "return None if the Concatenate regex is unambiguous" in {
    val a = Chars('a')
    val b = Chars('b')
    val r = Concatenate(a, b)
    r.unambiguous shouldEqual None
  }

  it should "return None if the Union regex is unambiguous" in {
    val a = Chars('a')
    val b = Chars('b')
    val r = Union(a, b)
    r.unambiguous shouldEqual None
  }

  it should "return None if the KleeneStar regex is unambiguous" in {
    val a = Chars('a')
    val b = Chars('b')
    val r = KleeneStar(Union(a, b))
    r.unambiguous shouldEqual None
  }

  it should "return None if the complex regex is unambiguous" in {
    val a = Chars('a')
    val b = Chars('b')
    val c = Chars('c')
    val d_z_set = Chars('d' -> 'z')
    val r = KleeneStar(Concatenate(Union(c, d_z_set), Union(a, b)))
    r.unambiguous shouldEqual None
  }

}
