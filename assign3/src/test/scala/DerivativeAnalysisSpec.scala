package edu.ucsb.cs.cs162.regex.derivative

import org.scalatest._
import edu.ucsb.cs.cs162.dfa._
import edu.ucsb.cs.cs162.range_set._
import edu.ucsb.cs.cs162.regex._
import edu.ucsb.cs.cs162.util._

class DerivativeAnalysisSpec extends FlatSpec with Matchers with Timeout {
  //----------------------------------------------------------------------------
  // Fixtures and helpers.
  // ---------------------------------------------------------------------------

  import Regex._

  // The timeout in milliseconds for potentially slow code.
  val timeout = 2000

  // Analyze the given expression subject to a timeout.
  def analyzeWithTimeout(re: Regex) =
    timeoutAfter(timeout) { DerivativeAnalysis.analyze(re) }

  //----------------------------------------------------------------------------
  // Tests.
  // ---------------------------------------------------------------------------

  val charA = Chars('a')
  val b = Chars('b')
  val c = Chars('c')
  val d = Chars('d')

  val r0 = ε | charA
  val r1 = charA | b.+
  val r2 = Chars('x', 'y').* ~ r1
  val r3 = Chars('y', 'x').+ ~ r1
  val r4 = (r1^0) ~ r2
  val r5 = r1 & r2
  val r6 = Chars('a', 'c')^5
  val r7 = (b | d) <> (0, 4)
  val r8 = !r1
  val r9 = c >= 3
  val r10 = d <= 5
  val r11 = r1.?
  val r12 = (c | d).*
  val r13 = Chars('b' -> 'g')
  val r14 = !r13

  behavior of "the analysis"

  it should "should always terminate 1" in {
    // Causes a timeout or stack overflow if expression similarity isn't
    // implemented correctly.
    val dfa = analyzeWithTimeout((charA | (charA ~ charA)).*)
  }

  it should "should always terminate 2" in {
    // This test should test that check if normalization and DFA
    // building work well together. If the regexes are not conflated
    // properly, DFA construction would cause a timeout or stack
    // overflow and this test should fail.

    val dfa = analyzeWithTimeout(Union(ε, Chars('a')).*)
  }

  it should "should always terminate 3" in {
    // This test should test that check if normalization and DFA
    // building work well together. If the regexes are not conflated
    // properly, DFA construction would cause a timeout or stack
    // overflow and this test should fail.

    val dfa = analyzeWithTimeout((Chars('a')^2 | Chars('b')^2 | Chars('c')^2).*)
  }


  it should "should always terminate 4" in {
    // This test should test that check if normalization and DFA
    // building work well together. If the regexes are not conflated
    // properly, DFA construction would cause a timeout or stack
    // overflow and this test should fail.

    val dfa = analyzeWithTimeout(((Chars('a' -> 'c')^2) & Chars('b'->'f')^2 | Chars('t' -> 'q')^2).*)
  }

  it should "should always terminate 5" in {
    // This test should test that check if normalization and DFA
    // building work well together. If the regexes are not conflated
    // properly, DFA construction would cause a timeout or stack
    // overflow and this test should fail.
    val a = Chars('a')
    val b = Chars('b')

    val dfa = analyzeWithTimeout((a.+ | b).*)
  }

  it should "should always terminate 6" in {
    // This test should test that check if normalization and DFA
    // building work well together. If the regexes are not conflated
    // properly, DFA construction would cause a timeout or stack
    // overflow and this test should fail.
    val a = Chars('a')
    val b = Chars('b')

    val dfa = analyzeWithTimeout((a & b).*)
  }

  it should "produce a DFA that recognizes the strings in language 0" in {
    val dfa = analyzeWithTimeout(r0)

    dfa.matches("") should equal (true)
    dfa.matches("a") should equal (true)
  }

  it should "produce a DFA that recognizes the strings in language 1" in {
    val dfa = analyzeWithTimeout(r1)

    dfa.matches("a") should equal(true)
    dfa.matches("b") should equal(true)
    dfa.matches("bbbbb") should equal(true)
  }

  it should "produce a DFA that recognizes the strings in language 2" in {
    val dfa = analyzeWithTimeout(r2)

    dfa.matches("a") should equal(true)
    dfa.matches("xyxxa") should equal(true)
  }

  it should "produce a DFA that recognizes the strings in language 3" in {
    val dfa = analyzeWithTimeout(r3)

    dfa.matches("yxya") should equal(true)
    dfa.matches("xbb") should equal(true)
  }

  it should "produce a DFA that recognizes the strings in language 4" in {
    val dfa = analyzeWithTimeout(r4)

    dfa.matches("a") should equal(true)
    dfa.matches("xyxbb") should equal(true)
  }

  it should "produce a DFA that recognizes the strings in language 5" in {
    val dfa = analyzeWithTimeout(r5)

    dfa.matches("a") should equal(true)
    dfa.matches("bb") should equal(true)
  }

  it should "produce a DFA that recognizes the strings in language 6" in {
    val dfa = analyzeWithTimeout(r6)

    dfa.matches("acccc") should equal(true)
    dfa.matches("acaca") should equal(true)
  }

  it should "produce a DFA that recognizes the strings in language 7" in {
    val dfa = analyzeWithTimeout(r7)

    dfa.matches("") should equal(true)
    dfa.matches("b") should equal(true)
    dfa.matches("dbbd") should equal(true)
  }

  it should "produce a DFA that recognizes the strings in language 8" in {
    val dfa = analyzeWithTimeout(r8)

    dfa.matches("rygh") should equal(true)
    dfa.matches("ba") should equal(true)
  }

  it should "produce a DFA that recognizes the strings in language 9" in {
    val dfa = analyzeWithTimeout(r9)

    dfa.matches("cccccccc") should equal(true)
    dfa.matches("ccc") should equal(true)
  }

  it should "produce a DFA that recognizes the strings in language 10" in {
    val dfa = analyzeWithTimeout(r10)

    dfa.matches("ddddd") should equal(true)
    dfa.matches("d") should equal(true)
    dfa.matches("") should equal(true)
  }

  it should "produce a DFA that recognizes the strings in language 11" in {
    val dfa = analyzeWithTimeout(r11)

    dfa.matches("") should equal(true)
    dfa.matches("a") should equal(true)
    dfa.matches("bb") should equal(true)
  }

  it should "produce a DFA that recognizes the strings in language 12" in {
    val dfa = analyzeWithTimeout(r12)

    dfa.matches("") should equal(true)
    dfa.matches("cddc") should equal(true)
  }

  it should "produce a DFA that recognizes the strings in language 13" in {
    val dfa = analyzeWithTimeout(r13)

    dfa.matches("d") should equal(true)
    dfa.matches("g") should equal(true)
  }

  it should "produce a DFA that recognizes the strings in language 14" in {
    val dfa = analyzeWithTimeout(r14)

    dfa.matches("a") should equal(true)
    dfa.matches("asdasd") should equal(true)
  }

  it should "produce a DFA that should not recognize strings not in the language 0" in {
    val dfa = analyzeWithTimeout(r0)

    dfa.matches("b") should equal (false)
    dfa.matches("aa") should equal (false)
  }

  it should "produce a DFA that should not recognize strings not in the language 1" in {
    val dfa = analyzeWithTimeout(r1)

    dfa.matches("") should equal(false)
    dfa.matches("ab") should equal(false)
    dfa.matches("ba") should equal(false)
  }

  it should "produce a DFA that should not recognize strings not in the language 2" in {
    val dfa = analyzeWithTimeout(r2)

    dfa.matches("xab") should equal(false)
  }

  it should "produce a DFA that should not recognize strings not in the language 3" in {
    val dfa = analyzeWithTimeout(r3)

    dfa.matches("") should equal(false)
    dfa.matches("a") should equal(false)
    dfa.matches("ba") should equal(false)
  }

  it should "produce a DFA that should not recognize strings not in the language 4" in {
    val dfa = analyzeWithTimeout(r4)

    dfa.matches("xab") should equal(false)
  }

  it should "produce a DFA that should not recognize strings not in the language 5" in {
    val dfa = analyzeWithTimeout(r5)

    dfa.matches("") should equal(false)
    dfa.matches("ab") should equal(false)
    dfa.matches("ba") should equal(false)
  }

  it should "not produce a DFA that recognizes the strings in language 6" in {
    val dfa = analyzeWithTimeout(r6)

    dfa.matches("cacccc") should equal(false)
    dfa.matches("acbca") should equal(false)
  }

  it should "not produce a DFA that recognizes the strings in language 7" in {
    val dfa = analyzeWithTimeout(r7)

    dfa.matches("z") should equal(false)
    dfa.matches("bbdddb") should equal(false)
  }

  it should "not produce a DFA that recognizes the strings in language 8" in {
    val dfa = analyzeWithTimeout(r8)

    dfa.matches("a") should equal(false)
    dfa.matches("bbb") should equal(false)
  }

  it should "not produce a DFA that recognizes the strings in language 9" in {
    val dfa = analyzeWithTimeout(r9)

    dfa.matches("") should equal(false)
    dfa.matches("cc") should equal(false)
  }

  it should "not produce a DFA that recognizes the strings in language 10" in {
    val dfa = analyzeWithTimeout(r10)

    dfa.matches("dddddddd") should equal(false)
    dfa.matches("x") should equal(false)
  }

  it should "not produce a DFA that recognizes the strings in language 11" in {
    val dfa = analyzeWithTimeout(r11)

    dfa.matches("ab") should equal(false)
    dfa.matches("aa") should equal(false)
  }

  it should "not produce a DFA that recognizes the strings in language 12" in {
    val dfa = analyzeWithTimeout(r12)

    dfa.matches("aj") should equal(false)
  }

  it should "not produce a DFA that recognizes the strings in language 13" in {
    val dfa = analyzeWithTimeout(r13)

    dfa.matches("a") should equal(false)
    dfa.matches("bg") should equal(false)
  }

  it should "not produce a DFA that recognizes the strings in language 14" in {
    val dfa = analyzeWithTimeout(r14)

    dfa.matches("c") should equal(false)
    dfa.matches("b") should equal(false)
  }

  it should "produce a DFA that has the correct structure 1" in {
    val a = Chars('a')
    val b = Chars('b')
    val Σ = α.chars
    val aSet = CharSet('a')
    val bSet = CharSet('b')

    DerivativeAnalysis.analyze(a ~ b) shouldEqual Dfa(
      Map[Regex, Seq[(CharSet, Regex)]](
        a ~ b -> Seq((aSet, b), (!aSet, Regex.∅)),
        b -> Seq((bSet, ε), (!bSet, ∅)),
        ε -> Seq((Σ, ∅)),
        ∅ -> Seq((Σ, ∅))
      ),
      a ~ b,
      Set[Regex](ε)
    )
  }

  it should "produce a DFA that has the correct structure 2" in {
    val a = Chars('a')
    val b = Chars('b')
    val Σ = α.chars

    DerivativeAnalysis.analyze((a | b).*) shouldEqual Dfa(
      Map[Regex, Seq[(CharSet, Regex)]](
        (a | b).* -> Seq((CharSet('a', 'b'), (a | b).*), (!CharSet('a', 'b'), ∅)),
        ∅ -> Seq((Σ, ∅))
      ),
      (a | b).*,
      Set[Regex]((a | b).*)
    )
  }

  it should "produce a DFA that has the correct structure 3" in {
    val a = Chars('a')
    val b = Chars('b')
    val Σ = α.chars

    DerivativeAnalysis.analyze((a & b).*) shouldEqual Dfa(
      Map[Regex, Seq[(CharSet, Regex)]](
        ε -> Seq((Σ, ∅)),
        ∅ -> Seq((Σ, ∅))
      ),
      ε,
      Set[Regex](ε)
    )
  }

  it should "produce a DFA that has the correct structure 4" in {
    val a = Chars('a')
    val b = Chars('b')
    val notAOrB = !CharSet('a' ,'b')
    val Σ = α.chars
    val aSet = CharSet('a')
    val bSet = CharSet('b')

    DerivativeAnalysis.analyze((a.+ | b).*) shouldEqual Dfa(
      Map[Regex, Seq[(CharSet, Regex)]](
        ((b | (a ~ (a).*))).* -> Seq((bSet, ((b | (a ~ (a).*))).*), (aSet, ((a).* ~ ((b | (a ~ (a).*))).*)), (notAOrB, ∅)),
        ((a).* ~ ((b | (a ~ (a).*))).*) -> Seq((aSet,((a).* ~ ((b | (a ~ (a).*))).*)), (bSet, ((b | (a ~ (a).*))).*), (notAOrB, ∅)),
        ∅ -> Seq((Σ,∅))
      ),
      ((b | (a ~ (a).*))).*,
      Set[Regex](((b | (a ~ (a).*))).*, ((a).* ~ ((b | (a ~ (a).*))).*))
    )    
  }
}
