package edu.ucsb.cs.cs162.dfa

import org.scalatest._
import edu.ucsb.cs.cs162.regex._
import edu.ucsb.cs.cs162.regex.derivative._
import edu.ucsb.cs.cs162.range_set._

class DfaSpec extends FlatSpec with Matchers with OptionValues {
  import Regex._

  behavior of "Dfa.getString"

  it should "return None if the DFA's language is empty 1" in {
    val δ = Map(∅ -> Seq(!CharSet() -> ∅))
    val dfa = Dfa(δ, ∅, Set.empty)
    dfa.getString shouldEqual None
  }

  it should "return None if the DFA's language is empty 2" in {
    val a_d_set = Chars('a' -> 'd')
    val e_g_set = Chars('e' -> 'g')
    val intersect_re = Intersect(a_d_set, e_g_set)
    val dfa = DerivativeAnalysis.analyze(intersect_re)
    dfa.getString shouldEqual None
  }

  it should "return None if the DFA's language is empty 3" in {
    val e_g_set = Chars('e' -> 'g')
    val intersect_re = Intersect(ε, e_g_set)
    val dfa = DerivativeAnalysis.analyze(intersect_re)
    dfa.getString shouldEqual None
  }

  it should "return None if the DFA's language is empty 4" in {
    val complement_re = Complement(α.*)
    val dfa = DerivativeAnalysis.analyze(complement_re)
    dfa.getString shouldEqual None
  }

  it should "return None if the DFA's language is empty 5" in {
    val union_re = Union(∅, ∅)
    val dfa = DerivativeAnalysis.analyze(union_re)
    dfa.getString shouldEqual None
  }

  it should "return None if the DFA's language is empty 6" in {
    val complex_re = Complement(Union(α.*, Union(∅, ∅)))
    val dfa = DerivativeAnalysis.analyze(complex_re)
    dfa.getString shouldEqual None
  }


  it should "return a string that the DFA recognizes if the DFA's language is not empty 1" in {
    val δ: Transitions[Regex] = Map(ε -> Seq(!CharSet() -> ∅), ∅ → Seq(!CharSet() -> ∅))
    val dfa = Dfa(δ, ε, Set[Regex](ε))
    val s = dfa.getString.value
    dfa matches s shouldEqual true
  }

  it should "return a string that the DFA recognizes if the DFA's language is not empty 2" in {
    val all_re = α.*
    val dfa = DerivativeAnalysis.analyze(all_re)
    val s = dfa.getString.value
    dfa matches s shouldEqual true
  }

  it should "return a string that the DFA recognizes if the DFA's language is not empty 3" in {
    val a_d_set = Chars('a' -> 'd')
    val d_g_set = Chars('d' -> 'g')
    val intersect_re = Intersect(a_d_set, d_g_set)
    val dfa = DerivativeAnalysis.analyze(intersect_re)
    val s = dfa.getString.value
    dfa matches s shouldEqual true
  }

  it should "return a string that the DFA recognizes if the DFA's language is not empty 4" in {
    val charset_re = Chars('d' -> 'z')
    val dfa = DerivativeAnalysis.analyze(charset_re)
    val s = dfa.getString.value
    dfa matches s shouldEqual true
  }

  it should "return a string that the DFA recognizes if the DFA's language is not empty 5" in {
    val dfa = DerivativeAnalysis.analyze(ε)
    val s = dfa.getString.value
    dfa matches s shouldEqual true
  }

  it should "return a string that the DFA recognizes if the DFA's language is not empty 6" in {
    val a_d_set = Chars('a' -> 'd')
    val d_g_set = Chars('d' -> 'g')
    val concatenate_re = Concatenate(a_d_set, d_g_set)
    val dfa = DerivativeAnalysis.analyze(concatenate_re)
    val s = dfa.getString.value
    dfa matches s shouldEqual true
  }

  it should "return a string that the DFA recognizes if the DFA's language is not empty 7" in {
    val a = Chars('a')
    val kleenestar_re = KleeneStar(a)
    val dfa = DerivativeAnalysis.analyze(kleenestar_re)
    val s = dfa.getString.value
    dfa matches s shouldEqual true
  }

  it should "return a string that the DFA recognizes if the DFA's language is not empty 8" in {
    val kleenestar_of_empty_language_re = KleeneStar(∅)
    val dfa = DerivativeAnalysis.analyze(kleenestar_of_empty_language_re)
    val s = dfa.getString.value
    dfa matches s shouldEqual true
  }

  it should "return a string that the DFA recognizes if the DFA's language is not empty 9" in {
    val a_d_set = Chars('a' -> 'd')
    val e_g_set = Chars('e' -> 'g')
    val empty_intersect_re = Intersect(a_d_set, e_g_set)
    val kleenestar_of_an_empty_language_re = KleeneStar(empty_intersect_re)
    val dfa = DerivativeAnalysis.analyze(kleenestar_of_an_empty_language_re)
    val s = dfa.getString.value
    dfa matches s shouldEqual true
  }

  it should "return a string that the DFA recognizes if the DFA's language is not empty 10" in {
    val a = Chars('a')
    val a_d_set = Chars('a' -> 'd')
    val d_g_set = Chars('d' -> 'g')
    val concatenate_re1 = Concatenate(a_d_set, d_g_set)
    val concatenate_re2 = Concatenate(Chars('a'), Chars('d'))
    val complex_re = KleeneStar(Union(Intersect(concatenate_re1, concatenate_re2), a))
    val dfa = DerivativeAnalysis.analyze(complex_re)
    val s = dfa.getString.value
    dfa matches s shouldEqual true
  }

  it should "return a string that the looping non-empty DFA recognizes if the DFA's language is not empty 11" in {
    val a = Chars('a')
    val δ = Map(∅ -> Seq(!CharSet('a') -> ∅, CharSet('a') -> a), a -> Seq(!CharSet() -> a))
    val dfa = Dfa(δ, ∅, Set(a))
    val s = dfa.getString.value
    dfa matches s shouldEqual true
  }
}
