// Provides a derivative-based static analysis of regular expressions that
// yields a DFA describing the language recognized by an expression.

package edu.ucsb.cs.cs162.regex.derivative

import edu.ucsb.cs.cs162.dfa._
import edu.ucsb.cs.cs162.range_set._
import edu.ucsb.cs.cs162.regex._

object DerivativeAnalysis {
  import Derive._
  import Regex._

  // Statically analyzes 're' using derivatives in order to compute the DFA of
  // the language recognized by 're'. The resulting DFA has an explicit error
  // state and is approximately minimal.
  def analyze(re: Regex): Dfa[Regex] = {
    val(states, transitions) = computeDfa(Set(re), Set(), Map[Regex, Seq[(CharSet, Regex)]]())
    Dfa[Regex](transitions, re, states.filter(_.nullable == ε))
  }

  // Return the set of all possible derivatives of 're'.
  def derivativeClosure(re: Regex): Set[Regex] =
    computeDfa(todo = Set(re), visitedStates = Set(), transitions = Map())._1


  //----------------------------------------------------------------------------
  // Private details.
  //----------------------------------------------------------------------------

  // Compute the transitions and set of reachable states (i.e., Regexes) for all
  // Regexes in 'todo'.
  @annotation.tailrec
  private def computeDfa(todo: Set[Regex], visitedStates: Set[Regex],
    transitions: Transitions[Regex]) : (Set[Regex], Transitions[Regex]) = {
    if (todo.isEmpty) {
      (visitedStates, transitions)
    } else {
      val(destination_states, todo_transition) = computeNext(todo.head)
      val visited = visitedStates ++ Set(todo.head)
      val added_todos = destination_states -- visited 
      computeDfa(added_todos ++ todo.tail, visited, transitions ++ todo_transition)
    }
  }
  
  def pairwiseIntersect(re1: Set[CharSet], re2: Set[CharSet]): Set[CharSet] = re1.flatMap(x => re2.map(y => x & y))

  // Computes the over-approximate partitions of a given regex
  def C(re: Regex): Set[CharSet] = (re match {
    case `∅` => Set(α.chars)
    case `ε` => Set(α.chars)
    case Chars(chars) => Set(chars, !chars)
    case KleeneStar(x) => C(x)
    case Complement(x) => C(x)
    case Union(x, y) => pairwiseIntersect(C(x), C(y))
    case Intersect(x, y) => pairwiseIntersect(C(x), C(y))
    case Concatenate(x, y) => if (x.nullable == ∅) C(x) else pairwiseIntersect(C(x), C(y))
    case _ => Set(α.chars)
  }).filter(!_.isEmpty)

  // Compute the transitions and destination states from the given regex.
  def computeNext(state: Regex): (Set[Regex], Transitions[Regex]) = {
    val partitions = C(state)
    val destinations = partitions.map(chars => (chars, (new DerivativeMachine(state).derive(chars.minElement.get))))
    val destination_states = destinations.map(_._2)
    (destination_states, Map[Regex, Seq[(CharSet, Regex)]](state -> destinations.toSeq))
  }
}
