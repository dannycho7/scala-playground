// Provides the Dfa class for deterministic finite automata.

package edu.ucsb.cs.cs162.dfa

import edu.ucsb.cs.cs162.range_set._
import edu.ucsb.cs.cs162.regex._

object `package` {
  type Transitions[State] = Map[State, Seq[(CharSet, State)]]
}

// The DFA. 'delta' is the state transition map; 'init' is the initial state;
// 'fin' is the set of final states. The DFA is assumed to have an explicit
// error state and the transitions are assumed to cover the complete range of
// possible characters.
case class Dfa[State](delta: Transitions[State], init: State, fin: Set[State]) {
  //----------------------------------------------------------------------------
  // Public API.
  //----------------------------------------------------------------------------

  // Returns true iff the given string is recognized by the DFA.
  def matches(str: String): Boolean =
    fin.contains(trace(init, str))

  // Returns a string that causes an arbitrary but non-looping path from the
  // init state to a final state, if such a path exists.
  def getString: Option[String] = {
    def bfs(visited: Set[State], todo: Set[(String, State)]): Option[String] = {
      if (todo.isEmpty) return None
      val (prefix, state) = todo.head
      if (visited.contains(state)) return bfs(visited, todo.tail)
      if (fin.contains(state)) return Some(prefix)
      val next_states = delta(state).map { case (charset, next_state) =>  (prefix + charset.minElement.get, next_state) }
      bfs(visited ++ Set(state), todo.tail ++ next_states)
    }

    bfs(Set[State](), Set(("", init)))
  }

  //----------------------------------------------------------------------------
  // Private details.
  //----------------------------------------------------------------------------

  // Compute the state reached by tracing the given string through the DFA.
  @annotation.tailrec
  private def trace(state: State, str: String): State =
    if (str.isEmpty) state
    else delta(state).find(_._1.contains(str.head)) match {
      case Some((_, next)) => trace(next, str.tail)
      case None => {
        assert(false, "should be unreachable")
        state
      }
    }
}
