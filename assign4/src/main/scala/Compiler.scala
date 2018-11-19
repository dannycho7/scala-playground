// This is the compiler that translates regex abstract syntax trees into regular
// expression matching virtual machine programs.

package edu.ucsb.cs.cs162.regex.vm.compiler

import edu.ucsb.cs.cs162.regex._
import edu.ucsb.cs.cs162.regex.vm._
import edu.ucsb.cs.cs162.regex.Regex._

object Compiler {
  // Return a virtual machine program that implements the given regex.
  def compile(re: Regex): Program = compileImmediate(re) ++ IndexedSeq(Accept)

  // Returns a virtual machine program of the immediate regex that's a part of a larger regex
  private def compileImmediate(re: Regex): Program = re match {
  	case `∅` => IndexedSeq(Reject)
  	case `ε` => IndexedSeq(PushEmpty)
  	case Chars(x) => IndexedSeq(MatchSet(x), PushChar)
  	case Concatenate(x, y) => compileImmediate(x) ++ compileImmediate(y) ++ IndexedSeq(PushConcat)
  	case Union(x, y) => {
  		val compiled_x = compileImmediate(x)
  		val compiled_y = compileImmediate(y)
  		IndexedSeq(Fork(1, compiled_x.size + 3)) ++ compiled_x ++ IndexedSeq(PushLeft, Jump(compiled_y.size + 2)) ++ compiled_y ++ IndexedSeq(PushRight)
  	}
  	case KleeneStar(x) => {
  		val compiled = compileImmediate(x)
  		val check_progress = if (x.nullable == ε) IndexedSeq(`CheckProgress`) else IndexedSeq()
  		val f2_offset = compiled.length + 3
  		val j_offset = -1 * (compiled.length + 2 + check_progress.size)
  		IndexedSeq(InitStar) ++ check_progress ++ IndexedSeq(Fork(1, f2_offset)) ++ compiled ++ IndexedSeq(`PushStar`) ++ IndexedSeq(Jump(j_offset))
  	}
  	case Capture(name, re) => {
  		val compiled = compileImmediate(re)
  		compiled ++ IndexedSeq(PushCapture(name))
  	}
  	case _ => throw new Exception("Reached unexpected regex")
  }
}
