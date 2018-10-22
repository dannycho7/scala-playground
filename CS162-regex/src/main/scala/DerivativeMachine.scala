// A virtual machine implementation of derivative-based matching.

package edu.ucsb.cs.cs162.regex.derivative

import edu.ucsb.cs.cs162.regex._

object `package` {
  // Programs for the DerivativeMachine.
  type Program = Seq[Instruction]

  // Pretty-print derivative virtual machine programs.
  def programToString(prog: Program): String = {
    val strs = for (inst <- prog) yield inst match {
      case `PushDerive` => "derive"
      case `PushConcatenate` => "concatenate"
      case `PushUnion` => "union"
      case `PushComplement` => "complement"
      case `PushIntersect` => "intersect"
      case `PushNullable` => "nullable"
      case PushRe(re) => "push " + re.toString
    }

    strs.mkString("\n")
  }
}

// Instructions for the virtual machine.
//
// - PushDerive: pop the top of the operand stack, compute its derivative w.r.t. the
//   machine's given char, then push the result back on the operand stack.
// - PushConcatentate: pop the top two elements of the operand stack and push
//   their concatenation back on.
// - PushUnion: pop the top two elements of the operand stack and push their
//   union back on.
// - PushComplement: pop the top of the operand stack, take its complement, and
//   push the result back on.
// - PushIntersect: pop the top two elements of the operand stack and push
//   their intersection back on.
// - PushNullable: pop the top of the operand stack, compute its nullability,
//   and push the result back on the operand stack.
// - PushRe(re): push re onto the top of the operand stack.
sealed abstract class Instruction
case object PushDerive extends Instruction
case object PushConcatenate extends Instruction
case object PushUnion extends Instruction
case object PushComplement extends Instruction
case object PushIntersect extends Instruction
case object PushNullable extends Instruction
case class PushRe(re: Regex) extends Instruction

class DerivativeMachine(re: Regex) {
  import Regex._

  //----------------------------------------------------------------------------
  // Public API.
  //----------------------------------------------------------------------------

  // Returns true iff 'str' is recognized by 're'.
  def eval(str: String): Boolean = if (str.length > 0) (new DerivativeMachine(derive(str(0)))).eval(str.tail) else re.nullable == ε
  
  // Returns the derivative of 're' w.r.t. 'char'.
  def derive(char: Char): Regex = run(Seq(re), Seq(PushDerive), char)

  //----------------------------------------------------------------------------
  // Private details.
  //----------------------------------------------------------------------------

  // Derives a regular expression from the top of 'operands' w.r.t. 'char'.
  @annotation.tailrec
  private def run(operands: Seq[Regex], program: Program, char: Char): Regex = {
    if (program.isEmpty) {
      assert(operands.size == 1)
      operands.head
    }
    else program.head match {
      case PushDerive => operands.head match {
        case `∅` | `ε` => run(Seq(∅) ++ operands.tail, program.tail, char)
        case Chars(chars) => run((if (chars.contains(char)) Seq(ε) else Seq(∅)) ++ operands.tail, program.tail, char)
        case Concatenate(re1, re2) => run(operands.tail, Seq(PushRe(re1), PushDerive, PushRe(re2), PushConcatenate, PushRe(re1), PushNullable, PushRe(re2), PushDerive, PushConcatenate, PushUnion) ++ program.tail, char)
        case Union(re1, re2) => run(operands.tail, Seq(PushRe(re1), PushDerive, PushRe(re2), PushDerive, PushUnion) ++ program.tail, char)
        case rek @ KleeneStar(re1) => run(operands.tail, Seq(PushRe(re1), PushDerive, PushRe(rek), PushConcatenate) ++ program.tail, char)
        case Complement(re1) => run(operands.tail, Seq(PushRe(re1), PushDerive, PushComplement) ++ program.tail, char)
        case Intersect(re1, re2) => run(operands.tail, Seq(PushRe(re1), PushDerive, PushRe(re2), PushDerive, PushIntersect) ++ program.tail, char)
      }
      case PushConcatenate => run(Seq(operands(0) ~ operands(1)) ++ operands.drop(2), program.tail, char)
      case PushUnion => run(Seq(operands(0) | operands(1)) ++ operands.drop(2), program.tail, char)
      case PushComplement => run(Seq(!operands(0)) ++ operands.tail, program.tail, char)
      case PushIntersect => run(Seq(operands(0) & operands(1)) ++ operands.drop(2), program.tail, char)
      case PushNullable => run(Seq(operands(0).nullable) ++ operands.tail, program.tail, char)
      case PushRe(re: Regex) => run(Seq(re) ++ operands, program.tail, char)
    }
  }
}
