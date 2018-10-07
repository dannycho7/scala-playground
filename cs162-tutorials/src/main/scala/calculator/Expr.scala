
package edu.ucsb.cs.cs162.tuts.calculator

// A mathematical expression.
sealed trait Expr

// A variable expression with a name.
final case class Var(name: String) extends Expr

// A number expression with a numeric value.
final case class Num(value: Double) extends Expr

// A unary operation expression (eg. -5 is UnOp("-", Num(5))).
final case class UnOp(op: String, value: Expr) extends Expr

// A binary operation expression (eg. 2+3 is BinOp("+", Num(2), Num(3))))
final case class BinOp(op: String, left: Expr, right: Expr) extends Expr

// The calculator object.
object Calculator {

  // Simplifies the head of the expression (should not simplify recursively!).  
  def simplifyHead(expr: Expr): Expr = {
    expr match {
      case UnOp("-", UnOp("-", n: Expr)) => n
      case BinOp("+", x: Expr, Num(0)) => x
      case BinOp("+", Num(0), x: Expr) => x
      case BinOp("*", Num(1), x: Expr) => x
      case BinOp("*", x: Expr, Num(1)) => x
      case BinOp("*", Num(0), x: Expr) => Num(0)
      case BinOp("*", x: Expr, Num(0)) => Num(0)
      case BinOp(op: String, Num(x), Num(y)) => {
        op match {
          case "+" => Num(x + y)
          case "-" => Num(x - y)
          case "*" => Num(x * y)
        }
      }
      case BinOp(op: String, x: Num, Var("DUP")) => BinOp(op, x, x)
      case BinOp(op: String, Var("DUP"), x: Num) => BinOp(op, x, x)              
      case BinOp("-", x: Expr, y: Expr) => if (x == y) Num(0) else expr
      case _ => expr
    }
  }
  
  // Evaluates the expression to a numeric value.
  def evaluate(expr: Expr): Double = {
    simplifyHead(expr) match {
      case BinOp(op: String, x: Expr, y: Expr) => {
        val left = evaluate(x)
        val right = evaluate(y)
        op match {
          case "+" => left + right
          case "-" => left - right
          case "*" => left * right
          case _ => 1
        }
      }
      case Var(x) => 1
      case Num(x) => x
      case UnOp("-", x: Expr) => -1 * evaluate(x)
      case UnOp(_, _) => 1
    }
  }
}
