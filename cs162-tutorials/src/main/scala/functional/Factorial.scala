
package edu.ucsb.cs.cs162.tuts.functional

// A number of list problems.
object Factorial {
  import scala.annotation.tailrec
  import scala.math.BigInt

  // Find the factorial of `n`.
  // The `apply` method can be called by using the `()` operator,
  //  as if `Factorial` was a function itself.
  // Example: Factorial(5) is actually Factorial.apply(5)
  def apply(n: BigInt): BigInt = {

    // Tail-recursively iterates through the factorial calculation,
    //  saving the intermediate results in the accumulator `acc`.
    @tailrec
    def iter(count: BigInt, acc: BigInt): BigInt = if (count > 1) iter(count - 1, acc * count) else acc

    iter(n, 1)
  }
}