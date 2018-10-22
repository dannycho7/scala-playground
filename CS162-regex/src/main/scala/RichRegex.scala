// This file uses the 'pimp my library' pattern to add builder methods and regex
// operations to Regex.

package edu.ucsb.cs.cs162.regex

object `package` {
  import Regex._

  // Convenient methods to build regular expressions.
  implicit class RegexBuilder(val re: Regex) extends AnyVal {
    //----------------------------------------------------------------------------
    // Public API.
    //----------------------------------------------------------------------------

    // Concatenate 're' with 'other', simplifying if possible (assumes that 're'
    // and 'other' have already been simplified).
    def ~(other: Regex): Regex = re match {
      case `∅` => ∅
      case `ε` => other
      case _ => other match {
        case `∅` => ∅
        case `ε` => re
        case _ => Concatenate(re, other)
      }
    }

    // Union 're' with 'other', simplifying if possible (assumes that 're' and
    // 'other' have already been simplified).
    def |(other: Regex): Regex = re match {
      case `∅` => other
      case Chars(x) if (other.isInstanceOf[Chars]) => Chars(x ++ other.asInstanceOf[Chars].chars)
      case _:KleeneStar if (other == `ε`) => re
      case `ε` if (other.isInstanceOf[KleeneStar]) => other
      case KleeneStar(`α`) => re
      case _ => other match {
        case `∅` => re
        case KleeneStar(`α`) => other
        case `re` => re
        case _ => Union(re, other)
      }
    }

    // Apply the Kleene star to 're', simplifying if possible (assumes that 're'
    // has already been simplified).
    def * : Regex = re match {
      case `∅` => ε
      case `ε` => ε
      case KleeneStar(x) => re
      case _ => KleeneStar(re)
    }

    // Complement 're', simplifying if possible (assumes that 're' has already
    // been simplified).
    def unary_! : Regex = re match {
      case `∅` => KleeneStar(`α`)
      case `ε` => α.+
      case Complement(x) => x // !!x
      case _ => Complement(re)
    }

    // Intersect 're' with 'other', simplifying if possible (assumes that 're'
    // and 'other' have already been simplified).
    def &(other: Regex): Regex = re match {
      case `∅` => ∅
      case Chars(x) if (other.isInstanceOf[Chars]) => Chars(x & other.asInstanceOf[Chars].chars)
      case KleeneStar(`α`) => other
      case _ => other match {
        case `∅` => ∅
        case `re` => re
        case KleeneStar(`α`) => re
        case _ => Intersect(re, other)
      }
    }

    // Shorthand for 1 or more repetitions of re regex.
    def + : Regex = Concatenate(re, KleeneStar(re))

    // Shorthand for 0 or 1 instances of re regex.
    def ? : Regex = Union(`ε`, re)

    // Shorthand for exactly 'num' repetitions of re regex.
    // Check if num > 0 ?
    def ^(num: Int): Regex = if (num > 1) Concatenate(re ^ (num - 1), re) else if (num == 1) re else `ε`

    // Shorthand for at least 'min' repetitions of re regex.
    def >=(min: Int): Regex = Concatenate(re ^ min, KleeneStar(re))

    // Shorthand for at most 'max' repetitions of re regex.
    def <=(max: Int): Regex = if (max > 0) Union(re <= (max - 1), re ^ max) else `ε`

    // Shorthand for at least 'min' but at most 'max' repetitions of re regex.
    def <>(min: Int, max: Int): Regex = Intersect(re >= min, re <= max)
  }

  // Add convenient methods to String for building simple regular expressions.
  implicit class StringToRegex(val str: String) extends AnyVal {
    // Builds the concatenation of each character in 'str' in sequence. Example:
    // "abc".concatenate == Chars('a') ~ Chars('b') ~ Chars('c').
    def concatenate: Regex =
      str.foldLeft(ε: Regex)((acc, char) => acc ~ Chars(char))

    // Builds a charset containing each character in 'str'. Example:
    // "abc".charset == Chars('a', 'b', 'c').
    def charset: Regex =
      if (str.isEmpty) ε else Chars(str.toSeq: _*)
  }

  // Operations on regular expressions.
  implicit class RegexOps(val re: Regex) extends AnyVal {
    // Returns ε if 're' is nullable, otherwise returns ∅.
    def nullable: Regex = re match {
      case `ε` | _: KleeneStar => ε
      case `∅` | _: Chars => ∅
      case Concatenate(re1, re2) => re1.nullable ~ re2.nullable
      case Union(re1, re2) => re1.nullable | re2.nullable
      case Complement(re1) => if (re1.nullable == ε) ∅ else ε
      case Intersect(re1, re2) => re1.nullable & re2.nullable
    }
  }
}