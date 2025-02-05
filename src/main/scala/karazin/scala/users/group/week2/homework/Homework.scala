package karazin.scala.users.group.week2.homework

import scala.annotation.targetName
import scala.language.postfixOps
import scala.math.{abs, signum}

object Homework:
  class Rational(x: Int, y: Int):
    require(y > 0, "Denominator must be positive")

    def this(x: Int) = this(x, 1)

    val numer = x / g
    val denom = y / g

    // Defines an external name for a definition
    @targetName("less than")
    // Annotation on a method definition allows using the method as an infix operation
    infix def <(that: Rational): Boolean =
      this.numer * that.denom < that.numer * this.denom

    @targetName("less or equal")
    infix def <=(that: Rational): Boolean =
      this < that || this == that

    @targetName("greater than")
    infix def >(that: Rational): Boolean =
      !(this <= that)

    @targetName("greater or equal")
    infix def >=(that: Rational): Boolean =
      !(this < that)

    @targetName("addition")
    infix def +(that: Rational): Rational = {
      new Rational(that.numer * this.denom + that.denom * this.numer, that.denom * this.denom)
    }

    @targetName("negation")
    infix def unary_- : Rational = {
      new Rational(-this.numer, this.denom)
    }

    @targetName("substraction")
    infix def -(that: Rational): Rational = {
      new Rational(that.denom * this.numer - that.numer * this.denom, that.denom * this.denom)
    }

    @targetName("multiplication")
    infix def *(that: Rational): Rational = {
      new Rational(that.numer * this.numer, that.denom * this.denom)
    }

    @targetName("division")
    infix def /(that: Rational): Rational = {
      // if numer is 0 -> then return null
      if that.numer == 0
        then throw IllegalArgumentException("Division by zero is not allowed")
      else
        new Rational(that.numer * this.denom, that.denom * this.numer)
    }

    override def toString: String = s"${this.numer}/${this.denom}"

    private def gcd(a: Int, b: Int): Int =
      if b == 0 then a else gcd(b, a % b)

    private lazy val g = gcd(abs(x), y)


    override def equals(other: Any): Boolean = other match {
      case that: Rational =>
          (denom == that.denom) &&
          (that.isInstanceOf[Rational]) &&
          (g == that.g) &&
          (numer == that.numer)
      case _              => false
    }

    override def hashCode(): Int = {
      val prime = 41
      prime * (prime + numer.hashCode()) + denom.hashCode()
    }

  end Rational

end Homework