package karazin.scala.users.group.week3

object Homework:

  // Peano numbers
  abstract class Nat:
    def isZero: Boolean
    def predecessor: Nat
    def successor: Nat = new Succ(this)

    infix def + (that: Nat): Nat

    infix def - (that: Nat): Nat

    // Optional task
    // def toInt: Int

    // Optional task
    // def fromInt(int: Int) = ???

    override def toString: String = s"Nat($predecessor)"

  type Zero = Zero.type
  object Zero extends Nat:
    def isZero: Boolean = true
    def predecessor: Nat = throw new Exception("0 doesn't have a predecessor")

    infix def +(that: Nat): Nat = that

    infix def -(that: Nat): Nat =
      // parameters validation
      require(that.isZero)
      this

    // Optional task
    // def toInt: Int = ???

    override def toString: String = "Zero"
    override def equals(obj: Any): Boolean = obj match {
      case zero: Zero => true
      case _ => false
    }

  class Succ(n: Nat) extends Nat:
    def isZero: Boolean = false
    def predecessor: Nat = n

    infix def +(that: Nat): Nat = new Succ(that + n)

    // I can use if-statement, but I think that is better to use match.
    infix def -(that: Nat): Nat = that.isZero match {
      case true => this
      case _ => n - that.predecessor
    }

    // Optional task
    // def toInt: Int = ???

    override def equals(obj: Any): Boolean = obj match {
      case n: Nat => this.predecessor == n.predecessor
      case zero: Zero => false
      case _ => false
    }