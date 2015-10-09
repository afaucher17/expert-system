package expertSystem
{
  import scala.util.{Try, Success, Failure}

  class ContradictoryRuleException(msg: String) extends RuntimeException(msg)

  object RuleType extends Enumeration {
    type RuleType = Value
    val IfAndOnlyIf, Implication = Value
  }

  /**
   * The class Rule describe a rule in the file.
   * Lhs is the antecedent and rhs is the consequent.
   * The ruletype defines the type of the rule (Implication or IfAndOnlyIf).
   * The line is the rule in string form.
   * Visitors prevents the program from looping infinitely by allowing a data to check its rule only once.
   **/
  class Rule(
    lhs: LogicTree,
    rhs: LogicTree,
    ruletype: RuleType.Value,
    line: String,
    var visitors: List[(Char, Int)])
    {
      def getLeftValue(): Int = lhs.getValue()
      def getLine(): String = line

      /**
       * Find the right value of the data according to the return values of the consequent.
       * If none of the values found are right, a ContradictoryRuleException is thrown.
       * If no conclusion can be reached, the value returned is -1
       **/
      private def _solver(data: Data, lvalue: Int, ret1: Int, ret2: Int): Int =
      {
        val test1: Int = lvalue + lvalue
        val test2: Int = (lvalue ^ 1) + (lvalue ^ 1)
        val res = (ret1 + ret2) match
        {
          case (`test1`) => -1
          case (`test2`) =>
          {
            removeVisitor(data)
            throw new ContradictoryRuleException(Console.RED +
              "Error: Rule " + Console.RESET + "< " + line + " >" +
              Console.RED + " is contradictory" + Console.RESET)
          }
          case 1 => if (ret1 == lvalue) 0 else 1
        }
        res
      }

      /**
       * Tests each possible value for the current variable (0 or 1) and checks
       * the resulting value of the consequent.
       **/
      private def _testVariable(data: Data, lvalue: Int): Int =
      {
        data.setVisited(true)
        val base = data.getInitialValue()
        data.setInitialValue(0)

        val ret1 = Try(rhs.getValue()) match {
          case Failure(e) => { lvalue ^ 1 }
          case Success(v) => if (v == -1) lvalue else v
        }

        data.setInitialValue(1)

        val ret2 = Try(rhs.getValue()) match {
          case Failure(e) => { lvalue ^ 1 }
          case Success(v) => if (v == -1) lvalue else v
        }

        data.setInitialValue(base)
        data.setVisited(false)
        val res = _solver(data, lvalue, ret1, ret2)
        res
      }

      /**
       * Add the given data to the visitors list
       **/
      def addVisitor(data: Data) =
      {
          if (visitors.filter(x => x._1 == data.getName()).isEmpty)
            visitors = (data.getName(),1)::visitors
          else
          {
            val save = visitors.filter(x => x._1 == data.getName()).head
            visitors = (save._1, save._2 + 1)::visitors.filter(x => x._1 != data.getName())
          }
      }

      def removeVisitor(data: Data) =
      {
        val save = visitors.filter(x => x._1 == data.getName()).head
        if (save._2 - 1 <= 0)
          visitors = visitors.filter(x => x._1 != data.getName())
        else
          visitors = (save._1, save._2 - 1)::visitors.filter(x => x._1 != data.getName())
      }

      /**
       * Get the value of a given data according to the rule.
       **/
      def getValue(data: Data): Int =
      {
        if (visitors.filter(x => x._1 == data.getName()).isEmpty ||
          !(visitors.filter(x => x._1 == data.getName() && x._2 <= 8).isEmpty))
        {
          addVisitor(data)
          val lvalue: Int = getLeftValue()
          if (((ruletype == RuleType.Implication) && (lvalue == 1)) ||
            ((ruletype == RuleType.IfAndOnlyIf) && (lvalue != -1)))
          {
            val res = _testVariable(data, lvalue)
            removeVisitor(data)
            res
          }
          else
          {
            removeVisitor(data)
            -2
          }
        }
        else
          -2
      }

      def getDataList(): List[Data] = rhs.getDataList()
      override def toString(): String = Console.YELLOW + "(Rule (" + line +
      ") : " + lhs + " " + rhs + ")" + Console.RESET
    }

    trait LogicTree {
      def getValue(): Int
      def getDataList(): List[Data]
    }

    class Node(
      lhs: Option[LogicTree],
      rhs: LogicTree,
      opname: Char,
      op: (Int, Int) => Int) extends LogicTree
      {
        def getValue(): Int =
          lhs match
          {
            case None => op(1, rhs.getValue())
            case Some(v) => op(v.getValue(), rhs.getValue())
          }

        def getDataList(): List[Data] =
          lhs match
          {
            case None => rhs.getDataList()
            case Some(v) => v.getDataList():::rhs.getDataList()
          }

        override def toString(): String = Console.BLUE + "(Tree " +
        Console.RED + opname + Console.BLUE +
        ": " + lhs + " % " + rhs + ")" + Console.RESET
      }

    class Leaf(data: Data) extends LogicTree
    {
      def getValue(): Int = data.getValue()

      def getDataList(): List[Data] = data::Nil

      override def toString(): String = Console.GREEN + "(Leaf " +
        Console.CYAN + data.getName() + Console.GREEN +
        ": " + data.getInitialValue() + ")" + Console.RESET
    }
}
