package tree
{
  import data.Data

  class ContradictoryRuleException(msg: String) extends RuntimeException(msg)

  object RuleType extends Enumeration {
    type RuleType = Value
    val IfAndOnlyIf, Implication = Value
  }

  /**
   * The class Rule describe a rule in the file. Lhs is the antecedent and rhs is the consequent. The ruletype defines the type of the rule (Implication or IfAndOnlyIf). The line is the rule in string form. Visitors prevents the program from looping infinitely by allowing a data to check its rule only once.
   **/
  class Rule(
    lhs: LogicTree,
    rhs: LogicTree,
    ruletype: RuleType.Value,
    line: String,
    var visitors: List[Data])
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
            visitors = visitors.filter(x => x.getName() != data.getName())
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
        // We set the visited boolean at true to prevent the data from checking itself
        // while we make an hypothesis.
        data.setVisited(true)
        // Saving the initial value to prevent data loss
        val base = data.getInitialValue()
        data.setInitialValue(0)
        // If an exception if thrown while checking the value of the consequent
        // it is not an error, but we have to assume ret1 is not the value
        // we are expecting (lvalue) but its contrary (lvalue ^ 1)
        var ret1 = try rhs.getValue() catch { case e: ContradictoryRuleException =>
        lvalue ^ 1}
        ret1 = if (ret1 == -1) lvalue else ret1
        data.setInitialValue(1)
        var ret2 = try rhs.getValue() catch { case e: ContradictoryRuleException =>
        lvalue ^ 1 }
        data.setInitialValue(base)
        // The hypothesis done, we set the visited boolean at false.
        data.setVisited(false)
        ret2 = if (ret2 == -1) lvalue else ret2
        val res = _solver(data, lvalue, ret1, ret2)
        res
      }

      /**
       * Get the value of a given data according to the rule.
       **/
      def getValue(data: Data): Int =
      {
        var res = -1
        if (visitors.filter(x => x.getName() == data.getName()).isEmpty)
        {
          visitors = data::visitors
          res = -2
          lazy val lvalue: Int = getLeftValue()
          if (((ruletype == RuleType.Implication) && (lvalue == 1)) ||
            ((ruletype == RuleType.IfAndOnlyIf) && (lvalue != -1)))
          res = _testVariable(data, lvalue)
          visitors = visitors.filter(x => x.getName() != data.getName())
        }
        res
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
      lhs: LogicTree,
      rhs: LogicTree,
      opname: Char,
      op: (Int, Int) => Int) extends LogicTree
      {
        def getValue(): Int =
        {
          val value = lhs match
          {
            case null => op(1, rhs.getValue())
            case _ => op(lhs.getValue(), rhs.getValue())
          }
          value
        }
        def getDataList(): List[Data] =
        {
          if (lhs == null)
            rhs.getDataList()
          else
            lhs.getDataList():::rhs.getDataList()
        }
        override def toString(): String = Console.BLUE + "(Tree " +
        Console.RED + opname + Console.BLUE +
        ": " + lhs + " % " + rhs + ")" + Console.RESET
      }

      class Leaf(data: Data) extends LogicTree
      {
        def getValue(): Int =
          data.getValue()
        def getDataList(): List[Data] = data::Nil
        override def toString(): String = Console.GREEN + "(Leaf " +
        Console.CYAN + data.getName() + Console.GREEN +
        ": " + data.getValue() + ")" + Console.RESET
      }
}
