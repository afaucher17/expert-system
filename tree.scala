package tree
{
  class ContradictoryRuleException(msg: String) extends RuntimeException(msg)

  object RuleType extends Enumeration {
    type RuleType = Value
    val IfAndOnlyIf, Implication = Value
  }


  class Rule(
    lhs: LogicTree,
    rhs: LogicTree,
    ruletype: RuleType.Value,
    line: String,
    var visited: Boolean)
    {
      def getLeftValue(): Int = lhs.getValue()
      def getLine(): String = line
      def getValue(data: Data): Int =
      {
        var res = -1
        if (!visited)
        {
          visited = true
          val lvalue: Int = getLeftValue()
          if (((lvalue == 1) && (ruletype == RuleType.Implication)) ||
            ((ruletype == RuleType.IfAndOnlyIf) && (lvalue != -1)))
          {
            data.setVisited(true)
            val base = data.getInitialValue()
            data.setInitialValue(0)
            var ret1 = try rhs.getValue() catch
            {
              case e: ContradictoryRuleException => ()
              lvalue ^ 1
            }
            ret1 = if (ret1 == -1) lvalue else ret1
            data.setInitialValue(1)
            var ret2 = try rhs.getValue() catch
            {
              case e: ContradictoryRuleException => ()
              lvalue ^ 1
            }
            data.setInitialValue(base)
            data.setVisited(false)
            ret2 = if (ret2 == -1) lvalue else ret2
            val test1: Int = lvalue + lvalue
            val test2: Int = (lvalue ^ 1) + (lvalue ^ 1)
            // println("rule: " + line + " looking for " + data.getName() + " " + ret1 + " and " + ret2)
            res = (ret1 + ret2) match
            {
              case (`test1`) => -1
              case (`test2`) =>
              {
                visited = false
                throw new ContradictoryRuleException(Console.RED +
                "Error: Rule " + Console.RESET + "< " + line + " >" +
                Console.RED + " is contradictory" + Console.RESET)
              }
              case 1 => if (ret1 == lvalue) 0 else 1
            }
          }
          visited = false
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

      class Data(
        var rules: List[Rule],
        var value: Int,
        name: Char,
        var visited: Boolean)
        {
          def this(value: Int, name: Char, visited: Boolean) = this(List[Rule](), value, name, visited)
          def setInitialValue(vl: Int) = this.value = vl
          def getInitialValue(): Int = this.value
          def setVisited(vl: Boolean) = this.visited = vl
          def getValue(): Int =
          {
            var ret : Int = value

            if ((ret == -1) && (rules.isEmpty))
              ret = 0
            else if (!visited)
            {
              for (rule <- rules)
              {
                val nret = rule.getValue(this)
                ret = nret match
                {
                  case -1 => ret
                  case (0 | 1) =>
                  {
                    if ((ret != -1) && (ret != nret))
                    {
                      throw new ContradictoryRuleException(Console.RED +
                        "Error: Contradiction found in the ruleset when resolving the value of " +
                        name + "." + Console.RESET)
                      -2
                    }
                    else
                     nret
                  }
                }
              }
            }
            ret
          }
          def getName(): Char = name
          def setRules(vlu: List[Rule]) = rules = vlu
          def getRules() : List[Rule] = rules
          override def toString(): String = "(Data " +
          name + ": " + value + ")"
        }

}
