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
      def getValue(data: Data): Int =
      {
        var res = -1
        if (!visited)
        {
          println(line)
          visited = true
          if (getLeftValue() == 1)
          {
            println("????")
            data.setInitialValue(false)
            var ret1 = rhs.getValue()
            ret1 = if (ret1 == -1) 1 else ret1
            data.setInitialValue(true)
            var ret2 = rhs.getValue()
            ret2 = if (ret1 == -1) 1 else ret2
            println(ret1 + " et " + ret2)
            res = (ret1 + ret2) match
            {
              case 2 => -1
              case 0 => throw new ContradictoryRuleException("Error: " + line + " is contradictory")
              case 1 => if (ret1 == 1) 0 else 1
            }
          }
          visited = false
        }
        res
      }
      def getDataList(): List[Data] = rhs.getDataList()
      override def toString(): String = Console.YELLOW + "(Rule (" + line + ") : " + lhs + " " + rhs + ")" + Console.RESET
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
          println("wewgwegewg: " + value)
          value
        }
        def getDataList(): List[Data] =
        {
          if (lhs == null)
            rhs.getDataList()
          else
            lhs.getDataList():::rhs.getDataList()
        }
        override def toString(): String = Console.BLUE + "(Tree " + Console.RED + opname + Console.BLUE + ": " + lhs + " % " + rhs + ")" + Console.RESET
      }

      class Leaf(data: Data) extends LogicTree
      {
        def getValue(): Int =
          data.getValue()
        def getDataList(): List[Data] = data::Nil
        override def toString(): String = Console.GREEN + "(Leaf " + Console.CYAN + data.getName() + Console.GREEN + ": " + data.getValue() + ")" + Console.RESET
      }

      class Data(
        var rules: List[Rule],
        var value: Boolean,
        name: Char)
        {
          def this(value: Boolean, name: Char) = this(List[Rule](), value, name)
          def setInitialValue(vl: Boolean) = this.value = vl
          def getInitialValue(): Boolean = value
          def getValue(): Int =
          {
            println("Hello my name is " + name)
            var ret : Int = if (value) 1 else -1
            if (ret == -1 & rules.isEmpty)
              ret = 0
            else
            {
              for (rule <- rules)
              {
                ret = rule.getValue(this) match
                {
                  case -1 => ret
                  case 0 =>
                    if (ret == 1)
                    {
                      throw new ContradictoryRuleException(Console.RED + "Error: Contradiction in the rules." + Console.RESET)
                      -2
                    }
                    else 0
                  case 1 =>
                    if (ret == 0)
                    {
                      throw new ContradictoryRuleException(Console.RED + "Error: Contradiction in the rules." + Console.RESET)
                      -2
                    }
                    else 1
                }
              }
            }
            println(name + " Value found : " + ret)
            ret
          }
          def getName(): Char = name
          def setRules(vlu: List[Rule]) = rules = vlu
          def getRules() : List[Rule] = rules
          override def toString(): String = "(Data "
          + name + ": " + value + ")"
        }

}
