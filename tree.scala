package tree
{
  class ContradictoryRuleException(msg: String) extends RuntimeException(msg)

  class Rule(
    lhs: LogicTree,
    rhs: LogicTree,
    line: String,
    visited: Boolean)
    {
      def getLeftValue(): Boolean = lhs.getValue()
      def getDataList(): List[Data] = rhs.getDataList()
      override def toString(): String = Console.YELLOW + "(Rule (" + line + ") : " + lhs + " " + rhs + ")" + Console.RESET
    }

    trait LogicTree {
      def getValue(): Boolean
      def getDataList(): List[Data]
    }

    class Node(
      lhs: LogicTree,
      rhs: LogicTree,
      opname: Char,
      op: (Int, Int) => Int) extends LogicTree
      {
        def getValue(): Boolean = op(lhs.getValue(), rhs.getValue())
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
        def getValue(): Boolean = data.getValue()
        def getDataList(): List[Data] = data::Nil
        override def toString(): String = Console.GREEN + "(Leaf " + Console.CYAN + data.getName() + Console.GREEN + ": " + data.getValue() + ")" + Console.RESET
      }

      class Data(
        var rules: List[Rule],
        value: Boolean,
        name: Char)
        {
          def this(value: Boolean, name: Char) = this(List[Rule](), value, name)
          def getInitialValue(): Boolean = value
          def getValue(): Int =
          {
            var ret = if (ret) true else -1
            if (rules.isEmpty())
              ret = false
            else
            {
              for (rule <- rules)
              {
                ret = rule.getValue() match
                {
                  case -1 => ret
                  case false => if (ret == true)
                    {
                      throw new ContradictoryRuleException
                      (Console.RED
                        + "Error: Contradiction in the rules."
                        + Console.RESET)
                      -2
                    }
                    else false
                  case true => true
                }
              }
            }
            ret
          }
          def getName(): Char = name
          def setRules(vlu: List[Rule]) = rules = vlu
          def getRules() : List[Rule] = rules
          override def toString(): String = "(Data " + name + ": " + value + ")"
        }

}
