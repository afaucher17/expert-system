package expertSystem
{

  /**
   * The Data class represents a data.
   */
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

      /**
       * Checks all the rules concerning the data to find its most appropriate value.
       * If there is a contradiction in the rules, throws a ContradictoryRuleException
       **/
      def _checkRules(ret: Int): Int =
      {
        var current = ret
        for (rule <- rules)
        {
          val nret = rule.getValue(this)
          current = nret match
          {
            case -2 => if (current == -1) -2 else ret
            case -1 => current
            case (0 | 1) =>
            {
              if ((current >= 0) && (current != nret))
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
        if (current == -2) current = 0
        current
      }

      /**
       * Get the value of the data according to its rules.
       **/
      def getValue(): Int =
      {
        var ret : Int = value

        if ((ret == -1) && (rules.isEmpty))
          ret = 0
        else if (!visited)
          ret = _checkRules(ret)
        ret
      }
      def getName(): Char = name
      def setRules(vlu: List[Rule]) = rules = vlu
      def getRules() : List[Rule] = rules
      override def toString(): String = "(Data " +
      name + ": " + value + ")"
    }
}
