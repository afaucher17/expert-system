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
        val evalCurrent = (acc: Int, rule: Rule) =>
        {
          val v = rule.getValue(this)
          v match
          {
            case -2 => acc
            case -1 => if (acc == -2) -1 else acc
            case (0 | 1) =>
              if ((acc >= 0) && (acc != v))
              {
                throw new ContradictoryRuleException("Contradiction found in the ruleset when resolving the value of "
                  + name + ".")
                -2
              }
              else
                v
          }
        }

        val current = rules.foldLeft(ret)(evalCurrent)

        if (current == -2)
          0
        else
          current
      }

      /**
       * Get the value of the data according to its rules.
       **/
      def getValue(): Int =
      {
        if ((value == -2) && (rules.isEmpty))
          0
        else if (!visited)
          _checkRules(value)
        else
          value
      }
      def getName(): Char = name
      def setRules(vlu: List[Rule]) = rules = vlu
      def getRules() : List[Rule] = rules
      override def toString(): String = "(Data " +
      name + ": " + value + ")"
    }
}
