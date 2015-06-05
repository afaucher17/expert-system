package tree
{

  class Rule(
    lhs: LogicTree,
    rhs: LogicTree,
    visited: Boolean)
    {
      def getLeftValue(): Boolean = lhs.getValue()
    }

    trait LogicTree {
      def getValue(): Boolean
    }

    class Node(
      lhs: LogicTree,
      rhs: LogicTree,
      op: (Boolean, Boolean) => Boolean) extends LogicTree
      {
        def getValue(): Boolean = op(lhs.getValue(), rhs.getValue())
      }

      class Leaf(data: Data) extends LogicTree
      {
        def getValue(): Boolean = data.getValue()
      }

      class Data(
        rules: List[Rule],
        value: Boolean,
        name: Char)
        {
          def this(value: Boolean, name: Char) = this(List[Rule](), value, name)
          def getValue(): Boolean = value
          def getName(): Char = name
          override def toString(): String = "(Data " + name + ": " + value + ")"
        }

}
