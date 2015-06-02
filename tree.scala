package tree
{

  class Rule(
    lhs: LogicTree,
    rhs: LogicTree,
    visited: Boolean)
    {
      def getLeftValue(): Int = lhs.getValue()
    }

    trait LogicTree {
      def getValue(): Int
    }

    class Node(
      lhs: LogicTree,
      rhs: LogicTree,
      op: (Int, Int) => Int) extends LogicTree
      {
        def getValue(): Int = op(lhs.getValue(), rhs.getValue())
      }

      class Leaf(data: Data) extends LogicTree
      {
        def getValue(): Int = data.getValue()
      }

      class Data(
        rules: List[Rule],
        value: Boolean)
        {
          def getValue(): Int = 1 /*change this BS*/
        }

}
