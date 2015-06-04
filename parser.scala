package parser
{
  import tree.Rule
  import tree.Data
  import tree.Node
  import tree.Leaf
  import tree.LogicTree
  import lexer.Token
  import lexer.TokenType

  class Parser(list: List[Token])
  {
    val imply = "([^<=>]+)(=>|<=>)([^<=>]+)".r
    val operators: Map[Any, (Boolean, Boolean) => Boolean] = Map('+' -> {(A: Boolean, B: Boolean) => (A && B)}, '|' -> {(A: Boolean, B: Boolean) => (A || B)}, '^' -> {(A: Boolean, B: Boolean) => (A ^ B)}, '!' -> {(A: Boolean, B: Boolean) => (!A)})

    var rules = List[Rule]()
    var datalist = List[Data]()

    def createTree(line: String) : LogicTree =
    {
      var pos: Array[Int] = Array(-1, '?')
      var i = 0
      for (c <- line)
      {
        c match {
          case ('+' | '|' | '^' | '!') =>
          {
            if (checkPriority(c, pos(1)))
              pos = Array(i, c)
          }
        }
        i += 1
      }
      val ret: LogicTree = pos(1) match {
        case '!' =>
        {
          new Node(null, createTree(line.slice(pos(0) + 1, line.length)), operators(pos(1)))
        }
        case ('+' | '|' | '^') =>
        {
          new Node(createTree(line.slice(0, pos(0) - 1)), createTree(line.slice(pos(0) + 1, line.length)), operators(pos(1)))
        }
        case '?' =>
        {
          val li = datalist.filter(x => x.getName() == line(0))
          val data = if (li.isEmpty) new Data(false, line(0)) else li(0)
          new Leaf(data)
        }
      }
      ret
    }

    def checkPriority(current: Char, save: Int) : Boolean =
    {
      val priority: Map[Any, Int] = Map('!' -> 0, '+' -> 1, '|' -> 2, '^' -> 3, '?' -> 9001)
      (priority(current) <= priority(save))
    }

    def splitRule()
    {
      /*var rule = new Rule()*/
     for (l <- list.filter(x => x.getTokenType() == TokenType.Rule))
     {
       val matches = imply.findAllMatchIn(l.getData)
       matches.foreach {
         m => println(createTree(m.group(1)))
       }
     }
    }

    def splitQuery()
    {
      for (l <- list.filter(x => x.getTokenType() == TokenType.Query))
      {

      }
    }

    def splitFact()
    {
      val fact = list.filter(x => x.getTokenType() == TokenType.Fact)(0).getData()
      for (ln <- fact)
      {
        ln match {
          case ('=' | ' ' | '\t' | '\n' | '\r') => ""
          case _ => {
            if (!datalist.exists(x => x.getName() == ln))
            {
              val data = new Data(true, ln)
              datalist = data::datalist
            }
          }
        }
      }
      println(datalist)
    }


    def parse()
    {
      splitFact()
      splitRule()
      splitQuery()
    }
  }
}
