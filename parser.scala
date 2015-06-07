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
    val parentheses = "^(\\()(.*)(\\))$".r
    val operators: Map[Char, (Boolean, Boolean) => Boolean] =
      Map('+' -> {(A: Boolean, B: Boolean) => (A && B)},
      '|' -> {(A: Boolean, B: Boolean) => (A || B)},
      '^' -> {(A: Boolean, B: Boolean) => (A ^ B)},
      '!' -> {(A: Boolean, B: Boolean) => (!A)})

    var rules = List[Rule]()
    var datalist = List[Data]()

    def trimParentheses(line: String) : String =
    {
      if (parentheses.findFirstIn(line).isEmpty)
        line
      else
      {
        val ret = parentheses.findFirstMatchIn(line).map(_.group(2)).getOrElse("")
        trimParentheses(ret)
      }
    }

    def createTree(oline: String) : LogicTree =
    {
      var pos: Array[Int] = Array(-1, '?')
      var i = 0
      var ignore = 0

      val line = trimParentheses(oline)
      for (c <- line)
      {
        c match {
          case '(' => ignore += 1
          case ')' => ignore -= 1
          case ('+' | '|' | '^' | '!') =>
          {
            if (ignore == 0 && checkPriority(c, pos(1).asInstanceOf[Char]))
              pos = Array(i, c)
          }
          case _ => ""
        }
        i += 1
      }
      val ret: LogicTree = pos(1) match {
        case '!' =>
        {
          new Node(null, createTree(line.slice(pos(0) + 1, line.length)),
            pos(1).asInstanceOf[Char],
            operators(pos(1).asInstanceOf[Char]))
        }
        case ('+' | '|' | '^') =>
        {
          new Node(createTree(line.slice(0, pos(0))),
            createTree(line.slice(pos(0) + 1, line.length)),
            pos(1).asInstanceOf[Char],
            operators(pos(1).asInstanceOf[Char]))
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

    def checkPriority(current: Char, save: Char) : Boolean =
    {
      val priority: Map[Char, Int] = Map('^' -> 0, '|' -> 1,
        '+' -> 2, '!' -> 3, '?' -> 9001)

      (priority(current) <= priority(save))
    }

    def splitRule()
    {
     for (l <- list.filter(x => x.getTokenType() == TokenType.Rule))
     {
       println(Console.CYAN + l.getData + Console.RESET)
       val m = imply.findFirstMatchIn(l.getData)
       val rule = new Rule(createTree(m.map(_.group(1)).getOrElse("")), createTree(m.map(_.group(3)).getOrElse("")), l.getData, false)
       if (m.map(_.group(2)).getOrElse("") == "<=>")
       {
          val rule2 = new Rule(createTree(m.map(_.group(3)).getOrElse("")), createTree(m.map(_.group(1)).getOrElse("")), l.getData, false)
          rules = rule2::rules
       }
       rules = rule::rules
     }
      for (rule <- rules)
      {
        val dl = rule.getDataList().groupBy(_.getName).map(_._2.head)
        for (data <- dl)
          data.setRules(rule::data.getRules())
      }
      for (dtt <- datalist)
        println(dtt.getName() + " =================> " + dtt.getRules())
    }

    def splitQuery()
    {
      for (l <- list.filter(x => x.getTokenType() == TokenType.Query))
      {

      }
    }

    def splitFact()
    {
      val fact =
        list.filter(x => x.getTokenType() == TokenType.Fact)(0).getData()

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
