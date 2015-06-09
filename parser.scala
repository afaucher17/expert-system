package parser
{
  import tree.Rule
  import tree.Data
  import tree.Node
  import tree.Leaf
  import tree.RuleType
  import tree.LogicTree
  import tree.ContradictoryRuleException
  import lexer.Token
  import lexer.TokenType

  class Parser(list: List[Token])
  {
    val imply = "([^<=>]+)(=>|<=>)([^<=>]+)".r
    val parentheses = "^(\\()(.*)(\\))$".r
    val operators: Map[Char, (Int, Int) => Int] =
      Map('+' -> {(A: Int, B: Int)
      => if (A * B == -1 || (A == -1 && B == -1)) -1 else (A & B)},
      '|' -> {(A: Int, B: Int)
      => if (A + B <= -1) -1 else (A | B)},
      '^' -> {(A: Int, B: Int)
      => if (A == -1 || B == -1) -1 else (A ^ B)},
      '!' -> {(A: Int, B: Int)
      => if (B == -1) -1 else (1 ^ B)})

    var rules = List[Rule]()
    var datalist = List[Data]()

    def trimParentheses(line: String) : String =
    {
      if (parentheses.findFirstIn(line).isEmpty)
        line
      else
      {
        val ret =
          parentheses.findFirstMatchIn(line).map(_.group(2)).getOrElse("")
        trimParentheses(ret)
      }
    }

    def addLogicTree(pos: Array[Int], line: String) : LogicTree =
    {
      pos(1) match {
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
          val data = if (li.isEmpty)
          {
            val data = new Data(-1, line(0), false)
            datalist = data::datalist
            data
          }
          else li(0)
          new Leaf(data)
        }
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
      addLogicTree(pos, line)
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
        // println(Console.CYAN + l.getData + Console.RESET)
        val m = imply.findFirstMatchIn(l.getData)
        val ruletype = if (m.map(_.group(2)).getOrElse("") == "=>") RuleType.Implication else RuleType.IfAndOnlyIf
        val rule = new Rule(createTree(m.map(_.group(1)).getOrElse("")),
          createTree(m.map(_.group(3)).getOrElse("")), ruletype, l.getData, false)
        rules = rule::rules
      }
      for (rule <- rules)
      {
        val dl = rule.getDataList().groupBy(_.getName).map(_._2.head)
        for (data <- dl)
          data.setRules(rule::data.getRules())
      }
      /* for (dtt <- datalist)
      println(dtt.getName() + " =================> " + dtt.getRules()) */
    }

    def printValue(value: Int, name: Char)
    {
      println(Console.GREEN + "Value " + name + ": " + (value match
      {
        case 0 => "False"
        case 1 => "True"
        case -1 => "Undetermined"
      }) + "." + Console.RESET)
    }

    def splitQuery()
    {
      val qry =
        list.filter(x => x.getTokenType() == TokenType.Query)(0).getData()

      for (c <- qry)
      {
        c match {
          case ('?' | ' ' | '\t' | '\n' | '\r') => ""
          case _ =>
          {
            if (!datalist.exists(x => x.getName() == c))
              printValue(0, c)
            else
            {
              try
              {
                printValue(datalist.filter(x => x.getName() == c)(0).getValue(), c)
              }
              catch
              {
                case e: ContradictoryRuleException =>
                {
                  println(e.getMessage)
                  System.exit(1)
                }
              }
            }
          }
        }
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
              val data = new Data(1, ln, false)
              datalist = data::datalist
            }
          }
        }
      }
    }


    def parse()
    {
      splitFact()
      splitRule()
      splitQuery()
    }
  }
}
