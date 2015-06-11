package expertSystem
{
  /**
   * Parser class, used for parsing operations
   **/
  class Parser(list: List[Token])
  {
    // The regex used in the parser
    val imply = "([^<=>]+)(=>|<=>)([^<=>]+)".r
    val parentheses = "^(\\()(.*)(\\))$".r

    // Operator functions for the rule tree
    val operators: Map[Char, (Int, Int) => Int] =
      Map('+' -> {(A: Int, B: Int)
      => if (A * B == -1 || (A == -1 && B == -1)) -1 else (A & B)},
      '|' -> {(A: Int, B: Int)
      => if (A + B <= -1) -1 else (A | B)},
      '^' -> {(A: Int, B: Int)
      => if (A == -1 || B == -1) -1 else (A ^ B)},
      '!' -> {(A: Int, B: Int)
      => if (B == -1) -1 else (1 ^ B)})

    // The rule list and the data list
    var rules = List[Rule]()
    var datalist = List[Data]()

    // Removes the parentheses if they are encapsulating the whole expression
    private def _trimParentheses(line: String) : String =
    {
      if (parentheses.findFirstIn(line).isEmpty)
        line
      else
      {
        val ret =
          parentheses.findFirstMatchIn(line).map(_.group(2)).getOrElse("")
        _trimParentheses(ret)
      }
    }

    // Creates a LogicTree (either a Node or a Leaf)
    private def _addLogicTree(pos: Array[Int], line: String) : LogicTree =
    {
      pos(1) match {
        case '!' =>
        {
          new Node(null, _createTree(line.slice(pos(0) + 1, line.length)),
            pos(1).asInstanceOf[Char],
            operators(pos(1).asInstanceOf[Char]))
        }
        case ('+' | '|' | '^') =>
        {
          new Node(_createTree(line.slice(0, pos(0))),
            _createTree(line.slice(pos(0) + 1, line.length)),
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

    private def _createTree(oline: String) : LogicTree =
    {
      var pos: Array[Int] = Array(-1, '?')
      var i = 0
      var ignore = 0

      val line = _trimParentheses(oline)
      for (c <- line)
      {
        c match {
          case '(' => ignore += 1
            case ')' => ignore -= 1
          case ('+' | '|' | '^' | '!') =>
          {
            if (ignore == 0 && _checkPriority(c, pos(1).asInstanceOf[Char]))
              pos = Array(i, c)
          }
          case _ => ""
        }
        i += 1
      }
      _addLogicTree(pos, line)
    }

    // Checks the priority of the operators in order to create the tree in the right order.
    private def _checkPriority(current: Char, save: Char) : Boolean =
    {
      val priority: Map[Char, Int] = Map('^' -> 0, '|' -> 1,
        '+' -> 2, '!' -> 3, '?' -> 9001)

      (priority(current) <= priority(save))
    }

    // Splits the rules and creates a Rule instance for each of them
    private def _splitRule()
    {
      for (l <- list.filter(x => x.getTokenType() == TokenType.Rule))
      {
        val m = imply.findFirstMatchIn(l.getData)
        val ruletype = if (m.map(_.group(2)).getOrElse("") == "=>") RuleType.Implication   else RuleType.IfAndOnlyIf
        val rule = new Rule(_createTree(m.map(_.group(1)).getOrElse("")),
          _createTree(m.map(_.group(3)).getOrElse("")), ruletype, l.getData, Nil)
        rules = rule::rules
      }
      for (rule <- rules)
      {
        val dl = rule.getDataList().groupBy(_.getName).map(_._2.head)
        for (data <- dl)
          data.setRules(rule::data.getRules())
      }
    }

    // Prints the value of the queried variable
    private def _printValue(value: Int, name: Char)
    {
      println(Console.GREEN + "Value " + name + ": " + (value match
      {
        case 0 => "False"
        case 1 => "True"
        case -1 => "Undetermined"
      }) + "." + Console.RESET)
    }

    // Split the query to get the value of each variable
    private def _splitQuery()
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
              _printValue(0, c)
            else
            {
              try
              _printValue(datalist.filter(x => x.getName() == c)(0).getValue(), c)
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

    // Split the facts to get the initial value of each variable
    private def _splitFact()
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

    // The main parser action
    def parse()
    {
      _splitFact()
      _splitRule()
      _splitQuery()
    }
  }
}
