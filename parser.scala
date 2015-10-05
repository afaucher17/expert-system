package expertSystem
{
  /**
   * Parser class, used for parsing operations
   **/
  class Parser(list: List[Token])
  {
    // The regex used in the parser
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

    // Removes the parentheses if they are encapsulating the whole expression
    private def _trimParentheses(line: String) : String =
    {
      val f = { (par: Int, i: Int) =>
        if (i >= line.length) false else
        line.charAt(i) match {
          case '(' => f(par + 1, i + 1)
          case ')' => if (par == 0 && i != (line.length - 1)) true else f(par - 1, i + 1)
          case _ => if (i == 0) true else f(par, i + 1)
        }
      }
      if (f(0, 0)) line else _trimParentheses(line.slice(1, line.length - 1))
    }

    // Creates a LogicTree (either a Node or a Leaf)
    private def _addLogicTree(pos: Array[Int], line: String, datalist: List[Data]) : LogicTree =
    {
      pos(1) match {
        case '!' =>
          new Node(null, _createTree(line.slice(pos(0) + 1, line.length)),
            pos(1).asInstanceOf[Char],
            operators(pos(1).asInstanceOf[Char]))
        case ('+' | '|' | '^') =>
          new Node(_createTree(line.slice(0, pos(0))),
            _createTree(line.slice(pos(0) + 1, line.length)),
            pos(1).asInstanceOf[Char],
            operators(pos(1).asInstanceOf[Char]))
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

    private def _createTree(oline: Option[String]) : LogicTree =
    {
      val line = oline match {
        case Some(str) => _trimParentheses(str)
        case None => throw RuntimeException("unknown error")
      }
      val f = { (i: Int, ignore: Int, pos: Tuple2) =>
        if (i >= line.length) pos else
        line.charAt(i) match
        {
          case '(' => f(i + 1, ignore + 1, pos)
          case ')' => f(i + 1, ignore - 1, pos)
          case ('+' | '|' | '^' | '!') => if (ignore == 0 && _checkPriority(c, pos._2))
                                            f(i + 1, ignore, (i, c))
                                          else f(i + 1, ignore, pos)
          case _ => f(i + 1, ignore, pos)
        }
      }
      _addLogicTree(f(0, 0, (-1, '?')), line)
    }

    // Checks the priority of the operators in order to create the tree in the right order.
    private def _checkPriority(current: Char, save: Char) : Boolean =
    {
      val priority: Map[Char, Int] = Map('^' -> 0, '|' -> 1,
        '+' -> 2, '!' -> 3, '?' -> 9001)

      (priority(current) <= priority(save))
    }

    // Splits the rules and creates a Rule instance for each of them
    private def _splitRule(datalist: Data[List]): List[Rule] =
    {
      val imply = "([^<=>]+)(=>|<=>)([^<=>]+)".r
      val f = { (acc: List[Rule], t: Token) =>
        val m = imply.findFirstMatchIn(t.getData)
        val rule = new Rule(_createTree(m.map(_.group(1))),
          _createTree(m.map(_.group(3))),
          if (m.map(_.group(2)).getOrElse("") == "=>") RuleType.Implication
          else RuleType.IfAndOnlyIf,
          l.getData, Nil)
        rule::acc
      }
      val rules = list.foldLeft(new Rule[List]())(f)
      val g = { (current: List[Rule]) =>
        val setData = { (dl: List[Data], rule: Rule)
          dl match
          {
            case hd::tl =>
            {
              hd.setRules(rule::data.getRules())
              setData(tl, rule)
            }
            case Nil => ()
          }
        }
        current match
        {
          case hd::tl =>
          {
            setData(hd.getDataList().groupBy(_.getName).map(_._2.head), hd)
            g(tl)
          }
          case _ => ()
        }
      }
      g(rules)
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
                  System.err.println(e.getMessage)
                  System.exit(1)
                }
              }
            }
          }
        }
      }
    }

    // Split the facts to get the initial value of each variable
    private def _splitFact() : List[Data] =
    {
      val fact =
        list.filter(x => x.getTokenType() == TokenType.Fact)(0).getData()
      val check = { (c: Char, datalist: List[Data]) =>
        if (!datalist.exists(x => x.getName() == c))
          new Data(1, c, false)::datalist
        else datalist
      }
      val loop = { (chars: List[Char], datalist: List[Data]) => {
        chars match {
          case Nil  => datalist
          case c :: tail => { c match {
                  case ('=' | ' ' | '\t' | '\n' | '\r') => loop(tail, datalist)
                  case _ => loop(tail, check(c, datalist))
              }
            }
          }
        }
      }
      loop(fact, List[Data]())
    }

    // The main parser action
    def parse()
    {
      val datalist = _splitFact()
      _splitRule()
      _splitQuery()
    }
  }
}
