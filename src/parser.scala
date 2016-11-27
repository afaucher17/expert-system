package expertSystem
{
  import scala.util.{Try, Success, Failure}
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
    var datalist = List[Data]()

    // Removes the parentheses if they are encapsulating the whole expression
    private def _trimParentheses(line: String) : String =
    {
      def f(par: Int, i: Int): Boolean =
        if (i >= line.length) false
        else
        {
          line.charAt(i) match {
            case '(' => f(par + 1, i + 1)
            case ')' => if ((par - 1) == 0 && i != (line.length - 1)) true else f(par - 1, i + 1)
            case _ => if (i == 0) true else f(par, i + 1)
          }
        }
      if (f(0, 0)) line else _trimParentheses(line.slice(1, line.length - 1))
    }

    // Creates a LogicTree (either a Node or a Leaf)
    private def _addLogicTree(pos: (Int, Char), line: String) : (LogicTree) =
    {
      pos._2 match {
        case '!' =>
          (new Node(None, _createTree(line.slice(pos._1 + 1, line.length)),
            pos._2, operators(pos._2)))
        case ('+' | '|' | '^') =>
          (new Node(Some(_createTree(line.slice(0, pos._1))),
            _createTree(line.slice(pos._1 + 1, line.length)),
            pos._2, operators(pos._2)))
        case '?' =>
        {
          val li = datalist.filter(x => x.getName() == line(0))
          if (li.isEmpty)
          {
            val data = new Data(-2, line(0), false)
            datalist = data::datalist
            new Leaf(data)
          }
          else new Leaf(li(0))
        }
      }
    }

    private def _createTree(oline: String) : LogicTree =
    {
      val line = _trimParentheses(oline)
      def findDelimitor(i: Int, ignore: Int, pos: (Int, Char)) : (Int, Char) =
      {
        if (i >= line.length) pos
        else
        {
          val c = line.charAt(i)
          c match
          {
            case '(' => findDelimitor(i + 1, ignore + 1, pos)
            case ')' => findDelimitor(i + 1, ignore - 1, pos)
            case ('+' | '|' | '^' | '!') => if (ignore == 0 && _checkPriority(c, pos._2))
                                              findDelimitor(i + 1, ignore, (i, c))
                                            else findDelimitor(i + 1, ignore, pos)
            case _ => findDelimitor(i + 1, ignore, pos)
          }
        }
      }
      _addLogicTree(findDelimitor(0, 0, (-1, '?')), line)
    }

    // Checks the priority of the operators in order to create the tree in the right order.
    private def _checkPriority(current: Char, save: Char) : Boolean =
    {
      val priority: Map[Char, Int] = Map('^' -> 0, '|' -> 1,
        '+' -> 2, '!' -> 3, '?' -> 9001)

      (priority(current) <= priority(save))
    }

    // Splits the rules and creates a Rule instance for each of them
    private def _splitRule()  =
    {
      val imply = "([^<=>]+)(=>|<=>)([^<=>]+)".r
      val f = (acc: List[Rule], t: Token) =>
              imply.findFirstMatchIn(t.getData) match
              {
                case Some(m) =>
                {
                  val ruletype = if (m.group(2) == "=>") RuleType.Implication
                    else RuleType.IfAndOnlyIf
                  val rule = new Rule(_createTree(m.group(1)), _createTree(m.group(3)), ruletype, t.getData, Nil)
                  rule::acc
                }
                case None => acc
              }
      rules = (list.filter(x => x.getTokenType() == TokenType.Rule)).foldLeft(List[Rule]())(f)
      def setDataLoop(current: List[Rule]): Unit =
      {
        def setData(dl: Iterable[Data], rule: Rule): Unit =
        {
          dl match
          {
            case data::tl => {
                data.setRules(rule::data.getRules())
                setData(tl, rule)
            }
            case Nil => ()
          }
        }
        current match
        {
          case rule::tl => {
            setData(rule.getDataList().groupBy(_.getName).map(_._2.head), rule)
            setDataLoop(tl)
          }
          case Nil => ()
        }
      }
      setDataLoop(rules)
    }

    // Prints the value of the queried variable
    private def _printValue(value: Int, name: Char) = println(Console.GREEN + "Value " + name + ": " + (value match
    {
        case 0 => "False"
        case 1 => "True"
        case -1 => "Undetermined"
    }) + "." + Console.RESET)

    // Split the query to get the value of each variable
    private def _splitQuery()
    {
      val qry =
        list.filter(x => x.getTokenType() == TokenType.Query)(0).getData()
      def solver(chars: List[Char]): Unit =
      {
        chars match {
          case Nil => ()
          case c :: tail =>
          {
            c match
            {
              case ('?' | ' ' | '\t' | '\n' | '\r') => solver(tail)
              case _ =>
              {
                if (!datalist.exists(x => x.getName() == c)) _printValue(0, c)
                else _printValue(datalist.filter(x => x.getName() == c)(0).getValue(), c)
                solver(tail)
              }
            }
          }
        }
      }
      solver(qry.toList)
    }

    // Split the facts to get the initial value of each variable
    private def _splitFact() =
    {
      val fact =
        list.filter(x => x.getTokenType() == TokenType.Fact)(0).getData()
      def check(c: Char, dtlst: List[Data]): List[Data] =
        if (!dtlst.exists(x => x.getName() == c))
          new Data(1, c, false)::dtlst
        else dtlst
      def explodeFacts(chars: List[Char], dtlst: List[Data]): List[Data] =
        chars match {
          case Nil => dtlst
          case c :: tail => { c match {
                  case ('=' | ' ' | '\t' | '\n' | '\r') => explodeFacts(tail, dtlst)
                  case _ => explodeFacts(tail, check(c, dtlst))
            }
          }
        }
      datalist = explodeFacts(fact.toList, datalist)
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
