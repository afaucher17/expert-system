package expertSystem
{
  import scala.util.{Try, Success, Failure}

  object TokenType extends Enumeration {
    val Query, Fact, Rule = Value
  }


  class Token(val ttype: TokenType.Value, val data: String)
  {
    def getTokenType() : TokenType.Value = ttype
    def getData() : String = data
  }

  // Exception classes
  class LexerException(msg: String) extends RuntimeException(msg)
  class TooManyQueriesException(msg: String) extends LexerException(msg)
  class NoQueryException(msg: String) extends LexerException(msg)
  class TooManyFactsException(msg: String) extends LexerException(msg)
  class NoFactException(msg: String) extends LexerException(msg)
  class ParenthesesMismatchException(msg: String) extends LexerException(msg)

  class Lexer(lines: Try[String])
  {
    private val result = _lex(_split())

    def getResult() : List[Token] = result

    private def _split() : List[String] =
    {
      val emptyline = "(?<=(\n|^))(?:\\s*\n)|(?:\\s*\n)(?=$)".r
      val comment = "#(?:.*)(?=\n)".r

      lines match
      {
        case Failure(e) => throw e
        case Success(str) => {
          val cleanedFile = emptyline.replaceAllIn(comment.replaceAllIn(str, ""), "").replaceAll("([\t \r]*)", "")
          cleanedFile.split("\n").toList
        }
      }
    }

    private def _checkParentheses(line: String)
    {
      val e = new ParenthesesMismatchException(Console.RED + "Error: " + "Parentheses mismatch" + Console.RESET)
      val f =
      { (acc: Int, x: Char) => if (acc >= 0) x match
        {
          case '(' => acc + 1
            case ')' => acc - 1
          case _ => acc
        }
        else throw e
      }
      if (line.foldLeft(0)(f) != 0) throw e
    }

    private def _stringToToken(str: String) : Token =
    {

    val query = "^\\s*\\?\\s*[A-Z]*\\s*$".r
    val fact = "^\\s*=\\s*[A-Z]*\\s*$".r
    val rule = "^\\s*!*\\s*\\(*\\s*!*\\s*[A-Z]\\s*\\)*\\s*(?:[+|^]\\s*\\(*\\s*!?\\s*[A-Z]\\s*\\)*\\s*)*(?:=>|<=>)\\s*!*\\s*\\(*\\s*!*\\s*[A-Z]\\s*\\)*\\s*(?:[+|^]\\s*\\(*\\s*!?\\s*[A-Z]\\s*\\)*\\s*)*$".r
      str match
      {
        case query() => new Token(TokenType.Query, str)
        case fact() => new Token(TokenType.Fact, str)
        case rule() => new Token(TokenType.Rule, str)
        case _ => throw new LexerException(Console.RED + "Error at line: " + Console.RESET + str)
      }
    }

    private def _checkListValidity(list: Try[List[Token]]) : List[Token] =
    {
      list match
      {
        case Success(l) =>
        {
          val counts = l.groupBy(_.ttype).mapValues(_.size)
          if (counts.get(TokenType.Query) == None)
            throw new NoQueryException(Console.RED + "Error: No query." + Console.RESET)
          if (counts(TokenType.Query) > 1)
            throw new TooManyQueriesException(Console.RED + "Error: Too many queries." + Console.RESET)
          if (counts.get(TokenType.Fact) == None)
            throw new NoFactException(Console.RED + "Error: No fact definition." + Console.RESET)
          if (counts(TokenType.Fact) > 1)
            throw new TooManyFactsException(Console.RED + "Error: Too many facts definition." + Console.RESET)
          l
        }
        case Failure(e) => throw e
      }
    }

    private def _lex(split: List[String]): List[Token] = _checkListValidity(Try(split.map(str =>
      { _checkParentheses(str)
        _stringToToken(str)
      })))
  }
}
