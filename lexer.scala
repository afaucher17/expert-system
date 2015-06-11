package expertSystem
{
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

  /**
   * Lexer class, used for lexer operations
   */
  class Lexer(lines: String)
  {
    // The regex used in the lexer
    val query = "^\\s*\\?\\s*[A-Z]*\\s*$".r
    val fact = "^\\s*=\\s*[A-Z]*\\s*$".r
    val rule = "^\\s*!*\\s*\\(*\\s*!*\\s*[A-Z]\\s*\\)*\\s*(?:[+|^]\\s*\\(*\\s*!?\\s*[A-Z]\\s*\\)*\\s*)*(?:=>|<=>)\\s*!*\\s*\\(*\\s*!*\\s*[A-Z]\\s*\\)*\\s*(?:[+|^]\\s*\\(*\\s*!?\\s*[A-Z]\\s*\\)*\\s*)*$".r
    val comment = "#(?:.*)(?=\n)".r
    val emptyline = "(?<=(\n|^))(?:\\s*\n)|(?:\\s*\n)(?=$)".r


    // Cleans the file then splits the file into lines
    def split() : Array[String] =
    {
      var cleanedFile = emptyline.replaceAllIn(comment.replaceAllIn(lines, ""), "")
      cleanedFile = cleanedFile.replaceAll("([\t \r]*)", "")
      val split = cleanedFile.split("\n")
      split
    }

    // Checks if the number of parentheses is correct, if incorrect, throws a ParenthesesMismatchException
    private def _checkParentheses(line: String) =
    {
      var count = 0
      for (c <- line)
      {
        c match
        {
          case '(' => count += 1
          case ')' => count -= 1
          case _ => ""
        }
        if (count < 0)
          throw new ParenthesesMismatchException(Console.RED + "Error: Parentheses mismatch" + Console.RESET)
      }
      if (count != 0)
          throw new ParenthesesMismatchException(Console.RED + "Error: Parentheses mismatch" + Console.RESET)
    }

    // The main action of the lexer, if an error is found, throws a LexerException
    def lex(split: Array[String]): List[Token] =
    {
      var list = List[Token]()
      for (ln <- split)
      {
        _checkParentheses(ln)
        ln match {
          case query() => list = new Token(TokenType.Query, ln)::list
          case fact() => list = new Token(TokenType.Fact, ln)::list
          case rule() => list = new Token(TokenType.Rule, ln)::list
          case _ =>
          {
            throw new LexerException(Console.RED + "Error at line: " + Console.RESET + ln)
          }
        }
      }
      if (list.groupBy(_.ttype).mapValues(_.size).getOrElse(TokenType.Query, null) == null)
        throw new NoQueryException(Console.RED + "Error: No query." + Console.RESET)
      if (list.groupBy(_.ttype).mapValues(_.size)(TokenType.Query) > 1)
        throw new TooManyQueriesException(Console.RED + "Error: Too many queries." + Console.RESET)
      if (list.groupBy(_.ttype).mapValues(_.size).getOrElse(TokenType.Fact, null) == null)
        throw new NoFactException(Console.RED + "Error: No fact definition." + Console.RESET)
      if (list.groupBy(_.ttype).mapValues(_.size)(TokenType.Fact) > 1)
        throw new TooManyFactsException(Console.RED + "Error: Too many facts definition." + Console.RESET)
      list
    }
  }
}
