package expertSystem
{
  /**
   * The main class of the program
   */
  object Main {
    // Get the content of the file, throw a FileNotFoundException if the file is not found.
    private def _getFile(filename: String) : String =
    {
      val source: (scala.io.BufferedSource) =
        try
        {
          scala.io.Source.fromFile(filename)
        }
        catch
        {
          case e: java.io.FileNotFoundException =>
          {
            System.err.println(Console.RED + "Error: File " + filename + " not found." + Console.RESET)
            System.exit(1)
            null
          }
        }
        val lines = try source.mkString finally source.close()
        lines
    }

    // main function
    def main(args: Array[String])
    {
      args.length match {
        case 0 => println(Console.MAGENTA + "usage: scala expertSystem.Main filename" + Console.RESET)
        case _ =>
        {
          val lines = _getFile(args(0))
          val lexer = new Lexer(lines)
          val list: (List[Token]) = try lexer.lex(lexer.split) catch {
            case e: LexerException =>
            {
              System.err.println(e.getMessage)
              System.exit(1)
              null
            }
          }
          val parser = new Parser(list)
          parser.parse
        }
      }
    }
  }
}
