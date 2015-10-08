package expertSystem
{
  import scala.io.Source
  import java.io.FileNotFoundException
  import scala.util.{Try, Success, Failure}

  /**
   * The main class of the program
   */
  object Main
  {
    private def _tryFile(filename: String) : Try[scala.io.Source] = Try(scala.io.Source.fromFile(filename))

    private def _getLines(filename: String) : Try[String] =
    {
      Try(_tryFile(filename)) match {
        case Failure(e) =>
        {
          System.err.println(Console.RED + e.getMessage() + Console.RESET)
          throw new FileNotFoundException("Error")
        }
        case Success(source) => source.mkString
      }
    }

    def main(args: Array[String])
    {
      args.length match {
        case 0 => println(Console.MAGENTA + "usage: scala expertSystem.Main filename" + Console.RESET)
        case _ => Try(new Lexer(_getLines(args(0)))) match
        {
          case Failure(e) => System.err.println(e.getMessage())
          case Success(lexer) => Try ((new Parser(lexer.getResult())).parse()) match
          {
              case Failure(e) => System.err.println(e.getMessage())
              case _ => ()
          }
        }
      }
    }
  }
}
