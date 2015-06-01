object TokenType extends Enumeration {
    val Query, Fact, Rule = Value
}
class Token(val ttype: TokenType.Value, val data: String)
{
    def getTokenType() : TokenType.Value = ttype
    def getData() : String = data
}
class LexerException(msg: String) extends RuntimeException(msg)
class TooManyQueriesException(msg:String) extends LexerException(msg)
class TooManyFactsException(msg:String) extends LexerException(msg)

val query = "^\\s*\\?\\s*[A-Z]*\\s*$".r
val fact = "^\\s*=\\s*[A-Z]*\\s*$".r
val rule = "^\\s*!*\\s*\\(*\\s*!*\\s*[A-Z]\\s*\\)*\\s*(?:[+|^]\\s*\\(*\\s*!?\\s*[A-Z]\\s*\\)*\\s*)*(?:=>|<=>)\\s*!*\\s*\\(*\\s*!*\\s*[A-Z]\\s*\\)*\\s*(?:[+|^]\\s*\\(*\\s*!?\\s*[A-Z]\\s*\\)*\\s*)*$".r
val comment = "#(?:.*)(?=\n)".r
val emptyline = "(?<=(\n|^))(?:\\s*\n)|(?:\\s*\n)(?=$)".r

if (args.length < 1)
{
    println("usage: scala expert_system.scala filename")
    System.exit(1)
}

val source: (scala.io.BufferedSource) =
try
{
    scala.io.Source.fromFile(args(0))
}
catch
{
    case e: java.io.FileNotFoundException =>
    {
        println(Console.RED + "Error: File " + args(0) + " not found." + Console.RESET)
        System.exit(1)
        null
    }
}

val lines = try source.mkString finally source.close()
val cleanedFile = emptyline.replaceAllIn(comment.replaceAllIn(lines, ""), "")
val split = cleanedFile.split("\n")
try lexer catch
{
    case e: LexerException => println(e.getMessage)
}

def lexer: List[Token] =
{
    var list = List[Token]()
    for (ln <- split)
    {
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
    if (list.groupBy(_.ttype).mapValues(_.size)(TokenType.Query) > 1) throw new TooManyQueriesException(Console.RED + "Error: Too many queries." + Console.RESET)
    if (list.groupBy(_.ttype).mapValues(_.size)(TokenType.Fact) > 1) throw new TooManyFactsException(Console.RED + "Error: Too many facts definition." + Console.RESET)
    list
}
