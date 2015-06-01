
val query = "^\\s*\\?\\s*[A-Z]*\\s*$".r
val fact = "^\\s*=\\s*[A-Z]*\\s*$".r
val rule = "^\\s*!*\\s*\\(*\\s*!*\\s*[A-Z]\\s*\\)*\\s*(?:[+|^]\\s*\\(*\\s*!?\\s*[A-Z]\\s*\\)*\\s*)*(?:=>|<=>)\\s*!*\\s*\\(*\\s*!*\\s*[A-Z]\\s*\\)*\\s*(?:[+|^]\\s*\\(*\\s*!?\\s*[A-Z]\\s*\\)*\\s*)*$".r
val comment = "#(?:.*)(?=\n)".r
val emptyline = "(?<=(\n|^))(?:\\s*\n)|(?:\\s*\n)(?=$)".r

val source = scala.io.Source.fromFile("testZaz.txt")
val lines = try source.mkString finally source.close()

val cleanedFile = emptyline.replaceAllIn(comment.replaceAllIn(lines, ""), "")

val split = cleanedFile.split("\n")

for (ln <- split)
{
    ln match {
        case rule() => println("rule")
        case fact() => println("fact")
        case query() => println("query")
        case _ => println(Console.RED + "FAIL" + Console.RESET)
    }
}
