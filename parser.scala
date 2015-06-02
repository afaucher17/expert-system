package parser
{

  import tree.Rule
  import lexer.Token

  class Parser(list: List[Token])
  {
    val imply = "([^<=>]+)(=>|<=>)([^<=>]+)\n".r

    def splitRule()
    {
      /*var rule = new Rule()*/
      for (l <- list)
      {
        l match {
          case imply(group) => {
            println("--->  " + group(1))
          }
          case _ => println("non matching")
        }
      }
    }
  }

}
