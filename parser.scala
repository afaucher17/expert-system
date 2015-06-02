package parser
{

  import tree.Rule
  import lexer.Token

  class Parser(list: List[Token])
  {
    val imply = "([^<=>]+)(=>|<=>)([^<=>]+)\n".r

    def splitRule(list: List[Token])
    {
      /*var rule = new Rule()*/
      for (l <- list)
      {
        val matches = imply.findAllIn(l.getData)
        println(matches.group(3))
      }
    }
  }

}
