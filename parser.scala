package parser
{

  import tree.Rule
  import tree.Data
  import lexer.Token
  import lexer.TokenType

  class Parser(list: List[Token])
  {
    val imply = "([^<=>]+)(=>|<=>)([^<=>]+)".r
    val rules = List[Rule]()
    val datalist = List[Data]()

    def splitRule()
    {
      /*var rule = new Rule()*/
     for (l <- list.filter(x => x.getTokenType() == TokenType.Rule))
     {
       val matches = imply.findAllMatchIn(l.getData)
       matches.foreach {
         m => println(m.group(1))
       }
     }
    }

    def splitQuery()
    {
      for (l <- list.filter(x => x.getTokenType() == TokenType.Query))
      {

      }
    }

    def splitFact()
    {
      val fact = list.filter(x => x.getTokenType() == TokenType.Fact)(0).getData()
      for (ln <- fact)
      {
        ln match {
          case ('=' | ' ' | '\t' | '\n' | '\r') => ""
          case _ => print(ln)
        }
      }
    }


    def parse()
    {
      splitRule()
      splitQuery()
      splitFact()
    }
  }
}
