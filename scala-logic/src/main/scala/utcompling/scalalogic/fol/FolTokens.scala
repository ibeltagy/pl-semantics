package utcompling.scalalogic.fol

import utcompling.scalalogic.base.Tokens

trait FolTokens extends Tokens {

    //Quantifiers
    val EXISTS = "exists"; val EXISTS_LIST = List("some", "exists", "exist")
    val ALL = "all"; val ALL_LIST = List("all", "forall")

    //Collections of tokens
    val BINOPS = AND_LIST ++ OR_LIST ++ IMP_LIST ++ LIMP_LIST ++ IFF_LIST
    val PUNCT = List(DOT, OPEN, CLOSE, COMMA)
    val QUANTS = EXISTS_LIST ++ ALL_LIST

    val TOKENS = BINOPS ++ EQ_LIST ++ NEQ_LIST ++ QUANTS ++ LAMBDA_LIST ++ PUNCT ++ NOT_LIST

    //Special
    val SYMBOLS = this.makeSymbolList(TOKENS)

}

object FolTokens extends FolTokens {

}
