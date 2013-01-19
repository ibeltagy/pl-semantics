package utcompling.scalalogic.drt

import utcompling.scalalogic.base.Tokens

trait DrtTokens extends Tokens {

    val DRS = "DRS"
    val DRS_CONC = "+"
    val PRONOUN = "PRO"
    val OPEN_BRACKET = "["
    val CLOSE_BRACKET = "]"
    val COLON = ":"

    //Collections of Tokens
    val BINOPS = OR_LIST ++ IMP_LIST
    val PUNCT = List(DRS_CONC, OPEN_BRACKET, CLOSE_BRACKET, DOT, OPEN, CLOSE, COMMA, COLON)

    val TOKENS = List(DRS) ++ BINOPS ++ EQ_LIST ++ NEQ_LIST ++ LAMBDA_LIST ++ PUNCT ++ NOT_LIST

    //Special
    val SYMBOLS = this.makeSymbolList(TOKENS)

}

object DrtTokens extends DrtTokens {

}
