package utcompling.scalalogic.fol.expression.parse

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.MapBuilder
import scala.collection.mutable.HashMap
import utcompling.scalalogic.base.expression.parse.BaseLogicParser
import utcompling.scalalogic.top.expression.parse.ExpectedMoreTokensException
import utcompling.scalalogic.top.expression.Variable
import utcompling.scalalogic.fol.expression._
import utcompling.scalalogic.fol.FolTokens

class FolLogicParser extends BaseLogicParser[FolExpression] {

    override protected def makeOperatorPrecedence(): Map[Option[String], Int] = {
        val operatorPrecedence = new MapBuilder[Option[String], Int, Map[Option[String], Int]](Map[Option[String], Int]())
        FolTokens.LAMBDA_LIST foreach { operatorPrecedence += Some(_) -> 1 }
        FolTokens.NOT_LIST foreach { operatorPrecedence += Some(_) -> 2 }
        operatorPrecedence += APP -> 3
        FolTokens.EQ_LIST ++ FolTokens.NEQ_LIST foreach { operatorPrecedence += Some(_) -> 4 }
        FolTokens.QUANTS foreach { operatorPrecedence += Some(_) -> 5 }
        FolTokens.AND_LIST foreach { operatorPrecedence += Some(_) -> 6 }
        FolTokens.OR_LIST foreach { operatorPrecedence += Some(_) -> 7 }
        FolTokens.IMP_LIST ++ FolTokens.LIMP_LIST foreach { operatorPrecedence += Some(_) -> 8 }
        FolTokens.IFF_LIST foreach { operatorPrecedence += Some(_) -> 9 }
        operatorPrecedence += None -> 10
        return operatorPrecedence.result
    }

    override protected def getAllSymbols() =
        FolTokens.SYMBOLS

    override protected def isvariable(tok: String) =
        !FolTokens.TOKENS.contains(tok)

    override protected def handle(tok: String, context: Option[String]): FolExpression = {
        if (this.isvariable(tok))
            return this.handlevariable(tok, context)

        else if (FolTokens.NOT_LIST.contains(tok))
            return this.handlenegation(tok, context)

        else if (FolTokens.LAMBDA_LIST.contains(tok))
            return this.handlelambda(tok, context)

        else if (FolTokens.QUANTS.contains(tok))
            return this.handlequant(tok, context)

        else if (FolTokens.OPEN == tok)
            return this.handleopen(tok, context)

        throw new RuntimeException
    }

    override protected def makeNegatedExpression(expression: FolExpression) =
        FolNegatedExpression(expression)

    override protected def makeEqualityExpression(first: FolExpression, second: FolExpression): FolExpression =
        return FolEqualityExpression(first, second)

    override protected def get_BooleanExpression_factory(tok: String): Option[((FolExpression, FolExpression) => FolBooleanExpression)] = {
        if (FolTokens.AND_LIST.contains(tok))
            return Some(((e1: FolExpression, e2: FolExpression) => FolAndExpression(e1, e2)))
        else if (FolTokens.OR_LIST.contains(tok))
            return Some(((e1: FolExpression, e2: FolExpression) => FolOrExpression(e1, e2)))
        else if (FolTokens.IMP_LIST.contains(tok))
            return Some(((e1: FolExpression, e2: FolExpression) => FolIfExpression(e1, e2)))
        else if (FolTokens.LIMP_LIST.contains(tok))
            return Some(((e1: FolExpression, e2: FolExpression) => FolIfExpression(e2, e1)))
        else if (FolTokens.IFF_LIST.contains(tok))
            return Some(((e1: FolExpression, e2: FolExpression) => FolIffExpression(e1, e2)))
        else
            return None
    }

    protected def handlequant(tok: String, context: Option[String]): FolExpression = {
        // Expression is a quantified expression: some x.M
        val factory = this.get_QuantifiedExpression_factory(tok)

        if (!this.inRange(0))
            throw new ExpectedMoreTokensException(Some(this.getCurrentIndex + 2), Some("Variable and Expression expected following quantifier '" + tok + "'."))
        val vars = this.get_next_token_variable("quantified") +: get_additional_bound_vars("quantified")
        if (this.inRange(0) && this.getToken(0) == FolTokens.DOT)
            this.nextToken() //swallow the dot

        return vars.foldRight(this.parseExpression(Some(tok)))(this.makeQuanifiedExpression(factory, _, _))
    }

    protected def get_QuantifiedExpression_factory(tok: String): ((Variable, FolExpression) => FolQuantifiedExpression) = {
        if (FolTokens.EXISTS_LIST.contains(tok))
            return ((v: Variable, term: FolExpression) => FolExistsExpression(v, term))
        else if (FolTokens.ALL_LIST.contains(tok))
            return ((v: Variable, term: FolExpression) => FolAllExpression(v, term))
        else
            this.assertToken(tok, FolTokens.QUANTS)
        throw new RuntimeException
    }

    protected def makeQuanifiedExpression(factory: ((Variable, FolExpression) => FolQuantifiedExpression), variable: Variable, term: FolExpression) = factory(variable, term)

    override protected def makeApplicationExpression(function: FolExpression, argument: FolExpression) =
        FolApplicationExpression(function: FolExpression, argument: FolExpression)

    override protected def makeVariableExpression(name: String) =
        FolVariableExpression(Variable(name))

    override protected def makeLambdaExpression(variable: Variable, term: FolExpression) =
        FolLambdaExpression(variable, term)

}
