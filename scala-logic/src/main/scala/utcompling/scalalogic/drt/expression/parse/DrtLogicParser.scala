package utcompling.scalalogic.drt.expression.parse

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.MapBuilder
import utcompling.scalalogic.base.expression.parse.BaseLogicParser
import utcompling.scalalogic.top.expression.Variable
import utcompling.scalalogic.drt.expression._
import utcompling.scalalogic.drt.DrtTokens

class DrtLogicParser extends BaseLogicParser[DrtExpression] {

    override protected def makeOperatorPrecedence(): Map[Option[String], Int] = {
        val operatorPrecedence = new MapBuilder[Option[String], Int, Map[Option[String], Int]](Map[Option[String], Int]())
        DrtTokens.LAMBDA_LIST foreach { operatorPrecedence += Some(_) -> 1 }
        DrtTokens.NOT_LIST foreach { operatorPrecedence += Some(_) -> 2 }
        operatorPrecedence += APP -> 3
        DrtTokens.EQ_LIST ++ DrtTokens.NEQ_LIST foreach { operatorPrecedence += Some(_) -> 4 }
        operatorPrecedence += Some(DrtTokens.COLON) -> 5
        operatorPrecedence += Some(DrtTokens.DRS_CONC) -> 6
        DrtTokens.OR_LIST foreach { operatorPrecedence += Some(_) -> 7 }
        DrtTokens.IMP_LIST foreach { operatorPrecedence += Some(_) -> 8 }
        operatorPrecedence += None -> 9
        return operatorPrecedence.result
    }

    override protected def getAllSymbols() =
        DrtTokens.SYMBOLS

    override protected def isvariable(tok: String) =
        !DrtTokens.TOKENS.contains(tok)

    override protected def handle(tok: String, context: Option[String]): DrtExpression = {
        if (DrtTokens.NOT_LIST.contains(tok))
            return this.handlenegation(tok, context)

        else if (DrtTokens.LAMBDA_LIST.contains(tok))
            return this.handlelambda(tok, context)

        else if (DrtTokens.OPEN == tok) {
            if (this.inRange(0) && this.getToken(0) == DrtTokens.OPEN_BRACKET)
                return this.handleDRS(tok, context)
            else
                return this.handleopen(tok, context)
        } else if (tok.toUpperCase == DrtTokens.DRS) {
            this.assertNextToken(DrtTokens.OPEN)
            return this.handleDRS(tok, context)
        } else if (this.isvariable(tok)) {
            if (this.inRange(0) && this.getToken(0) == DrtTokens.COLON) {
                return this.handleProp(tok, context)
            } else {
                return this.handlevariable(tok, context)
            }
        }

        throw new RuntimeException
    }

    override protected def makeNegatedExpression(expression: DrtExpression) =
        DrtNegatedExpression(expression)

    def handleDRS(tok: String, context: Option[String]): DrtBoxExpression = {
        val refs = this.handlerefs()
        if (this.inRange(0) && this.getToken(0) == DrtTokens.COMMA) //if there is a comma (it's optional)
            this.nextToken() // swallow the comma
        val conds = this.handleconds(context)
        this.assertNextToken(DrtTokens.CLOSE)
        return DrtBoxExpression(refs, conds, None)
    }

    def handlerefs(): List[Variable] = {
        this.assertNextToken(DrtTokens.OPEN_BRACKET)
        val refs = ListBuffer[Variable]()
        while (this.inRange(0) && this.getToken(0) != DrtTokens.CLOSE_BRACKET) {
            // Support expressions like: DRS([x y],C) == DRS([x,y],C)
            if (refs.nonEmpty && this.getToken(0) == DrtTokens.COMMA)
                this.nextToken() // swallow the comma
            refs.append(this.get_next_token_variable("quantified"))
        }
        this.assertNextToken(DrtTokens.CLOSE_BRACKET)
        return refs.result
    }

    def handleconds(context: Option[String]): List[DrtExpression] = {
        this.assertNextToken(DrtTokens.OPEN_BRACKET)
        val conds = ListBuffer[DrtExpression]()
        while (this.inRange(0) && this.getToken(0) != DrtTokens.CLOSE_BRACKET) {
            // Support expressions like: DRS([x y],C) == DRS([x, y],C)
            if (conds.nonEmpty && this.getToken(0) == DrtTokens.COMMA)
                this.nextToken() // swallow the comma
            conds.append(this.parseExpression(context))
        }
        this.assertNextToken(DrtTokens.CLOSE_BRACKET)
        return conds.result
    }

    override protected def makeEqualityExpression(first: DrtExpression, second: DrtExpression): DrtExpression =
        return DrtEqualityExpression(first, second)

    override protected def get_BooleanExpression_factory(tok: String): Option[((DrtExpression, DrtExpression) => DrtExpression)] = {
        if (tok == DrtTokens.DRS_CONC)
            return Some((first: DrtExpression, second: DrtExpression) => DrtConcatenationExpression(first, second, None))
        else if (DrtTokens.OR_LIST.contains(tok))
            return Some((first: DrtExpression, second: DrtExpression) => DrtOrExpression(first, second))
        else if (DrtTokens.IMP_LIST.contains(tok)) {
            return Some((first: DrtExpression, second: DrtExpression) =>
                first match {
                    case b: DrtBoxExpression => DrtBoxExpression(b.refs, b.conds, Some(second))
                    case c: DrtConcatenationExpression => DrtConcatenationExpression(c.first, c.second, Some(second))
                    case a: DrtApplicationExpression => DrtApplicationExpression(a.function, a.argument, Some(second))
                    case _ => throw new RuntimeException("Antecedent of implication must be a DRS")
                })
        } else
            return None
    }

    override protected def makeApplicationExpression(function: DrtExpression, argument: DrtExpression) =
        DrtApplicationExpression(function, argument)

    override protected def makeVariableExpression(name: String) =
        DrtVariableExpression(Variable(name))

    override protected def makeLambdaExpression(variable: Variable, term: DrtExpression) =
        DrtLambdaExpression(variable, term)
        
    protected def handleProp(tok: String, context: Option[String]): DrtPropositionExpression = {
        this.assertNextToken(":")
        return DrtPropositionExpression(Variable(tok), this.parseExpression(Some(DrtTokens.COLON)))
    }

}