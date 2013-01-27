package utcompling.scalalogic.base.expression.parse

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import utcompling.scalalogic.base.expression._
import utcompling.scalalogic.top.expression.Variable
import utcompling.scalalogic.base.Tokens
import utcompling.scalalogic.top.expression.parse._

abstract class BaseLogicParser[T <: BaseExpression[T]] extends LogicParser[T] {

    final protected val APP = Some("APP")

    protected val right_associated_operations = List(APP)

    private val operatorPrecedence = this.makeOperatorPrecedence()

    protected def makeOperatorPrecedence(): Map[Option[String], Int]

    protected def isvariable(tok: String): Boolean

    protected def doParseExpression(context: Option[String]): T =
        return this.handle(this.nextToken(), context)

    protected def handle(tok: String, context: Option[String]): T;

    override protected def attemptAdjuncts(expression: T, context: Option[String]): T = {
        var e = expression
        var curIdx: Int = -1
        while (curIdx != this.getCurrentIndex) { //while adjuncts are added
            curIdx = this.getCurrentIndex
            e = this.attemptEqualityExpression(e, context)
            e = this.attemptApplicationExpression(e, context)
            e = this.attemptBooleanExpression(e, context)
        }
        return e
    }

    protected def handlenegation(tok: String, context: Option[String]) =
        this.makeNegatedExpression(this.parseExpression(Some(Tokens.NOT)))

    protected def makeNegatedExpression(expression: T): T

    protected def handlevariable(tok: String, context: Option[String]): T = {
        // It's either: 1) a predicate expression: sees(x,y)
        //              2) an application expression: P(x)
        //              3) a solo variable: john OR x
        var accum = this.makeVariableExpression(tok)
        if (this.inRange(0) && this.getToken(0) == Tokens.OPEN) {
            //The predicate has arguments
            //            if(not isinstance(accum, FunctionVariableExpression) &&
            //               not isinstance(accum, ConstantExpression))
            //                throw ParseException(this.getCurrentIndex, 
            //                                     '\'%s\' is an illegal predicate name.  '
            //                                     'Individual variables may not be used as '
            //                                     'predicates.' % tok)
            this.nextToken() //swallow the Open Paren

            //curry the arguments
            accum = this.makeApplicationExpression(accum, this.parseExpression(APP))
            while (this.inRange(0) && this.getToken(0) == Tokens.COMMA) {
                this.nextToken() //swallow the comma
                accum = this.makeApplicationExpression(accum, this.parseExpression(APP))
            }
            this.assertNextToken(Tokens.CLOSE)
        }
        return accum
    }

    protected def get_next_token_variable(description: String): Variable = {
        var tok: String = null
        try {
            tok = this.nextToken()
        } catch {
            case e: ExpectedMoreTokensException => throw new ExpectedMoreTokensException(e.index, Some("Variable expected."), e)
        }
        //        if isinstance(this.makeVariableExpression(tok), ConstantExpression):
        //            throw ParseException(this.getCurrentIndex,
        //                                 "'%s' is an illegal variable name.  '
        //                                 'Constants may not be %s.' % (tok, description))
        return Variable(tok)
    }

    protected def handlelambda(tok: String, context: Option[String]): T = {
        // Expression is a lambda expression
        if (!this.inRange(0))
            throw new ExpectedMoreTokensException(Some(this.getCurrentIndex + 2), Some("Variable and Expression expected following lambda operator."))
        val vars = this.get_next_token_variable("abstracted") +: get_additional_bound_vars("abstracted")

        if (this.inRange(0) && this.getToken(0) == Tokens.DOT)
            this.nextToken() //swallow the dot

        return vars.foldRight(this.parseExpression(Some(tok)))(this.makeLambdaExpression(_, _))
    }

    protected def get_additional_bound_vars(description: String): ListBuffer[Variable] = {
        val vars = ListBuffer[Variable]()
        while (true) {
            if (!this.inRange(0) || (this.getToken(0) == Tokens.DOT && !this.inRange(1)))
                throw new ExpectedMoreTokensException(Some(this.getCurrentIndex + 2), Some("Expression expected."))
            if (!this.isvariable(this.getToken(0)))
                return vars
            // Support expressions like: \x y.M == \x.\y.M
            vars += this.get_next_token_variable(description)
        }
        throw new Exception()
    }

    protected def handleopen(tok: String, context: Option[String]): T = {
        //Expression is in parens
        val accum = this.parseExpression(None)
        this.assertNextToken(Tokens.CLOSE)
        return accum
    }

    protected def attemptEqualityExpression(expression: T, context: Option[String]): T = {
        /**
         * Attempt to make an equality expression.  If the next token is an
         * equality operator, then an EqualityExpression will be returned.
         * Otherwise, the parameter will be returned.
         */
        var e = expression
        if (this.inRange(0)) {
            val tok = this.getToken(0)
            if ((Tokens.EQ_LIST ++ Tokens.NEQ_LIST).contains(tok) && this.has_priority(Some(tok), context)) {
                this.nextToken() //swallow the "=" or "!="
                e = this.makeEqualityExpression(e, this.parseExpression(Some(tok)))
                if (Tokens.NEQ_LIST.contains(tok)) {
                    e = this.makeNegatedExpression(e)
                }
            }
        }
        return e
    }

    /**
     * This method serves as a hook for other logic parsers that
     * have different equality expression classes
     */
    protected def makeEqualityExpression(first: T, second: T): T

    /**
     * Attempt to make a boolean expression.  If the next token is a boolean
     * operator, then a BooleanExpression will be returned.  Otherwise, the
     * parameter will be returned.
     */
    protected def attemptBooleanExpression(expression: T, context: Option[String]): T = {
        var e = expression
        while (this.inRange(0)) {
            val tok = this.getToken(0)
            val factory = this.get_BooleanExpression_factory(tok)
            if (factory.isDefined && this.has_priority(Some(tok), context)) {
                this.nextToken() //swallow the operator
                e = this.makeBooleanExpression(factory.get, e, this.parseExpression(Some(tok)))
            } else
                return e
        }
        return e
    }

    /**
     * This method serves as a hook for other logic parsers that
     * have different boolean operators
     */
    protected def get_BooleanExpression_factory(tok: String): Option[((T, T) => T)]

    protected def makeBooleanExpression(factory: ((T, T) => T), first: T, second: T): T =
        return factory(first, second)

    /**
     * Attempt to make an application expression.  The next tokens are
     * a list of arguments in parens, then the argument expression is a
     * function being applied to the arguments.  Otherwise, return the
     * argument expression.
     */
    protected def attemptApplicationExpression(expression: T, context: Option[String]): T = {
        if (this.has_priority(APP, context)) {
            if (this.inRange(0) && this.getToken(0) == Tokens.OPEN) {
                if (!(expression.isInstanceOf[BaseLambdaExpression[T]]) &&
                    !(expression.isInstanceOf[BaseApplicationExpression[T]]) &&
                    !(expression.isInstanceOf[BaseVariableExpression[T]])) {
                    throw new ParseException(Some(this.getCurrentIndex),
                        "The function '" + expression + "' is not a Lambda Expression, an " +
                            "Application Expression, or a functional predicate, so it may not take arguments.", null)
                }
                this.nextToken() //swallow then open paren
                //curry the arguments
                var accum = this.makeApplicationExpression(expression, this.parseExpression(APP))
                while (this.inRange(0) && this.getToken(0) == Tokens.COMMA) {
                    this.nextToken() //swallow the comma
                    accum = this.makeApplicationExpression(accum, this.parseExpression(APP))
                }
                this.assertNextToken(Tokens.CLOSE)
                return accum
            }
        }
        return expression
    }

    protected def makeApplicationExpression(function: T, argument: T): T

    protected def makeVariableExpression(name: String): T

    protected def makeLambdaExpression(variable: Variable, term: T): T

    private def has_priority(operation: Option[String], context: Option[String]) =
        this.operatorPrecedence.get(operation).get < this.operatorPrecedence.get(context).get ||
            (this.right_associated_operations.contains(operation) && this.operatorPrecedence.get(operation).get == this.operatorPrecedence.get(context).get)

}

