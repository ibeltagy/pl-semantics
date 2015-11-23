package utcompling.scalalogic.top.expression.parse

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.MapBuilder
import scala.collection.mutable.HashMap
import scala.collection.SeqView

abstract class LogicParser[T] {

    private var currentIndex = 0

    final protected def getCurrentIndex() =
        currentIndex

    private var buffer = List[String]()

    /**
     * A list of tuples of quote characters.  The 4-tuple is comprised
     * of the start character, the end character, the escape character, and
     * a boolean indicating whether the quotes should be included in the
     * result. Quotes are used to signify that a token should be treated as
     * atomic, ignoring any special characters within the token.  The escape
     * character allows the quote end character to be used within the quote.
     * If True, the boolean indicates that the final token should contain the
     * quote and escape characters.
     * This method exists to be overridden
     */
    protected val quoteChars = List[(Char, Char, Char, Boolean)]()

    /**
     * Parse the expression.
     *
     * @param data: C{str} for the input to be parsed
     * @param signature: C{dict<str, str>} that maps variable names to type
     * strings
     * @returns: a parsed Expression
     */
    final def parse(data: String): T = {
        this.beforeParse()

        //I do not want the character " ' " in predicates names because it confuses 
        //our parser.
        //Update: It is not here. It is in processQuotedToken
        var trimmed = """\n[ \t]*""".r.replaceAllIn(data, " ").trim()//.replace("'", "");

		//in MAC, Boxer's output is slightly different. This condition fixes it.
		if (trimmed.endsWith("').'"))
			trimmed = trimmed.dropRight(4)
		
        this.currentIndex = 0
		
        
        val (buffer, mapping) = this.process(trimmed)
        this.buffer = buffer

        try {
            val result = this.parseExpression(None)
            if (this.inRange(0))
                throw new UnexpectedTokenException(this.currentIndex + 1, Some(this.getToken(0)))
            return result
        } catch {
            case e: ParseException => {
                throw new ParseException(None, "%s\n%s\n%s^".format(e, trimmed, " " * mapping(e.index.get)), e)
            }
        }
    }

    protected def beforeParse() = {
        // Do Nothing
    }

    /**Parse the next complete expression from the stream and return it.*/
    final protected def parseExpression(context: Option[String] = None): T = {
        try {
            val tok = this.getToken(0)

            val accum = this.doParseExpression(context)

            if (accum == None)
                throw new UnexpectedTokenException(this.currentIndex, Some(tok), List("Expression expected."))

            return this.attemptAdjuncts(accum, context)
        } catch {
            case e: ParseException => throw e
            case e => throw new ParseException(Some(this.currentIndex), "Unparseable Input", e)
        }
    }

    /**
     * This method is intended to be overridden for logics that
     * use different operators or expressions
     */
    protected def doParseExpression(context: Option[String] = None): T

    protected def attemptAdjuncts(expression: T, context: Option[String] = None): T =
        expression

    /**Split the data into tokens*/
    private def process(data: String): (List[String], Map[Int, Int]) = {
        val out = ListBuffer[String]()
        val mapping = new MapBuilder[Int, Int, Map[Int, Int]](Map[Int, Int]())
        val tokenTrie = new StringTrie(this.getAllSymbols())
        var token = new StringBuilder
        var data_idx = 0
        var token_start_idx = data_idx
        while (data_idx < data.length) {
            val (quoted_token, idx_inc) = processQuotedToken(data.view(data_idx, data.length).iterator)
            quoted_token match {
                case Some(quoted_token) => {
                    if (token.isEmpty)
                        token_start_idx = data_idx
                    data_idx += +idx_inc
                    token.append(quoted_token)
                }
                case _ => {
                    val symbol = this.attemptSymbolMatch(data.view(data_idx, data.length).iterator, tokenTrie)
                    if (symbol.isDefined) {
                        //token is a complete symbol
                        if (token.nonEmpty) {
                            mapping += out.length -> token_start_idx
                            out.append(token.toString)
                            token = new StringBuilder
                        }
                        mapping += out.length -> data_idx
                        out.append(symbol.get)
                        data_idx += symbol.get.length
                    } else {
                        if (" \t\n".contains(data(data_idx))) { //any whitespace
                            if (token.nonEmpty) {
                                mapping += out.length -> token_start_idx
                                out.append(token.toString)
                                token = new StringBuilder
                            }
                        } else {
                            if (token.isEmpty)
                                token_start_idx = data_idx
                            token += data(data_idx)
                        }
                        data_idx += 1
                    }
                }
            }
        }
        if (token.nonEmpty) {
            mapping += out.length -> token_start_idx
            out.append(token.toString)
        }
        mapping += out.length -> data.length
        mapping += out.length + 1 -> (data.length + 1)
        return (out.result, mapping.result)
    }

    private def processQuotedToken(data: Iterator[Char]): (Option[String], Int) = {
        var c = data.next
        var count = 0
        for ((start, end, escape, incl_quotes) <- this.quoteChars if start == c) {
            count += 1
            try {
                val token = new StringBuilder
                if (incl_quotes)
                    token += c
                c = data.next
                count += 1
                while (c != end) {
                    if (c == escape) {
                        if (incl_quotes)
                            token += c
                        try {
                            c = data.next
                            count += 1
                        } catch {
                            case e: IndexOutOfBoundsException => throw new ExpectedMoreTokensException(Some(getCurrentIndex + token.length), Some("Escape character [" + escape + "] found at end."), e)
                        }
                        c match {
                            case `escape` | `end` =>
                                token += c
                            case _ =>
                                token += escape;
                                token += c;
                                //throw new UnexpectedTokenException(getCurrentIndex + token.length, Some(c.toString), List(end.toString, escape.toString), Some("Escape character not followed by end quote or escape."))
                        }
                    } else
                        token += c
                    c = data.next
                    count += 1
                }
                if (incl_quotes)
                    token += c
                if (token.isEmpty)
                    throw new ParseException(None, "Empty quoted token found")
                //This text was quated that means it has some special characters. 
                //Remove all these special characters before returning the token.
                //Update: All quotes remain
                return (Some(token.toString/*.replaceAll("'", "")*/), count)
            } catch {
                case e: NoSuchElementException => throw new ExpectedMoreTokensException(Some(getCurrentIndex), Some("End of input reached.  Start quote character [" + start + "] not followed by end quote character [" + end + "]"), e)
            }
        }
        return (None, 0)
    }

    private def attemptSymbolMatch(data: Iterator[Char], tokenTrie: StringTrie): Option[String] = {
        val symbol = new StringBuilder
        var st = tokenTrie
        var c = data.next
        while (st.contains(c)) {
            symbol += c
            st = st(c)
            if (data.hasNext)
                c = data.next
            else {
                if (st.isTerminated)
                    return Some(symbol.toString)
                else
                    return None
            }
        }
        if (st.isTerminated)
            return Some(symbol.toString)
        else
            return None
    }

    /**This method exists to be overridden*/
    protected def getAllSymbols(): Iterable[String]

    /**Return TRUE if the given location is within the buffer*/
    final protected def inRange(location: Int) = this.currentIndex + location < this.buffer.length

    /**
     * Get the next waiting token.
     */
    final protected def nextToken(): String = {
        try {
            var tok = this.buffer(this.currentIndex)
            this.currentIndex += 1
            return tok
        } catch {
            case e: IndexOutOfBoundsException => throw new ExpectedMoreTokensException(Some(this.currentIndex + 1), e)
        }
    }

    /**
     * Return the token at currentIndex+location without advancing
     * currentIndex.  This gives lookahead/lookback capability.
     */
    final protected def getToken(location: Int): String = {
        try {
            return this.buffer(this.currentIndex + location)
        } catch {
            case e: IndexOutOfBoundsException => throw new ExpectedMoreTokensException(Some(this.currentIndex + location), e)
        }
    }

    /**
     * Return the index of the next occurrence of the specified token
     */
    final protected def findToken(tok: String, startLocation: Int = 0) =
        this.buffer.indexOf(tok, this.currentIndex + startLocation) match {
            case -1 => throw new UnexpectedTokenException(this.currentIndex + startLocation, expected = List(tok), message = Some("Expected to see token after this point."))
            case i => i - this.currentIndex
        }

    protected def assertNextToken(expected: String) = {
        try {
            val tok = this.nextToken()
            if (tok != expected)
                throw new UnexpectedTokenException(this.getCurrentIndex, Some(tok), List(expected))
        } catch {
            case e: ExpectedMoreTokensException => throw new ExpectedMoreTokensException(e.index, Some("Expected token '" + expected + "'."), e)
        }
    }

    protected def assertNextToken(expected: List[String]) = {
        try {
            val tok = this.nextToken()
            if (!expected.contains(tok))
                throw new UnexpectedTokenException(this.getCurrentIndex, Some(tok), expected)
        } catch {
            case e: ExpectedMoreTokensException => throw new ExpectedMoreTokensException(e.index, Some("Expected token '" + expected + "'."), e)
        }
    }

    protected def assertToken(tok: String, expected: String) = {
        if (tok != expected)
            throw new UnexpectedTokenException(this.getCurrentIndex, Some(tok), List(expected))
    }

    protected def assertToken(tok: String, expected: List[String]) = {
        if (!expected.contains(tok))
            throw new UnexpectedTokenException(this.getCurrentIndex, Some(tok), expected)
    }

    override def toString(): String = {
        val span = 5

        val before =
            if (this.currentIndex < span)
                this.buffer.take(this.currentIndex)
            else
                "..." +: this.buffer.take(this.currentIndex).takeRight(span)

        if (this.inRange(0)) {
            val after =
                if (this.buffer.length - this.currentIndex < span)
                    this.buffer.drop(this.currentIndex + 1)
                else
                    this.buffer.drop(this.currentIndex + 1).take(span) :+ "..."
            return (before ++ List("[" + this.buffer(this.currentIndex) + "]") ++ after).mkString(" ")
        } else
            return "No more Tokens: " + before.mkString(" ")
    }

    private class StringTrie(private val strings: Iterable[String] = List[String]()) extends HashMap[Char, StringTrie] {

        private val LEAF = "<leaf>"
        private var terminated = false

        strings.foreach(this.insert)

        private def insert(string: String) {
            if (string.nonEmpty)
                this.getOrElseUpdate(string.head, new StringTrie()).insert(string.tail)
            else
                this.terminate()
        }

        def isTerminated() =
            terminated

        def terminate() {
            this.terminated = true
        }

    }
}

class ParseException(val index: Option[Int], val message: String, val nested: Throwable = null) extends RuntimeException(message, nested) {
    def this(message: String, nested: Throwable) =
        this(None, message, nested)

    def this(message: String) =
        this(None, message)
}

class UnexpectedTokenException(index: Int, unexpected: Option[String] = None, expected: List[String] = List(), message: Option[String] = None, nested: Throwable = null)
    extends ParseException(Some(index),
        unexpected match {
            case Some(unexpected) => {
                if (expected.isEmpty)
                    format("Unexpected token: '%s'.  %s", unexpected, message.getOrElse(""))
                else if (expected.length == 1)
                    format("Unexpected token: '%s'.  Expected token '%s'.", unexpected, expected.head)
                else
                    format("Unexpected token: '%s'.  Expected one of %s.", unexpected, expected.mkString("[", ", ", "]"))
            }
            case None => format("Expected token '%s'.", expected)
        }, nested)

class ExpectedMoreTokensException(override val index: Option[Int], message: Option[String] = None, override val nested: Throwable = null)
    extends ParseException(index, "End of input found.  " + message.getOrElse("More tokens expected."), nested) {
    def this(index: Option[Int], nested: Throwable) =
        this(index, None, nested)
}


