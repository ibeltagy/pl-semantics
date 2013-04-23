package utcompling.scalalogic.discourse.candc.boxer.expression.parse

import utcompling.scalalogic.util.Counter
import utcompling.scalalogic.top.expression.parse._
import utcompling.scalalogic.top.expression.parse.UnexpectedTokenException
import utcompling.scalalogic.discourse.candc.boxer.expression._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.SetBuilder

/**
 * Parser for Boxer output
 */
class BoxerExpressionParser(discourseId: String = "0") extends LogicParser[BoxerExpression] {

    override protected val quoteChars = List(('\'', '\'', '\\', false))

    override protected def getAllSymbols() = List("(", ")", ",", "[", "]", ":")

    override protected def doParseExpression(context: Option[String] = None): BoxerExpression = {
        val tok0 = this.getToken(0)
        if (tok0 == "prs")
            this.parsePrs()
        else if (tok0 == "drs")
            this.parseDrs()
        else if (tok0 == "alfa")
            this.parseAlfa()
        else if (List("merge", "smerge") contains tok0)
            this.parseMerge()
        else if (tok0 == "lam")
            this.parseLam()
        else if (tok0 == "lam")
            this.parseApp()
        else if (tok0 == "date")
        	this.parseDate()
        else if (tok0 == "[" || tok0.startsWith("_"))  {
        	var predIdx = 0;
        	if (tok0 == "[" )
        		predIdx = this.findToken("]") + 2;
        	else 
        		predIdx = this.findToken(":") + 1; // This is to handle a bug 
        											//in the latest boxer with parameter
        										//--instantiate true.
        										//Sometimes, it outoputs "_G???" instead of 
        										// "[idx]:"	
            val pred = this.getToken(predIdx)
            if (pred == "not")
                this.parseNot()
            else if (pred == "imp")
                this.parseImp()
            else if (pred == "eq")
                this.parseEq()
            else if (pred == "pred")
                this.parsePred()
            else if (pred == "named")
                this.parseNamed()
            else if (pred == "rel")
                this.parseRel()
            else if (pred == "prop")
                this.parseProp()
            else if (pred == "card")
            	this.parseCard()
            else if (pred == "whq")
            	this.parseWhq()
            else if (pred == "or")
            	this.parseOr()
            else if (pred == "timex")
            	this.parseTimex()

            else
                throw new UnexpectedTokenException(predIdx, Some(pred), List("not", "imp", "eq", "pred", "rel", "named", "prop", "card", "whq", "or", "timex"))
        } else
            throw new UnexpectedTokenException(this.getCurrentIndex, Some(tok0), List("prs", "drs", "alfa", "merge", "smerge", "date"))
    }

    protected def parseDrs(): BoxerExpression = {
        this.assertNextToken("drs")
        this.assertNextToken("(")
        val refs = ListBuffer[(List[BoxerIndex], BoxerVariable)]()
        this.assertNextToken("[")
        while (this.getToken(0) != "]") {
            if (refs.nonEmpty)
                this.assertNextToken(",")
            val index = this.parseIndexList()
            this.assertNextToken(":")
            val ref = this.parseVariable()
            refs += index -> ref
        }
        this.assertNextToken("]")
        this.assertNextToken(",")

        val conds = ListBuffer[BoxerExpression]()
        this.assertNextToken("[")
        while (this.getToken(0) != "]") {
            if (conds.nonEmpty)
                this.assertNextToken(",")
            conds += this.doParseExpression()
        }
        this.assertNextToken("]")
        this.assertNextToken(")")

        return BoxerDrs(refs.result, conds.result)
    }
    
    protected def parsePrs(): BoxerExpression = {
        this.assertNextToken("prs")
        this.assertNextToken("(")

        val exps = ListBuffer[(BoxerExpression, Double)]()
        while (this.getToken(0) != ")") {
            if (exps.nonEmpty)
                this.assertNextToken(",")
            val p = this.doParseExpression()
            this.assertNextToken(",")
            val s = this.parseDouble()
            
            exps += ((p, s))
        }
        this.assertNextToken(")")

        return BoxerPrs(exps.result)
    }

    protected def parseAlfa(): BoxerExpression = {
        this.assertNextToken("alfa")
        this.assertNextToken("(")
        val variable = this.parseVariable()
        this.assertNextToken(",")
        val first = this.doParseExpression()
        this.assertNextToken(",")
        val second = this.doParseExpression()
        this.assertNextToken(")")
        return BoxerAlfa(variable, first, second)
    }

    protected def parseMerge(): BoxerExpression = {
        val pred = this.nextToken
        this.assertNextToken("(")
        val first = this.doParseExpression()
        this.assertNextToken(",")
        val second = this.doParseExpression()
        this.assertNextToken(")")
        return BoxerMerge(pred, first, second)
    }
    
    protected def parseLam(): BoxerExpression = {
        this.assertNextToken("lam")
        this.assertNextToken("(")
        val variable = this.parseVariable()
        this.assertNextToken(",")
        val term = this.doParseExpression()
        this.assertNextToken(")")
        return BoxerLam(variable, term)
    }

    protected def parseApp(): BoxerExpression = {
        this.assertNextToken("lam")
        this.assertNextToken("(")
        val function = this.doParseExpression()
        this.assertNextToken(",")
        val argument = this.doParseExpression()
        this.assertNextToken(")")
        return BoxerApp(function, argument)
    }

    protected def parseNot(): BoxerExpression = {
        val indices = this.parseIndexList()
        this.assertNextToken(":")
        this.assertNextToken("not")
        this.assertNextToken("(")
        val drs = this.doParseExpression()
        this.assertNextToken(")")
        return BoxerNot(this.discourseId, indices, drs)
    }

    protected def parseImp(): BoxerExpression = {
        val indices = this.parseIndexList()
        this.assertNextToken(":")
        this.assertNextToken("imp")
        this.assertNextToken("(")
        val first = this.doParseExpression()
        this.assertNextToken(",")
        val second = this.doParseExpression()
        this.assertNextToken(")")
        return BoxerImp(this.discourseId, indices, first, second)
    }

    protected def parseEq(): BoxerExpression = {
        val indices = this.parseIndexList()
        this.assertNextToken(":")
        this.assertNextToken("eq")
        this.assertNextToken("(")
        val first = this.parseVariable()
        this.assertNextToken(",")
        val second = this.parseVariable()
        this.assertNextToken(")")
        return BoxerEq(this.discourseId, indices, first, second)
    }

    protected def parsePred(): BoxerExpression = {
        val indices = this.parseIndexList()
        this.assertNextToken(":")
        this.assertNextToken("pred")
        this.assertNextToken("(")
        val variable = this.parseVariable()
        this.assertNextToken(",")
        val name = this.nextToken()
        this.assertNextToken(",")
        val pos = this.nextToken()
        this.assertNextToken(",")
        val sense = this.parseInt()
        this.assertNextToken(")")
        return BoxerPred(this.discourseId, indices, variable, name, pos, sense)
    }

    protected def parseNamed(): BoxerExpression = {
        val indices = this.parseIndexList()
        this.assertNextToken(":")
        this.assertNextToken("named")
        this.assertNextToken("(")
        val variable = this.parseVariable()
        this.assertNextToken(",")
        val name = this.nextToken()
        this.assertNextToken(",")
        val typ = this.nextToken()
        this.assertNextToken(",")
        val sense = this.parseInt()
        this.assertNextToken(")")
        return BoxerNamed(this.discourseId, indices, variable, name, typ, sense)
    }

    protected def parseRel(): BoxerExpression = {
        val indices = this.parseIndexList()
        this.assertNextToken(":")
        this.assertNextToken("rel")
        this.assertNextToken("(")
        val event = this.parseVariable()
        this.assertNextToken(",")
        val variable = this.parseVariable()
        this.assertNextToken(",")
        val name = this.nextToken()
        this.assertNextToken(",")
        val sense = this.parseInt()
        this.assertNextToken(")")
        return BoxerRel(this.discourseId, indices, event, variable, name, sense)
    }

    protected def parseProp(): BoxerExpression = {
        val indices = this.parseIndexList()
        this.assertNextToken(":")
        this.assertNextToken("prop")
        this.assertNextToken("(")
        val variable = this.parseVariable()
        this.assertNextToken(",")
        val drs = this.doParseExpression()
        this.assertNextToken(")")
        return BoxerProp(this.discourseId, indices, variable, drs)
    }

    protected def parseVariable(): BoxerVariable = {
    	return BoxerVariable(this.nextToken());
    }

    protected def parseIndexList(): List[BoxerIndex] = {
    	val nextTok =  this.nextToken();
    	if (nextTok.startsWith("_") )  //this is to handle the --instantiate bug
    	  return List ();
    	else if (!nextTok.equals("["))
    	  throw new UnexpectedTokenException (this.getCurrentIndex, Some(nextTok), List("["));
    	//this.assertNextToken("[")
        val indices = new ListBuffer[BoxerIndex]
        while (this.getToken(0) != "]") {
            if (indices.nonEmpty)
                this.assertNextToken(",")
            indices += this.parseIndex()
        }
        this.assertNextToken("]")
        return indices.result
    }

    protected def parseIndex(): BoxerIndex = {
        val index = this.parseInt()
        return BoxerIndex(index)
    }

    protected def parseInt(): Int = {
        try {
            return this.nextToken().toInt
        } catch {
            case pe: ParseException => throw pe
            case e => throw new UnexpectedTokenException(this.getCurrentIndex - 1, Some(this.getToken(-1)), message = Some("Expected an Integer."), nested = e)
        }
    }
    
    protected def parseDouble(): Double = {
        try {
            return this.nextToken().toDouble
        } catch {
            case pe: ParseException => throw pe
            case e => throw new UnexpectedTokenException(this.getCurrentIndex - 1, Some(this.getToken(-1)), message = Some("Expected a Double."), nested = e)
        }
    }
    
    
    protected def parseCard(): BoxerExpression = {
        val indices = this.parseIndexList()
        this.assertNextToken(":")
        this.assertNextToken("card")
        this.assertNextToken("(")
        val variable = this.parseVariable()
        this.assertNextToken(",")
        val num = this.nextToken()
        this.assertNextToken(",")
        val typ = this.nextToken()
        this.assertNextToken(")")
        return BoxerCard(this.discourseId, indices, variable, num, typ)
    }
    protected def parseWhq(): BoxerExpression = {
        //val pred = this.nextToken
        val pred = this.parseIndexList()
        this.assertNextToken(":")
        this.assertNextToken("whq")
        this.assertNextToken("(")
        this.assertNextToken("[")
        if (this.nextToken != "]"){
             this.assertNextToken(":")
             var answerType2 = this.nextToken()
             this.assertNextToken("]")
        }
       
        this.assertNextToken(",")
        val first = this.doParseExpression()
        this.assertNextToken(",")
        val recipient = this.parseVariable()
        this.assertNextToken(",")
        val second = this.doParseExpression()
        this.assertNextToken(")")
        return BoxerMerge("merge", first, second) //our system is not interested in questions now
        					//so, replece all questions with a merge for the two parts of the question.  
    }
    
    protected def parseOr(): BoxerExpression = {
        val indices = this.parseIndexList()
        this.assertNextToken(":")
        this.assertNextToken("or")
        this.assertNextToken("(")
        val first = this.doParseExpression()
        this.assertNextToken(",")
        val second = this.doParseExpression()
        this.assertNextToken(")")
        return BoxerOr(this.discourseId, indices, first, second)
    }
    protected def parseTimex(): BoxerExpression = {
        val indices = this.parseIndexList()
        this.assertNextToken(":")
        this.assertNextToken("timex")
        this.assertNextToken("(")
        val variable = this.parseVariable()
        this.assertNextToken(",")
        val timeExp = this.doParseExpression()
        this.assertNextToken(")")
        return BoxerTimex(this.discourseId, indices, variable, timeExp)
    }
    protected def parseDate(): BoxerExpression = {
        this.assertNextToken("date")
        this.assertNextToken("(")
        val indicesPol = this.parseIndexList()
        this.assertNextToken(":")
        //this.assertNextToken("(")
        val pol = this.nextToken()
        //this.assertNextToken(")")
        this.assertNextToken(",")
        val indicesYear = this.parseIndexList()
        this.assertNextToken(":")
        val year = this.nextToken()
        this.assertNextToken(",")
        val indicesMonth = this.parseIndexList()
        this.assertNextToken(":")
        val month = this.nextToken()
        this.assertNextToken(",")
        val indicesDay = this.parseIndexList()
        this.assertNextToken(":")
        val day = this.nextToken()
        this.assertNextToken(")")

        return BoxerDate(indicesPol, pol, indicesYear, year, indicesMonth, month, indicesDay, day)
    
    }
}
    
