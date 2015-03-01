/****
 * Robinson resolution
 * used to determine the difference between two formulas in CNF
 * 
 * Usually, Robinson resolution works like this:
 * Given two formulas F, G, form CNF(F & not(G))
 * then try to derive the empty clause in order to conclude that F |= G
 * Do search to find all possible ways to apply resolution to pairs of clauses,
 * as F |= G follows if we can derive the empty clause in any branch of the search tree.
 *
 * In our case, things work differently.
 * 
 * Given two formulas F, G in CNF
 * that we know represent two synonymous sentences,
 * use Robinson resolution but keep F, G separate
 * so we always know which clause came from F and which came from G.
 * Use resolution to remove common elements in F and G,
 * leaving behind the difference between F and G.
 * 
 * The aim is to derive an inference rule F' -> G'
 * where F' is a sub-formula of F, G' is a sub-formula of G,
 * and the meaning of "F' -> G'" is
 * "Given any formula H, you can rewrite a sub-formula F' of H to G'
 * without changing the semantics".
 * 
 * Do not use search, as we want to derive a _single_ inference rule "F' -> G'".
 * So we only do resolution on two clauses Cf of F and Cg of G if they contain
 * literals Lf, not(Lg) such that Lf, Lg are unifiable and
 * there is no other positive literal in F that Lg could unify with, and
 * there is no other negative literal in G that Lf could unify with.
 *
 * Method:
 * Keep F, G separate.
 * When doing resolution on clauses Cf of F and Cg of G such that Cf is a singleton:
 * remove Cf from F, remove the resolution literal from Cg.
 * If Cg becomes empty (that is, resolution succeeds in deriving the empty clause),
 * raise an EmptyClauseException, because in our setting we do not expect
 * F and G to be logically contradictory.
 * When doing resolution on clauses Cf of F and Cg of G such that neither Cf and Cg is singleton:
 * add the resulting clause Cr to both F and G.
 * In F, mark all those literals in Cr that came from Cg as "needs to be removed".
 * In G, mark all those literals in Cr that come from Cf as "needs to be removed."
 * If at the end of resolution, there are any literals left with a "needs to be removed" flag,
 * fail.
 *
 * (The case for singleton Cf is already implemented. The case for non-singleton Cf and Cg
 * is not implemented yet.)
 *
 * Transforming the result of a successful resolution run to an inference rule:
 * TO BE DONE.
***/

package utcompling.mlnsemantics.util

import scala.collection._
import scala.util.matching.Regex
import scala.util.control.Exception._
import scala.Option.option2Iterable
import utcompling.mlnsemantics.datagen.Lemmatize


//----------------------------------------------------------
// custom exception:
// In our case, we don't want to derive the empty clause,
// as that means that G follows from F logically, without 
// application of any rewriting rule
case class EmptyClauseException(data:String) extends Exception

// another custom exception:
// unexpected format in formula
case class UnexpectedFormatOfFormulaException(data:String) extends Exception

 
object Utils
{
	//----------------------------------------------------------
	// a substitution is a mapping from variables to terms
	type Substitution = mutable.Map[String,String];
	def newSubstitution = mutable.Map[String,String]()
	def newSubstitutionInit(values:(String,String)*) :Substitution = {
	  newSubstitution ++ values
	}	
}

object Equivalences 
{
	def isEquivalent (l: String, r:String):Boolean = {
		if (l == r) return true;
		if (Lemmatize.lemmatizeWord(l) == Lemmatize.lemmatizeWord(r)) return true;
		//if (l == "deer" && r == "wall") return true;
		//if (l == "fence" && r == "deer") return true;
		return false;
	}
}


//----------------------------------------------------------
// Given a literal (without negation), separate predicate and arguments
// and return them as a pair (predicate, argList)
// predicate is a string, and argList is a list of strings
//
// Given a possibly negated literal, take it apart into 
// positive literal and potential negation
trait TakeLiteralApart {
  // two regular expressions that take apart predicate and arguments
  val predArgRegex = new Regex("""^(.*?)\((.*?)\)$""", "pred", "argstring")
  val atomRegex = new Regex("""^([^(),]*)$""", "atom")
  // identify negated literal
  val negationRegex = new Regex("""^\-(.*?)$""", "pL")

  // use the regular expressions to take apart literals and detect atoms
  def separatePredArg(literalString: String) :Pair[String, List[String]] = {
    // do we have a predicate with arguments?
    predArgRegex findFirstIn literalString match {

      case Some(predArgRegex(pred, argstring)) => (pred, splitArgstring(argstring))

      // failing that, do we have an atom?
      case None => atomRegex findFirstIn literalString match { 

	case Some(atomRegex(atom)) => (atom, List[String]())
	case None => throw new UnexpectedFormatOfFormulaException("expected atom or literal, got " + literalString)
      }
    }
  }

  // split a string that represents a sequence of comma-separated arguments:
  // don't split where a comma is embedded in an argument, as in f(X, Y)
  def splitArgstring(argstring:String, priorBracketLevel:Int = 0, index:Int = 0, startOfArg:Int = 0) :List[String] = {
    if (index >= argstring.size) List(argstring.substring(startOfArg).replaceAll("""\s+""", ""))
    else {
      argstring.charAt(index) match {
	case '(' =>  splitArgstring(argstring, priorBracketLevel + 1, index + 1, startOfArg)
        case ')' =>  splitArgstring(argstring, priorBracketLevel - 1, index + 1, startOfArg)
	case ',' =>  if (priorBracketLevel == 0) { 
	  argstring.substring(startOfArg, index).replaceAll("""\s+""", "") :: 
	  splitArgstring(argstring, priorBracketLevel, index + 1, index + 1)
	  } else { splitArgstring(argstring, priorBracketLevel, index + 1, startOfArg) }
	case _ => splitArgstring(argstring, priorBracketLevel, index + 1, startOfArg)
      }
    }
  }

  // Note that we assume only a single negation symbol, no repeated negation
  // as we assume the formula is in CNF
  def separateLiteralAndNegation(literalString:String) :(String,Boolean) = {
    negationRegex findFirstIn literalString match {
    case Some(negationRegex(pL)) => (pL, true)
    case None => (literalString, false)
    }
  }

  def negateStringLiteral(literalString:String) : String = {
    val (posL, lIsNegated) = separateLiteralAndNegation(literalString)
      if (lIsNegated) { posL }
      else { "-" + posL }
  }
}


class TestTakeLiteralApart extends TakeLiteralApart {
  def testme :Unit = {
    List("p(f(a), g(g(X)), Y)", "p", "q(f(a, b), X, Y,   f(X, Y))").foreach { literal =>
      println("Taking apart " + literal + " : ")
      val (pred, args) = separatePredArg(literal)
      println("P=" + pred.toString + ".")
      args.foreach ( a => println("a: " + a))
    }

    println
    println("testing separating literal and negation: -p(a) : " + separateLiteralAndNegation("-p(a)"))
    println("testing separating literal and negation: q(X,Y,Z) : " + separateLiteralAndNegation("q(X,Y,Z)"))
    println("testing separating literal and negation: X : " + separateLiteralAndNegation("X"))
  }
}

//----------------------------------------------------------
trait VariableFindAndReplace {
  // distinguishing variables from constants/function symbols:
  // variables start with an uppercase letter
  def isVariable(term:String) :Boolean = { term(0).isUpper }

  // taking apart a term by splitting on "word boundaries"
  // this will split at commas and parentheses.
  // Note that sequences of parentheses will stay as they are
  def takeTermApart(term:String) :List[String] = {
    term.split("""\s*\b\s*""").toList.filter (s => s != "")
  }

  // occurs check: does the given variable occur in the given term?
  def occursCheck(variable:String, term:String) :Boolean = {
    takeTermApart(term).contains(variable)
  }

  def applySubstitution(term:String, substitution:Utils.Substitution) :String = {
    takeTermApart(term).map (subterm =>
      if (substitution.contains(subterm)) { substitution(subterm) }
      else { subterm }).mkString("")
  }
}

class TestVariableFindAndReplace extends VariableFindAndReplace {
  def testme :Unit = {
    println("testing isVariable on X:" + isVariable("X"))
    println("testing isVariable on p(X):" + isVariable("p(X)"))
    println("testing isVariable on aX:" + isVariable("aX"))
    println

    println("testing takeTermApart on p(f(a, XX),   g(g(g(a_23Q))), Y,Z):")
    takeTermApart("p(f(a, XX),   g(g(g(a_23Q))), Y,Z)").foreach (piece =>
      println("--" + piece.toString + "--"))
    println

    println("testing occursCheck on X and p(f(g(g(Y,aX)))): " + occursCheck("X", "p(f(g(g(Y,aX))))"))
    println("testing occursCheck on X and p(f(g(g(Y,X)))): " +  occursCheck("X", "p(f(g(g(Y,X))))"))
    println

    println("testing applySubstitution on p(g(g(Y,X, aX)), Z,bY) and X->c,Y->d : " + 
	    applySubstitution("p(g(g(Y,X, aX)), Z,bY)", Utils.newSubstitutionInit("X" -> "c", "Y" -> "d")))
    
    println("testing applySubstitution on U and U->a,Y->d : " + 
	    applySubstitution("U", Utils.newSubstitutionInit("U" -> "a", "Y" -> "d")))
    
  }
}

//----------------------------------------------------------
// compute a substitution using the naive recursive approach of Baader
trait Unification extends TakeLiteralApart with VariableFindAndReplace {

  // add a pair x |-> term to a substitution,
  // and at the same time apply the new substitutions to all right-hand sides 
  // in the existing substitution
  def addSubstitution( t1:String, t2:String, substitution:Utils.Substitution) :Utils.Substitution = {
    val t1ToT2:Utils.Substitution = Utils.newSubstitutionInit(t1 -> t2)
    
    val pairs = substitution.map { case (lhs, rhs) => (lhs, applySubstitution(rhs, t1ToT2)) }
    
    t1ToT2 ++ pairs
  }

  def unifyTerms(t1O:String,t2O:String, substitution:Utils.Substitution) :Option[Utils.Substitution] = {
    val t1:String = applySubstitution(t1O, substitution)
    val t2:String = applySubstitution(t2O, substitution)
    
    if (isVariable(t1)) {
      if (t1 == t2) { new Some(substitution) }
      else if (occursCheck(t1, t2)) { None }
      else {
	new Some(addSubstitution(t1, t2, substitution))
      }
    } else if (isVariable(t2)) {
      if (occursCheck(t2, t1)) { None }
      else {
	new Some(addSubstitution(t2, t1, substitution))
      }
    } else {
      val (pred1, args1) = separatePredArg(t1)
      val (pred2, args2) = separatePredArg(t2)
      if (pred1 == pred2 && args1.size == args2.size) {
	// Make pairs of matching arguments from 1st and 2ns term,
	// then iterate through them starting with an Option on an empty substitution.
	// Each successive term pair adds to the substitution, or changes the Option to None.
	// Once it is changed to None, it stays None.
	args1.zip(args2).foldLeft(new Some(substitution):Option[Utils.Substitution]) { (optsubs, tpair) => 
	  optsubs.flatMap (subs => unifyTerms(tpair._1, tpair._2, subs)) }
      } else { None }
    }
  }
    
  // given two lists of terms, compute a substitution
  // that unifies them.
  def computeSubstitution(l1:List[String], l2:List[String]) : Option[Utils.Substitution] = { 
    if (l1.size != l2.size) {
      throw new Exception("error in computeSubstitution: term lists differ in length " + l1.toString + " " + l2.toString)
    }

    // pair up terms in l1 with terms in l2, 
    // and try to unify them.
    // unifyTerms returns Option[Substitution].
    // if we got None for some term pair, just carry the None to the end of the foldLeft.
    // Otherwise, each call to unifyTerms starts with the substitution that the 
    // previous call generated.
    l1.zip(l2).foldLeft(new Some(Utils.newSubstitution):Option[Utils.Substitution]) { (optsubs, tpair) => 
	  optsubs.flatMap (subs => unifyTerms(tpair._1, tpair._2, subs)) }
  }
}

class TestUnification extends Unification {
  def testme :Unit = {
    val subs = addSubstitution("X", "a", Utils.newSubstitution)
    println("testing addSubstitution: adding X->a to an empty substitution : "+  subs)
    val subs2 = addSubstitution("Y", "Z", subs)
    println("testing addSubstitution: adding Y->Z to the previous substitution : " +  subs2)
    val subs3 = addSubstitution("Z", "f(U)", subs2)
    println("testing addSubstitution: adding Z->f(U) to the previous  substitution : " +  subs3)
    val subs4 = addSubstitution("U", "b", subs3)
    println("testing addSubstitution: adding U->b to the previous  substitution  : " +  subs4)

    println

    println("testing unification: Y=X,a=X,Z=f(f(Y)) : " + computeSubstitution(List("Y", "a", "Z"), List("X", "X", "f(f(Y))")))     
    println("testing unification: X=Y,Y=Z,Z=X : " + computeSubstitution(List("X", "Y", "Z"), List("Y", "Z", "X")))
    println("testing unification: X=X : " + computeSubstitution(List("X"), List("X")))
    println("testing unification: X=Y,Y=Z,Z=f(X) : " + computeSubstitution(List("X", "Y", "Z"), List("Y", "Z", "f(X)")))
    println("testing unification: g(g(a))=g(g(a)),f(X)=f(h(b)) : " + computeSubstitution(List("g(g(a))", "f(X)"), List("g(g(a))", "f(h(b))")))
  }
}


//----------------------------------------------------------
// literal class
// keeps the literal as a string,
// the polarity of the literal (isNegated),
// and the predicate and list of arguments as individual strings
class Literal(val originalLiteralString: String) extends Unification {

  // posLiteralString is the literal as string, without negation.
  // isNegated is true if literalString is negated.
  val (posLiteralString, isNegated) : (String, Boolean) = separateLiteralAndNegation(originalLiteralString)

  var (predSymbol, argList) = separatePredArg(posLiteralString)

  // arity: number of arguments
  def arity :Int = argList.size

  // content literal:
  // This is specific to Johan Bos' Boxer system, 
  // which uses Neo-Davidsonian semantics!
  // A content literal has only one argument, and its predicate is not 'event'
  def isContent : Boolean = {
    arity == 1 && predSymbol != "event"
  }

  // grounded literal: does not contain any variables
  // convert to string to get the string variant of the current form of this literal,
  // then break the term apart at "word boundaries" to get all terms and variables at all embedding depths,
  // then test that none of those pieces is a variable.
  def isGrounded : Boolean = {
    takeTermApart(this.toString).forall ( piece => !(isVariable(piece)))
  }

  // possibleMatch with other literal:
  // if they differ in negation, but have the same predicate symbol and arity
  def possibleMatch(otherL:Literal) :Boolean = {
    otherL.isNegated != isNegated &&
    Equivalences.isEquivalent(otherL.predSymbol, predSymbol) &&
    otherL.arity == arity
  }

  // check if this literal unifies with another literal.
  // input: both literals' argument lists.
  // try this only if both literals have the same predicate symbols and arity,
  // and if one of them is negated and the other is not.
  def unifiableAndNegated(otherL:Literal) : Option[Utils.Substitution] = {
    if (possibleMatch(otherL)) { computeSubstitution(argList, otherL.argList) }
    else { None }
  }

  // Two literals are the same if they have the same predicate and arguments.
  // Don't use literalString as a basis, as that can have extra whitespace
  override def equals(that:Any) :Boolean = {
    that.isInstanceOf[Literal] && 
    Equivalences.isEquivalent(predSymbol, that.asInstanceOf[Literal].predSymbol) && 
    argList == that.asInstanceOf[Literal].argList &&
    isNegated == that.asInstanceOf[Literal].isNegated
  }

  // Two literals are opposites if they are the same except for their polarity
  def isNegationOf(that:Any) :Boolean = {
    that.isInstanceOf[Literal] && 
    Equivalences.isEquivalent(predSymbol, that.asInstanceOf[Literal].predSymbol) && 
    argList == that.asInstanceOf[Literal].argList &&
    isNegated != that.asInstanceOf[Literal].isNegated
  }

  def applySubstitution(substitution:Utils.Substitution) : Unit = {
    argList = argList.map( a => applySubstitution(a, substitution))
  }

  // for inspection: represent this class as the literal string
  override def toString :String = (if (isNegated) "-" else "") + 
  predSymbol + 
  (if (argList.size > 0) "(" + argList.mkString(",") + ")"
   else "") 
}

class TestLiteral {
  def testme :Unit = { 
    val litlist = List("p(a, f(X), g(g(g(Y,X))))", "-X", "-g(Y)")
    litlist.foreach { ls =>
      val l = new Literal(ls)
      println("posLiteral, isNegated, predicate for " + ls + " : " + l.posLiteralString + " " + l.isNegated + 
	      " " + l.predSymbol)
      l.argList.foreach (a => println("argument " + a))
      println("arity: " + l.arity)
      println("isContent: " + l.isContent)
    }
    println

    val litpairs = List(("p(a)", "-p(b)"), ("p(a)", "-p(X)"), ("p(X)", "-p(p(X))"), ("p(X)", "-p(a, b)"), 
			("p(a, b, c)", "p(a, b, c)"), ("p(a, b, g(c))", "p(a, b,  g( c ))"),
			("p(a, b, c)", "-p(  a,b,   c )"), ("X", "-X"), ("p(X, Y)", "-p(Y, Z)"))

    litpairs.foreach { case (l1, l2) =>
		       val l1L = new Literal(l1)
		       val l2L = new Literal(l2)
		       println("Comparing: " + l1L.toString + " || " + l2L.toString)
		       println("possible match: " + l1L.possibleMatch(l2L))
		       println("unifiableAndNegated: " + l1L.unifiableAndNegated(l2L))
		       println("equals: " + l1L.equals(l2L))
		       println("isNegationOf: " + l1L.isNegationOf(l2L))
		     }
    
    println
    var l1 = new Literal("p(X, g(g(Y)), ZZ, f(Z))")
    val subst = Utils.newSubstitutionInit("X"->"a", "Y"->"b", "Z"->"c")
    println("applying substitution to " + l1 + " : " + subst)
    l1.applySubstitution(subst)
    println("result: " + l1)

    println
    val l2 = new Literal("-p(f(a, b, c, g(g(X))))")
    println("is -p(f(a, b, c, g(g(X)))) grounded? " + l2.isGrounded)
    l2.applySubstitution(Utils.newSubstitutionInit("X"->"f(c)"))
    println("after applying substitution X->f(c) : " + l2.isGrounded)
    println("is -a grounded? " + new Literal("-a").isGrounded)
    println("is -X grounded? " + new Literal("-X").isGrounded)
  }
}

//----------------------------------------------------------
// Clause class
// keeps a disjunction of literals as a set.
class Clause(val stringlist: List[String]) {
  // convert list of strings to mutable ListBuffer of literals
  var literals :Set[Literal] = (stringlist.map (litString => new Literal(litString))).toSet
  cleanup

  // size of the clause: the number of its literals
  def size :Int = literals.size

  // tests for singleton and empty clauses
  def isSingleton :Boolean = { size == 1 }
  def isEmpty :Boolean = { size == 0 }

  // clean up literals: 
  // collapse duplicates
  // remove pairs of literals that are inverses of each other.
  def cleanup : Unit = {
    this.literals = this.literals.toList.toSet.filter ( l => 
      !(this.literals.exists(l2 => l.isNegationOf(l2))))

    if (isEmpty) {
      throw new EmptyClauseException(this.toString)
    }
  }

  def applySubstitution(substitution:Utils.Substitution) : Unit = { 
    this.literals.foreach ( l => l.applySubstitution(substitution))
    // if some literals have become equal through substitution,
    // we need to convert to a list and back to a set in order to remove duplicates
    this.literals = this.literals.toList.toSet
    cleanup
  }

  // for inspection: represent this class as the list of literals
  override def toString :String = { "Clause(" + this.literals.toString + ")"}

  def toList :List[String] = this.literals.map (l => l.toString).toList
}

class TestClause {
  def testme :Unit =  {
    val litlist = List("p(a)", "-p(a)", "p(a,a)", "p(a)")
    var cl1 = new Clause(litlist)
    println("clause from literal list " + litlist)
    println(cl1)
    println
    println("is singleton: " + cl1.isSingleton)
    println("is empty: " + cl1.isEmpty)
    println("size: " + cl1.size)
    println

    val litlist2 = List("p(a)", "p(X)")
    var cl2 = new Clause(litlist2)
    println(cl2)
    cl2.applySubstitution(Utils.newSubstitutionInit("X"->"a"))
    println(cl2)

    val litlist3 = List("p(a)", "-p(X)")
    var cl3 = new Clause(litlist3)
    println(cl3)
    try { 
      cl3.applySubstitution(Utils.newSubstitutionInit("X"->"a"))
    } catch {
      case e: EmptyClauseException => println("got an empty clause exception")
    } 

    println(cl2.toList)

  }

}


//----------------------------------------------------------
// class CNF:
// keep a formula as a mapping from indices to Clause objects.
class CNF(val listlist: List[List[String]]) {
  // mutable map that maps indice to clauses.
  // mutable because some clauses may be deleted
  var clauses : mutable.Map[Int,Clause] = listlist.zipWithIndex.map { case (clauselist, index) => 
								    (index, new Clause(clauselist)) }(collection.breakOut)

  // clauseIndices: set of indices in the clauses map
  def clauseIndices :Set[Int] = clauses.keySet

  // uniqueUnificationMatch: check if the given literal
  // is only unifiable with a single literal in this CNF formula.
  // If this is the case, return a triple of 
  // (clause Index, literal in this CNF that is matched, substitution)
  // otherwise return None.
  // Note that matching requires the literals to be of opposite polarity.
  def uniqueUnificationMatch(literal:Literal) : Option[(Int,Literal,Utils.Substitution)] = {
    val matchingLiterals : List[(Int,Literal,Utils.Substitution)] = clauseIndices.toList.flatMap {
      // map each clause index to a list of tuples (clauseIx, literal, substitution)
      // for all the matching literals in that clause
      clauseIx => clauses(clauseIx).literals.flatMap {
	// map each literal to either Some(clauseIx, literal, substitution) or None
	thisLiteral => thisLiteral.unifiableAndNegated(literal) match {
	  case Some(substitution) => new Some(clauseIx, thisLiteral, substitution)
	  case None => None
	}
      }
    }
    if (matchingLiterals.size == 1) {
      // single matching literal.
      new Some(matchingLiterals(0))
    } else { None }
  }

  // groundedNegationMatch:
  // check whether the given literal is grounded, and the current formula contains a literal 
  // that is the exact negation of that literal
  def groundedNegationMatch(literal1:Literal) :Option[(Int, Literal)] = {
    if (!literal1.isGrounded) { None }
    else {
      clauseIndices.toList.foldLeft(None:Option[(Int, Literal)]) ( (prevResult, clauseIx) => 
	prevResult match {
	    // if we have previously found a matching clause, no need to look further
	  case Some(_) => prevResult
	    // if we haven't found anything yet, check this clause
	  case None => clauses(clauseIx).literals.toList.find( literal2 => literal2.isNegationOf(literal1)) match {
	    // did we find some literal now? if yes, then it is the next result to be passed on
	    case Some(literal2) => new Some((clauseIx, literal2))
	      // if we didn't find anything, then pass on None
	    case None => None
	  }
	}
      )
    }
  }

  // clauseIx is a singleton clause that has just been involved in a resolution step,
  // so this clause disappears. Remove it from the "clauses" map,
  // then apply the substitution to all remaining clauses.
  def resolveAsSingleton(clauseIx:Int, substitution:Utils.Substitution) : Unit = {
    // raise an error if we don't have this clause
    if (!this.clauses.contains(clauseIx)) {
      throw new Exception("Shouldn't be here: trying to resolve nonexisting clause as singleton " + clauseIx.toString)
    }
    this.clauses -= clauseIx
    applySubstitution(substitution)
 }

  // clauseIx is a clause that has just been involved in a resolution step with a singleton
  // targeting the literal 'literal' occurring in clause 'clauseIx'. Remove 'literal'
  // from the clause 'clauseIx'. (There will be no new literals added to 'clauseIx' because
  // the other clause was a singleton.)
  // Apply the substitution to all clauses. 
  def resolveWithSingleton(clauseIx:Int, literal:Literal, substitution:Utils.Substitution) : Unit = { 
    if (!this.clauses.contains(clauseIx)) {
      throw new Exception("Shouldn't be here: trying to resolve nonexisting clause as singleton " + clauseIx.toString)
    }
    if (!this.clauses(clauseIx).literals.contains(literal)) {
      throw new Exception("Shouldn't be here: trying to resolve with nonexisting literal " + literal.toString)
    }

    clauses(clauseIx).literals = clauses(clauseIx).literals - literal
    applySubstitution(substitution)
  }

  // apply substitution:
  // apply the given substitution to every literal of every clause of this formula
  def applySubstitution(substitution:Utils.Substitution) : Unit = { 
    this.clauses.valuesIterator.foreach ( c => c.applySubstitution(substitution))
  }

  // for inspection: represent this class as the literal string
  override def toString :String = this.clauses.valuesIterator.map (c => c.toString ).mkString(" & ")

  def toListList :List[List[String]] = this.clauses.valuesIterator.map (c => c.toList ).toList

}

class TestCNF {
  def testme :Unit = {
    val cnfList1 = List(List("-q(a)"), List("p(a)", "p(X)"), List("p(a)", "-p(a)", "p(g(X))"))
    val cnf1 = new CNF(cnfList1)
    println(cnf1)
    println("clause indices " + cnf1.clauseIndices)
    println("doing ResolveAsSingleton on 0 with substitution X->b")
    cnf1.resolveAsSingleton(0, Utils.newSubstitutionInit("X" -> "b"))
    println(cnf1)
    println("trying ResolveAsSingleton with nonexistent clause")
    try {
      cnf1.resolveAsSingleton(3, Utils.newSubstitution)
    } catch {
      case e: Exception => println("caught an exception")
    }

    println("clause indices " + cnf1.clauseIndices)
    println("doing ResolveWithSingleton on 1 with empty substitution")
    cnf1.resolveWithSingleton(1, new Literal("p(b)"), Utils.newSubstitution)
    println(cnf1)
    println("doing ResolveWithSingleton with nonexistent literal")
    try {
      cnf1.resolveWithSingleton(1, new Literal("d"), Utils.newSubstitution)
    } catch {
      case e:Exception => println("caught an exception")
    }
    println

    val cnfList2 = List(List("p(a)", "p(X)"))
    val cnf2 = new CNF(cnfList2)
    println(cnf2)
    println("applying substitution: X->a")
    cnf2.applySubstitution(Utils.newSubstitutionInit("X"->"a"))
    println(cnf2)
    println

    println(cnf2.toListList)
    println

    val cnfList3 = List(List("p(f(X))", "-q(Y)"), List("p(a, a)", "-q(Z)"))
    val cnf3 = new CNF(cnfList3)
    println(cnf3)
    println("applying substitution: X->Z, Y->c")
    cnf3.applySubstitution(Utils.newSubstitutionInit("X" -> "Z", "Y"->"c"))
    println(cnf3)
    println

    // uniqueUnificationMatch
    println("uniqueUnificationMatch for q(X): " + cnf3.uniqueUnificationMatch(new Literal("q(X)")))
    println("uniqueUnificationMatch for q(a): " + cnf3.uniqueUnificationMatch(new Literal("q(a)")))

    println(cnf3.toListList)

    println
    println("grounded negation match for q(X) in cnf3: " + cnf3.groundedNegationMatch(new Literal("q(X)")))
    println("grounded negation match for p(a, a) in cnf3: " + cnf3.groundedNegationMatch(new Literal("p(a,a)")))
    println("grounded negation match for -p(a,a) in cnf3: " + cnf3.groundedNegationMatch(new Literal("-p( a,   a)")))
  }
}

//----------------------------------------------------------
class InferenceRule(lhsCNF:CNF, rhsCNF:CNF) {

  def cnfToLitList(f:CNF) : List[Literal] = {
    // both lhs and rhs CNFs have to consist of singleton clauses only
    // we are not handling anything else for now
    if (f.clauses.valuesIterator.exists( clause => !(clause.isSingleton)))
      throw new UnexpectedFormatOfFormulaException("formula not flat " + f)

    // map f to a flat list of literals
    val litListUnsorted :List[Literal] = f.clauses.values.map (clause => clause.literals.head).toList

    // map a literal to its negation status and predicate symbol
    def negAndPredSymbol(lit:Literal) :String = {
      (if (lit.isNegated) "-" else "") + lit.predSymbol
    }

    // check if we have any predicates that occur more than once. those will go
    // all the way to the end of the list of sorted literals
    val predCount :Map[String,Int] = litListUnsorted.foldLeft(Map():Map[String,Int]) ((pcmap, lit) =>
      pcmap updated (negAndPredSymbol(lit), pcmap.getOrElse(negAndPredSymbol(lit), 0) + 1))

    // convert the CNF to a list of literals sorted alphabetically by negation and predicate symbols.
    // Literals whose predicate symbol occurs more than once appear all the way at the end
    litListUnsorted.sortWith((l1, l2) => {
      val l1ps = negAndPredSymbol(l1)
      val l2ps = negAndPredSymbol(l2)
      val l1pc = predCount.getOrElse(l1ps, 0)
      val l2pc = predCount.getOrElse(l2ps, 0)
      if (l1pc != l2pc) { l1pc < l2pc }
      else { l1ps < l2ps }
    })
  }
    
  // left-hand side of the rule (with old variable names, to be changed below),
  // sorted alphabetically by predicate symbols
  val lhsOld :List[Literal] = cnfToLitList(lhsCNF)
  val rhsOld :List[Literal] = cnfToLitList(rhsCNF)

  val constantVariableRegex = new Regex("""^[A-Za-z][A-Za-z0-9_]*$""")

  // make a list of all the constants and variables in both the lhs and RHS CNF,
  // remove all occurrences of a duplicate except the first,
  // and pair each constant or variable with a string
  // Xsi
  // for s = l/r for left-hand or right-hand side 
  //and i the position in the list of constants and variables
  def mapArgumentsToNewVariables(f:List[Literal], formulaLabel:String, 
				 exceptArguments:Set[String] = Set()) :Map[String,String] = {

    val argListNodup :List[String] = f.flatMap( literal => 
      // extract list of arguments
      literal.argList ).filter ( arg => 
      // remove arguments thar are in the exceptArguments list
      !exceptArguments.contains(arg)).foldLeft(List():List[String])((arglist,arg) =>
      // remove entries that have occurred earlier in the list
      if (arglist.contains(arg)) { arglist } else {arg :: arglist}).reverse

    // sanity check: must be all constants or variables, no function symbols
    argListNodup.foreach { arg => arg match {
	// if we match the constant & variable regex, we are good
      case constantVariableRegex() => {}
	// if we don't match it, we have an error
      case somethingElse => throw new UnexpectedFormatOfFormulaException(arg)
    }}

    // pair each constant or variable with a string Xsi,
    // and transform to a map 
    argListNodup.zipWithIndex.map{ case (oldArg, oldArgIndex) =>
				   (oldArg, "X" + formulaLabel + oldArgIndex.toString) 
				 }(collection.breakOut).toMap
  }

  // create a mapping from lhs constants and variables to new variable names
  val lhsArgumentMap :Map[String,String] = mapArgumentsToNewVariables(lhsOld, "l")
  // create a mapping from rhs constants and variables to new variable names,
  // remove constants and variables that already occur in the lhs
  val rhsArgumentMap :Map[String,String] = mapArgumentsToNewVariables(rhsOld, "r",
								      lhsArgumentMap.keySet)
  val combinedMap :Map[String,String] = lhsArgumentMap ++ rhsArgumentMap

  def literalToStringWithNewArguments(literal:Literal, newArgMap: Map[String,String]) :String = {
    (if (literal.isNegated) { "-" } else { "" }) + literal.predSymbol + 
    (if (literal.arity > 0) { "(" + literal.argList.map(arg => newArgMap.getOrElse(arg, "XXX")).mkString(",") + ")" } 
     else { "" } )
  }

  override def toString :String = {
    // forall X,Y,Z exists A,B,C
    quantifiers.map (q => q._1 + " " + q._2.mkString(",")).mkString(" ") + ": " + 
    // LHS
    lhs.mkString(" & ") + 
    " -> " + 
    // RHS
    rhs.mkString(" & ")
  }

  // ===============================
  // now starting with the parts of this class to be used from outside
  // quantifiers:
  // a list of pairs of a string ("forall", "exists") and a set of variables that have this scope
  // The variables introduced in the LHS are universally quantified and non-nested.
  // The variables introduced in the RHS, if any, are existentially quantified and nested below the universal quantifiers
  val quantifiers :List[(String,Set[String])] = 
    if (rhsArgumentMap.size > 0) 
      { List(("forall", lhsArgumentMap.values.toSet), ("exists", rhsArgumentMap.values.toSet)) }
    else { List(("forall", lhsArgumentMap.values.toSet)) }

  // lhs is a conjunction of literals, realized as a list of strings.
  // negation is realized as "-".
  val lhs :List[String] = lhsOld.map (lit => literalToStringWithNewArguments(lit, lhsArgumentMap))

  // rhs is a conjunction of literals, realized as a list of strings.
  // negation is realized as "-".
  val rhs :List[String] = rhsOld.map (lit => literalToStringWithNewArguments(lit, combinedMap))

  // equality test:
  // It's enough to just compare the lhs and the rhs formulas.
  // We don't need to compare the quantifiers, as every variable
  // that has an "l" in its name (is introduced in the LHS) is universally quantified,
  // and every variable that has an "r" in its name (is introduced in the RHS)
  // is existentially quantified. So you could in principle read the quantifier off the formula.
  override def equals(that:Any) :Boolean = {
    //that.isInstanceOf[InstanceRule] && 
    //(lhs == that.asInstanceOf[Literal].lhs) && 
    //rhs == that.asInstanceOf[Literal].rhs
  	throw new RuntimeException("Not implemented")
  	true
  	//TODO: fix this
  	
  }
}

class TestInferenceRule { 
  def testme :Unit = {
    val cnf1 = new CNF(List(List("p(a)"), List("q(b)"), List("-p(a)"), List("r(b, b)")))
    val cnf2 = new CNF(List(List("q(a)"), List("-o(b)"), List("p(X)"), List("p(Y)"), List("r(X)")))

    testPair(cnf1, cnf2)

    val cnf3 = new CNF(List(List("a(X)"), List("d(Y)"), List("c(X, X)")))
    val cnf4 = new CNF(List(List("b(Y, X)"), List("-a(X, Y)")))

    testPair(cnf3, cnf4)
  }

  def testPair(cnf1:CNF, cnf2:CNF) :Unit = {
    val ir = new InferenceRule(cnf1, cnf2)

    println("CNF1: " + cnf1)
    println("CNF2 : " + cnf2)
    println
    println("lhsOld " + ir.lhsOld)
    println("rhsOld " + ir.rhsOld)

    println
    println("lhs argument map: " + ir.lhsArgumentMap)
    println("rhs argument map: " + ir.rhsArgumentMap)
    println("combined map: " + ir.combinedMap)
    println

    println("quantifiers: " + ir.quantifiers)
    println("lhs: " + ir.lhs)
    println("rhs: " + ir.rhs)
    println
    println(ir)
  }
}

//----------------------------------------------------------
object Resolution extends TakeLiteralApart {

  // Resolve to find difference:
  // Use Robinson's resolution to find the "difference" between two first-order formulas.
  // Keep the two formulas apart rather than joining them, as is usual in resolution.
  // Also only use resolution on a literal when there is only a single literal in the other formula
  // that is available for resolution. 
  // That is, be conservative in determining what the possible difference can be between formulas.
  // The function returns a pair of lists of string lists,
  // wrapped in an Option.

  def resolveToFindDifference(f1:List[List[String]], f2:List[List[String]]) :Option[(InferenceRule,InferenceRule)] = {
    
    var formula1 = new CNF(f1)
    var formula2 = new CNF(f2)
    try { 
      doResolution(formula1, formula2) match { 
	case Some((res1, res2)) => 
	  // resolution succeeded.
	  // negate formula 2.
	  val res2Neg:CNF = negateFormula(res2)
	  // then form 2 inference rules, one inferring -res2 from res1
	  // and one inferring res1 from -res2
	  new Some((new InferenceRule(res1, res2Neg), new InferenceRule(res2Neg, res1)))
	  
	case None => None
      }
    } catch {
	// if we found the formulas too complex to process, return None
      case e:UnexpectedFormatOfFormulaException => None
    }
  }

  def doResolution(formula1:CNF, formula2:CNF) :Option[(CNF, CNF)] = {
    // In a first step, do resolution on literal L1 of clause C1 and L2 of C2 only 
    // if 
    // * either C1 or C2 is a singleton clause,
    // * both L1 and L2 are "content" rather than meta-predicates,
    //   where meta-predicates are predicates like agent(X, Y) or patient(X, Y)
    //   or event(X) that stem from a Neo-Davidsonian representation
    // * L2 is the only literal that L1 unifies with in formula 1, and vice versa -- 
    //   or L1 and L2 are fully grounded, and one is the negation of the other

    val isContentLiteral = (l:Literal) => l.isContent

    try { 
      singletonClauseIndices(formula1).foreach ( ix => 
	singletonClauseUnificationStep(formula1, formula2, ix, isContentLiteral)
      )

      singletonClauseIndices(formula2).foreach ( ix => 
	singletonClauseUnificationStep(formula2, formula1, ix, isContentLiteral)
      )

      // Now do resolution on L1 of C1 and L2 of C2 only if 
      // * either C1 or C2 is singleton, 
      // * L2 is the only formula-2 literal that L1 unifies with, and vice versa --
      //   or L1 and L2 are fully grounded, and one is the negation of the other
      // * either L1, L2 are content literals, or they are fully grounded
      // run this while there is change in either formula.
      
      val isContentOrGrounded = (l:Literal) => l.isContent || l.isGrounded
      
      var change:Boolean = false
      do {
	change = 
	  singletonClauseIndices(formula1).exists ( ix => 
	    singletonClauseUnificationStep(formula1, formula2, ix, isContentOrGrounded)) || 
	singletonClauseIndices(formula2).exists ( ix =>
	    singletonClauseUnificationStep(formula2, formula1, ix, isContentOrGrounded))
      } while (change)
    } catch {
      case e:EmptyClauseException => return None
    }
      
    new Some(formula1, formula2)
  }

  // invert the second of the two formulas.
  // currently only works when we have a single clause
  def negateFormula(f:CNF) :CNF = {
    if (f.clauses.size > 1) {
      throw new UnexpectedFormatOfFormulaException(f.toString)
    }
    
    val newclauses :List[List[String]] = f.clauses(0).literals.toList.map ( oldL =>
      List(negateStringLiteral(oldL.toString))
    )

    new CNF(newclauses)
  }

  // filter for singleton clause indices in formula f
  def singletonClauseIndices( f:CNF) :List[Int] = {
    f.clauseIndices.toList.filter ( ix => f.clauses(ix).isSingleton )
  }

  // given a literal in a formula f1, find if it has a usable match in the given formula f2:
  // either a literal that is the exact negation of the literal, and both are grounded,
  // or a unique unification match
  def usableMatch(literal1:Literal, f1:CNF, f2:CNF, literalOK: (Literal) => Boolean) :Option[(Int,Literal,Utils.Substitution)] = {
    if (!literalOK(literal1)) { None }
    f2.groundedNegationMatch(literal1) match {
	// matching literal2 found in f2 such that literal is the negation of literal2
	// and are both grounded.
	// return empty substitution, as we are unifying two grounded literals
      case Some((c2Index, literal2)) => new Some((c2Index, literal2, Utils.newSubstitution))
	// no groundedNegationMatch found: then check for unique unifying literal
      case None => 
	f2.uniqueUnificationMatch(literal1) match { 
	  // if there is one, check that it only unifies with this particular literal in f1
	  case Some((c2Index, literal2, substitution)) =>
	    if (!literalOK(literal2)) { None }
	    else {
	      f1.uniqueUnificationMatch(literal2) match {
		  // if we get a positive answer, we know that it is for literal1
		case Some(_) => new Some((c2Index, literal2, substitution))
		case None => None
	      }
	    }
	  case None => None
	}
    }
  }

  // given a singleton clause and a second formula,
  // try to find a unique unification match for the literal in the singleton clause
  // in the second formula, and do a resolution step if it works
  // returns true if a resolution step was done
  def singletonClauseUnificationStep( f1:CNF, f2:CNF, c1Index:Int, testLit: (Literal) => Boolean) :Boolean = {
    val literal1 :Literal = f1.clauses(c1Index).literals.head
    usableMatch(literal1, f1, f2, testLit) match {
	// if we have found a usable match for literal1 in the form of literal2
	// of clause c2Index of f2,
	// we can do the resolution step.
      case Some((c2Index, literal2, substitution)) =>
	f2.resolveWithSingleton(c2Index, literal2, substitution)
	f1.resolveAsSingleton(c1Index, substitution)
	true
      case None => false
    }
  }
}

object ResolutionTest {
  import Resolution._
  def main(args: Array[String]) 
  { 
/*
  	testThisPair(List(List("solve(a)"), List("event(a)"), List("agent(a, b)"), List("person(b)"), 
		      List("patient(a, c)"), List("problem(c)")),
		 List(List("-find(E)", "-event(E)", "-agent(E, X)", "-person(X)", 
			   "-patient(E, Y)", "-solution(Y)", "-to(Y, Z)", "-problem(Z)")))
    println

    testThisPair(List(List("p(a)")), List(List("-p(a)")))
    println

testSentpair("""    In Beijing, officials said Thursday that visit would take place on May 26  to 29. In Moscow the premier's office confirmed that Chernomyrdin would visit  at the end of the month but did not give precise dates.""", """    In Beijing, officials said Thursday that visit would take be on May 26  to 29. In Moscow the premier's office confirmed that Chernomyrdin would visit  at the end of the month but did not give precise dates.""", """place on@R@->be on@R@""", 
List(List("tim1moscow(a1)"), List("n1office(c1)"), List("r1of(c1, b1)"), List("n1premierC39s(b1)"), List("tim1moscow(d1)"), List("n1month(e1)"), List("n1end(f1)"), List("per1chernomyrdin(g1)"), List("n1office(i1)"), List("r1of(i1, h1)"), List("n1premierC39s(h1)"), List("r1in(n1, m1)"), List("r1on(k1, j1)"), List("n1thursday(j1)"), List("r1theme(k1, l1)"), List("r1agent(k1, m1)"), List("v1say(k1)"), List("r1in(r1, d1)"), List("r1to(r1, p1)"), List("n129C46(p1)"), List("r1on(r1, q1)"), List("t_XXXX0526(q1)"), List("r1patient(r1, s1)"), List("r1agent(r1, t1)"), List("v1take(r1)"), List("n1place(s1)"), List("n1visit(t1)"), List("n1official(m1)"), List("org1beijingC44(m1)"), List("r1theme(n1, o1)"), List("r1agent(n1, i1)"), List("v1confirm(n1)"), List("r1at(u1, f1)"), List("r1of(f1, e1)"), List("r1agent(u1, g1)"), List("v1visit(u1)")), List(List("-org1beijingC44(E2)", "-tim1moscow(F2)", "-per1chernomyrdin(A2)", "-n1month(B2)", "-n1end(C2)", "-per1chernomyrdin(D2)", "-n1office(H2)", "-r1of(H2, G2)", "-n1premierC39s(G2)", "-r1in(J2, E2)", "-r1in(J2, F2)", "-r1to(J2, I2)", "-n129C46(I2)", "-r1on(N2, Q2)", "-t_XXXX0526(Q2)", "-r1on(L2, K2)", "-n1thursday(K2)", "-r1theme(L2, M2)", "-r1agent(L2, N2)", "-v1say(L2)", "-r1agent(R2, S2)", "-v1take(R2)", "-n1visit(S2)", "-n1official(N2)", "-r1theme(O2, P2)", "-r1agent(O2, H2)", "-v1confirm(O2)", "-r1at(T2, C2)", "-r1of(C2, B2)", "-r1agent(T2, D2)", "-v1visit(T2)")))

testSentpair("""    It was Feyenoord's third win in four years and ninth all-told.""", """    It was Feyenoord's third fight in four years and ninth all-told.""", """win in->fight in""", 
List(List("a1neuter(a1)"), List("eq(a1, f1)"), List("r1subset_of(c1, f1)"), List("n1allC45toldC46(c1)"), List("a1ninth(c1)"), List("r1subset_of(e1, f1)"), List("r1in(e1, d1)"), List("n1year(d1)"), List("card(d1, g1)"), List("c4number(g1)"), List("n1numeral(g1)"), List("n1win(e1)"), List("a1third(e1)"), List("nam1feyenoordC39s(e1)")), List(List("-a1neuter(A2)", "-eq(A2, F2)", "-r1subset_of(C2, F2)", "-n1allC45toldC46(C2)", "-a1ninth(C2)", "-r1subset_of(E2, F2)", "-r1in(E2, D2)", "-n1year(D2)", "-card(D2, G2)", "-c4number(G2)", "-n1numeral(G2)", "-n1fight(E2)", "-a1third(E2)", "-nam1feyenoordC39s(E2)")))

testSentpair("""    It was Feyenoord's third win in four years and ninth all-told.""", """    It was Feyenoord's third fight in four years and ninth all-told.""", """win in@R@->fight in@R@""", 
List(List("a1neuter(a1)"), List("eq(a1, f1)"), List("r1subset_of(c1, f1)"), List("n1allC45toldC46(c1)"), List("a1ninth(c1)"), List("r1subset_of(e1, f1)"), List("r1in(e1, d1)"), List("n1year(d1)"), List("card(d1, g1)"), List("c4number(g1)"), List("n1numeral(g1)"), List("n1win(e1)"), List("a1third(e1)"), List("nam1feyenoordC39s(e1)")), List(List("-a1neuter(A2)", "-eq(A2, F2)", "-r1subset_of(C2, F2)", "-n1allC45toldC46(C2)", "-a1ninth(C2)", "-r1subset_of(E2, F2)", "-r1in(E2, D2)", "-n1year(D2)", "-card(D2, G2)", "-c4number(G2)", "-n1numeral(G2)", "-n1fight(E2)", "-a1third(E2)", "-nam1feyenoordC39s(E2)")))

testSentpair("""    The 73-year-old Indian leader, who embarks on the visit Saturday, is under  tremendous domestic pressure to stand up to persistent US demands to cap the  country's nuclear regime and freeze its missile defence programme.""", """    The 73-year-old Indian leader, who embarks on the visit Saturday, is under  tremendous domestic pressure to stand up to persistent US demands to cap the  country's nuclear regime and wake its missile defence programme.""", """freeze->wake""", 
List(List("n1programmeC46(c1)"), List("r1of(c1, a1)"), List("n1defence(a1)"), List("r1of(c1, b1)"), List("n1missile(b1)"), List("r1of(c1, d1)"), List("a1neuter(d1)"), List("n1regime(c1)"), List("a1nuclear(c1)"), List("n1countryC39s(d1)"), List("nam1saturdayC44(e1)"), List("n1visit(f1)"), List("n1leaderC44(g1)"), List("a1indian(g1)"), List("a173C45yearC45old(g1)"), List("a1topic(g1)"), List("r1rel(h1, f1)"), List("r1under(e1, k1)"), List("n1pressure(k1)"), List("r1patient(o1, c1)"), List("r1agent(o1, k1)"), List("v1freeze(o1)"), List("r1patient(p1, c1)"), List("r1agent(p1, k1)"), List("v1cap(p1)"), List("r1to(n1, m1)"), List("n1demand(m1)"), List("loc1us(m1)"), List("a1persistent(m1)"), List("a1up(n1)"), List("r1agent(n1, k1)"), List("v1stand(n1)"), List("a1domestic(k1)"), List("a1tremendous(k1)"), List("r1on(i1, f1)"), List("r1agent(i1, g1)"), List("v1embark(i1)")), List(List("-n1wake(A2)", "-n1regime(B2)", "-r1subset_of(A2, D2)", "-r1subset_of(B2, D2)", "-a1nuclear(D2)", "-r1of(D2, C2)", "-n1countryC39s(C2)", "-n1programmeC46(D2)", "-r1of(D2, E2)", "-n1defence(E2)", "-r1of(D2, F2)", "-n1missile(F2)", "-a1neuter(C2)", "-nam1saturdayC44(G2)", "-n1visit(C2)", "-n1leaderC44(H2)", "-a1indian(H2)", "-a173C45yearC45old(H2)", "-a1topic(H2)", "-r1rel(K2, C2)", "-r1under(K2, J2)", "-n1pressure(J2)", "-r1patient(P2, D2)", "-r1agent(P2, J2)", "-v1cap(P2)", "-r1to(O2, N2)", "-n1demand(N2)", "-loc1us(N2)", "-a1persistent(N2)", "-a1up(O2)", "-r1agent(O2, J2)", "-v1stand(O2)", "-a1domestic(J2)", "-a1tremendous(J2)", "-eq(G2, D2)", "-r1on(L2, C2)", "-r1agent(L2, H2)", "-v1embark(L2)")))

testSentpair("""    The 73-year-old Indian leader, who embarks on the visit Saturday, is under  tremendous domestic pressure to stand up to persistent US demands to cap the  country's nuclear regime and freeze its missile defence programme.""", """    The 73-year-old Indian leader, who embarks on the visit Saturday, is under  tremendous domestic pressure to stand up to persistent US demands to cap the  country's nuclear regime and wake its missile defence programme.""", """freeze@R@->wake@R@""", 
List(List("n1programmeC46(c1)"), List("r1of(c1, a1)"), List("n1defence(a1)"), List("r1of(c1, b1)"), List("n1missile(b1)"), List("r1of(c1, d1)"), List("a1neuter(d1)"), List("n1regime(c1)"), List("a1nuclear(c1)"), List("n1countryC39s(d1)"), List("nam1saturdayC44(e1)"), List("n1visit(f1)"), List("n1leaderC44(g1)"), List("a1indian(g1)"), List("a173C45yearC45old(g1)"), List("a1topic(g1)"), List("r1rel(h1, f1)"), List("r1under(e1, k1)"), List("n1pressure(k1)"), List("r1patient(o1, c1)"), List("r1agent(o1, k1)"), List("v1freeze(o1)"), List("r1patient(p1, c1)"), List("r1agent(p1, k1)"), List("v1cap(p1)"), List("r1to(n1, m1)"), List("n1demand(m1)"), List("loc1us(m1)"), List("a1persistent(m1)"), List("a1up(n1)"), List("r1agent(n1, k1)"), List("v1stand(n1)"), List("a1domestic(k1)"), List("a1tremendous(k1)"), List("r1on(i1, f1)"), List("r1agent(i1, g1)"), List("v1embark(i1)")), List(List("-n1wake(A2)", "-n1regime(B2)", "-r1subset_of(A2, D2)", "-r1subset_of(B2, D2)", "-a1nuclear(D2)", "-r1of(D2, C2)", "-n1countryC39s(C2)", "-n1programmeC46(D2)", "-r1of(D2, E2)", "-n1defence(E2)", "-r1of(D2, F2)", "-n1missile(F2)", "-a1neuter(C2)", "-nam1saturdayC44(G2)", "-n1visit(C2)", "-n1leaderC44(H2)", "-a1indian(H2)", "-a173C45yearC45old(H2)", "-a1topic(H2)", "-r1rel(K2, C2)", "-r1under(K2, J2)", "-n1pressure(J2)", "-r1patient(P2, D2)", "-r1agent(P2, J2)", "-v1cap(P2)", "-r1to(O2, N2)", "-n1demand(N2)", "-loc1us(N2)", "-a1persistent(N2)", "-a1up(O2)", "-r1agent(O2, J2)", "-v1stand(O2)", "-a1domestic(J2)", "-a1tremendous(J2)", "-eq(G2, D2)", "-r1on(L2, C2)", "-r1agent(L2, H2)", "-v1embark(L2)")))

testSentpair("""Russian ultranationalist Vladimir Zhirinovsky said  Thursday he had approached Prime Minister Viktor Chernomyrdin with a view to  his party being given places in the government, Interfax said.""", """Russian ultranationalist Vladimir Zhirinovsky said  Thursday he had approached Prime Minister Viktor Chernomyrdin with a span to  his party being given places in the government, Interfax said.""", """view to->span to""", 
List(List("n1saidC46(c1)"), List("r1of(c1, a1)"), List("n1interfax(a1)"), List("r1of(c1, b1)"), List("n1governmentC44(b1)"), List("n1party(c1)"), List("per1prime_minister_viktor_chern(d1)"), List("per1vladimir_zhirinovsky(c1)"), List("n1ultranationalist(b1)"), List("a1russian(c1)"), List("r1on(f1, b1)"), List("n1thursday(b1)"), List("r1with(j1, i1)"), List("r1to(i1, c1)"), List("r1theme(g1, h1)"), List("r1recipient(g1, c1)"), List("v1give(g1)"), List("r1in(h1, c1)"), List("n1place(h1)"), List("n1view(i1)"), List("r1patient(j1, d1)"), List("r1agent(j1, b1)"), List("v1approach(j1)"), List("a1male(b1)"), List("r1theme(f1, e1)"), List("r1agent(f1, c1)"), List("v1say(f1)")), List(List("-n1saidC46(C2)", "-r1of(C2, A2)", "-n1interfax(A2)", "-r1of(C2, B2)", "-n1governmentC44(B2)", "-n1party(C2)", "-per1prime_minister_viktor_chern(D2)", "-per1vladimir_zhirinovsky(C2)", "-n1ultranationalist(B2)", "-a1russian(C2)", "-r1on(F2, B2)", "-n1thursday(B2)", "-r1with(J2, I2)", "-r1to(I2, C2)", "-r1theme(G2, H2)", "-r1recipient(G2, C2)", "-v1give(G2)", "-r1in(H2, C2)", "-n1place(H2)", "-n1span(I2)", "-r1patient(J2, D2)", "-r1agent(J2, B2)", "-v1approach(J2)", "-a1male(B2)", "-r1theme(F2, E2)", "-r1agent(F2, C2)", "-v1say(F2)")))

testSentpair("""Russian ultranationalist Vladimir Zhirinovsky said  Thursday he had approached Prime Minister Viktor Chernomyrdin with a view to  his party being given places in the government, Interfax said.""", """Russian ultranationalist Vladimir Zhirinovsky said  Thursday he had approached Prime Minister Viktor Chernomyrdin with a span to  his party being given places in the government, Interfax said.""", """view to@R@->span to@R@""", 
List(List("n1saidC46(c1)"), List("r1of(c1, a1)"), List("n1interfax(a1)"), List("r1of(c1, b1)"), List("n1governmentC44(b1)"), List("n1party(c1)"), List("per1prime_minister_viktor_chern(d1)"), List("per1vladimir_zhirinovsky(c1)"), List("n1ultranationalist(b1)"), List("a1russian(c1)"), List("r1on(f1, b1)"), List("n1thursday(b1)"), List("r1with(j1, i1)"), List("r1to(i1, c1)"), List("r1theme(g1, h1)"), List("r1recipient(g1, c1)"), List("v1give(g1)"), List("r1in(h1, c1)"), List("n1place(h1)"), List("n1view(i1)"), List("r1patient(j1, d1)"), List("r1agent(j1, b1)"), List("v1approach(j1)"), List("a1male(b1)"), List("r1theme(f1, e1)"), List("r1agent(f1, c1)"), List("v1say(f1)")), List(List("-n1saidC46(C2)", "-r1of(C2, A2)", "-n1interfax(A2)", "-r1of(C2, B2)", "-n1governmentC44(B2)", "-n1party(C2)", "-per1prime_minister_viktor_chern(D2)", "-per1vladimir_zhirinovsky(C2)", "-n1ultranationalist(B2)", "-a1russian(C2)", "-r1on(F2, B2)", "-n1thursday(B2)", "-r1with(J2, I2)", "-r1to(I2, C2)", "-r1theme(G2, H2)", "-r1recipient(G2, C2)", "-v1give(G2)", "-r1in(H2, C2)", "-n1place(H2)", "-n1span(I2)", "-r1patient(J2, D2)", "-r1agent(J2, B2)", "-v1approach(J2)", "-a1male(B2)", "-r1theme(F2, E2)", "-r1agent(F2, C2)", "-v1say(F2)")))

testSentpair("""    Juppe said France had let it be known it would not accept this "parody of  justice" and would not bargain for their freedom.""", """    Juppe said France had let it be known it would not accept this "impersonate of  justice" and would not bargain for their freedom.""", """parody->impersonate""", 
List(List("n1freedomC46(a1)"), List("r1of(a1, b1)"), List("n12thing(b1)"), List("loc1france(c1)"), List("n1C34parody(d1)"), List("a1neuter(e1)"), List("loc1france(f1)"), List("per1juppe(g1)"), List("-r1patient(N1, d1)", "-r1agent(N1, e1)", "-v1accept(N1)", "-r1of(d1, O1)", "-n1justiceC34(O1)"), List("r1theme(m1, l1)"), List("r1agent(m1, e1)"), List("v1know(m1)"), List("r1theme(k1, j1)"), List("r1agent(k1, f1)"), List("r1patient(k1, e1)"), List("v1let(k1)"), List("r1theme(i1, h1)"), List("r1agent(i1, g1)"), List("v1say(i1)")), List(List("r1patient(n2(M2, L2, K2, J2, I2, H2, G2, F2, E2, D2, C2, B2, A2), D2)", "-n1freedomC46(A2)", "-r1of(A2, B2)", "-n12thing(B2)", "-loc1france(C2)", "-n1C34impersonate(D2)", "-a1neuter(E2)", "-loc1france(F2)", "-per1juppe(G2)", "-r1theme(M2, L2)", "-r1agent(M2, E2)", "-v1know(M2)", "-r1theme(K2, J2)", "-r1agent(K2, F2)", "-r1patient(K2, E2)", "-v1let(K2)", "-r1theme(I2, H2)", "-r1agent(I2, G2)", "-v1say(I2)"), List("r1agent(n2(M2, L2, K2, J2, I2, H2, G2, F2, E2, D2, C2, B2, A2), E2)", "-n1freedomC46(A2)", "-r1of(A2, B2)", "-n12thing(B2)", "-loc1france(C2)", "-n1C34impersonate(D2)", "-a1neuter(E2)", "-loc1france(F2)", "-per1juppe(G2)", "-r1theme(M2, L2)", "-r1agent(M2, E2)", "-v1know(M2)", "-r1theme(K2, J2)", "-r1agent(K2, F2)", "-r1patient(K2, E2)", "-v1let(K2)", "-r1theme(I2, H2)", "-r1agent(I2, G2)", "-v1say(I2)"), List("v1accept(n2(M2, L2, K2, J2, I2, H2, G2, F2, E2, D2, C2, B2, A2))", "-n1freedomC46(A2)", "-r1of(A2, B2)", "-n12thing(B2)", "-loc1france(C2)", "-n1C34impersonate(D2)", "-a1neuter(E2)", "-loc1france(F2)", "-per1juppe(G2)", "-r1theme(M2, L2)", "-r1agent(M2, E2)", "-v1know(M2)", "-r1theme(K2, J2)", "-r1agent(K2, F2)", "-r1patient(K2, E2)", "-v1let(K2)", "-r1theme(I2, H2)", "-r1agent(I2, G2)", "-v1say(I2)"), List("r1of(D2, o2(M2, L2, K2, J2, I2, H2, G2, F2, E2, D2, C2, B2, A2))", "-n1freedomC46(A2)", "-r1of(A2, B2)", "-n12thing(B2)", "-loc1france(C2)", "-n1C34impersonate(D2)", "-a1neuter(E2)", "-loc1france(F2)", "-per1juppe(G2)", "-r1theme(M2, L2)", "-r1agent(M2, E2)", "-v1know(M2)", "-r1theme(K2, J2)", "-r1agent(K2, F2)", "-r1patient(K2, E2)", "-v1let(K2)", "-r1theme(I2, H2)", "-r1agent(I2, G2)", "-v1say(I2)"), List("n1justiceC34(o2(M2, L2, K2, J2, I2, H2, G2, F2, E2, D2, C2, B2, A2))", "-n1freedomC46(A2)", "-r1of(A2, B2)", "-n12thing(B2)", "-loc1france(C2)", "-n1C34impersonate(D2)", "-a1neuter(E2)", "-loc1france(F2)", "-per1juppe(G2)", "-r1theme(M2, L2)", "-r1agent(M2, E2)", "-v1know(M2)", "-r1theme(K2, J2)", "-r1agent(K2, F2)", "-r1patient(K2, E2)", "-v1let(K2)", "-r1theme(I2, H2)", "-r1agent(I2, G2)", "-v1say(I2)")))

testSentpair("""    Juppe said France had let it be known it would not accept this "parody of  justice" and would not bargain for their freedom.""", """    Juppe said France had let it be known it would not accept this "impersonate of  justice" and would not bargain for their freedom.""", """parody@R@->impersonate@R@""", 
List(List("n1freedomC46(a1)"), List("r1of(a1, b1)"), List("n12thing(b1)"), List("loc1france(c1)"), List("n1C34parody(d1)"), List("a1neuter(e1)"), List("loc1france(f1)"), List("per1juppe(g1)"), List("-r1patient(N1, d1)", "-r1agent(N1, e1)", "-v1accept(N1)", "-r1of(d1, O1)", "-n1justiceC34(O1)"), List("r1theme(m1, l1)"), List("r1agent(m1, e1)"), List("v1know(m1)"), List("r1theme(k1, j1)"), List("r1agent(k1, f1)"), List("r1patient(k1, e1)"), List("v1let(k1)"), List("r1theme(i1, h1)"), List("r1agent(i1, g1)"), List("v1say(i1)")), List(List("r1patient(n2(M2, L2, K2, J2, I2, H2, G2, F2, E2, D2, C2, B2, A2), D2)", "-n1freedomC46(A2)", "-r1of(A2, B2)", "-n12thing(B2)", "-loc1france(C2)", "-n1C34impersonate(D2)", "-a1neuter(E2)", "-loc1france(F2)", "-per1juppe(G2)", "-r1theme(M2, L2)", "-r1agent(M2, E2)", "-v1know(M2)", "-r1theme(K2, J2)", "-r1agent(K2, F2)", "-r1patient(K2, E2)", "-v1let(K2)", "-r1theme(I2, H2)", "-r1agent(I2, G2)", "-v1say(I2)"), List("r1agent(n2(M2, L2, K2, J2, I2, H2, G2, F2, E2, D2, C2, B2, A2), E2)", "-n1freedomC46(A2)", "-r1of(A2, B2)", "-n12thing(B2)", "-loc1france(C2)", "-n1C34impersonate(D2)", "-a1neuter(E2)", "-loc1france(F2)", "-per1juppe(G2)", "-r1theme(M2, L2)", "-r1agent(M2, E2)", "-v1know(M2)", "-r1theme(K2, J2)", "-r1agent(K2, F2)", "-r1patient(K2, E2)", "-v1let(K2)", "-r1theme(I2, H2)", "-r1agent(I2, G2)", "-v1say(I2)"), List("v1accept(n2(M2, L2, K2, J2, I2, H2, G2, F2, E2, D2, C2, B2, A2))", "-n1freedomC46(A2)", "-r1of(A2, B2)", "-n12thing(B2)", "-loc1france(C2)", "-n1C34impersonate(D2)", "-a1neuter(E2)", "-loc1france(F2)", "-per1juppe(G2)", "-r1theme(M2, L2)", "-r1agent(M2, E2)", "-v1know(M2)", "-r1theme(K2, J2)", "-r1agent(K2, F2)", "-r1patient(K2, E2)", "-v1let(K2)", "-r1theme(I2, H2)", "-r1agent(I2, G2)", "-v1say(I2)"), List("r1of(D2, o2(M2, L2, K2, J2, I2, H2, G2, F2, E2, D2, C2, B2, A2))", "-n1freedomC46(A2)", "-r1of(A2, B2)", "-n12thing(B2)", "-loc1france(C2)", "-n1C34impersonate(D2)", "-a1neuter(E2)", "-loc1france(F2)", "-per1juppe(G2)", "-r1theme(M2, L2)", "-r1agent(M2, E2)", "-v1know(M2)", "-r1theme(K2, J2)", "-r1agent(K2, F2)", "-r1patient(K2, E2)", "-v1let(K2)", "-r1theme(I2, H2)", "-r1agent(I2, G2)", "-v1say(I2)"), List("n1justiceC34(o2(M2, L2, K2, J2, I2, H2, G2, F2, E2, D2, C2, B2, A2))", "-n1freedomC46(A2)", "-r1of(A2, B2)", "-n12thing(B2)", "-loc1france(C2)", "-n1C34impersonate(D2)", "-a1neuter(E2)", "-loc1france(F2)", "-per1juppe(G2)", "-r1theme(M2, L2)", "-r1agent(M2, E2)", "-v1know(M2)", "-r1theme(K2, J2)", "-r1agent(K2, F2)", "-r1patient(K2, E2)", "-v1let(K2)", "-r1theme(I2, H2)", "-r1agent(I2, G2)", "-v1say(I2)")))

testSentpair("""    The event reopens debate on the issue. National talk show host Phil  Donohue has joined Lawson in his request to the North Carolina Supreme Court  to overturn the ban.""", """    The event reopens be divided on the issue. National talk show host Phil  Donohue has joined Lawson in his request to the North Carolina Supreme Court  to overturn the ban.""", """debate on->be divided on""", 
List(List("n1banC46(a1)"), List("org1north_carolina_supreme_cour(b1)"), List("n1request(c1)"), List("r1of(c1, d1)"), List("a1male(d1)"), List("per1lawson(d1)"), List("per1phil_donohue(c1)"), List("r1of(c1, e1)"), List("n1host(e1)"), List("r1of(c1, f1)"), List("n1show(f1)"), List("n1talk(d1)"), List("org1national(c1)"), List("org1issueC46(c1)"), List("n1event(g1)"), List("r1patient(l1, a1)"), List("r1agent(l1, g1)"), List("v1overturn(l1)"), List("r1in(i1, c1)"), List("r1to(c1, b1)"), List("r1patient(i1, d1)"), List("r1agent(i1, g1)"), List("v1join(i1)"), List("r1patient(j1, k1)"), List("r1agent(j1, g1)"), List("v1reopen(j1)"), List("r1on(k1, c1)"), List("n1debate(k1)")), List(List("-n1banC46(A2)", "-org1north_carolina_supreme_cour(B2)", "-n1request(C2)", "-r1of(C2, D2)", "-a1male(D2)", "-per1lawson(D2)", "-per1phil_donohue(C2)", "-r1of(C2, E2)", "-n1host(E2)", "-r1of(C2, F2)", "-n1show(F2)", "-n1talk(D2)", "-org1national(C2)", "-org1issueC46(C2)", "-n1event(G2)", "-r1patient(K2, A2)", "-r1agent(K2, G2)", "-v1overturn(K2)", "-r1in(I2, C2)", "-r1to(C2, B2)", "-r1patient(I2, D2)", "-r1agent(I2, G2)", "-v1join(I2)", "-r1on(J2, C2)", "-r1patient(J2, G2)", "-v1divide(J2)")))

testSentpair("""    The event reopens debate on the issue. National talk show host Phil  Donohue has joined Lawson in his request to the North Carolina Supreme Court  to overturn the ban.""", """    The event reopens be divided on the issue. National talk show host Phil  Donohue has joined Lawson in his request to the North Carolina Supreme Court  to overturn the ban.""", """debate on@R@->be divided on@R@""", 
List(List("n1banC46(a1)"), List("org1north_carolina_supreme_cour(b1)"), List("n1request(c1)"), List("r1of(c1, d1)"), List("a1male(d1)"), List("per1lawson(d1)"), List("per1phil_donohue(c1)"), List("r1of(c1, e1)"), List("n1host(e1)"), List("r1of(c1, f1)"), List("n1show(f1)"), List("n1talk(d1)"), List("org1national(c1)"), List("org1issueC46(c1)"), List("n1event(g1)"), List("r1patient(l1, a1)"), List("r1agent(l1, g1)"), List("v1overturn(l1)"), List("r1in(i1, c1)"), List("r1to(c1, b1)"), List("r1patient(i1, d1)"), List("r1agent(i1, g1)"), List("v1join(i1)"), List("r1patient(j1, k1)"), List("r1agent(j1, g1)"), List("v1reopen(j1)"), List("r1on(k1, c1)"), List("n1debate(k1)")), List(List("-n1banC46(A2)", "-org1north_carolina_supreme_cour(B2)", "-n1request(C2)", "-r1of(C2, D2)", "-a1male(D2)", "-per1lawson(D2)", "-per1phil_donohue(C2)", "-r1of(C2, E2)", "-n1host(E2)", "-r1of(C2, F2)", "-n1show(F2)", "-n1talk(D2)", "-org1national(C2)", "-org1issueC46(C2)", "-n1event(G2)", "-r1patient(K2, A2)", "-r1agent(K2, G2)", "-v1overturn(K2)", "-r1in(I2, C2)", "-r1to(C2, B2)", "-r1patient(I2, D2)", "-r1agent(I2, G2)", "-v1join(I2)", "-r1on(J2, C2)", "-r1patient(J2, G2)", "-v1divide(J2)")))

testSentpair("""A 26-year-old anorexic's fight for  survival hung in the balance Thursday as the British woman awaited the outcome  of a media bidding battle for her exclusive story.""", """A 26-year-old anorexic's fight for  survival hung in the balance Thursday as the British woman awaited the outcome  of a media bidding battle for her live chat story.""", """exclusive->live chat""", 
List(List("n1storyC46(a1)"), List("a1exclusive(a1)"), List("r1of(a1, b1)"), List("a1female(b1)"), List("n1outcome(b1)"), List("n1woman(c1)"), List("a1british(c1)"), List("tim1thursday(a1)"), List("n1balance(b1)"), List("r1patient(d1, b1)"), List("r1agent(d1, k1)"), List("v1await(d1)"), List("r1of(b1, g1)"), List("r1for(g1, a1)"), List("n1battle(g1)"), List("r1of(g1, e1)"), List("n1bidding(e1)"), List("r1of(g1, f1)"), List("n1media(f1)"), List("r1as(k1, c1)"), List("r1in(k1, a1)"), List("r1for(k1, i1)"), List("n1hung(i1)"), List("r1of(i1, h1)"), List("n1survival(h1)"), List("n1fight(k1)"), List("r1of(k1, j1)"), List("n1anorexicC39s(j1)"), List("a126C45yearC45old(k1)")), List(List("-n1storyC46(B2)", "-r1of(B2, A2)", "-n1chat(A2)", "-a1live(B2)", "-r1of(B2, C2)", "-a1female(C2)", "-n1outcome(C2)", "-n1woman(D2)", "-a1british(D2)", "-tim1thursday(B2)", "-n1balance(C2)", "-r1patient(E2, C2)", "-r1agent(E2, L2)", "-v1await(E2)", "-r1of(C2, H2)", "-r1for(H2, B2)", "-n1battle(H2)", "-r1of(H2, F2)", "-n1bidding(F2)", "-r1of(H2, G2)", "-n1media(G2)", "-r1as(L2, D2)", "-r1in(L2, B2)", "-r1for(L2, J2)", "-n1hung(J2)", "-r1of(J2, I2)", "-n1survival(I2)", "-n1fight(L2)", "-r1of(L2, K2)", "-n1anorexicC39s(K2)", "-a126C45yearC45old(L2)")))

testSentpair("""    The woman, whose twin sister has already dieted herself to death, was  relying on the outcome of a battle between such British media giants as  Granada Television and the Daily Mirror newspaper to pay her medical bills at  an exclusive clinic which specializes in bringing victims of anorexia and  bulimia back to the world of the living.""", """    The woman, whose twin sister has already dieted herself to death, was  relying on the outcome of a battle between such British media giants as  Granada Television and the Daily Mirror newspaper to pay her medical bills at  an live chat clinic which specializes in bringing victims of anorexia and  bulimia back to the world of the living.""", """exclusive->live chat""", 
List(List("n1livingC46(a1)"), List("n1world(b1)"), List("n1newspaper(c1)"), List("org1mirror(c1)"), List("org1daily(c1)"), List("org1granada_television(d1)"), List("n1outcome(e1)"), List("a1female(f1)"), List("n1womanC44(f1)"), List("r1on(l1, e1)"), List("r1of(e1, k1)"), List("r1patient(o1, g1)"), List("r1agent(o1, k1)"), List("v1pay(o1)"), List("r1at(g1, w1)"), List("a1back(p1)"), List("r1to(p1, b1)"), List("r1of(b1, a1)"), List("r1patient(p1, t1)"), List("r1agent(p1, u1)"), List("v1bring(p1)"), List("r1of(t1, s1)"), List("n1bulimium(q1)"), List("n1anorexia(r1)"), List("r1subset_of(q1, s1)"), List("r1subset_of(r1, s1)"), List("n1victim(t1)"), List("r1in(v1, u1)"), List("r1agent(v1, w1)"), List("v1specialize(v1)"), List("n1clinic(w1)"), List("a1exclusive(w1)"), List("n1bill(g1)"), List("a1medical(g1)"), List("r1of(g1, h1)"), List("a1female(h1)"), List("r1between(k1, j1)"), List("a1such(j1)"), List("r1subset_of(c1, j1)"), List("r1subset_of(g1, j1)"), List("r1as(g1, d1)"), List("n1giant(g1)"), List("n1media(h1)"), List("a1british(g1)"), List("n1battle(k1)"), List("r1agent(l1, f1)"), List("v1rely(l1)"), List("a1already(m1)"), List("r1patient(m1, n1)"), List("r1agent(m1, h1)"), List("v1diet(m1)"), List("r1agent(x1, f1)"), List("v1deathC44(x1)"), List("n1sister(h1)"), List("a1twin(h1)"), List("r1of(h1, f1)")), List(List("-n1livingC46(A2)", "-n1world(B2)", "-n1newspaper(C2)", "-org1mirror(C2)", "-org1daily(C2)", "-org1granada_television(D2)", "-n1outcome(E2)", "-a1female(F2)", "-n1womanC44(F2)", "-r1on(L2, E2)", "-r1of(E2, K2)", "-r1patient(O2, G2)", "-r1agent(O2, K2)", "-v1pay(O2)", "-r1at(G2, X2)", "-a1back(P2)", "-r1to(P2, B2)", "-r1of(B2, A2)", "-r1patient(P2, T2)", "-r1agent(P2, U2)", "-v1bring(P2)", "-r1of(T2, S2)", "-n1bulimium(Q2)", "-n1anorexia(R2)", "-r1subset_of(Q2, S2)", "-r1subset_of(R2, S2)", "-n1victim(T2)", "-r1in(V2, U2)", "-r1agent(V2, X2)", "-v1specialize(V2)", "-n1clinic(X2)", "-r1of(X2, W2)", "-n1chat(W2)", "-a1live(X2)", "-n1bill(G2)", "-a1medical(G2)", "-r1of(G2, H2)", "-a1female(H2)", "-r1between(K2, J2)", "-a1such(J2)", "-r1subset_of(C2, J2)", "-r1subset_of(G2, J2)", "-r1as(G2, D2)", "-n1giant(G2)", "-n1media(H2)", "-a1british(G2)", "-n1battle(K2)", "-r1agent(L2, F2)", "-v1rely(L2)", "-a1already(M2)", "-r1patient(M2, N2)", "-r1agent(M2, H2)", "-v1diet(M2)", "-r1agent(Y2, F2)", "-v1deathC44(Y2)", "-n1sister(H2)", "-a1twin(H2)", "-r1of(H2, F2)")))

testSentpair("""    The woman, whose twin sister has already dieted herself to death, was  relying on the outcome of a battle between such British media giants as  Granada Television and the Daily Mirror newspaper to pay her medical bills at  an exclusive clinic which specializes in bringing victims of anorexia and  bulimia back to the world of the living.""", """    The woman, whose twin sister has already dieted herself to death, was  relying on the outcome of a battle between such British media giants as  Granada Television and the Daily Mirror newspaper to pay her medical bills at  an live chat clinic which specializes in bringing victims of anorexia and  bulimia back to the world of the living.""", """exclusive@R@->live chat@R@""", 
List(List("n1livingC46(a1)"), List("n1world(b1)"), List("n1newspaper(c1)"), List("org1mirror(c1)"), List("org1daily(c1)"), List("org1granada_television(d1)"), List("n1outcome(e1)"), List("a1female(f1)"), List("n1womanC44(f1)"), List("r1on(l1, e1)"), List("r1of(e1, k1)"), List("r1patient(o1, g1)"), List("r1agent(o1, k1)"), List("v1pay(o1)"), List("r1at(g1, w1)"), List("a1back(p1)"), List("r1to(p1, b1)"), List("r1of(b1, a1)"), List("r1patient(p1, t1)"), List("r1agent(p1, u1)"), List("v1bring(p1)"), List("r1of(t1, s1)"), List("n1bulimium(q1)"), List("n1anorexia(r1)"), List("r1subset_of(q1, s1)"), List("r1subset_of(r1, s1)"), List("n1victim(t1)"), List("r1in(v1, u1)"), List("r1agent(v1, w1)"), List("v1specialize(v1)"), List("n1clinic(w1)"), List("a1exclusive(w1)"), List("n1bill(g1)"), List("a1medical(g1)"), List("r1of(g1, h1)"), List("a1female(h1)"), List("r1between(k1, j1)"), List("a1such(j1)"), List("r1subset_of(c1, j1)"), List("r1subset_of(g1, j1)"), List("r1as(g1, d1)"), List("n1giant(g1)"), List("n1media(h1)"), List("a1british(g1)"), List("n1battle(k1)"), List("r1agent(l1, f1)"), List("v1rely(l1)"), List("a1already(m1)"), List("r1patient(m1, n1)"), List("r1agent(m1, h1)"), List("v1diet(m1)"), List("r1agent(x1, f1)"), List("v1deathC44(x1)"), List("n1sister(h1)"), List("a1twin(h1)"), List("r1of(h1, f1)")), List(List("-n1livingC46(A2)", "-n1world(B2)", "-n1newspaper(C2)", "-org1mirror(C2)", "-org1daily(C2)", "-org1granada_television(D2)", "-n1outcome(E2)", "-a1female(F2)", "-n1womanC44(F2)", "-r1on(L2, E2)", "-r1of(E2, K2)", "-r1patient(O2, G2)", "-r1agent(O2, K2)", "-v1pay(O2)", "-r1at(G2, X2)", "-a1back(P2)", "-r1to(P2, B2)", "-r1of(B2, A2)", "-r1patient(P2, T2)", "-r1agent(P2, U2)", "-v1bring(P2)", "-r1of(T2, S2)", "-n1bulimium(Q2)", "-n1anorexia(R2)", "-r1subset_of(Q2, S2)", "-r1subset_of(R2, S2)", "-n1victim(T2)", "-r1in(V2, U2)", "-r1agent(V2, X2)", "-v1specialize(V2)", "-n1clinic(X2)", "-r1of(X2, W2)", "-n1chat(W2)", "-a1live(X2)", "-n1bill(G2)", "-a1medical(G2)", "-r1of(G2, H2)", "-a1female(H2)", "-r1between(K2, J2)", "-a1such(J2)", "-r1subset_of(C2, J2)", "-r1subset_of(G2, J2)", "-r1as(G2, D2)", "-n1giant(G2)", "-n1media(H2)", "-a1british(G2)", "-n1battle(K2)", "-r1agent(L2, F2)", "-v1rely(L2)", "-a1already(M2)", "-r1patient(M2, N2)", "-r1agent(M2, H2)", "-v1diet(M2)", "-r1agent(Y2, F2)", "-v1deathC44(Y2)", "-n1sister(H2)", "-a1twin(H2)", "-r1of(H2, F2)")))

testSentpair("""A 26-year-old anorexic's fight for  survival hung in the balance Thursday as the British woman awaited the outcome  of a media bidding battle for her exclusive story.""", """A 26-year-old anorexic's fight for  survival hung in the balance Thursday as the British woman awaited the outcome  of a media bidding battle for her live chat story.""", """exclusive@R@->live chat@R@""", 
List(List("n1storyC46(a1)"), List("a1exclusive(a1)"), List("r1of(a1, b1)"), List("a1female(b1)"), List("n1outcome(b1)"), List("n1woman(c1)"), List("a1british(c1)"), List("tim1thursday(a1)"), List("n1balance(b1)"), List("r1patient(d1, b1)"), List("r1agent(d1, k1)"), List("v1await(d1)"), List("r1of(b1, g1)"), List("r1for(g1, a1)"), List("n1battle(g1)"), List("r1of(g1, e1)"), List("n1bidding(e1)"), List("r1of(g1, f1)"), List("n1media(f1)"), List("r1as(k1, c1)"), List("r1in(k1, a1)"), List("r1for(k1, i1)"), List("n1hung(i1)"), List("r1of(i1, h1)"), List("n1survival(h1)"), List("n1fight(k1)"), List("r1of(k1, j1)"), List("n1anorexicC39s(j1)"), List("a126C45yearC45old(k1)")), List(List("-n1storyC46(B2)", "-r1of(B2, A2)", "-n1chat(A2)", "-a1live(B2)", "-r1of(B2, C2)", "-a1female(C2)", "-n1outcome(C2)", "-n1woman(D2)", "-a1british(D2)", "-tim1thursday(B2)", "-n1balance(C2)", "-r1patient(E2, C2)", "-r1agent(E2, L2)", "-v1await(E2)", "-r1of(C2, H2)", "-r1for(H2, B2)", "-n1battle(H2)", "-r1of(H2, F2)", "-n1bidding(F2)", "-r1of(H2, G2)", "-n1media(G2)", "-r1as(L2, D2)", "-r1in(L2, B2)", "-r1for(L2, J2)", "-n1hung(J2)", "-r1of(J2, I2)", "-n1survival(I2)", "-n1fight(L2)", "-r1of(L2, K2)", "-n1anorexicC39s(K2)", "-a126C45yearC45old(L2)")))

testSentpair("""    Juppe said France had let it be known it would not accept this "parody of  justice" and would not bargain for their freedom.""", """    Juppe said France had let it be known it would not accept this "satirize of  justice" and would not bargain for their freedom.""", """parody->satirize""", 
List(List("n1freedomC46(a1)"), List("r1of(a1, b1)"), List("n12thing(b1)"), List("loc1france(c1)"), List("n1C34parody(d1)"), List("a1neuter(e1)"), List("loc1france(f1)"), List("per1juppe(g1)"), List("-r1patient(N1, d1)", "-r1agent(N1, e1)", "-v1accept(N1)", "-r1of(d1, O1)", "-n1justiceC34(O1)"), List("r1theme(m1, l1)"), List("r1agent(m1, e1)"), List("v1know(m1)"), List("r1theme(k1, j1)"), List("r1agent(k1, f1)"), List("r1patient(k1, e1)"), List("v1let(k1)"), List("r1theme(i1, h1)"), List("r1agent(i1, g1)"), List("v1say(i1)")), List(List("r1patient(n2(M2, L2, K2, J2, I2, H2, G2, F2, E2, D2, C2, B2, A2), D2)", "-n1freedomC46(A2)", "-r1of(A2, B2)", "-n12thing(B2)", "-loc1france(C2)", "-n1C34satirize(D2)", "-a1neuter(E2)", "-loc1france(F2)", "-per1juppe(G2)", "-r1theme(M2, L2)", "-r1agent(M2, E2)", "-v1know(M2)", "-r1theme(K2, J2)", "-r1agent(K2, F2)", "-r1patient(K2, E2)", "-v1let(K2)", "-r1theme(I2, H2)", "-r1agent(I2, G2)", "-v1say(I2)"), List("r1agent(n2(M2, L2, K2, J2, I2, H2, G2, F2, E2, D2, C2, B2, A2), E2)", "-n1freedomC46(A2)", "-r1of(A2, B2)", "-n12thing(B2)", "-loc1france(C2)", "-n1C34satirize(D2)", "-a1neuter(E2)", "-loc1france(F2)", "-per1juppe(G2)", "-r1theme(M2, L2)", "-r1agent(M2, E2)", "-v1know(M2)", "-r1theme(K2, J2)", "-r1agent(K2, F2)", "-r1patient(K2, E2)", "-v1let(K2)", "-r1theme(I2, H2)", "-r1agent(I2, G2)", "-v1say(I2)"), List("v1accept(n2(M2, L2, K2, J2, I2, H2, G2, F2, E2, D2, C2, B2, A2))", "-n1freedomC46(A2)", "-r1of(A2, B2)", "-n12thing(B2)", "-loc1france(C2)", "-n1C34satirize(D2)", "-a1neuter(E2)", "-loc1france(F2)", "-per1juppe(G2)", "-r1theme(M2, L2)", "-r1agent(M2, E2)", "-v1know(M2)", "-r1theme(K2, J2)", "-r1agent(K2, F2)", "-r1patient(K2, E2)", "-v1let(K2)", "-r1theme(I2, H2)", "-r1agent(I2, G2)", "-v1say(I2)"), List("r1of(D2, o2(M2, L2, K2, J2, I2, H2, G2, F2, E2, D2, C2, B2, A2))", "-n1freedomC46(A2)", "-r1of(A2, B2)", "-n12thing(B2)", "-loc1france(C2)", "-n1C34satirize(D2)", "-a1neuter(E2)", "-loc1france(F2)", "-per1juppe(G2)", "-r1theme(M2, L2)", "-r1agent(M2, E2)", "-v1know(M2)", "-r1theme(K2, J2)", "-r1agent(K2, F2)", "-r1patient(K2, E2)", "-v1let(K2)", "-r1theme(I2, H2)", "-r1agent(I2, G2)", "-v1say(I2)"), List("n1justiceC34(o2(M2, L2, K2, J2, I2, H2, G2, F2, E2, D2, C2, B2, A2))", "-n1freedomC46(A2)", "-r1of(A2, B2)", "-n12thing(B2)", "-loc1france(C2)", "-n1C34satirize(D2)", "-a1neuter(E2)", "-loc1france(F2)", "-per1juppe(G2)", "-r1theme(M2, L2)", "-r1agent(M2, E2)", "-v1know(M2)", "-r1theme(K2, J2)", "-r1agent(K2, F2)", "-r1patient(K2, E2)", "-v1let(K2)", "-r1theme(I2, H2)", "-r1agent(I2, G2)", "-v1say(I2)")))

testSentpair("""    Juppe said France had let it be known it would not accept this "parody of  justice" and would not bargain for their freedom.""", """    Juppe said France had let it be known it would not accept this "satirize of  justice" and would not bargain for their freedom.""", """parody@R@->satirize@R@""", 
List(List("n1freedomC46(a1)"), List("r1of(a1, b1)"), List("n12thing(b1)"), List("loc1france(c1)"), List("n1C34parody(d1)"), List("a1neuter(e1)"), List("loc1france(f1)"), List("per1juppe(g1)"), List("-r1patient(N1, d1)", "-r1agent(N1, e1)", "-v1accept(N1)", "-r1of(d1, O1)", "-n1justiceC34(O1)"), List("r1theme(m1, l1)"), List("r1agent(m1, e1)"), List("v1know(m1)"), List("r1theme(k1, j1)"), List("r1agent(k1, f1)"), List("r1patient(k1, e1)"), List("v1let(k1)"), List("r1theme(i1, h1)"), List("r1agent(i1, g1)"), List("v1say(i1)")), List(List("r1patient(n2(M2, L2, K2, J2, I2, H2, G2, F2, E2, D2, C2, B2, A2), D2)", "-n1freedomC46(A2)", "-r1of(A2, B2)", "-n12thing(B2)", "-loc1france(C2)", "-n1C34satirize(D2)", "-a1neuter(E2)", "-loc1france(F2)", "-per1juppe(G2)", "-r1theme(M2, L2)", "-r1agent(M2, E2)", "-v1know(M2)", "-r1theme(K2, J2)", "-r1agent(K2, F2)", "-r1patient(K2, E2)", "-v1let(K2)", "-r1theme(I2, H2)", "-r1agent(I2, G2)", "-v1say(I2)"), List("r1agent(n2(M2, L2, K2, J2, I2, H2, G2, F2, E2, D2, C2, B2, A2), E2)", "-n1freedomC46(A2)", "-r1of(A2, B2)", "-n12thing(B2)", "-loc1france(C2)", "-n1C34satirize(D2)", "-a1neuter(E2)", "-loc1france(F2)", "-per1juppe(G2)", "-r1theme(M2, L2)", "-r1agent(M2, E2)", "-v1know(M2)", "-r1theme(K2, J2)", "-r1agent(K2, F2)", "-r1patient(K2, E2)", "-v1let(K2)", "-r1theme(I2, H2)", "-r1agent(I2, G2)", "-v1say(I2)"), List("v1accept(n2(M2, L2, K2, J2, I2, H2, G2, F2, E2, D2, C2, B2, A2))", "-n1freedomC46(A2)", "-r1of(A2, B2)", "-n12thing(B2)", "-loc1france(C2)", "-n1C34satirize(D2)", "-a1neuter(E2)", "-loc1france(F2)", "-per1juppe(G2)", "-r1theme(M2, L2)", "-r1agent(M2, E2)", "-v1know(M2)", "-r1theme(K2, J2)", "-r1agent(K2, F2)", "-r1patient(K2, E2)", "-v1let(K2)", "-r1theme(I2, H2)", "-r1agent(I2, G2)", "-v1say(I2)"), List("r1of(D2, o2(M2, L2, K2, J2, I2, H2, G2, F2, E2, D2, C2, B2, A2))", "-n1freedomC46(A2)", "-r1of(A2, B2)", "-n12thing(B2)", "-loc1france(C2)", "-n1C34satirize(D2)", "-a1neuter(E2)", "-loc1france(F2)", "-per1juppe(G2)", "-r1theme(M2, L2)", "-r1agent(M2, E2)", "-v1know(M2)", "-r1theme(K2, J2)", "-r1agent(K2, F2)", "-r1patient(K2, E2)", "-v1let(K2)", "-r1theme(I2, H2)", "-r1agent(I2, G2)", "-v1say(I2)"), List("n1justiceC34(o2(M2, L2, K2, J2, I2, H2, G2, F2, E2, D2, C2, B2, A2))", "-n1freedomC46(A2)", "-r1of(A2, B2)", "-n12thing(B2)", "-loc1france(C2)", "-n1C34satirize(D2)", "-a1neuter(E2)", "-loc1france(F2)", "-per1juppe(G2)", "-r1theme(M2, L2)", "-r1agent(M2, E2)", "-v1know(M2)", "-r1theme(K2, J2)", "-r1agent(K2, F2)", "-r1patient(K2, E2)", "-v1let(K2)", "-r1theme(I2, H2)", "-r1agent(I2, G2)", "-v1say(I2)")))

testSentpair("""    Both Indian officials and US diplomats admit that the two countries have  never been so far apart on crucial policy issues, but emphasise that the trip  is nevertheless important to set the course for future ties.""", """    Both Indian officials and US diplomats admit that the two countries have  never been so far apart on crucial policy issues, but emphasise that the trip  is nevertheless important to be decisive for future ties.""", """set the course for->be decisive for""", 
List(List("n1course(a1)"), List("n1trip(b1)"), List("n1country(c1)"), List("card(c1, e1)"), List("c2number(e1)"), List("n1numeral(e1)"), List("n1official(d1)"), List("a1indian(d1)"), List("r1theme(f1, g1)"), List("r1agent(f1, i1)"), List("v1admit(f1)"), List("a1never(o1)"), List("r1on(o1, n1)"), List("r1subset_of(k1, n1)"), List("r1patient(s1, a1)"), List("r1agent(s1, b1)"), List("v1set(s1)"), List("r1for(a1, t1)"), List("n1tiesC46(t1)"), List("a1future(t1)"), List("a1nevertheless(b1)"), List("a1important(b1)"), List("r1theme(k1, j1)"), List("n1emphasise(k1)"), List("r1subset_of(m1, n1)"), List("n1issuesC44(m1)"), List("r1of(m1, l1)"), List("n1policy(l1)"), List("a1crucial(m1)"), List("a1so(c1)"), List("a1far(c1)"), List("a1apart(c1)"), List("r1subset_of(h1, i1)"), List("n1diplomat(h1)"), List("loc1us(h1)"), List("r1subset_of(d1, i1)")), List(List("-n1trip(A2)", "-n1country(B2)", "-card(B2, D2)", "-c2number(D2)", "-n1numeral(D2)", "-n1official(C2)", "-a1indian(C2)", "-r1theme(E2, F2)", "-r1agent(E2, H2)", "-v1admit(E2)", "-a1never(N2)", "-r1on(N2, M2)", "-r1subset_of(J2, M2)", "-r1for(S2, R2)", "-n1tiesC46(R2)", "-a1future(R2)", "-a1decisive(A2)", "-a1nevertheless(A2)", "-a1important(A2)", "-r1theme(J2, I2)", "-n1emphasise(J2)", "-r1subset_of(L2, M2)", "-n1issuesC44(L2)", "-r1of(L2, K2)", "-n1policy(K2)", "-a1crucial(L2)", "-a1so(B2)", "-a1far(B2)", "-a1apart(B2)", "-r1subset_of(G2, H2)", "-n1diplomat(G2)", "-loc1us(G2)", "-r1subset_of(C2, H2)")))

testSentpair("""    Both Indian officials and US diplomats admit that the two countries have  never been so far apart on crucial policy issues, but emphasise that the trip  is nevertheless important to set the course for future ties.""", """    Both Indian officials and US diplomats admit that the two countries have  never been so far apart on crucial policy issues, but emphasise that the trip  is nevertheless important to be decisive for future ties.""", """set the course for@R@->be decisive for@R@""", 
List(List("n1course(a1)"), List("n1trip(b1)"), List("n1country(c1)"), List("card(c1, e1)"), List("c2number(e1)"), List("n1numeral(e1)"), List("n1official(d1)"), List("a1indian(d1)"), List("r1theme(f1, g1)"), List("r1agent(f1, i1)"), List("v1admit(f1)"), List("a1never(o1)"), List("r1on(o1, n1)"), List("r1subset_of(k1, n1)"), List("r1patient(s1, a1)"), List("r1agent(s1, b1)"), List("v1set(s1)"), List("r1for(a1, t1)"), List("n1tiesC46(t1)"), List("a1future(t1)"), List("a1nevertheless(b1)"), List("a1important(b1)"), List("r1theme(k1, j1)"), List("n1emphasise(k1)"), List("r1subset_of(m1, n1)"), List("n1issuesC44(m1)"), List("r1of(m1, l1)"), List("n1policy(l1)"), List("a1crucial(m1)"), List("a1so(c1)"), List("a1far(c1)"), List("a1apart(c1)"), List("r1subset_of(h1, i1)"), List("n1diplomat(h1)"), List("loc1us(h1)"), List("r1subset_of(d1, i1)")), List(List("-n1trip(A2)", "-n1country(B2)", "-card(B2, D2)", "-c2number(D2)", "-n1numeral(D2)", "-n1official(C2)", "-a1indian(C2)", "-r1theme(E2, F2)", "-r1agent(E2, H2)", "-v1admit(E2)", "-a1never(N2)", "-r1on(N2, M2)", "-r1subset_of(J2, M2)", "-r1for(S2, R2)", "-n1tiesC46(R2)", "-a1future(R2)", "-a1decisive(A2)", "-a1nevertheless(A2)", "-a1important(A2)", "-r1theme(J2, I2)", "-n1emphasise(J2)", "-r1subset_of(L2, M2)", "-n1issuesC44(L2)", "-r1of(L2, K2)", "-n1policy(K2)", "-a1crucial(L2)", "-a1so(B2)", "-a1far(B2)", "-a1apart(B2)", "-r1subset_of(G2, H2)", "-n1diplomat(G2)", "-loc1us(G2)", "-r1subset_of(C2, H2)")))

testSentpair("""    The cause of the fire, which broke out at around 1:30 a.m. and raged for  over an hour, was under investigation, police said.""", """    The cause of the fire, which broke out at around 1:30 a.m. and raged for  over an hour, was since investigation, police said.""", """under->since""", 
List(List("n1fireC44(a1)"), List("n1cause(b1)"), List("r1under(b1, i1)"), List("n1saidC46(i1)"), List("r1of(i1, h1)"), List("n1police(h1)"), List("a1investigationC44(i1)"), List("r1of(b1, a1)"), List("r1for(e1, d1)"), List("a1over(d1)"), List("n1hourC44(d1)"), List("r1agent(e1, a1)"), List("v1rage(e1)"), List("r1at(g1, f1)"), List("n1aC46mC46(f1)"), List("a11C5830(f1)"), List("a1around(f1)"), List("a1out(g1)"), List("r1agent(g1, a1)"), List("v1break(g1)")), List(List("-n1fireC44(A2)", "-n1cause(B2)", "-r1since(B2, I2)", "-n1saidC46(I2)", "-r1of(I2, H2)", "-n1police(H2)", "-a1investigationC44(I2)", "-r1of(B2, A2)", "-r1for(E2, D2)", "-a1over(D2)", "-n1hourC44(D2)", "-r1agent(E2, A2)", "-v1rage(E2)", "-r1at(G2, F2)", "-n1aC46mC46(F2)", "-a11C5830(F2)", "-a1around(F2)", "-a1out(G2)", "-r1agent(G2, A2)", "-v1break(G2)")))

testSentpair("""    UN Secretary-general Boutros Boutros-Ghali called Thursday in a statement  published in New York for the boundaries of the safe areas, where Moslems are  under siege from Serbs, to be clearly defined.""", """    UN Secretary-general Boutros Boutros-Ghali called Thursday in a statement  published in New York for the boundaries of the safe areas, where Moslems are  since siege from Serbs, to be clearly defined.""", """under->since""", 
List(List("nam1serbsC44(a1)"), List("n1areasC44(b1)"), List("a1safe(b1)"), List("n1boundary(c1)"), List("loc1new_york(d1)"), List("tim1thursday(e1)"), List("org1un_secretaryC45general_bout(f1)"), List("r1in(j1, i1)"), List("r1for(h1, c1)"), List("r1of(c1, b1)"), List("n1moslem(l1)"), List("r1under(l1, n1)"), List("a1clearly(n1)"), List("a1definedC46(n1)"), List("n1siege(n1)"), List("r1from(o1, a1)"), List("n1siege(o1)"), List("r1where(b1, g1)"), List("r1in(h1, d1)"), List("r1patient(h1, i1)"), List("v1publish(h1)"), List("n1statement(i1)"), List("r1patient(j1, e1)"), List("r1agent(j1, f1)"), List("v1call(j1)")), List(List("-nam1serbsC44(A2)", "-n1areasC44(B2)", "-a1safe(B2)", "-n1boundary(C2)", "-loc1new_york(D2)", "-tim1thursday(E2)", "-org1un_secretaryC45general_bout(F2)", "-r1in(J2, I2)", "-r1for(H2, C2)", "-r1of(C2, B2)", "-n1moslem(L2)", "-r1since(L2, N2)", "-a1clearly(N2)", "-a1definedC46(N2)", "-n1siege(N2)", "-r1from(O2, A2)", "-n1siege(O2)", "-r1where(B2, G2)", "-r1in(H2, D2)", "-r1patient(H2, I2)", "-v1publish(H2)", "-n1statement(I2)", "-r1patient(J2, E2)", "-r1agent(J2, F2)", "-v1call(J2)")))
*/
testSentpair("""    A deer is jumping over the wall""", """    A deer is jumping over the fence""", """jump over->jump over""", 
List(List("deer(d1)"), List("agent(j1, d1)"), List("jump(j1)"), List("over(j1, f1)"), List("wall(f1)")), 
List(List("-deer(d1)", "-agent(j1, d1)", "-jump(j1)", "-over(j1)", "-patient(j1, f1)", "-fence(f1)") ))

}

  def testThisPair(f1:List[List[String]], f2:List[List[String]]) :Unit = {
    val formula1 = new CNF(f1)
    val formula2 = new CNF(f2)

    println("F1: " + formula1)
    println("F2: " + formula2)
    println
    val result  = resolveToFindDifference(f1, f2)
    result match {
      case Some((ir1, ir2)) =>
	println(ir1)
	println
	println(ir2)
      case None => println("failed.")
    }
  }

  def testSentpair(sent1Text:String, sent2Text:String, ruleText:String, f1:List[List[String]], f2:List[List[String]]) :Unit = {
    println("---------------------------------")
    println
    println("S1: " + sent1Text)
    println
    println("S2 : " + sent2Text)
    println
    println("Rule : " + ruleText)
    println
    testThisPair(f1, f2)
  }
}


