package utcompling.mlnsemantics.util

import utcompling.scalalogic.fol.expression._

/*
object CNF
{
  /**
   * 
  * Takes an f.Expr and transforms it into Conjunction Normal Form.
  * Steps: 
  * -eliminate implications
  * -push negations to atom level
  * -skolemize
  * -drop universals
  * -distribute all disjunctions over conjunctions
  */
	
  def toCNF(exp: FolExpression) : FolExpression = 
  {
	eliminateImpPushNegation(exp, List(), List(), false);
  }
  
  
  private def eliminateImpPushNegation(e: FolExpression, univVars: List[String], existVars: List[String], isNegated: Boolean): FolExpression =
  {
  	  e match 
      {
      	case FolExistsExpression(v, term) => {
	      	 isNegated match {
	      	   case true => FolAllExpression(v, eliminateImpPushNegation(term, univVars:+v.name, existVars, isNegated))
	      	   case false => FolExistsExpression(v, eliminateImpPushNegation(term, univVars, existVars:+v.name, isNegated)) 
	      	 }
      	} 
        case FolAllExpression(v, term) => {
        	isNegated match {
	      	   case true => FolExistsExpression(v, eliminateImpPushNegation(term, univVars, existVars:+v.name, isNegated))
	      	   case false => FolAllExpression(v, eliminateImpPushNegation(term, univVars:+v.name, existVars, isNegated))
	      	 }
        } 
        case FolNegatedExpression(term) => eliminateImpPushNegation(term, univVars, existVars, !isNegated)
        case FolAndExpression(first, second) => 
        	isNegated match {
        		case true => FolOrExpression(eliminateImpPushNegation(first, univVars, existVars, isNegated),
        				eliminateImpPushNegation(second, univVars, existVars, isNegated))
        		case false => FolAndExpression(eliminateImpPushNegation(first, univVars, existVars, isNegated),
        				eliminateImpPushNegation(second, univVars, existVars, isNegated))
        	}
      	case FolOrExpression(first, second) => 
        	isNegated match {
        		case true => FolAndExpression(eliminateImpPushNegation(first, univVars, existVars, isNegated),
        				eliminateImpPushNegation(second, univVars, existVars, isNegated))
        		case false => FolOrExpression(eliminateImpPushNegation(first, univVars, existVars, isNegated),
        				eliminateImpPushNegation(second, univVars, existVars, isNegated))
        	}
      	case FolIfExpression(first, second) => eliminateImpPushNegation(FolOrExpression(FolNegatedExpression(first), second), univVars, existVars, isNegated)
      	case FolIffExpression(first, second) => throw new RuntimeException("not reachable")
        case FolEqualityExpression(first, second) => 
        	isNegated match {
        		case true => FolNegatedExpression(e)
        		case false => e
        	}
        case FolAtom(pred, args @ _*) => 
        	isNegated match {
        		case true => FolNegatedExpression(e)
        		case false => e
        	}
        case _ => throw new RuntimeException("not reachable")
        
      }
/*
	  e match 
      {
      	case FolExistsExpression(v, term) => goExist(term, univVars, existVars:+v.name, isNegated)
      	case FolAllExpression(v, term) if (isNegated) => goExist(term, univVars, existVars:+v.name, isNegated)      	
        case _ => 
      }
      * 
      */
  }

  /*  
  private def rename(v: Variable): Variable =
	Variable(v.name + "_q")	  
    
  private def rename(e: FolExpression): FolExpression =
  {
    e match {
    	case FolExistsExpression(v, term) => FolExistsExpression(rename(v), rename(term))	
    	case FolAllExpression(v, term) => FolAllExpression(rename(v), rename(term))
    	case FolAtom(pred, args @ _*) => FolAtom(pred, args.map(rename(_)) :_ * )	
    	case FolVariableExpression(v) => FolVariableExpression(rename(v)) 
    	case _=>e.visitStructured(rename, e.construct)
    }
  }
  * 
  */
  /*
  private def replaceSimplify(e: FolExpression, quantifiers: Set[String]): FolExpression =
  {
      e match 
      {
      	case FolExistsExpression(v, term) => 
            val mTerm = replaceSimplify(term, quantifiers + v.name)
      		getBoolean3(mTerm)  match 
      		{
      		  case 't' => trueFolExp
      		  case 'f' => falseFolExp
      		  case 'd' => FolExistsExpression(v, mTerm);
      		} 
        case FolAllExpression(v, term) =>
            val mTerm = replaceSimplify(term, quantifiers + v.name)
      		getBoolean3(mTerm)  match 
      		{
      		  case 't' => trueFolExp
      		  case 'f' => falseFolExp
      		  case 'd' => FolAllExpression(v, mTerm);
      		}
        case FolNegatedExpression(term) => 
            val mTerm = replaceSimplify(term, quantifiers) 
      		getBoolean3(mTerm)  match 
      		{
      		  case 't' => falseFolExp
      		  case 'f' => trueFolExp
      		  case 'd' => FolNegatedExpression(mTerm);
      		}            
        case FolAndExpression(first, second) =>
            val mFirst = replaceSimplify(first, quantifiers) 
      		val mSecond = replaceSimplify(second, quantifiers)
      		(getBoolean3(mFirst),getBoolean3(mSecond)) match 
      		{
      		  case ('t', 't') => trueFolExp
      		  case ('t', 'f') => falseFolExp
      		  case ('t', 'd') => mSecond
      		  case ('f', 't') => falseFolExp
      		  case ('f', 'f') => falseFolExp
      		  case ('f', 'd') => falseFolExp
      		  case ('d', 't') => mFirst
      		  case ('d', 'f') => falseFolExp
      		  case ('d', 'd') => FolAndExpression(mFirst, mSecond);      		    
      		}
      	case FolOrExpression(first, second) =>
				//println (e)
      	   val mFirst = replaceSimplify(first, quantifiers) 
      		val mSecond = replaceSimplify(second, quantifiers)
      		(getBoolean3(mFirst),getBoolean3(mSecond)) match 
      		{
      		  case ('t', 't') => trueFolExp
      		  case ('t', 'f') => trueFolExp
      		  case ('t', 'd') => trueFolExp
      		  case ('f', 't') => trueFolExp
      		  case ('f', 'f') => falseFolExp
      		  case ('f', 'd') => mSecond
      		  case ('d', 't') => trueFolExp
      		  case ('d', 'f') => mFirst
      		  case ('d', 'd') => FolOrExpression(mFirst, mSecond);      		    
      		}
      	case FolIfExpression(first, second) => 
      		val mFirst = replaceSimplify(first, quantifiers) 
      		val mSecond = replaceSimplify(second, quantifiers)
      		(getBoolean3(mFirst),getBoolean3(mSecond)) match 
      		{
      		  case ('t', 't') => trueFolExp
      		  case ('t', 'f') => falseFolExp
      		  case ('t', 'd') => mSecond
      		  case ('f', 't') => trueFolExp
      		  case ('f', 'f') => trueFolExp
      		  case ('f', 'd') => trueFolExp
      		  case ('d', 't') => trueFolExp
      		  case ('d', 'f') => FolNegatedExpression(mFirst)
      		  case ('d', 'd') => FolIfExpression(mFirst, mSecond);      		    
      		}
      		
      	case FolIffExpression(first, second) => throw new RuntimeException("not reachable")
        case FolEqualityExpression(first, second) => 
        	if (!quantifiers.contains(first.asInstanceOf[FolVariableExpression].variable.name )
        		&& !quantifiers.contains(second.asInstanceOf[FolVariableExpression].variable.name ))
        	{
        		if(first == second)
        			trueFolExp
        		else 
        			falseFolExp
        	}
        	else
        		FolEqualityExpression(replaceSimplify(first, quantifiers), replaceSimplify(second, quantifiers));
        case FolAtom(pred, args @ _*) => 
          val changedArgs = args.map(replaceSimplify(_, quantifiers))
          val changedE = FolAtom(pred,changedArgs:_*);
          
          if( (changedArgs.map(_.name).toSet & quantifiers).isEmpty )
          { //all constants
            if(allEvidence.contains(changedE))
              trueFolExp
            else if (allEvidence.contains(FolNegatedExpression(changedE)))
              falseFolExp
            else 
              changedE
          }else 
            changedE //some variables
        
        case FolVariableExpression(v) => FolVariableExpression(replaceSimplify(v, quantifiers));

        case _ => println (e); throw new RuntimeException("not reachable")
      }
  }
  */
/*
  private def goUniv(e: FolExpression, univVars: List[String], existVars: List[String], isNegated: Boolean): FolExpression = 
  {
      e match 
      {
      	case FolExistsExpression(v, term) => {
	      	 isNegated match {
	      	   case true => FolExistsExpression(v, goUniv(term, univVars:+v.name, existVars, isNegated))
	      	   case false => goExist(e, univVars, List(), isNegated)
	      	 }
      	} 
        case FolAllExpression(v, term) => {
        	isNegated match {
	      	   case true => goExist(e, univVars, List(), isNegated)
	      	   case false => FolAllExpression(v, goUniv(term, univVars:+v.name, existVars, isNegated))
	      	 }
        } 
        case FolNegatedExpression(term) => FolNegatedExpression(goUniv(term, univVars, existVars, !isNegated))
        case FolAndExpression(first, second) => FolAndExpression(goUniv(first, univVars, existVars, isNegated), 
        														goUniv(second, univVars, existVars, isNegated))
      	case FolOrExpression(first, second) => FolOrExpression(goUniv(first, univVars, existVars, isNegated),
      														goUniv(second, univVars, existVars, isNegated))
      	case FolIfExpression(first, second) => FolIfExpression(goUniv(first, univVars, existVars, !isNegated),
      														goUniv(second, univVars, existVars, isNegated))
      	case FolIffExpression(first, second) => goUniv(FolAndExpression(FolIfExpression(first, second), FolIfExpression(second, first)), univVars, existVars, isNegated)
        case FolEqualityExpression(first, second) => e
        case FolAtom(pred, args @ _*) => e
        case _ => throw new RuntimeException("not reachable")
      }
  }
  * 
  */
}
*/