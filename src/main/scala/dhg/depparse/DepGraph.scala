package dhg.depparse

import opennlp.scalabha.util.CollectionUtil._

case class DepRel[E, W](rel: Dependency, gov: DepNode[E], dep: DepNode[W])

/**
 * A Dependency Graph holder class
 * 
 * @param relations		The set of dependency relations
 * @param sourceTree	The dependency tree from which the graph was constructed
 * @param source		The original sentence from which the graph was taken
 */
case class DepGraph[E, W](nodes:Iterable[DepNode[String]], relations: Set[DepRel[E, W]], sourceTree: DepNode[String], source: String) {
  //lazy val nodes = childMap.keySet
  lazy val childMap = groupRelations(relations)

  private[this] def groupRelations(relations: Set[DepRel[E, W]]) = relations.groupBy((_: DepRel[E, W]).gov).mapVals(_.map(r => (r.rel, r.dep)))

  def graphviz =
    ("digraph G {" +:
      relations.toVector.map{
        case DepRel(reln, DepNode(gov, _, govTag, govIdx, _), DepNode(dep, _, depTag, depIdx, _)) =>
          """  "%s-%s-%s" -> "%s-%s-%s" [ label = "%s" ]""".format(gov, govTag, govIdx, dep, depTag, depIdx, reln)
      } :+
      "}").mkString("\n")
   
  def logic: List[Predicate] =  
  {
    //http://www.comp.leeds.ac.uk/ccalas/tagsets/upenn.html
    val eventTags = List("VB", "VBD", "VBG", "VBN", "VBP", "VBZ");
    val indvTags = List("JJ", "JJR"/*adjective, comparative*/, "JJS"/*adjective, superlative*/,
    					"NN", "NNP", "NNPS", "NNS", 
    					"RB" /*stirringly prominently technologically*/, "RBR" /*further gloomier grander graver*/, "RBS"/*best biggest bluntest earliest*/, 
    					"PRP" /*hers, herself, me, ...*/, "PRP$" /*her his mine my our ours their thy your*//*need better handling with corrference resolution*/);
    
    val ignoreTags = List("CC" /*Coordinating conjunction*/, "DT"/*Determiner*/, "EX"/*existential there*/, "IN", "LS"/*list item marker*/,
    					  "POS" /*'s*/, "SYM", "TO");
    val todoTags = List("CD"/*Cardinality*/, "FW"/*Foreign word*/, "MD"/*modal auxiliary*/, "PDT" /*all both half many quite such sure this*/, 
    					"RP"/*particle*/, "UH" /*goodbye, honey, ..*/);
    val whTags = List("WDT", "WP", "WP$", "WRB");//all of them are TODO

    var unaryPreds = nodes.toList.flatMap ( n=> 
      if (eventTags.contains(n.tag))
    	  List((n.index, Predicate(n.index, n.lemma, "e"+n.index, n.tag, "", n.token)));
      else if (indvTags.contains(n.tag))
    	  List((n.index, Predicate(n.index, n.lemma, "x"+n.index, n.tag, "", n.token)));
      else 
    	 None
    ).toMap
    
    var binaryPreds: List[Predicate] = List();
    
    def addBinaryPred (rel: DepRel[E, W], name: String) = {
      unaryPreds.find(p => p._1 == rel.gov.index) match {
        case Some(gov) => unaryPreds.find(p => p._1 == rel.dep.index) match {
        	case Some(dep) =>
        	  binaryPreds = Predicate(0, name, gov._2.varName, "rel", name, rel.rel.value, Option(dep._2.varName)) :: binaryPreds
        	case None =>
        	}
        case None =>
      }
      
    }
    def remanVar(rel: DepRel[E, W]) = { //dep as gov
    	val var1 = unaryPreds.apply(rel.gov.index).varName;
        val pred2 = unaryPreds.apply(rel.dep.index);
    	unaryPreds -= rel.dep.index;
    	unaryPreds += (pred2.idx -> Predicate(pred2.idx, pred2.name, var1, pred2.tag, pred2.typ, pred2.token));
    }
    def remanVarGovAsDep(rel: DepRel[E, W]) = {
    	val var1 = unaryPreds.apply(rel.dep.index).varName;
        val pred2 = unaryPreds.apply(rel.gov.index);
    	unaryPreds -= rel.gov.index;
    	unaryPreds += (pred2.idx -> Predicate(pred2.idx, pred2.name, var1, pred2.tag, pred2.typ, pred2.token));
    }
    def removeUnaryPred(idx: Int) = {
    	unaryPreds -= idx;
    }
    
    relations.foreach{ rel =>
      rel.rel match {
//////////////////////
	 case Root(x) => throw new Exception("Unexpected dependency: %s".format(x) ) // n/a  TODO: for all relations below, make sure they are correct for all POS
	    case Dep(x) => addBinaryPred(rel, "rel")
	    case AuxPass(x) => removeUnaryPred(rel.dep.index)
	    case Aux(x) => removeUnaryPred(rel.dep.index)
	    case Cop(x) => removeUnaryPred(rel.dep.index)//TODO: THe correct handling requires changes in NSubj
	 case Arg(x) => throw new Exception("Unexpected dependency: %s".format(x) ) // n/a
	    case Agent(x) => addBinaryPred(rel, "by")
	 case Comp(x) => throw new Exception("Unexpected dependency: %s".format(x) ) // n/a 
	    case AComp(x) => addBinaryPred(rel, "patient") //TODO: This is wrong.
	    case Attr(x) =>  //TODO: I think it is ok to do nothing
	    case CComp(x) => addBinaryPred(rel, "prop") //TODO: this should be replaced with proposition and EXIST.
	    case PComp(x) => throw new Exception("Unexpected dependency: %s".format(x) ) // n/a because of collapsed dependencies 
	    case XComp(x) => addBinaryPred(rel, "prop") //TODO: this should be replaced with proposition and EXIST
	    case Complm(x) => removeUnaryPred(rel.dep.index);
	 case Obj(x) =>  throw new Exception("Unexpected dependency: %s".format(x) ) // n/a
	    case DObj(x) => addBinaryPred(rel, "patient")
	    case IObj(x) => addBinaryPred(rel, "theme")
	    case PObj(x) => throw new Exception("Unexpected dependency: %s".format(x) ) // n/a because of collapsed dependencies
	    case Mark(x) => //TODO: this is really complex. Every marker needs a separate handling
	    case Rel(x) => throw new Exception("Unexpected dependency: %s".format(x) ) // TODO: weird. I did not understand it. 
	    case Subj(x) => throw new Exception("Unexpected dependency: %s".format(x) ) // n/a
	    case NSubjPass(x) => addBinaryPred(rel, "patient") //TODO: does the problem of NSubj also applies here ?  
	    case NSubj(x) => if (rel.gov.tag.startsWith("JJ")) remanVar(rel) else addBinaryPred(rel, "agent") //TODO: not very correct. man is eating vs. man is big
	    case CSubjPass(x) => throw new Exception("Unsupported dependency: %s".format(x) ) //TODO: did not understand it.  
	    case CSubj(x) => throw new Exception("Unsupported dependency: %s".format(x) ) //TODO: did not understand it.
	    case CC(x) => throw new Exception("Unexpected dependency: %s".format(x) ) // n/a because of collapsed dependencies
	    case Conj(x) =>  //TODO: in case it is conj_and, do nothing. More work is required for other conjunctions, and they are a lot.  
	    case Expl(x) =>  removeUnaryPred(rel.gov.index) //TODO: I think the right thing is to do nothing, or do I need to delete the verb ?
	    case Mod(x) => throw new Exception("Unexpected dependency: %s".format(x) ) // n/a
	    case Abbrev(x) => remanVar(rel)
	    case AMod(x) => remanVar(rel)
	    case Appos(x) => remanVarGovAsDep(rel)
	    case AdvCl(x) => addBinaryPred(rel, "prop") //TODO: this should be replaced with proposition and EXIST.
	    case PurpCl(x) => addBinaryPred(rel, "prop") //TODO: this should be replaced with proposition and EXIST.
	    case Det(x) =>  //TODO: for now, do nothing. It is important to handle all determiners carefully
	    case PreDet(x) => //TODO: for now, do nothing. It is important to handle all determiners carefully
	    case PreConj(x) => //TODO: for now, do nothing. It is important to handle all preconjunctions carefully. Probably, the right thing to do is nothing
	    case Infmod(x) => addBinaryPred(rel, "rel") //TODO: well, it does not seem there is another way to do this. 
	    case MWE(x) => throw new Exception("Unexpected dependency: %s".format(x) ) // n/a because of collapsed dependencies
	    case PartMod(x) => throw new Exception("Unsupported dependency: %s".format(x) ) //TODO: did not understand it.
	    case AdvMod(x) => remanVar(rel);
	    case Neg(x) => //TODO: this should be replaced with a negation and EXIST.
	      unaryPreds += (rel.dep.index -> Predicate(rel.dep.index, rel.dep.lemma.toString(), unaryPreds.apply(rel.gov.index).varName, rel.dep.tag, "", rel.dep.token));
	    case RcMod(x) => //TODO: DO nothing, I think this is the right thing to do
	    case QuantMod(x) => remanVar(rel); 
	    case NN(x) => addBinaryPred(rel, "nn")
	    case NpAdvMod(x) => addBinaryPred(rel, "rel") //TODO: this is according to Boxer's notation, but is it correct ? 
	    case TMod(x) => x
	    case Number(x) => x
	    case Num(x) => x
	    case Prep(x) => x
	    case Possessive(x) => x
	    case Poss(x) => x
	    case Prt(x) => x
	    case Parataxis(x) => x
	    case Punct(x) => x
	    case Ref(x) => x
	    case SDep(x) => x
	    case XSubj(x) => x
//////////////////////	  
    	  case _=> println("ERROR: unsupported relation" + rel.rel.value)
    	} 
    }
    return (binaryPreds ++ unaryPreds.map(_._2)).toList.sortBy(p=>p.varName.substring(1).toInt*100 + (p.varName2 match {
      	case Some(v)=> v.substring(1).toInt
    	case _ => 0
    }) );
  }

  
  
  /**
   * Add additional relations that are implied by the structure
   */
      /*
  def modify() = {
    var newRelations = Set[DepRel[E, W]]()

    //
    // Copy dependencies from parent to child of a "conj_and" relationship 
    //
    newRelations ++= {
      val childMap = groupRelations(relations ++ newRelations)
      (for (
        (_, children) <- childMap;
        (Conj(rel), depNode @ DepNode(_, _, dTag, _, _)) <- children if rel.value == "conj_and"
      ) yield {
        children.filterNot(_._2 == depNode).map { case (r, n) => DepRel(r, depNode.asInstanceOf[DepNode[E]], n) }
      }).flatten
    }

    //
    // From "He was found murdered", extract "He was murdered"
    //
    newRelations ++= {
      val childMap = groupRelations(relations ++ newRelations)
      (for (
        (_, children) <- childMap if children.exists(_._1.isInstanceOf[NSubjPass]);
        (Dep(rel), depNode @ DepNode(_, _, dTag, _, _)) <- children if dTag.startsWith("VB")
      ) yield {
        val dChildren = childMap.get(depNode.asInstanceOf[DepNode[E]])
        if (dChildren == None || !dChildren.get.exists(_._1.isInstanceOf[NSubj]))
          children.filterNot(_._2 == depNode).map { case (r, n) => DepRel(r, depNode.asInstanceOf[DepNode[E]], n) }
        else
          Seq()
      }).flatten
    }

    DepGraph(relations ++ newRelations, sourceTree, source)
  }
  */
}

