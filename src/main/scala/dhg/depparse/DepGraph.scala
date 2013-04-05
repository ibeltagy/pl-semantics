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
    					"PRP" /*hers, herself, me, ...*/, "PRP$" /*her his mine my our ours their thy your*/,
    					"RB" /*stirringly prominently technologically*/, "RBR" /*further gloomier grander graver*/, "RBS"/*best biggest bluntest earliest*/);
    
    val ignoreTags = List("CC" /*Coordinating conjunction*/, "DT"/*Determiner*/, "EX"/*existential there*/, "IN", "LS"/*list item marker*/,
    					  "POS" /*'s*/, "SYM", "TO");
    val todoTags = List("CD"/*Cardinality*/, "FW"/*Foreign word*/, "MD"/*modal auxiliary*/, "PDT" /*all both half many quite such sure this*/, 
    					"RP"/*particle*/, "UH" /*goodbye, honey, ..*/);
    val whTags = List("WDT", "WP", "WP$", "WRB");//all of them are TODO

    
    var preds_1 = nodes.toList.flatMap ( n=> 
      if (eventTags.contains(n.tag))
    	  List((n.index, Predicate(n.index, n.lemma, "e"+n.index, n.tag, "", n.token)));
      else if (indvTags.contains(n.tag))
    	  List((n.index, Predicate(n.index, n.lemma, "x"+n.index, n.tag, "", n.token)));
      else 
    	 None
    ).toMap
    
    var preds_2: List[Predicate] = List(); 
    
    relations.foreach{ rel =>
      
    	rel.rel.value match {
    	  case "nsubj" => preds_2 = Predicate(0, "agent", preds_1.apply(rel.gov.index).varName, "rel", "agent", "token", Option(preds_1.apply(rel.dep.index).varName)) :: preds_2
    	  case _=> 
    	}
    }
    
    return preds_2 ++ preds_1.map(_._2);
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

