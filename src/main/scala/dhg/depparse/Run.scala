package dhg.depparse

object Run {

  def main(args: Array[String]) {

    val texts = Vector(
/***************************************************************/
/*Root*/ // n/a
/*Dep*/  "Then, as if to show that he could",
/*AuxPass*/ "a man has been killed", 
/*Aux*/ "a man is eating",
/*Cop*/ "bill is big", //TODO
/*Arg*/ // n/a 
/*Agent*/ "a man has been killed by the police",
/*Comp*/ // n/a
/*AComp*/ "She looks beautiful", //TODO 
/*Attr*/ "What is that?", //TODO
/*CComp*/ "a man says that he swims", //TODO
/*PComp*/ "They heard about you missing classes",// n/a
/*XComp*/ "a man is  happy to eat", //TODO
/*Complm*/ "He says that you like to swim", 
/*Obj*/  // n/a
/*DObj(x)*/ "a man eats food", 
/*IObj(x)*/ "a man gives a poor a charity",
/*PObj*/ "a man sits on a chair", 
/*Mark*/ "he is late because the rain is heavy", //TODO
/*Rel*/  // TODO: Dunno what to do??
/*Subj*/  // n/a
/*NSubjPass*/ "a man has been shot",  
/*NSubj(x)*/ "a man eats", //TODO: what about "a man is tall" ?  
/*CSubjPass*/ //TODO: could not find an example
/*CSubj*/  //TODO: could not find an example
/*CC*/  "Bill is big or honest", 
/*Conj*/ "They either ski or snowboard", //TODO: implement it for all possible conjunction
/*Expl*/ "There is a ghost in the room", //TODO: is it correct to do nothing, or do I remove the verb ?
/*Mod*/  // n/a 
/*Abbrev*/ "The Australian Broadcasting Corporation (ABC)", 
/*AMod(x)*/ "tall man",
/*Appos*/ "Sam, my brother", 
/*AdvCl*/ "The accident happened as the night was falling", //TODO
/*PurpCl*/ "He talked to him in order to secure the account", //TODO
/*Det*/  "all man are smart", //TODO
/*PreDet*/ "all the men are smart", //TODO
/*PreConj*/ "Both the boys and the girls are here", //TODO
/*Infmod*/ "Points to establish are . . .", //TODO
/*MWE*/ "he went home rather than work ", 
/*PartMod*/ //TODO: could not find an example
/*AdvMod(x)*/"a man drives quickly", 
/*Neg*/ "a man can not walk", //TODO
/*RcMod*/ "I saw the book  you bought", //TODO
/*QuantMod*/ "About 200 people came to the party",
/*NN(x)*/ "a man eats a pizza slice", 
/*NpAdvMod*/ "it is 6 feet long", //TODO
/*TMod*/ "Last night, I swam in the pool", //TODO 
/*Number*/ "I lost $ 3.2 billion", //TODO
/*Num*/ "Sam eats 3 sheeps.", //TODO
/*Prep*/ "I saw a cat in a hat", 
/*Possessive*/ "Bill's clothes", 
/*Poss*/ "Bill's clothes",
/*Prt*/ "They shut down the station", //TODO
/*Parataxis*/ //TODO: could not find an example
/*Punct*/ "Go home!",
/*Ref*/ //TODO: could not find an example 
/*SDep*/ // n/a
/*XSubj*/ //TODO: could not find an example
/***************************************************************/
	"null"        
    //"John saw the man with the red clean bike.",
    //"The accident happened as the night was falling", 
    //"She gave me a raise",
    //"A man eats slowly disgustingly "
    
    //"This is a test.",
    //"Bills on ports and immigration were submitted by Senator Brownback, Republican of Kansas.",
    //"John likes dogs and does not like cats."

    )
    
    val parser = DepParser.load()
    val graphs = texts.map(t => parser.apply(t))
    
    graphs.foreach(_.foreach{g =>
      println(g.source + "\t")
      //println(g.relations)
      //println(g.sourceTree.graphviz)
      println(g.graphviz)
      println(g.logic)
      println
    })
  }

}
