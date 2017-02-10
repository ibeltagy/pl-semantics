package utcompling.mlnsemantics.util

import edu.stanford.nlp.util.logging.Redwood
import edu.stanford.nlp.ling.HasWord
import edu.stanford.nlp.ling.TaggedWord
import edu.stanford.nlp.parser.nndep.DependencyParser
import edu.stanford.nlp.process.DocumentPreprocessor
import edu.stanford.nlp.tagger.maxent.MaxentTagger
import edu.stanford.nlp.trees.GrammaticalStructure
import edu.stanford.nlp.parser.nndep.demo.DependencyParserDemo
import java.io.StringReader
import scala.collection.JavaConversions._
import edu.stanford.nlp.trees.TypedDependency
import edu.stanford.nlp.trees.GrammaticalRelation
import edu.stanford.nlp.ling.IndexedWord
import scala.collection.mutable.ListBuffer
import com.sun.org.apache.commons.logging.LogFactory
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerVariable
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerExpression
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerPred
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerRel
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerDrs
import utcompling.scalalogic.discourse.candc.boxer.expression.BoxerIndex
import utcompling.mlnsemantics.util.Config
import java.util.Properties
import edu.stanford.nlp.pipeline.StanfordCoreNLP
import edu.stanford.nlp.pipeline.Annotation
import edu.stanford.nlp.process.Morphology
import utcompling.mlnsemantics.run.Sts
import edu.stanford.nlp.ling.CoreLabel

object DepParseUtil 
{
	private val LOG = LogFactory.getLog(classOf[DepParseUtil])
	private var tagger:MaxentTagger = null;
	private var parser:DependencyParser = null;
	private var morphology:Morphology = null;
	private var isLoaded = false;
	private var lastEntityId = 0;
	def main(args: Array[String]) 
	{
		System.err.println("host: ");
		if (Sts.opts == null)
			Sts.opts = new Config()
	//val text = "I can almost always tell when movies use fake dinosaurs. All of them are. A man is walking to the car. A man bought an apple and an orange. ";
		val text = List(
				"Sue is in shape. ",
				"A man is driving Mary's red car fast. ",
				"A man is driving his car. ", // TODO
				"The Arab League is expected to give its official blessing to Determination Storm on Saturday, which could clear the way for a ground invasion, CNN's Becky Anderson reported.",
				"Liam Neeson, Ed Harris and Joel Kinnaman star in director Jaume Collet-Serra's crime film about a hit man trying to save his estranged son from a revenge plot. ", 
				"Sanaa, Yemen (CNN) In just a few weeks' time, good relations with neighbors have become a matter of survival for Yemen President Abdu Rabu Mansour Hadi. "
				).mkString(" ");
		process(text, "h", false)

		// creates a StanfordCoreNLP object, with POS tagging, lemmatization, NER, parsing, and coreference resolution 
		//val props = new Properties();
		//props.setProperty("annotators", "tokenize, ssplit,pos,lemma");
		//val pipeline = new StanfordCoreNLP(props);
				
		// create an empty Annotation just with the given text
		//val document = new Annotation(text);
		
		// run all Annotators on this text
		//pipeline.annotate(document);
		
		// these are all the sentences in this document
		// a CoreMap is essentially a Map that uses class objects as keys and has values with custom types
		//val sentences = document.get(SentencesAnnotation.class);
		/*
		for(CoreMap sentence: sentences) {
		  // traversing the words in the current sentence
		  // a CoreLabel is a CoreMap with additional token-specific methods
		  for (CoreLabel token: sentence.get(TokensAnnotation.class)) {
		    // this is the text of the token
		    String word = token.get(TextAnnotation.class);
		    // this is the POS tag of the token
		    String pos = token.get(PartOfSpeechAnnotation.class);
		    // this is the NER label of the token
		    String ne = token.get(NamedEntityTagAnnotation.class);
		  }
		
		  // this is the parse tree of the current sentence
		  Tree tree = sentence.get(TreeAnnotation.class);
		
		  // this is the Stanford dependency graph of the current sentence
		  SemanticGraph dependencies = sentence.get(CollapsedCCProcessedDependenciesAnnotation.class);
		}*/		
		// This is the coreference link graph
		// Each chain stores a set of mentions that link to each other,
		// along with a method for getting the most representative mention
		// Both sentence and token offsets start at 1!
		//Map<Integer, CorefChain> graph = 
		//  document.get(CorefChainAnnotation.class);
		
	}
	def load () = 
	{
		tagger = new MaxentTagger("edu/stanford/nlp/models/pos-tagger/english-left3words/english-left3words-distsim.tagger");
		parser = DependencyParser.loadFromModelFile(DependencyParser.DEFAULT_MODEL);
		morphology = new Morphology();
		isLoaded = true;
	}
	def process(doc: String, discId: String, wordSequence:Boolean) : BoxerExpression = 
	{
		if(!DepParseUtil.isLoaded)
			DepParseUtil.load();
		lastEntityId = 1;
		val docTokensCnt = doc.split(" ").length;
		var docTokensCntIncreamental = 0;
		val tokenizer:DocumentPreprocessor = new DocumentPreprocessor(new StringReader(doc));
		val sentences = tokenizer.iterator();
		var currentWordCounter = 0;
		val boxExpRef:ListBuffer[BoxerVariable] = ListBuffer();
		val boxExpCond:ListBuffer[BoxerExpression] = ListBuffer();
		while (sentences.hasNext())  
		{
			val sentence = sentences.next();
			docTokensCntIncreamental = docTokensCntIncreamental + sentence.length;
			
			val tagged = tagger.tagCoreLabelsOrHasWords(sentence, morphology, true)
			val (e, r) = if (wordSequence)
			{
				val stopWords = List("a", "the", "and", "or", ".", ",", "!", "?", "am", "is", "are", "have", "has", "been", "will");
				val e: ListBuffer[utcompling.mlnsemantics.util.DepParseUtil.Entity] = ListBuffer();
				val r: ListBuffer[utcompling.mlnsemantics.util.DepParseUtil.Rel] = ListBuffer();
				var entityIdx  = 1;
				var lastEntity:Entity = null
				tagged.foreach( w =>
				{
					val word = w.asInstanceOf[CoreLabel];
					//println (word.category() + " - " + word.word() + " - " + word.after() + " - " + word.index() + " - " + word.sentIndex())
					if (posMapToBoxerPos(posMap(word.tag())) != "x" || word.word() =="@placeholder") //TODO: and not in stop words
					{
						val idxWord = new IndexedWord(word);
						idxWord.setIndex(entityIdx);
						//idxWord.setLemma( word.word());
						//idxWord.setWord(word.word());
						//idxWord.setTag("NN");
						val newEnt = new Entity(idxWord.index() + currentWordCounter, ListBuffer(idxWord))
						e.add(newEnt);
						
						if (lastEntity != null)
						{
							val grmRel = GrammaticalRelation.DEPENDENT
							val tDep = new TypedDependency(grmRel, newEnt.words.head, lastEntity.words.head);
							val newRel = new Rel(tDep, newEnt, lastEntity);
							r.add(newRel);
						}

						lastEntity = newEnt
						entityIdx = entityIdx + 1;
					}
				})
				(e,r)
			}
			else
			{
				//println("POS: " + tagged);
				val gs:GrammaticalStructure = parser.predict(tagged);
				val depStruct = ( if (Sts.opts.treeDep) gs.typedDependenciesCollapsedTree()
								  else gs.typedDependenciesCCprocessed()
								).toList;
				
				val sortedDep = sortDep(depStruct);
				LOG.trace("Sorted DEP: " + sortedDep);
				//sorted dependencies to set of entities and relations between them
				depToEntityRel(sortedDep)
			}
			boxExpRef ++= e.map(x => BoxerVariable("x" + x.id))
			boxExpCond ++= e.flatMap(x => x.words.map(w => {
				val predName = if (Sts.qaEntities.contains(w.word()))  w.word //this is the name of an entity
								else w.lemma(); 
				BoxerPred(discId, List(BoxerIndex(w.index() + currentWordCounter)), BoxerVariable("x" + x.id), predName, posMapToBoxerPos(posMap(w.tag())), 0)
			}));
			boxExpCond ++= r.map(x => BoxerRel(discId, List(), BoxerVariable("x" + x.from.id), BoxerVariable("x" + x.to.id), x.rel.reln().getShortName(), 0))

			currentWordCounter = currentWordCounter + sentence.length;
		}
		//assert (docTokensCntIncreamental == docTokensCnt);
		val ref = boxExpRef.map(x=> (List(), x)).toList
		val cond = boxExpCond.toList
		return BoxerDrs(ref, cond);
	}
	def sortDep (depStruct:List[TypedDependency]) : List[TypedDependency]=
	{
		//sorted by breadth first traversal of the dependency structure
		val rootRel = depStruct.filter(r => r.reln() == GrammaticalRelation.ROOT && r.gov().value() == "ROOT")
		assert (rootRel.length == 1, "A dependency strcutre without a root node (or more than one root node) " + rootRel )

		val relQueue = scala.collection.mutable.Queue[TypedDependency]();
		relQueue.enqueue(rootRel.head)
		
		val relSortedList = scala.collection.mutable.ListBuffer[TypedDependency]();
		while (!relQueue.isEmpty)
		{
			val current = relQueue.dequeue
			relSortedList += current
			val toEnqueue = depStruct.filter( rel => rel.gov() == current.dep() && !relQueue.contains(rel) && !relSortedList.contains(rel))
			relQueue ++= toEnqueue
		}
		assert (depStruct.length == relSortedList.length, "sorted list of relations is not complete: " + depStruct + " -- vs -- " + relSortedList)
		return relSortedList.toList
	}
	
	class Entity(x:Int, y:ListBuffer[IndexedWord]) { 
		val id:Int = x;
		val words:ListBuffer[IndexedWord] = y;
		override def toString(): String =  id + ":" + words.mkString("_")

	}
	class Rel(x: TypedDependency, y:Entity, z:Entity){
		val rel:TypedDependency = x;
		val from:Entity = y;
		val to:Entity = z;
		override def toString(): String = rel.toString()
	}
	
	def depToEntityRel(depStruct:List[TypedDependency]) : (List[Entity], List[Rel])=
	{
		//Loop over the sorted relations. For each relation do one of the following: 
		//ignore, add new entity, merge with previous entity, add relation between two entities (implicitly create the dep() entity if missing)
		//sorted dependencies to set of entities and relations between them
		val entities:ListBuffer[Entity] = ListBuffer();
		val rels:ListBuffer[Rel] = ListBuffer();
		
		def findEntity (word:IndexedWord) : Option[Entity] = 
		{
			val existingEntity = entities.filter(x => x.words.contains(word));
			if (existingEntity.length > 1)
				LOG.error("Word " + word +" can not be attached with more than one entity: " + existingEntity);
			if (existingEntity.length >= 1)
				return Some(existingEntity.head)
			else return None
		}
		def newEntity (rel:TypedDependency) : Entity = 
		{
			val e:Entity = new Entity(lastEntityId, ListBuffer(rel.dep()));
			lastEntityId = lastEntityId + 1;
			entities.append(e);
			return e;
		}
		def dropEntity (rel:TypedDependency)  = 
		{
			//Do nothing unless it is a named entity
			/*if (Sts.qaEntities.contains(rel.dep().word()))
			{
				println ("A NAMED ENTITY SAVED") 
				newEntity(rel) //new entity
			}*/
		}
		def mergeEntity (rel:TypedDependency) : Option[Entity] = 
		{
			if (Sts.opts.noMergeEntity)
				return Some(newEntity(rel))

			val existingEntity = findEntity(rel.gov());
			if (existingEntity.isEmpty)
			{
				LOG.error("Merging with entity that does not exist " + rel);
				return None
			}
			else
			{
				if (Sts.qaEntities.contains(rel.dep().word()))
				{
					if (existingEntity.get.words.map(_.word()).toSet.intersect(Sts.qaEntities.keys.toSet).size > 0)
					{
						LOG.trace ("New entity created inseat of merging")
						return Some (newEntity(rel));
					}
				}
					
				existingEntity.get.words.append(rel.dep());
				return existingEntity
			}
		}
		def newRel (rel:TypedDependency):Any = 
		{
			var govEntity = findEntity(rel.gov())
			if(govEntity.isEmpty)
			{
				LOG.error("Can not add relation " + rel + " where the gov entity does not exist");
				return;
			}
			var depEntity = findEntity(rel.dep())
			if (depEntity.isEmpty)
				depEntity = Some(newEntity(rel));
			rels += new Rel(rel, govEntity.get, depEntity.get);
		}
		
		for (rel <- depStruct)
		{
			rel.reln().getShortName() match 
			{
				case "nsubj" => if(posMap(rel.dep().tag()) == "NOUN" || posMap(rel.dep().tag()) ==  "PROPN") newRel(rel)
								else if(posMap(rel.dep().tag()) == "ADJ" || posMap(rel.dep().tag()) ==  "ADV") mergeEntity(rel)
								else {
									LOG.error("Unexpected: " + rel)
									newRel(rel)
								}
				case "nsubjpass" => if(posMap(rel.dep().tag()) == "NOUN" || posMap(rel.dep().tag()) ==  "PROPN") newRel(rel)
									else if(posMap(rel.dep().tag()) == "ADJ" || posMap(rel.dep().tag()) ==  "ADV") mergeEntity(rel)
									else {
										LOG.error("Unexpected: " + rel)
										newRel(rel)
									}
				case "dobj" => /*assert(posMap(rel.dep().tag()) == "NOUN" || posMap(rel.dep().tag()) ==  "PROPN", rel);*/ newRel(rel);
				case "iobj" => newRel(rel);
				
				case "csubj" => newRel(rel);
				case "csubjpass" => newRel(rel);
				case "ccomp" => newRel(rel); //TODO: better representation if we have support for nested scopes
				
				case "xcomp" => newRel(rel); //TODO: better representation if we have support for nested scopes 
				
				case "nummod" => newRel(rel);
				case "appos" => newRel(rel) //TODO: better representation required
				case "nmod" => newRel(rel);
	/*English*/	case "nmod:npmod" => mergeEntity(rel)
	/*English*/	case "nmod:poss" => if(posMap(rel.dep().tag()) == "NOUN" || posMap(rel.dep().tag()) ==  "PROPN") newRel(rel); 
									else dropEntity(rel)  //TODO: A man is driving **his** car 
	/*English*/	case "nmod:tmod" => newRel(rel);
				
				case "acl" => newRel(rel); //TODO: better representation if we have support for nested scopes
	/*English*/	case "acl:relcl" => newRel(rel); //TODO: better representation if we have support for nested scopes 
				
				
				case "amod" => mergeEntity(rel);
				case "det" => dropEntity(rel); //TODO: WH question and generalized quantifiers
	/*English*/	case "det:predet" => dropEntity(rel); //TODO: Generalized quantifiers
				case "neg" => dropEntity(rel); //TODO: negations

				case "case" =>  dropEntity(rel); //TODO: this is more involved. The best thing to do now is to drop it
				
				//case "nmod" =>;
				
				case "advcl" => newRel(rel)
				case "advmod" => /*assert(posMap(rel.dep().tag()) == "ADV", rel);*/ mergeEntity(rel);
				//case "neg" =>;
				
				case "compound" => mergeEntity(rel);
	/*English*/	case "compound:prt" => mergeEntity(rel);
				case "name" => mergeEntity(rel);
				
				case "mwe" => mergeEntity(rel);
				case "foreign" =>  mergeEntity(rel); 
				
				case "goeswith" =>  mergeEntity(rel);
				
				case "list" => newRel(rel)
				case "dislocated" => dropEntity(rel); //TODO: not sure what this is. I will drop it
				
				case "parataxis" => newRel(rel) //TODO: check again
				
				case "remnant" => dropEntity(rel); //TODO 
				case "reparandum" => newRel(rel); //TODO: maybe it is better to drop it
				
				case "vocative" => newRel(rel);
				case "discourse" => dropEntity(rel); 
				case "expl" => newRel(rel);
				
				case "aux" => dropEntity(rel)//assert(posMap(rel.dep().tag()) == "VERB"); 
				case "auxpass" => dropEntity(rel)//assert(posMap(rel.dep().tag()) == "VERB");
				case "cop" => dropEntity(rel)//assert(posMap(rel.dep().tag()) == "VERB");
				
				case "mark" => dropEntity(rel)//TODO: for now, ignoring it is the best thing to do
				case "punct" => dropEntity(rel); //assert(posMap(rel.dep().tag()) == "PUNCT");

				case "conj" => newRel(rel);//typedDependenciesCCprocessed adds additional dependencies 
								//from conjuncts to the gov word. The "conj" relations are generally useless
								//but sometimes the dependencies processing fails and they become needed.
				case "cc" => dropEntity(rel);
	/*English*/	case "cc:preconj" => dropEntity(rel);
				
				//case "punct" =>;
				
				case "root" => if (rel.gov().value() == "ROOT") newEntity(rel); //TODO: if the root is the verb "Be"
								else newRel(rel) //sentences with multiple roots
				case "dep" => newRel(rel)

				
				//Relations that are in the Universal Dependencies
				//I do not know where they come from
				case "ref" => dropEntity(rel)
				
				case  _ => throw new RuntimeException("Unknown relation: " + rel)
			}
		}
		LOG.trace("Entities: " + entities.mkString(", "));
		LOG.trace("Relations: " + rels.mkString(", "));
		return (entities.toList, rels.toList);
	}
	
	def posMap (pos:String) : String = 
	{
		val map = Map(
		 "#"	->	"SYM",
		 "$"	->	"SYM",
		"''"	->	"PUNCT",
		 ","	->	"PUNCT",
		"-LRB-"	->	"PUNCT",
		"-RRB-"	->	"PUNCT",
		 "."	->	"PUNCT",
		 ":"	->	"PUNCT",
		"AFX"	->	"ADJ",
		"CC"	->	"CONJ",
		"CD"	->	"NUM",
		"DT"	->	"DET",
		"EX"	->	"ADV",
		"FW"	->	"X",
		"HYPH"	->	"PUNCT",
		"IN"	->	"ADP",
		"JJ"	->	"ADJ",
		"JJR"	->	"ADJ",
		"JJS"	->	"ADJ",
		"LS"	->	"PUNCT",
		"MD"	->	"VERB",
		"NIL"	->	"X",
		"NN"	->	"NOUN",
		"NNP"	->	"PROPN",
		"NNPS"	->	"PROPN",
		"NNS"	->	"NOUN",
		"PDT"	->	"DET",
		"POS"	->	"PART",
		"PRP"	->	"PRON",
		"PRP$"	->	"DET",
		"RB"	->	"ADV",
		"RBR"	->	"ADV",
		"RBS"	->	"ADV",
		"RP"	->	"PART",
		"SYM"	->	"SYM",
		"TO"	->	"PART",
		"UH"	->	"INTJ",
		"VB"	->	"VERB",
		"VBD"	->	"VERB",
		"VBG"	->	"VERB",
		"VBN"	->	"VERB",
		"VBP"	->	"VERB",
		"VBZ"	->	"VERB",
		"WDT"	->	"DET",
		"WP"	->	"PRON",
		"WP$"	->	"DET",
		"WRB"	->	"ADV",
		"``"	->	"PUNCT")
		assert(map.contains(pos), "Unknown POS: " + pos);
		return map(pos);
	} 
	def posMapToBoxerPos (pos:String) : String =
	{
		val map = Map(
		"ADJ"	->	"a",
		"ADV"	->	"a",
		"INTJ"->	"n",
		"NOUN"	->	"n",
		"PROPN"	->	"n",
		"VERB"	->	"v",
		////
		"ADP"	->	"r",
		"AUX"	->	"x",
		"CONJ"->	"r",
		"DET"	->	"x",
		"NUM"	->	"n",
		"PART"->	"r",
		"PRON"	->	"n",
		"SCONJ"->	"x",
		///
		"PUNCT"->	"x",
		"SYM"	->	"x",
		"X"	->	"x"
		)
		assert(map.contains(pos), "Unknown POS: " + pos);
		return map(pos);
	}
}

class DepParseUtil
{
}
