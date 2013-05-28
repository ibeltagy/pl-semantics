package psl;

import java.util.Map;

import edu.umd.cs.psl.groovy.*;
import edu.umd.cs.psl.groovy.syntax.FormulaContainer;
import edu.umd.cs.psl.groovy.syntax.GenericVariable;
import edu.umd.cs.psl.database.RDBMS.DatabaseDriver;
import edu.umd.cs.psl.model.DistanceNorm;
import edu.umd.cs.psl.model.argument.Term;
import edu.umd.cs.psl.model.argument.TextAttribute;
import edu.umd.cs.psl.model.argument.type.ArgumentTypes;
import edu.umd.cs.psl.model.atom.TemplateAtom;
import edu.umd.cs.psl.model.formula.Negation;
import edu.umd.cs.psl.model.function.AttributeSimilarityFunction;
import edu.umd.cs.psl.config.*;
import edu.umd.cs.psl.model.predicate.SpecialPredicates;
import edu.umd.cs.psl.model.predicate.type.*;
import edu.umd.cs.psl.ui.ModelUI.PredicateInfo;
import edu.umd.cs.psl.ui.functions.textsimilarity.*;

class Sim implements AttributeSimilarityFunction {
	private HashMap sim;
	@Override
	public double similarity(String a, String b) {
		Double score = sim.get(a);
		if (score == null)
			throw new Exception("score for " + a + " not found");
		else return score.value;
	}
	Sim (String simFilePath){
		sim = new HashMap<String, Double>();
		BufferedReader fr =  new BufferedReader(new FileReader(simFilePath));
		String l;
		while((l = fr.readLine()) != null){
			String[] splits = l.split(",");
			sim.put(splits[0], splits[1].toDouble());
		}
	}
}

simFilePath = "run/1.sim";
pslFilePath = "run/1.psl";
fileIndx = "test"
if (this.args.length != 0)
{
	fileIndx = this.args[0]
	simFilePath = "psl/run/"+this.args[0]+".sim";
	pslFilePath = "psl/run/"+this.args[0]+".psl";
}
	 
println "### Pair " + fileIndx;
println "Time: " + new Date()
m = new PSLModel(this);
predicates = new HashMap<String,PredicateInfo>();

arg = new LinkedHashMap();
arg.put("name1", ArgumentTypes.Text)
arg.put("name2", ArgumentTypes.Text)
arg.put("implementation", new Sim(simFilePath))
simFun = m.addFunction("sim", arg)

boolean evdStarted = false;
DataStore data;
BufferedReader fr =  new BufferedReader(new FileReader(pslFilePath));
while(( l = fr.readLine()) != null){
	println l;
	if (l.startsWith("predicate,")) //"predicate,predicateName,argsCount"
	{
		splits = l.split(",");
		arg = new LinkedHashMap();
		for (i = 0;i< Integer.parseInt(splits[2]);i++)
			arg.put("arg"+i, ArgumentTypes.Entity);
		arg.put("open", true)
		pred = m.addPredicate(splits[1], arg);
		predicates.put(splits[1], pred);
	}
	else if (l.startsWith("prior,"))//"prior,predicateName,weight"
	{
		splits = l.split(",");
		arg = new LinkedHashMap();
		arg.put("on", predicates.get(splits[1]))
		arg.put("weight", Double.parseDouble(splits[2]))
		m.addPrior(Prior.Simple, arg)

	}else if (l.startsWith("rule,"))
	{
//		m.add rule: (TX4 ^ TX2) >> entailment_h(), constraint: true
		splits = l.split(",", 2);
		arg = new LinkedHashMap();
		arg.put("constraint", true)
		f = parseFormula(splits[1])
		m.addRule(f, arg);		
	}
	else if (l.startsWith("data,"))//data,predName,val1,val2
								   //data,predName,val1
	{
		if (!evdStarted)
		{
			data = new RelationalDataStore(m);
			data.setup db : DatabaseDriver.H2;
			evdStarted = true;			
		}
		splits = l.split(",");
		pred = predicates.get(splits[1])
		if (splits.length == 3) //one arg
			data.getInserter(pred).insert(Integer.parseInt(splits[2]));
		else //two args
			data.getInserter(pred).insert(Integer.parseInt(splits[2]), Integer.parseInt(splits[3]));
	}
	else if (l.startsWith("query,"))
	{
		println m
		splits = l.split(",");
		pred = predicates.get(splits[1])
		ConfigManager cm = ConfigManager.getManager();
		ConfigBundle exampleBundle = cm.getBundle("example");
		def result = m.mapInference(data.getDatabase(), exampleBundle)
		result.printAtoms(pred, false);
	}	
}
def parseFormula(String s)
{
	splitsBodyHead = s.split(">>");
	body = splitsBodyHead[0];
	head = splitsBodyHead[1];
	splitsBody = body.split("&");
	FormulaContainer f = (parseAtom(splitsBody[0]))
	for (i = 1;i<splitsBody.length;i++)
	{
		f = f.and(parseAtom(splitsBody[i]))
	}
	f = f.rightShift(parseAtom(head));
	return f;
}

def parseAtom(String s)
{
	s = s.substring(0, s.length()-1);
	atomSplits = s.split("\\(");
	predicateName = atomSplits[0];
	if (atomSplits.length > 1) 
		termsSplits =  atomSplits[1].split(","); //predicateName(Var1, ..., VarN)
	else
		termsSplits = new String[0] //predicateName()
	
	boolean negated = false;
	if (predicateName.startsWith("~")) //~predicateName(Var1, ...., VarN)   
	{								  //~predicateName()
		negated = true;
		predicateName = predicateName.substring(1);
	}
		
	if (predicateName.equals("sim"))
		pred = simFun				//read predicate from similarityFUnction
	else 
		pred = predicates.get(predicateName) //read predicate from list of predicates
	
	Term[] terms = new Term[termsSplits.length];
	for (int i=0;i<terms.length;i++) {
		if (termsSplits[i].charAt(0) == "\"") {
			terms[i]=new TextAttribute(termsSplits[i].replace("\"", ""));
		} else if (termsSplits[i].charAt(0).isUpperCase() ) {
			terms[i]=new GenericVariable(termsSplits[i], m).toAtomVariable();
		}		
	}
	if (predicateName == "#NonSymmetric") //!(Var1=Var2)
		formula = new FormulaContainer(new TemplateAtom(SpecialPredicates.NonSymmetric,terms[0], terms[1]));
	else
		formula = new FormulaContainer(new TemplateAtom(pred,terms));
	
	if (negated) //if negated, then it is not an equality constraint  
		return new FormulaContainer(new Negation(formula.getFormula()));
	else return formula;  
}
