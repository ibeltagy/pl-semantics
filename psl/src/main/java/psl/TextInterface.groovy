package psl;

import java.sql.DatabaseMetaData;
import java.util.Map;

import edu.umd.cs.psl.groovy.*;
import edu.umd.cs.psl.groovy.syntax.FormulaContainer;
import edu.umd.cs.psl.groovy.syntax.GenericVariable;
import edu.umd.cs.psl.database.RDBMS.DatabaseDriver;
import edu.umd.cs.psl.database.RDBMS.Formula2SQL;
import edu.umd.cs.psl.database.RDBMS.Formula2SQL.QueryJoinMode;
import edu.umd.cs.psl.model.DistanceNorm;
import edu.umd.cs.psl.model.argument.Term;
import edu.umd.cs.psl.model.argument.TextAttribute;
import edu.umd.cs.psl.model.argument.type.ArgumentTypes;
import edu.umd.cs.psl.model.atom.TemplateAtom;
import edu.umd.cs.psl.model.formula.Negation;
import edu.umd.cs.psl.model.function.AttributeSimilarityFunction;
import edu.umd.cs.psl.application.GroundingMode;
import edu.umd.cs.psl.config.*;
import edu.umd.cs.psl.model.predicate.SpecialPredicates;
import edu.umd.cs.psl.model.predicate.type.*;
import edu.umd.cs.psl.ui.ModelUI.PredicateInfo;
import edu.umd.cs.psl.ui.functions.textsimilarity.*;
import edu.umd.cs.psl.util.config.PSLConfiguration;

class Sim implements AttributeSimilarityFunction {
	@Override
	public double similarity(String a, String b) {
		return Double.parseDouble(a)
	}
}

String pslFilePath = "run/test.psl";
if (this.args.length != 0 && this.args[0].endsWith(".psl") )
{
	pslFilePath = this.args[0];
	if (this.args.length >= 2 )
	{
		switch (this.args[1])
		{
			case "InnerJoin":  Formula2SQL.queryJoinMode = QueryJoinMode.InnerJoin; break;
			case "OuterJoin":  Formula2SQL.queryJoinMode = QueryJoinMode.OuterJoin; break;
			case "OuterJoinWithDummy":  Formula2SQL.queryJoinMode = QueryJoinMode.OuterJoinWithDummy; break;
		} 
	}
	if(this.args.length >= 3 ) {
	
		try{
			PSLConfiguration.timeout = this.args[2].toLong();
		}catch(Exception e){
			
		}
	}
}

println "### Pair " + pslFilePath.substring(pslFilePath.lastIndexOf('/')+1, pslFilePath.lastIndexOf('.'))
println "Time: " + new Date()
println "Mode: " + Formula2SQL.queryJoinMode;
println "Timeout: " + PSLConfiguration.timeout;
println "Arglist: " + this.args.length;
println "Arglist: " + this.args.toString();
m = new PSLModel(this);
predicates = new HashMap<String,PredicateInfo>();

arg = new LinkedHashMap();
arg.put("name1", ArgumentTypes.Text)
arg.put("name2", ArgumentTypes.Text)
arg.put("implementation", new Sim())
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
		if (!splits[1].startsWith("all")) //predicates "all" are closed. 
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
			//m.add rule: (r_patient_dt(TX0,TX1)&r_agent_dt(TX0,TX2)&ride_v_dt(TX0)&bicycle_n_dt(TX1)&man_n_dt(TX2)&r_patient_dh(HX0,HX1)&r_agent_dh(HX0,HX2)&ride_v_dh(HX0)&bike_n_dh(HX1)&man_n_dh(HX2))>>entailment_h(), weight: 1
			//m.add rule: (r_agent_dt(TX0,TX2)&sing_v_dt(TX0)&r_of_dt(TX2,TX1)&people_n_dt(TX1)&group_n_dt(TX2)&r_agent_dh(HX0,HX1)&sing_v_dh(HX0)&people_n_dh(HX1))>>entailment_h(),  weight: 1
			data = new RelationalDataStore(m);
			data.setup db : DatabaseDriver.H2, type : DatabaseDriver.Type.Disk;
			evdStarted = true;
			for (predicate in predicates)
			{
				//println predicate.value;
				arity = predicate.value.getArity();
				//println arity;
				switch (arity)
				{
					case 1:
						data.getInserter(predicate.value).insertValue(0, -1);
						break;
					case 2:
						data.getInserter(predicate.value).insertValue(0, -1, -1);
						break;
				}
			}
		}
		splits = l.split(",");
		pred = predicates.get(splits[1])
		if (splits.length == 3) //one arg
			//data.getInserter(pred).insertValue(0.4, Integer.parseInt(splits[2]));
			data.getInserter(pred).insert(Integer.parseInt(splits[2]));
		else //two args
			data.getInserter(pred).insert(Integer.parseInt(splits[2]), Integer.parseInt(splits[3]));
	}
	else if (l.startsWith("query."))
	{
		//println m
		//splits = l.split(",");
		//pred = predicates.get(splits[1])
		ConfigManager cm = ConfigManager.getManager();
		ConfigBundle exampleBundle = cm.getBundle("example");
		def result = m.mapInference(data.getDatabase(), exampleBundle)
		result.printAtoms(entailment_h, false);
		result.printAtoms(entailment_t, false);
		result.printAtoms(entailment, false);
/*		result.printAtoms(r_agent_dt, false);
		result.printAtoms(sing_v_dt, false);
		result.printAtoms(r_of_dt, false);
		result.printAtoms(people_n_dt, false);
		result.printAtoms(group_n_dt, false);
		result.printAtoms(r_agent_dh, false);
		result.printAtoms(sing_v_dh, false);
		result.printAtoms(people_n_dh, false);
		result.printAtoms(all, false);
		*/
	}	
}
def parseFormula(String s)
{
	splitsAndingModeRule = s.split(",", 2) //avg,rule
											//and,rule
											//min,rule
	splitsBodyHead = splitsAndingModeRule[1].split(">>");
	body = splitsBodyHead[0];
	head = splitsBodyHead[1];
	splitsBody = body.split("&");
	FormulaContainer f = (parseAtom(splitsBody[0]))
	for (i = 1;i<splitsBody.length;i++)
	{
		switch (splitsAndingModeRule[0])
		{
			case "avg": f = f.mod(parseAtom(splitsBody[i]))	; break;
			case "and": f = f.and(parseAtom(splitsBody[i])); break;
			case "min": throw new Exception ("MIN is not supported yet"); break;
			default: throw new Exception ("Unrecognized combiner: " + splitsAndingModeRule[0]);
		}
		
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
	else if (predicateName == "#Unequal")
		formula = new FormulaContainer(new TemplateAtom(SpecialPredicates.Unequal,terms[0], terms[1]));
	else
		formula = new FormulaContainer(new TemplateAtom(pred,terms));
	
	if (negated) //if negated, then it is not an equality constraint  
		return new FormulaContainer(new Negation(formula.getFormula()));
	else return formula;  
}
