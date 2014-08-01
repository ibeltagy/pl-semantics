package edu.umd.cs.psl.groovy;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.sql.DatabaseMetaData;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

import org.junit.Assert;

import edu.umd.cs.psl.groovy.syntax.FormulaContainer;
import edu.umd.cs.psl.groovy.syntax.GenericVariable;
import edu.umd.cs.psl.database.RDBMS.DatabaseDriver;
import edu.umd.cs.psl.database.RDBMS.Formula2SQL;
import edu.umd.cs.psl.database.RDBMS.Formula2SQL.QueryJoinMode;
import edu.umd.cs.psl.database.Database;
import edu.umd.cs.psl.evaluation.resultui.UIFullInferenceResult;
import edu.umd.cs.psl.evaluation.resultui.printer.ListAtomPrintStream;
import edu.umd.cs.psl.model.DistanceNorm;
import edu.umd.cs.psl.model.argument.Term;
import edu.umd.cs.psl.model.argument.TextAttribute;
import edu.umd.cs.psl.model.argument.type.ArgumentTypes;
import edu.umd.cs.psl.model.atom.TemplateAtom;
import edu.umd.cs.psl.model.formula.Negation;
import edu.umd.cs.psl.model.function.AttributeSimilarityFunction;
import edu.umd.cs.psl.application.GroundingMode;
import edu.umd.cs.psl.config.*;
import edu.umd.cs.psl.model.predicate.FunctionalPredicate;
import edu.umd.cs.psl.model.predicate.Predicate;
import edu.umd.cs.psl.model.predicate.SpecialPredicates;
import edu.umd.cs.psl.model.predicate.type.*;
import edu.umd.cs.psl.ui.ModelUI.PredicateInfo;
import edu.umd.cs.psl.ui.functions.textsimilarity.*;
import edu.umd.cs.psl.util.config.PSLConfiguration;

class Sim implements AttributeSimilarityFunction {
	@Override
	public double similarity(String a, String b) {
		return Double.parseDouble(a);
	}
}

public class TextInterface {

	public static void main(String[] args) {
		TextInterface t = new TextInterface();
		t.CallPSLTextInterface(args);
	}
		
	double[] results = new double[3];
	HashMap<String, Predicate> predicates;
	PSLModel m;
	FunctionalPredicate simFun;

	public double[] getResults() {
		return results;
	}
	
	public void CallPSLTextInterface(String[] args) 
	{
		String pslFilePath = "run/test.psl";
	   PSLConfiguration.startTime = System.currentTimeMillis(); //STATIC

		if (args.length != 0 && args[0].endsWith(".psl")) {
			pslFilePath = args[0];
			if (args.length >= 2) {
				if(args[1].equals("InnerJoin"))
					Formula2SQL.queryJoinMode = QueryJoinMode.InnerJoin;
				else if(args[1].equals("OuterJoin"))
					Formula2SQL.queryJoinMode = QueryJoinMode.OuterJoin;
				else if(args[1].equals("OuterJoinWithDummy"))
					Formula2SQL.queryJoinMode = QueryJoinMode.OuterJoinWithDummy;

			}
			if (args.length >= 3) {

				try {
					PSLConfiguration.timeout = Long.parseLong(args[2]);
				} catch (Exception e) {

				}
			}
			if (args.length >= 4) {

				try {
					PSLConfiguration.groundLimit = Integer.parseInt(args[3]);
				} catch (Exception e) {

				}
			}
			if (args.length >= 5) {

				try {
					PSLConfiguration.metaW = Double.parseDouble(args[4]);
					assert (PSLConfiguration.metaW <= 1 && (PSLConfiguration.metaW >= 0 || PSLConfiguration.metaW == -1));
				} catch (Exception e) {

				}
			}
			if (args.length >= 6) {

				try {
					PSLConfiguration.relW = Double.parseDouble(args[5]);
					assert (PSLConfiguration.relW <= 1 && (PSLConfiguration.relW >= 0 || PSLConfiguration.relW == -1));
					double w1 = 0;
					double w2 = 0;
					if (PSLConfiguration.metaW >= 0)
						w1 = PSLConfiguration.metaW;
					if (PSLConfiguration.relW >= 0)
						w2 = PSLConfiguration.relW;
					assert (w1 + w2 <= 1);
				} catch (Exception e) {

				}
			}

		}

		System.out.println("### Pair "
				+ pslFilePath.substring(pslFilePath.lastIndexOf('/') + 1,
						pslFilePath.lastIndexOf('.')));
		System.out.println("Time: " + new Date());
		System.out.println("Mode: " + Formula2SQL.queryJoinMode);
		System.out.println("Timeout: " + PSLConfiguration.timeout);
		System.out.println("Arglist: " + args.length);
		System.out.println("Arglist: " + args.toString());

		predicates = new HashMap<String, Predicate>();
		m = new PSLModel();
		LinkedHashMap<String, Object> arg = new LinkedHashMap<String, Object>();
		arg.put("name1", ArgumentTypes.Text);
		arg.put("name2", ArgumentTypes.Text);
		arg.put("implementation", new Sim());
		simFun = m.addFunction("sim", arg);

		boolean evdStarted = false;
		RelationalDataStore data = null;
		BufferedReader fr;
		try {
			fr = new BufferedReader(new FileReader(pslFilePath));
			String l;
			while ((l = fr.readLine()) != null) {
				PSLModel.log.info(l);
				if (l.startsWith("predicate,")) // "predicate,predicateName,argsCount"
				{
					String[] splits = l.split(",");
					arg = new LinkedHashMap();
					for (int i = 0; i < Integer.parseInt(splits[2]); i++)
						arg.put("arg" + i, ArgumentTypes.Entity);
					// if (!splits[1].startsWith("all")) //predicates "all" are
					// closed.
					// all predicates are open
					arg.put("open", true);

					Predicate pred = m.addPredicate(splits[1], arg);
					predicates.put(splits[1], pred);
				} else if (l.startsWith("prior,"))// "prior,predicateName,weight"
				{
					String[] splits = l.split(",");
					arg = new LinkedHashMap();
					arg.put("on", predicates.get(splits[1]));
					arg.put("weight", Double.parseDouble(splits[2]));
					m.addPrior(Prior.Simple, arg);
				} else if (l.startsWith("constraint,")) {
					String[] splits = l.split(",");
					arg = new LinkedHashMap();
					arg.put("on", predicates.get(splits[1]));
					m.addConstraint(PredicateConstraint.PartialFunctional, arg);
				} else if (l.startsWith("rule,")) {
					// m.add rule: (TX4 ^ TX2) >> entailment_h(), constraint:
					// true
					String[] splits = l.split(",", 2);
					int lastIndexOfComma = splits[1].lastIndexOf(",");
					String fStr = splits[1].substring(0, lastIndexOfComma);
					String wStr = splits[1].substring(lastIndexOfComma + 1);
					arg = new LinkedHashMap();
					if (wStr.compareTo("inf") == 0)
						arg.put("constraint", true);
					else
						arg.put("weight", Double.parseDouble(wStr));

					FormulaContainer f = parseFormula(fStr);
					m.addRule(f, arg);
				}

				else if (l.startsWith("data,"))// data,predName,val1,val2
												// data,predName,val1
				{
					if (!evdStarted) {
						// m.add rule:
						// (r_patient_dt(TX0,TX1)&r_agent_dt(TX0,TX2)&ride_v_dt(TX0)&bicycle_n_dt(TX1)&man_n_dt(TX2)&r_patient_dh(HX0,HX1)&r_agent_dh(HX0,HX2)&ride_v_dh(HX0)&bike_n_dh(HX1)&man_n_dh(HX2))>>entailment_h(),
						// weight: 1
						// m.add rule:
						// (r_agent_dt(TX0,TX2)&sing_v_dt(TX0)&r_of_dt(TX2,TX1)&people_n_dt(TX1)&group_n_dt(TX2)&r_agent_dh(HX0,HX1)&sing_v_dh(HX0)&people_n_dh(HX1))>>entailment_h(),
						// weight: 1
						data = new RelationalDataStore(m);
						data.setup(DatabaseDriver.H2,
								DatabaseDriver.Type.Memory, "psldb", "");
						evdStarted = true;
						/*
						 * for (predicate in predicates) { //println
						 * predicate.value; arity = predicate.value.getArity();
						 * //println arity; switch (arity) { case 1:
						 * data.getInserter(predicate.value).insertValue(0, -1);
						 * break; case 2:
						 * data.getInserter(predicate.value).insertValue(0, -1,
						 * -1); break; } }
						 */
					}
					String[] splits = l.split(",");
					Predicate pred = predicates.get(splits[1]);
					if (splits.length == 3) // one arg
						// data.getInserter(pred).insertValue(0.4,
						// Integer.parseInt(splits[2]));
						data.getInserter(pred).insert(
								Integer.parseInt(splits[2]));
					else if (splits.length == 4)// two args
						data.getInserter(pred).insert(
								Integer.parseInt(splits[2]),
								Integer.parseInt(splits[3]));
					else
						throw new Exception("Unsuported number of arguments");
				} else if (l.startsWith("query.")) {
					// println m
					// splits = l.split(",");
					// pred = predicates.get(splits[1])
					ConfigManager cm = ConfigManager.getManager();
					ConfigBundle exampleBundle = cm.getBundle("example");
					Database db = data.getDatabase();
					UIFullInferenceResult result = m.mapInference(
							db, exampleBundle);

					ListAtomPrintStream queryAtomsList = new ListAtomPrintStream();
					queryAtomsList.clear(); //STATIC
					result.printAtoms(m.getPredicate("entailment_h"),
							queryAtomsList, false);
					result.printAtoms(m.getPredicate("entailment_t"),
							queryAtomsList, false);
					result.printAtoms(m.getPredicate("entailment"),
							queryAtomsList, false);
					db.close();			
					data.close();

					/*
					 * result.printAtoms(r_agent_dt, false);
					 * result.printAtoms(sing_v_dt, false);
					 * result.printAtoms(r_of_dt, false);
					 * result.printAtoms(people_n_dt, false);
					 * result.printAtoms(group_n_dt, false);
					 * result.printAtoms(r_agent_dh, false);
					 * result.printAtoms(sing_v_dh, false);
					 * result.printAtoms(people_n_dh, false);
					 * result.printAtoms(all, false);
					 */
				}
			}
		} catch (Exception e) {
			throw new RuntimeException(e);
		}

	}

	FormulaContainer parseFormula(String s) throws Exception {
		String[] splitsAndingModeRule = s.split(",", 2); // avg,rule
		// and,rule
		// min,rule
		String[] splitsBodyHead = splitsAndingModeRule[1].split(">>");
		String body = splitsBodyHead[0];
		String head = splitsBodyHead[1];
		String[] splitsBody = body.split("&");
		FormulaContainer f = (parseAtom(splitsBody[0]));
		for (int i = 1; i < splitsBody.length; i++) {
			if (splitsAndingModeRule[0].equals("avg"))
				f = (FormulaContainer) f.mod(parseAtom(splitsBody[i]));
			else if (splitsAndingModeRule[0].equals("and"))
				f = (FormulaContainer) f.and(parseAtom(splitsBody[i]));
			else if (splitsAndingModeRule[0].equals("min"))
				throw new Exception("MIN is not supported yet");
			else
				throw new Exception("Unrecognized combiner: "
						+ splitsAndingModeRule[0]);
		}
		f = (FormulaContainer) f.rightShift(parseAtom(head));
		return f;
	}

	FormulaContainer parseAtom(String s) {
		s = s.substring(0, s.length() - 1);
		String[] atomSplits = s.split("\\(");
		String predicateName = atomSplits[0];
		String[] termsSplits = null;
		if (atomSplits.length > 1)
			termsSplits = atomSplits[1].split(","); // predicateName(Var1, ...,
													// VarN)
		else
			termsSplits = new String[0]; // predicateName()

		boolean negated = false;
		if (predicateName.startsWith("~")) // ~predicateName(Var1, ...., VarN)
		{ // ~predicateName()
			negated = true;
			predicateName = predicateName.substring(1);
		}
		Predicate pred = null;
		if (predicateName.equals("sim"))
			pred = simFun; // read predicate from similarityFUnction
		else
			pred = predicates.get(predicateName); // read predicate from list of
													// predicates

		Term[] terms = new Term[termsSplits.length];
		for (int i = 0; i < terms.length; i++) {
			if (termsSplits[i].charAt(0) == '\"') {
				terms[i] = new TextAttribute(termsSplits[i].replace("\"", ""));
			} else if (Character.isUpperCase(termsSplits[i].charAt(0))) {
				terms[i] = new GenericVariable(termsSplits[i], m).toAtomVariable();
			}
		}
		FormulaContainer formula = null;
		if (predicateName == "#NonSymmetric") // !(Var1=Var2)
			formula = new FormulaContainer(new TemplateAtom(
					SpecialPredicates.NonSymmetric, new Term[]{terms[0], terms[1]} ));
		else if (predicateName == "#Unequal")
			formula = new FormulaContainer(new TemplateAtom(
					SpecialPredicates.Unequal, new Term[]{terms[0], terms[1]}));
		else if (predicateName == "#Equal")
			formula = new FormulaContainer(new TemplateAtom(
					SpecialPredicates.Equal, new Term[]{terms[0], terms[1]}));
		else
			formula = new FormulaContainer(new TemplateAtom(pred, terms));

		if (negated) // if negated, then it is not an equality constraint
			return new FormulaContainer(new Negation(formula.getFormula()));
		else
			return formula;
	}
}
