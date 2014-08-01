/*
 * This file is part of the PSL software.
 * Copyright 2011 University of Maryland
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */


package edu.umd.cs.psl.groovy;


import edu.umd.cs.psl.learning.weight.WeightLearningGlobalOpt;
import edu.umd.cs.psl.config.ConfigBundle;
import edu.umd.cs.psl.config.EmptyBundle;
import edu.umd.cs.psl.config.WeightLearningConfiguration;
import edu.umd.cs.psl.model.predicate.FunctionalPredicate;
import edu.umd.cs.psl.model.predicate.Predicate;
import edu.umd.cs.psl.model.predicate.StandardPredicate;
import edu.umd.cs.psl.database.PredicateDBType;
import java.util.HashSet;
import java.math.BigDecimal;
import java.sql.Time;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Arrays;
import java.util.List;
import java.util.Map.Entry;

import edu.umd.cs.psl.groovy.util.*;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import edu.umd.cs.psl.model.predicate.type.*;
import edu.umd.cs.psl.database.Partition;
import edu.umd.cs.psl.database.RDBMS.Formula2SQL;
import edu.umd.cs.psl.model.argument.type.*;
import edu.umd.cs.psl.model.argument.*;
import edu.umd.cs.psl.model.predicate.*;
import edu.umd.cs.psl.model.kernel.Kernel;
import edu.umd.cs.psl.model.kernel.rule.AbstractRuleKernel;
import edu.umd.cs.psl.model.kernel.rule.CompatibilityRuleKernel;
import edu.umd.cs.psl.model.kernel.rule.ConstraintRuleKernel;
import edu.umd.cs.psl.model.kernel.priorweight.*;
import edu.umd.cs.psl.model.kernel.predicateconstraint.*;
import edu.umd.cs.psl.model.kernel.setdefinition.*;
import edu.umd.cs.psl.model.formula.Conjunction;
import edu.umd.cs.psl.model.formula.Formula;
import edu.umd.cs.psl.model.formula.Rule;
import edu.umd.cs.psl.model.Model;
import edu.umd.cs.psl.application.ModelApplication;
import edu.umd.cs.psl.application.inference.MaintainedMemoryFullInference;
import edu.umd.cs.psl.model.function.*;
import edu.umd.cs.psl.model.atom.*;
import edu.umd.cs.psl.model.set.term.SetTerm;
import edu.umd.cs.psl.groovy.syntax.*;
import edu.umd.cs.psl.ui.ModelUI;

public class PSLModel extends ModelUI {
	
	public static final Logger log = LoggerFactory.getLogger(PSLModel.class);
	
	private static final String predicateKey ="predicate";
	private static final String functionKey = "function";
	private static final String ruleKey = "rule";
	private static final String setComparisonKey = "setcomparison";

	private static final String auxPredicateSeparator = "__" ;	
	
	
	HashMap setComparisons = new HashMap();
	private int auxPredicateCounter = 0;
	
	/*
	 * 	
	PSLModel(Object context) {
		ExpandoMetaClass emc = new ExpandoMetaClass( context.getClass(), false );

		emc.propertyMissing = 
		{ String name ->
			if (predicates.containsKey(name)) {
				return predicates[name].definition;
			} else if (simFunctions.containsKey(name)) {
				return simFunctions[name].definition;
			} else if (name=="Entity") {
				return ArgumentTypes.Entity;
			} else if (name=="Text") {
				return ArgumentTypes.Text;
			} else if (name=="Number") {
				return ArgumentTypes.Number;
			} else if (name=="inverse") {
				return "inverse";
			} else if (name=="inv") {
				return "inverse";
			} else if (name.charAt(0).isUpperCase()){
				return new GenericVariable(name,this);
			}
		}
*/		
/*
		emc.methodMissing = { String name, args ->
			if (predicates.containsKey(name) || simFunctions.containsKey(name)) {
				Predicate pred = null;
				if (predicates.containsKey(name)) pred = predicates[name].definition;
				else if (simFunctions.containsKey(name)) pred = simFunctions[name].definition;
				assert pred!=null;
				Term[] terms = new Term[args.length];
				for (int i=0;i<terms.length;i++) {
					if (args[i] instanceof GenericVariable) {
						terms[i]=args[i].toAtomVariable();
					} else if ((args[i] instanceof Term)) {
						terms[i]=(Term)args[i];
					} else if (args[i] instanceof String) {
						terms[i]=new TextAttribute(args[i]);
					}else throw new IllegalArgumentException("The arguments to predicate ${name} must be terms");
					
				}
				return new FormulaContainer(new TemplateAtom(pred,terms));
			} else if (setComparisons.containsKey(name)) {
				def setcomp = setComparisons[name];
				if (args.length!=2) 
					throw new IllegalArgumentException("Expected 2 set definition for set comparison, but got: ${args}");
				if (!(args[0] instanceof Closure && args[1] instanceof Closure)) 
					throw new IllegalArgumentException("Expected set definitions for set comparison, but got: ${args}");
				SetTerm t1 = args[0].call().getSetTerm();
				SetTerm t2 = args[1].call().getSetTerm();
				
				VariableTypeMap vars = t2.getAnchorVariables(t1.getAnchorVariables(new VariableTypeMap()));
				
				List<String> argumentNames = [];
				ArgumentType[] types = new ArgumentType[vars.size()];
				Variable[] variables = new Variable[vars.size()];
				Term[] terms = new Term[vars.size()];
				String predname = name+auxPredicateSeparator+(++auxPredicateCounter);
				int i=0;
				vars.each { var , varType ->
					variables[i]=var;
					types[i]=varType;
					terms[i]=var;
					argumentNames.add(var.getName()+'_'+varType.toString());
					i++;
				}
				StandardPredicate auxpred = addAggregatePredicate(predname,types,argumentNames);
				
				addKernel(new SetDefinitionKernel(auxpred,t1,t2,variables,setcomp['predicate'],setcomp['aggregator']));
			
				return new FormulaContainer(new TemplateAtom(auxpred,terms));
				
			} else if (name == 'when') {
				return args[0];
			} else throw new MissingMethodException(name, context.class, args);
		}
		emc.initialize()
		context.metaClass = emc	
	
	}
*/	
	
	Object add(Map args) {
		if (args.containsKey(predicateKey)) {
			if (!(args.get(predicateKey) instanceof String)) throw new IllegalArgumentException("Expected a STRING as predicate name, but got: ${args[predicateKey]}");
			String predname = (String) args.get(predicateKey);
			args.remove(predicateKey);
			return addPredicate(predname,args);
		} else if (args.containsKey(functionKey)) {
			String functionname = (String) args.get(functionKey);
			args.remove(functionKey);
			return addFunction(functionname,args);
		} else if (args.containsKey(setComparisonKey)) {
			if (!(args.get(setComparisonKey) instanceof String)) throw new IllegalArgumentException("Expected a STRING as set comparison function name, but got: ${args[setComparisonKey]}");
			String setcompname = (String) args.get(setComparisonKey);
			args.remove(setComparisonKey);
			return addSetComparison(setcompname,args);
		} else if (args.containsKey(ruleKey)) {
			if (!(args.get(ruleKey) instanceof FormulaContainer)) throw new IllegalArgumentException("Expected a formula, but got: ${args[ruleKey]}");
			FormulaContainer ruledef = (FormulaContainer) args.get(ruleKey);
			args.remove(ruleKey);
			return addRule(ruledef,args);
		} else {
			throw new IllegalArgumentException("Unrecognized element added to model: ${args}");
		}
	}
	
	void add(Map args, Prior type) {
		addPrior(type,args);
	}
	
	void add(Map args, PredicateConstraint type) {
		addConstraint(type,args);
	}

	/*
	def learn(Map args, DataStore data) {
		Partition[] evidenceIDs = ArgumentParser.getArgumentPartitionArray(args, 'evidence', new PartitionConverter(data));
		if (evidenceIDs == null) throw new IllegalArgumentException("Need to specify the partitions containing the evidence.");
		Partition[] inferedIDs = ArgumentParser.getArgumentPartitionArray(args, 'infered', new PartitionConverter(data));
		if (inferedIDs == null) throw new IllegalArgumentException("Need to specify the partitions containing the infered atoms.");
		HashSet<Partition> merge = new HashSet<Partition>(Arrays.asList(evidenceIDs));
		merge.addAll(Arrays.asList(inferedIDs));
		inferedIDs = merge.toArray(new Partition[merge.size()]);
		if (args['close']==null) throw new IllegalArgumentException("Need to specify which predicates are infered via the 'close' label");
		
		def inferedArgs = args.clone();
		inferedArgs.remove('evidence'); inferedArgs.remove('infered');
		def evidenceArgs = inferedArgs.clone();
		evidenceArgs.remove('close');
		
		inferedArgs['read']=inferedIDs;
		evidenceArgs['read']=evidenceIDs;
		
		WeightLearningConfiguration configuration = null;
		if (args['configuration']!=null) {
			if (args['configuration'] instanceof WeightLearningConfiguration) configuration = args['configuration'];
			else throw new IllegalArgumentException("The configuration parameter must be of type 'WeightLearningConfiguration'");
		}
		
		ConfigBundle config = null;
		if (args['config']!=null) {
			if (args['config'] instanceof ConfigBundle) config = args['config'];
			else throw new IllegalArgumentException("The configuration parameter must be of type 'ConfigBundle'");
		}
		
		WeightLearningGlobalOpt w;
		if (configuration==null) configuration = new WeightLearningConfiguration();
		if (config==null) config = new EmptyBundle();
		w =  new WeightLearningGlobalOpt(getModel(),data.getDatabase(inferedArgs),data.getDatabase(evidenceArgs),configuration, config);
		w.learn();
	}
	*/
	
	Predicate addPredicate(String name, Map<String, Object> args) {
		if (predicates.containsKey(name)) {
			throw new IllegalArgumentException("A predicate with the name [${name}] has already been defined.");
		}
		boolean isOpen = false;
		if (args.containsKey("open")) {
			if (!(args.get("open") instanceof Boolean)) 
				throw new IllegalArgumentException("The parameter [open] for a predicate can only be TRUE or FALSE");
			isOpen = (Boolean) args.get("open");
			args.remove("open");
		}
		PredicateType type = PredicateTypes.SoftTruth;
		if (args.containsKey("type")) {
			type = (PredicateType) args.get("type");
			args.remove("type");
		}
		List<String> argumentNames = new ArrayList<String>();
		List<ArgumentType> argumentTypes = new ArrayList<ArgumentType>();
		for(Map.Entry<String, Object> e: args.entrySet()) 
		{ 
			assert e.getKey() instanceof String;
			if (!(e.getValue() instanceof ArgumentType)) {
				throw new IllegalArgumentException("Unrecognized argument type: ${v}");
			}
			argumentNames.add(e.getKey());
			argumentTypes.add((ArgumentType) e.getValue());
		}
		return addBasicPredicate(name,type,argumentTypes.toArray(new ArgumentType[argumentTypes.size()]), argumentNames, isOpen);
	}
	
	FunctionalPredicate  addFunction(String name, Map<String, Object> args) {
		if (simFunctions.containsKey(name)) {
			throw new IllegalArgumentException("A similarity function with the name [${name}] has already been defined.");
		}
		ExternalFunction implementation = null;
		if (args.containsKey("implementation")) {
			if (args.get("implementation") instanceof AttributeSimilarityFunction)
				implementation = new AttributeSimFunAdapter((AttributeSimilarityFunction)args.get("implementation"));
			else if (args.get("implementation") instanceof ExternalFunction)
				implementation = (ExternalFunction)args.get("implementation");
			else throw new IllegalArgumentException("The implementation of an external function must implement the respective interface");
			args.remove("implementation");
		}
		List<String> argumentNames = new ArrayList<String>();
		int pos=0;
		for(Map.Entry<String, Object> e: args.entrySet())
		{	
			assert e.getKey() instanceof String;
			if (!(e.getValue() instanceof ArgumentType)) {
				throw new IllegalArgumentException("Unrecognized argument type: ${v}");
			}
			if (!((ArgumentType)e.getValue()).isSubTypeOf(implementation.getArgumentTypes()[pos])) {
				throw new IllegalArgumentException("Argument type does not match external function: ${k}");
			}
			argumentNames.add(e.getKey());
			pos++;
		}
		if (argumentNames.size()!=implementation.getArity()) throw new IllegalArgumentException("The arity of the external function does not match provided number of arguments.");
		return addFunctionalPredicate(name,argumentNames,implementation);
	}
	
	Kernel addRule(FormulaContainer rule, Map args) {
		boolean isFact = false;
		if (args.containsKey("constraint")) {
			if (!(args.get("constraint") instanceof Boolean)) throw new IllegalArgumentException("The parameter [constraint] for a rule must be either TRUE or FALSE");
			isFact = (Boolean) args.get("constraint");
		}
		double weight = Double.NaN;
		if (args.containsKey("weight")) {
			if (isFact) throw new IllegalArgumentException("Cannot set a weight on a fact rule.");
			if (!isNumber(args.get("weight"))) 
				throw new IllegalArgumentException("The weight parameter is expected to be a real number: ${args['weight'].class}");
			weight = (Double) args.get("weight");
		}
		
		if (!Double.isNaN(weight) && isFact)
			throw new IllegalArgumentException("A rule cannot be a constraint and have a weight.");

		Formula ruleformula = rule.getFormula();
		if (!(ruleformula instanceof Rule)) throw new IllegalArgumentException("Expected a rule definition but got: ${ruleformula}");
		
		AbstractRuleKernel pslrule;
		if (isFact) {
			pslrule = new ConstraintRuleKernel(getModel(), ruleformula);
		} else {
			pslrule = new CompatibilityRuleKernel(getModel(), ruleformula, weight);
		}
		
		addKernel(pslrule);
		return pslrule;
	}
	
	Predicate addSetComparison(String name, Map args) {
		if (setComparisons.containsKey(name)) throw new IllegalArgumentException("Set comparison [${name}] has already been defined.");
		StandardPredicate predicate = getBasicPredicate(args,"on");
		if (!(args.get("using") instanceof SetComparison)) throw new IllegalArgumentException("Expected set comparison operator for [using] label, but got: ${args['using']}");
		HashMap h = new HashMap();
		h.put("predicate", predicate);
		h.put("aggregator", ((SetComparison)args.get("using")).getAggregator());
		setComparisons.put(name, h);
		return predicate;
	}
	
	Kernel addPrior(Prior type, Map args) {
		StandardPredicate predicate = getBasicPredicate(args,"on");
		double weight = Double.NaN;
		if (args.containsKey("weight")) {
			if (!isNumber(args.get("weight")))
				throw new IllegalArgumentException("The weight for a prior must be a real number.");
			weight = (Double) args.get("weight");
		}
		PriorWeightKernel wt = new PriorWeightKernel(getModel(),predicate,weight);
		addKernel(wt);
		return wt;
	}
	
	Kernel addConstraint(PredicateConstraint type, Map args) {
		StandardPredicate predicate = getBasicPredicate(args,"on");
		PredicateConstraintKernel con = new PredicateConstraintKernel(predicate,type.getPSLConstraint());
		addKernel(con);
		return con;
	}
		
	private StandardPredicate getBasicPredicate(Map args, String key) {
		Predicate predicate = (Predicate) args.get(key);
		if (predicate==null)
			throw new IllegalArgumentException("Need to define predicate via [${key}] argument label.");
		if (!predicates.containsKey(predicate.getName()))
			throw new IllegalArgumentException("Unknown predicate used: ${predicate}.\n Predicate has not been registered in this model.");
		if (!(predicate instanceof StandardPredicate)) {
			throw new IllegalArgumentException("Expected basic predicate, but got: ${predicate}");
		}
		return (StandardPredicate) predicate;
	}
	
	private boolean isNumber(Object n) {
		return (n instanceof Double || n instanceof Integer || n instanceof BigDecimal);
	}
}
