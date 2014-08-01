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
package edu.umd.cs.psl.model.kernel.rule;

import java.util.HashSet;
import java.util.Set;

import org.apache.commons.lang.builder.HashCodeBuilder;
import edu.umd.cs.psl.model.atom.Atom;
import edu.umd.cs.psl.model.formula.Conjunction;
import edu.umd.cs.psl.model.formula.Formula;
import edu.umd.cs.psl.model.formula.Negation;
import edu.umd.cs.psl.model.formula.Tnorm;
import edu.umd.cs.psl.model.formula.AbstractBranchFormula.ConjunctionTypes;
import edu.umd.cs.psl.model.formula.traversal.FormulaEvaluator;
import edu.umd.cs.psl.model.kernel.BindingMode;
import edu.umd.cs.psl.model.kernel.GroundKernel;
import edu.umd.cs.psl.model.kernel.Kernel;
import edu.umd.cs.psl.reasoner.function.ConstantNumber;
import edu.umd.cs.psl.reasoner.function.FunctionSum;
import edu.umd.cs.psl.reasoner.function.FunctionSummand;
import edu.umd.cs.psl.util.config.PSLConfiguration;

/**
 * Currently, we assume that the body of the rule is a conjunction and the head of the rule is
 * a disjunction of atoms.
 * 
 * @author Matthias Broecheler
 * @author Stephen Bach
 */

abstract public class AbstractGroundRule implements GroundKernel {
	
	public static final Tnorm tnorm = Tnorm.LUKASIEWICZ;
	public static final FormulaEvaluator formulaNorm =FormulaEvaluator.LUKASIEWICZ;
	
	protected final AbstractRuleKernel kernel;
	protected final Conjunction formula;
	
	protected int numGroundings;

	private final int hashcode;
	
	public AbstractGroundRule(AbstractRuleKernel k, Formula f) {
		kernel = k;
		formula = ((Conjunction) f).flatten();
		numGroundings=1;
		
		hashcode = new HashCodeBuilder().append(kernel).append(f).toHashCode();
	}
	
	int getNumGroundings() {
		return numGroundings;
	}
	
	void increaseGroundings() {
		numGroundings++;
	}
	
	void decreaseGroundings() {
		numGroundings--;
		assert numGroundings>0;
	}
	
	@Override
	public boolean updateParameters() {
		return true;
	}
	
	private int isMetaSingleRel(Atom a)
	{
		if ((a.getName().contains("negationPred")  || a.getName().contains("dummyPred")) && PSLConfiguration.metaW != -1)
			return 0;
		else if (a.getArity() == 2 && PSLConfiguration.relW != -1)
			return 2;
		else return 1;
	}
	
	protected FunctionSum getFunction(double multiplier) {
		Formula f;
		Atom a;
		double constant = 0.0;
		FunctionSum sum = new FunctionSum();
		int noFormulas = formula.getNoFormulas();
		//boolean headFound = false;

		int noZeroVarPred = 0;
		int noSingleVarPred = 0;
		int noDoubleVarPred = 0;
		if (formula.conjType == ConjunctionTypes.avg)
		{
			for (int i = 0; i < noFormulas; i++) {
				f = formula.get(i);
			
				if (f instanceof Atom) {
					a = (Atom) f;
					switch (isMetaSingleRel(a))
					{
					case 0: noZeroVarPred++; break; 
					case 1: noSingleVarPred++; break;
					case 2: noDoubleVarPred++; break;
					default: throw new RuntimeException("not supported predicate " + a + "of arity" + a.getArity() + " in formula " + this);
					}
 
				}else if (f instanceof Negation) {
					a = (Atom) ((Negation) f).getFormula();
					if (i == formula.headPos ) //always use Strong AND for the head
						continue;
					switch (isMetaSingleRel(a))
					{
					case 0: noZeroVarPred++; break; 
					case 1: noSingleVarPred++; break;
					case 2: noDoubleVarPred++; break;
					default: throw new RuntimeException("not supported predicate " + a + "of arity" + a.getArity() + " in formula " + this);
					}
				}
			}
	
			assert(noZeroVarPred + noSingleVarPred + noDoubleVarPred + 1 == noFormulas);
		}
		double zeroVarPredsW = PSLConfiguration.metaW;
		if(zeroVarPredsW<0 ||noZeroVarPred == 0)
			zeroVarPredsW = 0;
		double doubleVarPredsW = PSLConfiguration.relW;
		if(doubleVarPredsW<0 || noDoubleVarPred == 0)
			doubleVarPredsW = 0;		
		double singleVarPredsW = 1 - zeroVarPredsW - doubleVarPredsW;
		assert(singleVarPredsW >=0 && singleVarPredsW <=1 );

		for (int i = 0; i < noFormulas; i++) {
			f = formula.get(i);
		
			if (f instanceof Atom) {
				a = (Atom) f;
				assert a.getNumberOfValues() == 1;
				switch (formula.conjType)
				{
				case and:
					sum.add(new FunctionSummand(multiplier, a.getVariable()));
					constant++;					
					break;
				case avg: 
					//sum.add(new FunctionSummand(multiplier/(noFormulas-1), a.getVariable()));
					switch (isMetaSingleRel(a))
					{
					case 0: if(zeroVarPredsW != 0)
								sum.add(new FunctionSummand(multiplier*zeroVarPredsW/noZeroVarPred, a.getVariable()));
							break; 
					case 1: if(singleVarPredsW != 0)
								sum.add(new FunctionSummand(multiplier*singleVarPredsW/noSingleVarPred, a.getVariable()));
							break;
					case 2: if(doubleVarPredsW != 0)
								sum.add(new FunctionSummand(multiplier*doubleVarPredsW/noDoubleVarPred, a.getVariable()));
							break;
					}
					break;
				case min: 
					throw new RuntimeException("not supported combiner MIN");
				default: 
					if (noFormulas == 2) //If number of formulas = 2, then no combiner in needed at all
						sum.add(new FunctionSummand(multiplier, a.getVariable()));
					else
						throw new RuntimeException("conjunction combiner is not set");
				}

			}
			else if (f instanceof Negation) {
				a = (Atom) ((Negation) f).getFormula();
				assert a.getNumberOfValues() == 1;
				
				if (i == formula.headPos ) //always use Strong AND for the head
				{
					sum.add(new FunctionSummand(-1*multiplier, a.getVariable()));
					//headFound = true;
				}
				else //for the body, use ConjunctionTypes
				{
					switch (formula.conjType)
					{
					case and:
						sum.add(new FunctionSummand(-1*multiplier, a.getVariable()));
						break;
					case avg: 
						//sum.add(new FunctionSummand(-1*multiplier/(noFormulas-1), a.getVariable()));
						switch (isMetaSingleRel(a))
						{
						case 0: if(zeroVarPredsW != 0)
									sum.add(new FunctionSummand(-multiplier*zeroVarPredsW/noZeroVarPred, a.getVariable()));
								break; 
						case 1: if(singleVarPredsW != 0)
									sum.add(new FunctionSummand(-multiplier*singleVarPredsW/noSingleVarPred, a.getVariable()));
								break;
						case 2: if(doubleVarPredsW != 0)
									sum.add(new FunctionSummand(-multiplier*doubleVarPredsW/noDoubleVarPred, a.getVariable()));
								break;
						}						
						constant++;
						break;
					case min: 
						throw new RuntimeException("not supported combiner MIN");
					default:
						if (noFormulas == 2) //If number of formulas = 2, then no combiner in needed at all
							sum.add(new FunctionSummand(-1*multiplier, a.getVariable()));
						else
							throw new RuntimeException("conjunction combiner is not set");
					}
				}
			}
			else
				throw new IllegalStateException();
		}
		
		switch (formula.conjType)
		{
		case and:
			sum.add(new FunctionSummand(multiplier, new ConstantNumber(1.0 - constant)));
			break;
		case avg: 
			sum.add(new FunctionSummand(multiplier/(noFormulas-1), new ConstantNumber(constant)));
			break;
		case min: 
			throw new RuntimeException("not supported combiner MIN");
		default: 
			if (noFormulas == 2) //If number of formulas = 2, then no combiner in needed at all
				;//do nothing
			else
				throw new RuntimeException("conjunction combiner is not set");
		}
		
		//if(formula.conjType ==ConjunctionTypes.avg)
		//{
		//	System.out.println(this + "==>" + sum );
		//}
		//if (!headFound)
		//	throw new RuntimeException("NO head found");
		return sum;
	}
	
	@Override
	public Set<Atom> getAtoms() {
		return (Set<Atom>) formula.getAtoms(new HashSet<Atom>());
	}

	@Override
	public Kernel getKernel() {
		return kernel;
	}
	
	public double getTruthValue() {
		return 1 - Math.max(getFunction(1.0).getValue(), 0);
	}
	
	@Override
	public BindingMode getBinding(Atom atom) {
		if (getAtoms().contains(atom)) {
			return BindingMode.StrongRV;
		}
		return BindingMode.NoBinding;
	}
	
	@Override
	public boolean equals(Object other) {
		if (other==this) return true;
		if (other==null || !(other instanceof GroundCompatibilityRule)) return false;
		GroundCompatibilityRule otherRule = (GroundCompatibilityRule) other;
		return kernel.equals(otherRule.kernel) && formula.equals(otherRule.formula);
	}
	
	@Override
	public int hashCode() {
		return hashcode;
	}
}
