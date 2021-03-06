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
package edu.umd.cs.psl.model.formula;

import java.util.Arrays;
import java.util.Collection;

import edu.umd.cs.psl.model.argument.type.VariableTypeMap;
import edu.umd.cs.psl.model.atom.Atom;

/**
 * This class represent an abstract branching formula.
 * Note, the order in which formulas appear in an AbstractBranchFormula is important!
 *
 */
public abstract class AbstractBranchFormula implements Formula {

	protected final Formula[] formulas;
	
	public enum ConjunctionTypes{
		and, min, avg, notSet;
	}
	public int headPos = -1;
	
	//An anchor variable is a variable used in the RHS of an Averaging rule
	//We assume it is a single variable
	//e.g.  pred1(x) ^ pred2(y) ^ rel(x, y) => result(y)
	//"y" is the anchor variable in the formula above
	//We are assuming no negations
	public String anchorVar = null;
	
	public ConjunctionTypes conjType = ConjunctionTypes.notSet;
	
	public AbstractBranchFormula(Formula... f) {
		if (f.length<2) throw new IllegalArgumentException("Must provide at least two formulas!");
		//TODO: Should we copy here?
		formulas = f;
		for (int i=0;i<f.length;i++) {
			if (formulas[i]==null) throw new IllegalArgumentException("Formulas must not be null!");
		}
	}

	public int getNoFormulas() {
		return formulas.length;
	}
	
	public Formula get(int pos) {
		return formulas[pos];
	}
	
	@Override
	public VariableTypeMap getVariables(VariableTypeMap varMap) {
		for (int i=0;i<formulas.length;i++) {
			formulas[i].getVariables(varMap);
		}
		return varMap;
	}
	
	@Override
	public Collection<Atom> getAtoms(Collection<Atom> atoms) {
		for (int i=0;i<formulas.length;i++) {
			formulas[i].getAtoms(atoms);
		}
		return atoms;
		
	}	

	@Override
	public int hashCode() {
		return Arrays.hashCode(formulas);
	}
	
	@Override
	public boolean equals(Object oth) {
		if (oth==this) return true;
		if (oth==null || !(getClass().isInstance(oth)) ) return false;
		AbstractBranchFormula of = (AbstractBranchFormula)oth;
		return Arrays.equals(formulas, of.formulas);
	}
	
	protected abstract String separatorString();

	@Override
	public String toString() {
		StringBuilder s = new StringBuilder();
		s.append("( ");
		for (int i=0;i<formulas.length;i++) {
			if (i>0) s.append(" ").append(separatorString()).append(" ");
			s.append(formulas[i]);
		}
		s.append(" )");
		return s.toString();
	}


}
