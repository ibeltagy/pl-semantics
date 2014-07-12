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
package edu.umd.cs.psl.groovy.syntax;

import edu.umd.cs.psl.model.formula.*;
import edu.umd.cs.psl.model.formula.AbstractBranchFormula.ConjunctionTypes;
import edu.umd.cs.psl.model.atom.Atom;

public class FormulaContainer {
	
	private final Formula formula;
	
	public FormulaContainer(Formula f) {
		formula = f;
	}
	
	public Formula getFormula() {
		return formula;
	}
	
	private void checkFormula(Object f) {
		if (!(f instanceof FormulaContainer)) throw new IllegalArgumentException("Expected formula but got : ${f}");
	}
	
	private void checkAtom(FormulaContainer f) {
		if (!(f instanceof FormulaContainer)) throw new IllegalArgumentException("Expected formula but got : ${f}");
		if (!(f.formula instanceof Atom)) throw new IllegalArgumentException("Expected atom but got : ${f.formula}");
	}
	
	public FormulaContainer and(FormulaContainer f2) {
		checkFormula(f2);
		Conjunction conj = new Conjunction(formula,f2.formula);
		conj.conjType = ConjunctionTypes.and;
		return new FormulaContainer(conj);
	}
	
	public FormulaContainer mod(FormulaContainer f2) { //for average
		checkFormula(f2);
		Conjunction conj = new Conjunction(formula,f2.formula);
		conj.conjType = ConjunctionTypes.avg;
		return new FormulaContainer(conj);
	}
	
	public FormulaContainer rightShift(FormulaContainer f2) {
		return then(f2);
	}
	
	public FormulaContainer leftShift(FormulaContainer f2) {
		checkFormula(f2);
		return new FormulaContainer(new Rule(f2.formula,formula));
	}
	
	public FormulaContainer then(FormulaContainer f2) {
		checkFormula(f2);
		return new FormulaContainer(new Rule(formula,f2.formula));
	}
	
	public FormulaContainer bitwiseNegate() {
		return new FormulaContainer(new Negation(formula));
	}
	
}
