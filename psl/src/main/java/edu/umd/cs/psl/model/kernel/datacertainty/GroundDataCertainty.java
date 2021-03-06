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
package edu.umd.cs.psl.model.kernel.datacertainty;

import java.util.Set;

import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableSet;

import edu.umd.cs.psl.model.atom.Atom;
import edu.umd.cs.psl.model.kernel.BindingMode;
import edu.umd.cs.psl.model.kernel.GroundConstraintKernel;
import edu.umd.cs.psl.model.kernel.Kernel;
import edu.umd.cs.psl.optimizer.NumericUtilities;
import edu.umd.cs.psl.reasoner.function.ConstraintTerm;
import edu.umd.cs.psl.reasoner.function.FunctionComparator;
import edu.umd.cs.psl.reasoner.function.FunctionSummand;

public class GroundDataCertainty implements GroundConstraintKernel {

	private final Atom atom;
	private double[] values;
	
	public GroundDataCertainty(Atom atom, double[] vals) {
		Preconditions.checkArgument(atom.getNumberOfValues()==1 && vals.length==1);
		this.atom = atom;
		values = vals.clone();
	}
	
	public void updateValues(double[] values) {
		this.values = values.clone();
	}
	
	@Override
	public ConstraintTerm getConstraintDefinition() {
		return new ConstraintTerm(new FunctionSummand(1,atom.getVariable()),FunctionComparator.Equality,values[0]);
	}
	
	@Override
	public double getIncompatibility() {
		if (NumericUtilities.equals(atom.getSoftValues(), values)) return 0;
		else return Double.POSITIVE_INFINITY;
	}

	@Override
	public Set<Atom> getAtoms() {
		return ImmutableSet.of(atom);
	}

	@Override
	public BindingMode getBinding(Atom atom) {
		if (atom.equals(this.atom)) return BindingMode.WeakCertainty;
		else return BindingMode.NoBinding;
	}

	@Override
	public Kernel getKernel() {
		return DataCertaintyKernel.get();
	}

	@Override
	public boolean updateParameters() {
		throw new UnsupportedOperationException("Parameters of data certainty cannot change since there are none!");
	}
	
	@Override
	public int hashCode() {
		return atom.hashCode()*94231;
	}
}
