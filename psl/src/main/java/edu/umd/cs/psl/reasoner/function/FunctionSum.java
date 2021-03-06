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
package edu.umd.cs.psl.reasoner.function;

import java.util.*;

/**
 * A numeric function defined as a sum of {@link FunctionSummand FunctionSummands}.
 */
public class FunctionSum implements Iterable<FunctionSummand>, FunctionTerm {

	private final List<FunctionSummand> sum;
	
	public FunctionSum() {
		sum = new ArrayList<FunctionSummand>();
	}
	
	/**
	 * Adds a {@link FunctionSummand} to the sum.
	 *
	 * @param summand  the summand to add
	 */
	public void add(FunctionSummand summand) {
		sum.add(summand);
	}

	@Override
	public Iterator<FunctionSummand> iterator() {
		return sum.iterator();
	}
	
	public int size() {
		return sum.size();
	}
	
	public FunctionSummand get(int pos) {
		return sum.get(pos);
	}

	/**
	 * Returns the sum of the {@link FunctionSummand} values.
	 *
	 * @return  the FunctionSum's value
	 */
	@Override
	public double getValue() {
		double val = 0.0;
		for (FunctionSummand s : sum) val+=s.getValue();
		return val;
	}
	
	
	@Override
	public double getValue(Map<? extends FunctionVariable,Double> values, boolean assumeDefaultValue) {
		double val = 0.0;
		for (FunctionSummand s : sum) val+=s.getValue(values,assumeDefaultValue);
		return val;
	}

	@Override
	public boolean isLinear() {
		for (FunctionSummand s : sum) {
			if (!s.isLinear()) return false;
		}
		return true;
	}
	
	@Override
	public boolean isConstant() {
		for (FunctionSummand s : sum) {
			if (!s.isConstant()) return false;
		}
		return true;
	}
	
	public String toString()
	{
		String res = "";
		for (FunctionSummand s: sum)
		{
			res = res + " + " + s.toString();
		}
		return res.substring(2);
	}
	
}
