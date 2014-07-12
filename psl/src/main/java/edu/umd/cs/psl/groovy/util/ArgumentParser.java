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
package edu.umd.cs.psl.groovy.util;

import java.util.List;
import java.util.Map;

import edu.umd.cs.psl.database.Partition;

public class ArgumentParser {
	
	public static final int[] getArgumentIntArray(Map args, String arg) {
		int[] ints=null;
		if (args.get(arg)!=null) {
			if (args.get(arg) instanceof Integer) {
				ints = new int[1];
				ints[0]=(Integer)args.get(arg);
			} else if (args.get(arg) instanceof List) {
				ints = new int[((List)args.get(arg)).size()];
				int i=0;
				for (Object it: (List)args.get(arg)) { 
					if (!(it instanceof Integer)) throw new IllegalArgumentException("Expected a list of integers, but encountered element: " + it);
					ints[i]=(Integer)it;
					i++;
				}
			} else if (args.get(arg) instanceof int[]) {
				return (int[])args.get(arg);
			} else throw new IllegalArgumentException("Invalid argument for '${arg}' label.");
		}
		return ints;
	}
	
	public static final Partition[] getArgumentPartitionArray(Map args, String arg, PartitionConverter conv) {
		Partition[] res=null;
		if (args.get(arg)!=null) {
			if (args.get(arg) instanceof List) {
				res = new Partition[((List)args.get(arg)).size()];
				int i=0;
				for (Object it : (List)args.get(arg) ) { 
					res[i]=conv.get(it);
					i++;
				}
			} else if (args.get(arg) instanceof Partition[]) {
				return (Partition[])args.get(arg);
			} else {
				res = new Partition[1];
				res[0]=conv.get(args.get(arg));
			}
		}
		return res;
	}
	
}
