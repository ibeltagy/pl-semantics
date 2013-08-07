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

import java.util.Map;

import edu.umd.cs.psl.database.Partition;
import edu.umd.cs.psl.model.predicate.Predicate;
import edu.umd.cs.psl.database.Database;
import edu.umd.cs.psl.database.loading.Inserter;

public interface DataStore extends edu.umd.cs.psl.database.DataStore {
	
	public static final int defaultPartitionID = 1;
	
	Database getDatabase(Map args);

	Database getDatabase();

	Partition getDefaultPartition();

	Partition getNextPartition();

	Partition getPartition(int pID);

	Inserter getInserter(Predicate predicate);
	
	
	
}