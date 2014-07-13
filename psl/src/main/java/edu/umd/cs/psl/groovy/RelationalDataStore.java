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


import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import edu.umd.cs.psl.database.RDBMS.RDBMSPredicateHandle;
import edu.umd.cs.psl.model.predicate.Predicate;
import edu.umd.cs.psl.model.predicate.PredicateFactory;

import edu.umd.cs.psl.database.RDBMS.*;
import edu.umd.cs.psl.database.DataFormat;
import edu.umd.cs.psl.database.Database;
import edu.umd.cs.psl.database.Partition;
import edu.umd.cs.psl.database.PredicateDBType;
import edu.umd.cs.psl.database.partition.PartitionID;
import edu.umd.cs.psl.groovy.util.*;
import edu.umd.cs.psl.groovy.syntax.*;
import edu.umd.cs.psl.model.predicate.*;
import edu.umd.cs.psl.database.loading.Inserter;
import edu.umd.cs.psl.ui.loading.InserterUtils;

class RelationalDataStore extends RDBMSDataStore
implements DataStore {
	
	private static final int foldIDUpperBound = 1000;
	
	private static int writeIDCounter = foldIDUpperBound;
	private boolean hasStringEntities = false;
	
	RelationalDataStore(Map<String, String> args) {
		super();
		writeIDCounter = foldIDUpperBound;  //STATIC

		if (args.get("valuecol")!=null) {
			if (args.get("valuecol") instanceof String) valueColumnSuffix = args.get("valuecol");
			else throw new IllegalArgumentException("Expected STRING argument for value column name, but got: " + args.get("valuecol"));
		}
		if (args.get("confcol")!=null) {
			if (args.get("confcol") instanceof String) confidenceColumnSuffix = args.get("confcol");
			else throw new IllegalArgumentException("Expected STRING argument for confidence column name, but got: " + args.get("confcol"));
		}
		if (args.get("pslcol")!=null) {
			if (args.get("pslcol") instanceof String) pslColumnName = args.get("pslcol");
			else throw new IllegalArgumentException("Expected STRING argument for psl column name, but got: " + args.get("pslcol"));
		}
		if (args.get("partitioncol")!=null) {
			if (args.get("partitioncol") instanceof String) partitionColumnName = args.get("partitioncol");
			else throw new IllegalArgumentException("Expected STRING argument for partition column name, but got: " + args.get("partitioncol"));
		}
		if (args.get("entityid")!=null && args.get("entityid") == "string") {
			hasStringEntities=true;
		}
		
/*
		Inserter.metaClass.loadFromFile = { String filename ->
			InserterUtils.loadDelimitedData(delegate, filename);
		}
		Inserter.metaClass.loadFromFile = { String filename, String delim ->
			InserterUtils.loadDelimitedData(delegate, filename, delim);
		}
		Inserter.metaClass.loadFromFileWithTruth = { String filename ->
			InserterUtils.loadDelimitedDataTruth(delegate, filename);
		}
		Inserter.metaClass.loadFromFileWithTruth = { String filename, String delim ->
			InserterUtils.loadDelimitedDataTruth(delegate, filename, delim);
		}
		Inserter.metaClass.loadFactIntersectionTable = { String filename ->
			InserterUtils.loadFactIntersectionTable(delegate, filename);
		}
		Inserter.metaClass.loadFactIntersectionTable = { String filename, String delim ->
			InserterUtils.loadFactIntersectionTable(delegate, filename, delim);
		}
		Inserter.metaClass.loadFactEntityIntersectionTable = { String filename ->
			InserterUtils.loadFactEntityIntersectionTable(delegate, filename);
		}
		Inserter.metaClass.loadFactEntityIntersectionTable = { String filename, String delim ->
			InserterUtils.loadFactEntityIntersectionTable(delegate, filename, delim);
		}
	*/	
	}

	RelationalDataStore() {
		this(new HashMap());
	}
	
	RelationalDataStore(Map args, PSLModel model) {
		this(args);
		registerModel(model);
	}

	RelationalDataStore(PSLModel model) {
		this(new HashMap(),model);
	}

	public void registerModel(PSLModel model) {
		model.registerPredicates(this);
	}
	
	public void registerPredicate(Predicate predicate, List<String> argnames, PredicateDBType type) {
		super.registerPredicate(predicate,argnames,type,DataFormat.getDefaultFormat(predicate, hasStringEntities));
	}
	
	public void loadFactTable(PredicateFactory pf, String file, String delimeter) {
		InserterUtils.loadFactTable(pf, this, file, getDefaultPartition(), delimeter);
	}

	
	private void preprocessConnectionArgs(Map<String, Object> args) {
		if (args.get("db")==null) throw new IllegalArgumentException("Need to specify a database driver to connect with using the [db] label.");
		if (args.get("type")==null) args.put("type", "");
		else args.put("type", args.get("type").toString().toLowerCase());
		
		if (args.get("type") == null || args.get("type").equals(""))
			args.put("type", DatabaseDriver.Type.Disk);
		else if (args.get("type").equals("disk"))
			args.put("type", DatabaseDriver.Type.Disk);
		else  if (args.get("type").equals("memory"))
			args.put("type", DatabaseDriver.Type.Memory);
		else 
			throw new IllegalArgumentException("Unrecognized database type: " + args.get("type"));

		if (args.get("name")==null) args.put("name", "psldb");
		if (args.get("folder")==null) args.put("folder", "");
	}
	
	void connect(Map args) {
		preprocessConnectionArgs(args);
		super.connect((DatabaseDriver)args.get("db"),(DatabaseDriver.Type)args.get("type"),(String)args.get("name"),(String)args.get("folder"));
	}
	
	void setup(Map args) {
		preprocessConnectionArgs(args);
		super.setup((DatabaseDriver)args.get("db"),(DatabaseDriver.Type)args.get("type"),(String)args.get("name"),(String)args.get("folder"));
	}
	
	@Override
	public	Database getDatabase() {
		return getDatabase(new HashMap());
	}
	
	@Override
	public Database getDatabase(Map args) {
		Partition writeID;
		PartitionConverter pconv = new PartitionConverter(this);
		if (args.get("write")!=null) {
			writeID=pconv.get(args.get("write"));
		} else writeID = getNextPartition();
		Partition[] parts = ArgumentParser.getArgumentPartitionArray(args, "read", new PartitionConverter(this));
		if (parts==null) {
			parts = new Partition[1];
			parts[0] = getDefaultPartition();
		}
		
		Set<Predicate> toclose = new HashSet<Predicate>();
		
		if (args.get("close")!=null) 
		{	
			if (args.get("close") instanceof List) 
			{
				for(Object it: (List)args.get("close")) 
				{
					if (it instanceof Predicate) toclose.add((Predicate)it);
					//else if (it instanceof String) toclose.add getPredicate(it);
					else throw new IllegalArgumentException("Expected a list of strings or predicates to close, but was given: "+it);
				}
			} 
			else if (args.get("close") instanceof Predicate) toclose.add((Predicate)args.get("close"));
			//else if (args['close'] instanceof String) toclose.add getPredicate(args['close']);
			else  throw new IllegalArgumentException("Expected a list of strings or predicates to close, but was given: "+args.get("close"));
		}
		
		return getDatabase(writeID, toclose, parts);
	}
	
	public Partition getNextPartition() {
		return getPartition(writeIDCounter);
	}
	
	public Partition getDefaultPartition() {
		return getPartition(DataStore.defaultPartitionID);
	}
	
	public Partition getPartition(int pID) {
		if (pID>=writeIDCounter) writeIDCounter = pID+1;
		return new PartitionID(pID);
	}
	
	public Inserter getInserter(Predicate predicate, int partitionID) {
		return this.getInserter(predicate,getPartition(partitionID));
	}
	
	public Inserter getInserter(Predicate predicate) {
		return getInserter(predicate,getDefaultPartition());
	}
	
	
}
