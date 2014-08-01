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
package edu.umd.cs.psl.database.RDBMS;

import java.sql.SQLException;
import java.sql.Statement;
import java.util.*;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.collect.HashMultimap;
import com.google.common.collect.Multimap;
import com.healthmarketscience.common.util.Tuple2;
import com.healthmarketscience.common.util.Tuple3;
import com.healthmarketscience.sqlbuilder.BinaryCondition;
import com.healthmarketscience.sqlbuilder.ComboCondition;
import com.healthmarketscience.sqlbuilder.ComboCondition.Op;
import com.healthmarketscience.sqlbuilder.Condition;
import com.healthmarketscience.sqlbuilder.CreateTableQuery;
import com.healthmarketscience.sqlbuilder.CustomCondition;
import com.healthmarketscience.sqlbuilder.CustomSql;
import com.healthmarketscience.sqlbuilder.FunctionCall;
import com.healthmarketscience.sqlbuilder.InCondition;
import com.healthmarketscience.sqlbuilder.InsertSelectQuery;
import com.healthmarketscience.sqlbuilder.OrderObject.Dir;
import com.healthmarketscience.sqlbuilder.SelectQuery;
import com.healthmarketscience.sqlbuilder.SelectQuery.JoinType;
import com.healthmarketscience.sqlbuilder.SetOperationQuery.Type;
import com.healthmarketscience.sqlbuilder.UnionQuery;

import edu.umd.cs.psl.database.PSLValue;
import edu.umd.cs.psl.model.argument.Attribute;
import edu.umd.cs.psl.model.argument.Entity;
import edu.umd.cs.psl.model.argument.Term;
import edu.umd.cs.psl.model.argument.Variable;
import edu.umd.cs.psl.model.atom.Atom;
import edu.umd.cs.psl.model.atom.VariableAssignment;
import edu.umd.cs.psl.model.formula.*;
import edu.umd.cs.psl.model.formula.AbstractBranchFormula.ConjunctionTypes;
import edu.umd.cs.psl.model.formula.traversal.FormulaTraverser;
import edu.umd.cs.psl.model.predicate.StandardPredicate;
import edu.umd.cs.psl.model.predicate.FunctionalPredicate;
import edu.umd.cs.psl.model.predicate.ExternalFunctionPredicate;
import edu.umd.cs.psl.model.predicate.SpecialPredicates;

public class Formula2SQL extends FormulaTraverser {

	private static final Logger log = LoggerFactory.getLogger(Formula2SQL.class);
	
	public enum QueryJoinMode
	{
		InnerJoin, OuterJoin, OuterJoinWithDummy;
	}
	
	public static QueryJoinMode queryJoinMode = QueryJoinMode.OuterJoin;
	
	private static final String tablePrefix = "t";
	
	public static final Set<String> varsTables = new HashSet<String>(); //STATIC: clear this table 
																							//on the creation of a new databse. 
	
	private final List<Variable> projection;
	private final VariableAssignment partialGrounding;
	private final RDBMSDatabase database;
	
	private final Map<Variable,String> joins;
	private final Multimap<Variable,Tuple3<String, String, String>> queryColumnsByVar;
	private final Multimap<String,Tuple2<Term, String>> queryColumnsByPred;
	//									VariableName, tableAliase, columnName
	private final Map<String, RDBMSPredicateHandle> tableAliasToPredicate;
 
	private final List<Atom> functionalAtoms;

	private SelectQuery query;

	private int tableCounter;
	
	private int predCount = 0;
	private int allowedNulls = 0;
	
	private static final int rowsCountLimit = 2;
	
	
	public Formula2SQL(VariableAssignment pg, List<Variable> proj, RDBMSDatabase db) {
		
		partialGrounding = pg;
		projection = proj;
		joins = new HashMap<Variable,String>();
		queryColumnsByPred = HashMultimap.create();
		queryColumnsByVar = HashMultimap.create();
		tableAliasToPredicate = new HashMap<String, RDBMSPredicateHandle>();
		database = db;
		query = new SelectQuery();
		query.setIsDistinct(true);
		functionalAtoms = new ArrayList<Atom>(4);
		tableCounter = 1;
		if (projection.isEmpty()) query.addAllColumns(); //query.addAllTableColumns(tablePrefix+tableCounter);
	}
	
	public List<Atom> getFunctionalAtoms() {
		return functionalAtoms;
	}
	
	
	//sort predicates to be used in the left outer-join query
	private List<String> getSortTableAliases()
	{
		Vector<String> predList = new Vector<String>(tableAliasToPredicate.keySet());
		Set<String> selectedVarList = new HashSet<String>(); 
		for (int i = 0; i < predList.size() - 1; i++)
		{
			int minReqIdx = i;
			int minReqVal = 0;
			int minVarCnt = 0;
			Collection<Tuple2<Term, String>> varNameColNameList = queryColumnsByPred.get(predList.get(i));
			for (Tuple2<Term, String> varNameColName : varNameColNameList)
			{
				if (varNameColName.get0() instanceof Variable)
				{
					Variable var = (Variable)varNameColName.get0();
										
					if (!partialGrounding.hasVariable(var)) {
						minVarCnt ++;
						if(!selectedVarList.contains(var.getName()))//var not in selectedVarList
							minReqVal++;
					}
				}
			}

			for (int j = i+1; j < predList.size(); j++)
			{
				int req = 0;
				
				varNameColNameList = queryColumnsByPred.get(predList.get(j));
				int varCnt = 0;
				for (Tuple2<Term, String> varNameColName : varNameColNameList)
				{
					if (varNameColName.get0() instanceof Variable)
					{
						Variable var = (Variable)varNameColName.get0();
											
						if (!partialGrounding.hasVariable(var)) {
							varCnt++;
							if(!selectedVarList.contains(var.getName()))//var not in selectedVarList
								req++;
						}
					}
				}	
				
				if (req < minReqVal || (req == minReqVal && varCnt < minVarCnt ))
				{
					minReqVal = req;
					minVarCnt = varCnt;
					minReqIdx = j;
				}
			}
			String tmp = predList.get(i);
			predList.set(i, predList.get(minReqIdx));
			predList.set(minReqIdx, tmp);
			
			varNameColNameList = queryColumnsByPred.get(predList.get(i));
			for (Tuple2<Term, String> varNameColName : varNameColNameList)
			{
				if (varNameColName.get0() instanceof Variable)
				{
					Variable var = (Variable)varNameColName.get0();
										
					if (!partialGrounding.hasVariable(var)) {
						selectedVarList.add(var.getName());
					}
				}
			}
			
		}
		/*for (String pred:predList)
		{
			System.out.println(tableAliasToPredicate.get(pred).tableName() + " " + queryColumnsByPred.get(pred).toString() );
		}*/
		return predList;
	}
	
	@Override
	public void afterConjunction(int noFormulas) {
		//Supported

		if (conjType == ConjunctionTypes.notSet)
			throw new RuntimeException("error");
		else if (conjType == ConjunctionTypes.and)
			return;
		else if (conjType == ConjunctionTypes.min)
			return;
		
		if (queryJoinMode == QueryJoinMode.InnerJoin)
			return;
		
		predCount = noFormulas;
		
		if (allowedNulls < 0)
			return;
		
		query = new SelectQuery();
		query.setIsDistinct(true);
		
		boolean isFirst = true;
		
		/*if(projection.isEmpty()) 
			query.addAllColumns();
		else
		{*/
			SelectQuery qdummy = new SelectQuery();
			//qdummy.addAliasedColumn(new CustomSql("-2147483648"), "dummy_id");
			qdummy.addAliasedColumn(new CustomSql("0"), "nullCounts");
			qdummy.addCustomFromTable("dual");
			//query = qdummy; 
			//query.addCustomFromTable("("+qdummy.validate().toString()+")tblDummy");
			query.addAllColumns();
			//isFirst = false;
		/*}*/
		
		//String whereClauseLimitNullsCount = "";
		String orderByClause = "";
		
		SelectQuery tmpQuery = new SelectQuery();
		//tmpQuery = qdummy;
		
		
		Variable prevVariable = null;
		
		List<String> sortedTableAliasList = getSortTableAliases();
		Set<Variable> addedVars = new HashSet<Variable>();
		
		Set<Variable> usedVariablesInQuery = new HashSet<Variable>();
		Vector<Tuple2<String, Condition>> leftJoins = new Vector<Tuple2<String,Condition>>();
		
		for (String tableAlias:sortedTableAliasList)
		{
			Collection<Tuple2<Term, String>> tableVars = queryColumnsByPred.get(tableAlias);
			for (Tuple2<Term, String> tableVar:tableVars)
			{
				if (tableVar.get0() instanceof Variable) 
				{
					Variable var = (Variable)tableVar.get0();
					if (addedVars.contains(var));
						//nothing
					else
					{
						if (!orderByClause.equals(""))
						{
							//Do nothing, do not add condtition and do not add column <<--------------
							//query.addCondition(new BinaryCondition(BinaryCondition.Op.LESS_THAN_OR_EQUAL_TO, new CustomSql(whereClauseLimitNullsCount), allowedNulls));
							//query.addAliasedColumn(new CustomSql(whereClauseLimitNullsCount), "nullCounts");
							
							tmpQuery.addCustomOrderings(new CustomSql("max("+orderByClause + ") desc"));
						}

						//SelectQuery tmp = query;
						//query = new SelectQuery();
						//query.addCustomFromTable("("+tmp+" limit 1 ) innerTbl");
						/*if (isFirst)
							query.addCustomFromTable("("+tmpQuery+") tblDummy");
						else 
							query.addCustomFromTable("("+tmpQuery+") tbl" + prevVariable.getName());
						*/
						try {
							Statement stmt = database.getConnection().createStatement();
							try {
								if(!isFirst)
								{
									usedVariablesInQuery.remove(prevVariable);
									for (Variable v:usedVariablesInQuery)
									{
										//tmpQuery.addCustomFromTable(new CustomSql("tbl"+v.getName()));
										tmpQuery.addCustomJoin(JoinType.INNER, "", "tbl"+v.getName(),  new CustomCondition("true"));
									}
									for (Tuple2<String, Condition> leftJoin:  leftJoins)
									{
										tmpQuery.addCustomJoin(JoinType.LEFT_OUTER, "", leftJoin.get0(), leftJoin.get1());
									}
									tmpQuery.addCustomGroupings(new CustomSql(prevVariable.getName()));
									//execute tmpQuery as an insertSelect
									log.trace(tmpQuery.toString());
									stmt.execute("INSERT INTO tbl" + prevVariable.getName() + " (" + prevVariable.getName() + ")" 
											+ tmpQuery + " limit " + rowsCountLimit);
									
								}
								isFirst = false;
								//create table for the variable if it does not exist already
								if (!varsTables.contains(var.getName())) 
								{
									//create table and hash index
									CreateTableQuery q = new CreateTableQuery("tbl" + var.getName());
									q.addCustomColumns(new CustomSql(var.getName()+ " int "));
		
										    stmt.executeUpdate(q.validate().toString());
										    //Create indexes
											stmt.executeUpdate("CREATE HASH INDEX " + "tbl" + var.getName()+var.getName()+"hashidx ON " 
																+ "tbl" + var.getName() + " (" + var.getName() + " ) ");
										
									varsTables.add(var.getName());
								}
								//clear data in the table
								stmt.execute("DELETE FROM " + "tbl" + var.getName());
							} finally {
							    stmt.close();
							}
						} catch(Exception e) {
							log.error("SQL error: {}",e.getMessage());
							throw new AssertionError(e);
						}
						//dummy is the first column
						//select from all tables of variables in addedVars
						

						//query.addCustomColumns(new CustomSql("innerTbl.*"));
						//whereClauseLimitNullsCount = "innerTbl.nullCounts";
						orderByClause = "";
						//tmpQuery = new SelectQuery(true);
						tmpQuery = new SelectQuery();
						tmpQuery.addCustomColumns(new CustomSql(var.getName()));
						tmpQuery.addCustomFromTable("(" + qdummy.toString() + ") dummy");
						usedVariablesInQuery.clear();
						leftJoins.clear();
						/*for (Variable v:addedVars)
						{
							//tmpQuery.addCustomFromTable(new CustomSql("tbl"+v.getName()));
							tmpQuery.addCustomJoin(JoinType.INNER, "", "tbl"+v.getName(),  new CustomCondition("true"));
						}*/


						prevVariable = var;
						/*if (partialGrounding.hasVariable(var)) {
							query.addAliasedColumn(new CustomSql(partialGrounding.getVariable(var)), var.getName()); 
						} else {
							query.addCustomColumns(new CustomSql(var.getName()));
						}*/
						//System.out.println("add Variable:" + var.getName());
						addedVars.add(var);
//-----------------------------Build union
						if (projection.contains(var))
						{
							UnionQuery uq = new UnionQuery(Type.UNION);
							String nonNullColumn = "";
							for (Tuple3<String, String, String> column : queryColumnsByVar.get(var))
							{
								SelectQuery cq = new SelectQuery();
								cq.addAliasedColumn(new CustomSql(column.get1() + "." + column.get2()), /*columnName*/var.getName());
								cq.addCustomFromTable(column.get0() + " " + column.get1());
								uq.addQueries(cq);
								nonNullColumn = column.get1() + "." + column.get2() + ", " + nonNullColumn;
							}
							/*if(isFirst)
							{
								query.addCustomFromTable("("+uq.validate().toString()+")tbl"+var.getName());
								isFirst = false;
							}
							else*/ //It is never first.
								//tmpQuery.addCustomFromTable("("+uq.validate().toString()+")tbl"+var.getName());
							leftJoins.add(new Tuple2<String, Condition>("("+uq.validate().toString()+")tbl"+var.getName(), new CustomCondition("true")));
							//tmpQuery.addCustomJoin(JoinType.LEFT_OUTER, "", "("+uq.validate().toString()+")tbl"+var.getName(),  new CustomCondition("true"));
								
	
							//Line below can be used for debugging
							//query.addAliasedColumn(new CustomSql("tbl"+var.getName()+".id"), var.getName());
							
							//Select column
							//query.addAliasedColumn(new CustomSql("COALESCE("+nonNullColumn+"-2147483648)"), var.getName());
							//query.addCustomColumns(new CustomSql(var.getName()));//....
						}
//-----------------------------End Build union
					}
				}
			}
			//System.out.println("processTable: " + tableAliasToPredicate.get(tableAlias).tableName() + " " + queryColumnsByPred.get(tableAlias).toString() );
			//alias: var, predicate, col
			Collection<Tuple2<Term, String>> allColumns = queryColumnsByPred.get(tableAlias);
			//Tuple3<Term, RDBMSPredicateHandle, String> firstColumn = allColumns.iterator().next();
			RDBMSPredicateHandle pred = tableAliasToPredicate.get(tableAlias);
			Condition totalCond  = new InCondition(new CustomSql(tableAlias+"."+pred.partitionColumn()),database.getReadIDs());
			if (!pred.isClosed()) {
				totalCond = new ComboCondition (Op.AND, totalCond, new BinaryCondition(BinaryCondition.Op.LESS_THAN, 
																new CustomSql(tableAlias+"."+pred.pslColumn()),
																PSLValue.getNonDefaultUpperBound()));
			}
			
			assert allColumns.size() != 0;
			
			boolean isFirstPredColumn = true;
			for (Tuple2<Term, String> column : allColumns)
			{
				Term arg = column.get0();
				if (arg == null)
					continue;
				 
				if (arg instanceof Variable) {
					Variable var = (Variable)arg;
					
					//Line below can be used for debugging
					//query.addCustomColumns(new CustomSql(tableAlias+"."+column.get2()));					
					if(isFirstPredColumn)
					{
						/*if(whereClauseLimitNullsCount.equals(""))
							whereClauseLimitNullsCount = "NVL2("  + tableAlias + "." + column.get1() + ", 0, 1)";  
						else
							whereClauseLimitNullsCount = whereClauseLimitNullsCount + " + NVL2("  + tableAlias + "." + column.get1() + ", 0, 1)";
						*/
						
						if(orderByClause.equals(""))
							orderByClause = "IFNULL(" + tableAlias + ".truth_value, 0)";  
						else
							orderByClause = orderByClause + " + " + "IFNULL(" + tableAlias + ".truth_value, 0)";
						
						isFirstPredColumn = false;
					}
					
					if (partialGrounding.hasVariable(var)) {
						arg = partialGrounding.getVariable(var);
					} else {
						Condition cond = new BinaryCondition(BinaryCondition.Op.EQUAL_TO, 
								new CustomSql(tableAlias+"."+column.get1()),
								//new CustomSql("tbl"+var.getName()+".id"));
								new CustomSql(var.getName())); //changed column name
						
						usedVariablesInQuery.add(var);
						
		
						if (totalCond instanceof ComboCondition)
							((ComboCondition)totalCond).addCondition(cond);
						else totalCond = new ComboCondition(Op.AND, totalCond, cond);
					}
				}
				
				if (arg instanceof Attribute) {
					Condition cond = new BinaryCondition(BinaryCondition.Op.EQUAL_TO, 
							new CustomSql(tableAlias+"."+column.get1()),
							((Attribute)arg).getAttribute());
	
					if (totalCond instanceof ComboCondition)
						((ComboCondition)totalCond).addCondition(cond);
					else totalCond = new ComboCondition(Op.AND, totalCond, cond);
				} else if (arg instanceof Entity) { //Entity
					Entity e = (Entity)arg;
					Condition cond = new BinaryCondition(BinaryCondition.Op.EQUAL_TO, 
							new CustomSql(tableAlias+"."+column.get1()),
							e.getID().getDBID());
	
					if (totalCond instanceof ComboCondition)
						((ComboCondition)totalCond).addCondition(cond);
					else totalCond = new ComboCondition(Op.AND, totalCond, cond);

				} else assert arg instanceof Variable;
			}
			
			/*if(isFirst)
			{
				tmpQuery.addCustomFromTable(pred.tableName() +" "+ tableAlias);
				isFirst = false;
			}
			else*/
			leftJoins.add(new Tuple2<String, Condition>(pred.tableName() +" "+ tableAlias, totalCond));
			//tmpQuery.addCustomJoin(JoinType.LEFT_OUTER, "", pred.tableName() +" "+ tableAlias, totalCond);
		}
		
		/*for (Variable v:addedVars)
		{
			if (partialGrounding.hasVariable(v)) {
				query.addAliasedColumn(new CustomSql(partialGrounding.getVariable(v)), v.getName()); 
			} else {
				query.addCustomColumns(new CustomSql(v.getName()));
			}
		}*/
		
		if (!orderByClause.equals(""))
		{
			//do nothing <<------------------------
			//query.addCondition(new BinaryCondition(BinaryCondition.Op.LESS_THAN_OR_EQUAL_TO, new CustomSql(whereClauseLimitNullsCount), allowedNulls));
			//query.addAliasedColumn(new CustomSql(whereClauseLimitNullsCount), "nullCounts");
			
			//tmpQuery.addCustomOrderings(new CustomSql(orderByClause + " desc"));
			tmpQuery.addCustomOrderings(new CustomSql("max("+orderByClause + ") desc"));

		}
		usedVariablesInQuery.remove(prevVariable);
		for (Variable v:usedVariablesInQuery)
		{
			//tmpQuery.addCustomFromTable(new CustomSql("tbl"+v.getName()));
			tmpQuery.addCustomJoin(JoinType.INNER, "", "tbl"+v.getName(),  new CustomCondition("true"));
		}
		for (Tuple2<String, Condition> leftJoin:  leftJoins)
		{
			tmpQuery.addCustomJoin(JoinType.LEFT_OUTER, "", leftJoin.get0(), leftJoin.get1());
		}
		
		tmpQuery.addCustomGroupings(new CustomSql(prevVariable.getName()));
		//execute tmpQuery as an insertSelect
		log.trace(tmpQuery.toString());
		try {
			Statement stmt = database.getConnection().createStatement();
			try {
				stmt.execute("INSERT INTO tbl" + prevVariable.getName() + " (" + prevVariable.getName() + ")" 
						+ tmpQuery + " limit " + rowsCountLimit);
			} finally {
			    stmt.close();
			}
		} catch(Exception e) {
			log.error("SQL error: {}",e.getMessage());
			throw new AssertionError(e);
		}
		query = new  SelectQuery();
		query.addAllColumns();
		query.addCustomFromTable("(" + qdummy.toString() + ") dummy");
		
		for (Variable v:projection)
		{
			if (addedVars.contains(v))
				query.addCustomJoin(JoinType.INNER, "", "tbl"+v.getName(),  new CustomCondition("true"));
			else
				query.addCustomJoin(JoinType.INNER, "", "(SELECT 0 AS " + v.getName() + " FROM DUAL )tbl"+v.getName(),  new CustomCondition("true"));
				
		}
		
		return;
		
	}

	@Override
	public void afterDisjunction(int noFormulas) {
		throw new AssertionError("Disjunction is currently not supported by database");
	}

	@Override
	public void afterNegation() {
		throw new AssertionError("Negation is currently not supported by database");
	}

	
	private void visitFunctionalAtom(Atom atom) {
		assert atom.getPredicate() instanceof FunctionalPredicate;
		Term[] arguments = atom.getArguments();
		assert arguments.length==2;
		Object[] convert = convertArguments(arguments);
		
		if (atom.getPredicate() instanceof ExternalFunctionPredicate) {
			ExternalFunctionPredicate predicate = (ExternalFunctionPredicate)atom.getPredicate();
			FunctionCall fun = new FunctionCall(RDBMSDatabase.aliasFunctionName);
			fun.addCustomParams(RDBMSDatabase.getSimilarityFunctionID(predicate.getExternalFunction()));
			for (int i=0;i<arguments.length;i++) fun.addCustomParams(convert[i]);
			query.addCondition(BinaryCondition.greaterThan(fun, 0.0, false));
		} else {
			if (allowedNulls>=0)
			{
				FunctionalPredicate predicate = (FunctionalPredicate)atom.getPredicate();
				if (predicate==SpecialPredicates.Unequal) {
					query.addCondition(BinaryCondition.notEqualTo(new CustomSql(arguments[0]), new CustomSql(arguments[1])));
				} else if (predicate==SpecialPredicates.Equal) {
					query.addCondition(BinaryCondition.equalTo(new CustomSql(arguments[0]), new CustomSql(arguments[1])));
				} else if (predicate==SpecialPredicates.NonSymmetric) {
					query.addCondition(BinaryCondition.lessThan(new CustomSql(arguments[0]), new CustomSql(arguments[1]),false));
				} else throw new UnsupportedOperationException("Unrecognized functional Predicate: " + predicate);
			}
			else 
			{
				FunctionalPredicate predicate = (FunctionalPredicate)atom.getPredicate();
				if (predicate==SpecialPredicates.Unequal) {
					query.addCondition(BinaryCondition.notEqualTo(convert[0], convert[1]));
				} else if (predicate==SpecialPredicates.Equal) {
					query.addCondition(BinaryCondition.equalTo(convert[0], convert[1]));
				} else if (predicate==SpecialPredicates.NonSymmetric) {
					query.addCondition(BinaryCondition.lessThan(convert[0], convert[1],false));
				} else throw new UnsupportedOperationException("Unrecognized functional Predicate: " + predicate);	
			}
		}
		
		
	}
	
	private Object[] convertArguments(Term[] arguments) {
		Object[] convert = new Object[arguments.length];
		
		for (int i=0;i<arguments.length;i++) {
			Term arg = arguments[i];
			if (arg instanceof Variable) {
				if (partialGrounding.hasVariable((Variable)arg)) {
					arg = partialGrounding.getVariable((Variable)arg);
				} else {
					assert joins.containsKey((Variable)arg) : arg;
					convert[i]=new CustomSql(joins.get((Variable)arg));
				}
			}
			if (arg instanceof Attribute) {
				convert[i] = ((Attribute)arg).getAttribute();
			} else if (arg instanceof Entity) {
				Entity e = (Entity)arg;
				convert[i] = e.getID().getDBID();
			} else assert arg instanceof Variable;
		}
		return convert;
	}

	
	
	@Override
	public void visitAtom(Atom atom) {
		if (atom.getPredicate() instanceof FunctionalPredicate) {
			functionalAtoms.add(atom);
		} else {
			assert atom.getPredicate() instanceof StandardPredicate;
			RDBMSPredicateHandle ph = database.getHandle(atom.getPredicate());
			
			String tableName = tablePrefix+tableCounter;
			String tableDot = tableName+".";
			query.addCustomFromTable(ph.tableName()+" "+tableName);
			tableAliasToPredicate.put(tableName, ph);
			Term[] arguments = atom.getArguments();
			//boolean tableAdded = false;
			for (int i=0;i<ph.argumentColumns().length;i++) {
				Term arg = arguments[i];
	
				queryColumnsByPred.put(tableName, new Tuple2<Term, String>
								(arg, ph.argumentColumns()[i]));
				//tableAdded = true;

			
				if (arg instanceof Variable) {
					Variable var = (Variable)arg;
					queryColumnsByVar.put(var, new Tuple3<String, String, String>
											(ph.tableName(), tableName, ph.argumentColumns()[i]));

					
					if (partialGrounding.hasVariable(var)) {
						//assert !projection.contains(var);
						arg = partialGrounding.getVariable(var);
					} else {
						if (joins.containsKey(var)) {
							String to = joins.get(var);
							String [] toSplits = to.split("\\.");
							if (toSplits.length != 2)
								throw new RuntimeException("Unexpected tableName.columName " + joins.get(var));
							query.addCondition(BinaryCondition.equalTo(new CustomSql(tableDot+ph.argumentColumns()[i]),  new CustomSql(joins.get(var)) ));
						} else {
							if (projection.contains(var)) {
								query.addAliasedColumn(new CustomSql(tableDot+ph.argumentColumns()[i]), var.getName());
							}
							joins.put(var, tableDot+ph.argumentColumns()[i]);
						}
						
					}
				}
				
				if (arg instanceof Attribute) {
					query.addCondition(BinaryCondition.equalTo(new CustomSql(tableDot+ph.argumentColumns()[i]),  ((Attribute)arg).getAttribute() ));
				} else if (arg instanceof Entity) { //Entity
					Entity e = (Entity)arg;
					query.addCondition(BinaryCondition.equalTo(new CustomSql(tableDot+ph.argumentColumns()[i]),  e.getID().getDBID() ));
				} else assert arg instanceof Variable;
			}
			/*if(!tableAdded)
				queryColumnsByPred.put(tableName, new Tuple2<Term, String>
					(null, null));				
			*/
			
			query.addCondition(new InCondition(new CustomSql(tableDot+ph.partitionColumn()),database.getReadIDs()));
			if (!ph.isClosed()) {
				query.addCondition(BinaryCondition.lessThan(new CustomSql(tableDot+ph.pslColumn()), 
										PSLValue.getNonDefaultUpperBound(), false) );
			}
			tableCounter++;
		}
	}
	
	
	public Tuple2<String, Integer> getSQL(Formula f, int allowedNulls) {
		this.allowedNulls = allowedNulls;	
		FormulaTraverser.traverse(f, this);
		for (Atom atom : functionalAtoms) visitFunctionalAtom(atom);
		
		SelectQuery q1 = new SelectQuery();
		q1.addAliasedColumn(new CustomSql("woman_n_dh.arg0"), "X2");
		q1.addAliasedColumn(new CustomSql("man_n_dh.arg0"), "X3");
		q1.addAliasedColumn(new CustomSql("NVL2(man_n_dh.arg0, 1, 0)"), "X4");
		q1.addAliasedColumn(new CustomSql("NVL2(woman_n_dh.arg0, 1, 0)"), "X5");
		q1.addAliasedColumn(new CustomSql("(man_n_dh.arg0 is null)"), "X6");
		q1.addAliasedColumn(new CustomSql("(woman_n_dh.arg0 is null)"), "X7");
		//q1.addAliasedColumn(new CustomSql("(cast((woman_n_dh.arg0 is null) as TINYINT)  + cast((woman_n_dh.arg0 is null) as TINYINT))"), "X8");
		q1.addAliasedColumn(new CustomSql("cast((woman_n_dh.arg0 is null) as TINYINT)"), "X8");
		q1.addAliasedColumn(new CustomSql("cast((man_n_dh.arg0 is null) as TINYINT)"), "X9");
		q1.addCustomFromTable("woman_n_dh");
		q1.addCustomJoin(JoinType.LEFT_OUTER, "", "man_n_dh", 
				BinaryCondition.equalTo(new CustomSql("woman_n_dh.arg0"),  new CustomSql("man_n_dh.arg0") ));
		//q1.addCondition(new BinaryCondition(BinaryCondition.Op.LESS_THAN, 
		//		new CustomSql("(NVL(man_n_dh.arg0, 1, 0)) + (NVL(woman_n_dh.arg0, 1, 0))"), 20));

		//q1.addCustomJoin(JoinType.LEFT_OUTER, "", "c t3", BinaryCondition.equalTo(new CustomSql("t2.arg0"),  new CustomSql("t3.arg0") ));
		
		
		SelectQuery q2 = new SelectQuery();
		q2.addAliasedColumn(new CustomSql("t2.arg0"), "X2");
		q2.addAliasedColumn(new CustomSql("t3.arg0"), "X3");
		q2.addCustomFromTable("b t2");
		q2.addCustomJoin(JoinType.RIGHT_OUTER, "", "c t3", BinaryCondition.equalTo(new CustomSql("t2.arg0"),  new CustomSql("t3.arg0") ));
		
		//q.addCustomJoin("Inner join b t2 on t1.arg0 = t2.arg0");
		//q.addCustomFromTable("a t1");
		
		UnionQuery uq = new UnionQuery(Type.UNION, q1, q2);
		//System.out.println(uq.validate().toString());
		
		CustomSql cq = new  CustomSql("select a.ida, b.idb, c.idc from ( select ida as id from a union  select idb as id from b union  select idc as id from c )tbl left outer join a on tbl.id = a.ida left outer join b on tbl.id = b.idb left outer join c on tbl.id = c.idc");
		
		SelectQuery qa = new SelectQuery();
		qa.addAliasedColumn(new CustomSql("t1.arg0"), "X");
		qa.addCustomFromTable("a t1");
		
		SelectQuery qb = new SelectQuery();
		qb.addAliasedColumn(new CustomSql("t2.arg0"), "X");
		qb.addCustomFromTable("b t2");
		
		SelectQuery qc = new SelectQuery();
		qc.addAliasedColumn(new CustomSql("t3.arg0"), "X");
		qc.addCustomFromTable("c t3");
		
		UnionQuery innerQ = new UnionQuery(Type.UNION, qa, qb, qc);
		
		SelectQuery outerQ = new SelectQuery();
		//outerQ.addAliasedColumn(new CustomSql("t1.arg0"), "X1");
		//outerQ.addAliasedColumn(new CustomSql("t2.arg0"), "X2");
		//outerQ.addAliasedColumn(new CustomSql("t3.arg0"), "X3");
		//outerQ.addCustomColumns(new CustomSql("COALESCE(-2147483648, t2.arg0, t3.arg0)"));
		outerQ.addAllColumns();
		
		//outerQ.addCustomFromTable("("+innerQ.validate().toString()+")tblY");
		
		//outerQ.addCustomFromTable("("+innerQ.validate().toString()+")tblX");
		//outerQ.addCustomFromTable("a t1");
		outerQ.addCustomJoin(JoinType.INNER, "", "b t2", new CustomCondition("true"));
		outerQ.addCustomJoin(JoinType.INNER, "", "("+innerQ.validate().toString()+")tblX", new CustomCondition("true"));
		outerQ.addCustomJoin(JoinType.INNER, "", "("+innerQ.validate().toString()+")tblY", new CustomCondition("true"));
		outerQ.addCustomJoin(JoinType.INNER, "", "("+innerQ.validate().toString()+")tblZ", new CustomCondition("true"));
		//outerQ.addCustomJoin(JoinType.LEFT_OUTER, "", "a t1", BinaryCondition.equalTo(new CustomSql("t1.arg0"),  new CustomSql("tblX.X") ));
		//
		//outerQ.addCustomJoin(JoinType.LEFT_OUTER, "", "b t2", BinaryCondition.equalTo(new CustomSql("t2.arg0"),  new CustomSql("tblX.X") ));
		//outerQ.addCustomJoin(JoinType.LEFT_OUTER, "", "c t3", BinaryCondition.equalTo(new CustomSql("t3.arg0"),  new CustomSql("tblX.X") ));
		//System.out.println(outerQ.validate().toString());
		//return outerQ.validate().toString();
		//SelectQuery newQ  = new CustomSql("SELECT DISTINCT t1.arg0 AS X,t4.arg0 AS Y,tblX.id,tblY.id FROM a t1, b t2, c t3, d t4, e t5, f t6, g t7 INNER JOIN (SELECT t3.arg0 AS id FROM c t3 UNION SELECT t1.arg0 AS id FROM a t1 UNION SELECT t7.arg0 AS id FROM g t7 UNION SELECT t2.arg0 AS id FROM b t2)tblX ON (true) INNER JOIN (SELECT t6.arg0 AS id FROM f t6 UNION SELECT t5.arg0 AS id FROM e t5 UNION SELECT t4.arg0 AS id FROM d t4 UNION SELECT t7.arg1 AS id FROM g t7)tblY ON (true) WHERE ((t1.part IN (1,1000) ) AND (t1.psl < 50) AND (t2.arg0 = t1.arg0) AND (t2.part IN (1,1000) ) AND (t2.psl < 50) AND (t3.arg0 = t1.arg0) AND (t3.part IN (1,1000) ) AND (t3.psl < 50) AND (t4.part IN (1,1000) ) AND (t4.psl < 50) AND (t5.arg0 = t4.arg0) AND (t5.part IN (1,1000) ) AND (t5.psl < 50) AND (t6.arg0 = t4.arg0) AND (t6.part IN (1,1000) ) AND (t6.psl < 50) AND (t7.arg0 = t1.arg0) AND (t7.arg1 = t4.arg0) AND (t7.part IN (1,1000) ) AND (t7.psl < 50))");
				
		//return q1.validate().toString();
		
		SelectQuery subQ = new SelectQuery();
		subQ.addAllColumns();
		subQ.addCustomFromTable("r_agent_dh");
		SelectQuery outQ = new SelectQuery();
		outQ.addAllColumns();
		outQ.addCustomFromTable("("+subQ.toString()+")");
		outQ.addCustomFromTable("someone_n_dh");
		
		
		
		//return new Tuple2<String, Integer> (outQ.validate().toString(), predCount);
		return new Tuple2<String, Integer> (query.validate().toString(), predCount);
	}
	
		
}
