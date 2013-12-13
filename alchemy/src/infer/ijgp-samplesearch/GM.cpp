#include "GM.h"
#include "JG.h"
#include "DRC.h"
#include <map>
#include <set>
#include <queue>
#include "SampleSearch.h"
#include "Clause.h"
#include "Random.h"
#include "SATInstance.h"

namespace ss{

//bool less_than_comparator_variable(const Variable* a, const Variable* b)
//{
//	if(a->id()<b->id())
//		return true;
//	return false;
//}
struct less_than_comparator_function
{
	bool operator() (const Function* a, const Function* b) const
	{
		return (a->id()<b->id());
	}
};

/*void GM::deleteEmptyFunctions()
{
	vector<Function*> new_functions;

	for(int i=0;i<functions.size();i++){
		if(functions[i]->table().empty()){
			delete(functions[i]);
		}
		else{
			new_functions.push_back(functions[i]);
			new_functions[new_functions.size()-1]->id()=(int)new_functions.size()-1;
		}
	}
	functions=new_functions;
}
void Function::readCNF(istream& in, vector<Variable*>& all_variables,vector<Lit>& clause,double damp,int c_bound)
{
	do{
		int curr_var;
		in>>curr_var;
		if(curr_var ==0)
			break;
		int var_id=((curr_var > 0)? (curr_var-1):(-curr_var-1));
		variables_.push_back(all_variables[var_id]);

		if(curr_var>0)
		{
			all_variables[var_id]->addr_value()=0;

			clause.push_back(Lit(var_id));

		}
		else
		{
			all_variables[var_id]->addr_value()=1;

			clause.push_back(~Lit(var_id));

		}

	}while(1);

	sort(variables_.begin(),variables_.end(),less_than_comparator_variable);

	if((int)variables_.size() > c_bound){
		table_=vector<Double>();
		return;
	} 
	int table_size=Variable::getDomainSize(variables_);
	table_=vector<Double> (table_size);
	for(int i=0;i<table_.size();i++)
	{
		table_[i]=Double(1.0);
	}
	if (damp > 0.0){
		table_[Variable::getAddress(variables_)]=Double(damp);
	} else {
		table_[Variable::getAddress(variables_)]=Double();
	}
}

bool GM::readCNF(const char* infilename)
{
	ifstream xInputFile(infilename);
	char cCheck;
	char buffer[5000];
	int num_variables,num_constraints;
	while (1) 
	{
		xInputFile >> cCheck;
		if (cCheck == 'c') 
		{
			xInputFile.getline(buffer, 5000);
			continue;
		}
		else if (cCheck == 'p') 
		{
			xInputFile >> buffer;
			xInputFile >> num_variables;
			xInputFile >> num_constraints;
			break;
		}
		else 
		{
			cout << "\nError: File not in DIMACS format?\n";
			//return 0;
			return false;
		}
	}
	clauses=vector<vector<Lit> > (num_constraints);
	// Read the variables
	variables=vector<Variable*>(num_variables);
	for(int i=0;i<num_variables;i++)
	{
		vector<int> dom(2);
		dom[0]=0;dom[1]=1;
		variables[i]=new Variable(i,dom);
	}
	functions=vector<Function*>(num_constraints);
	for(int i=0;i<num_constraints;i++)
		functions[i]=new Function(i);
	int i=0;
	while (1) 
	{
		if(i==num_constraints)
			break;
		if (xInputFile.eof()) 
		{
			cout << "\nError: Unexpected end of file.\n";
			//return 0;
			exit(1);
		}
		xInputFile >> cCheck;
		if (cCheck == 'c') 
		{
			xInputFile.getline(buffer, 5000);
			continue;
		}
		else xInputFile.putback(cCheck);
		functions[i]->readCNF(xInputFile,variables,clauses[i],damp,c_bound);
		i++;
	}
	cout<<"Problem read\n";
	return true;
}
*/
void GM::convertToSATUAI10()
{
	instance.vClear();
	vector<bool> is_relevant(variables.size());
	csp_to_sat_variables = vector<vector<int> > (variables.size());
	sat_to_csp_variables = vector<int> ();
	int count = 0;
	for (int i = 0; i < variables.size(); i++) {
		for (int j = 0; j < variables[i]->domain_size(); j++) {
			sat_to_csp_variables.push_back(i);
			csp_to_sat_variables[i].push_back(count+j);

		}
		count+=variables[i]->domain_size();
	}
	instance.iVariableCount=count;
	VariableList pos(count),neg(count);
	for (int i = 0; i < functions.size(); i++) {
		Function* function = functions[i];
		for (int j = 0; j < function->tableSize(); j++) {
			Variable::setAddress(function->variables(), j);
			if (function->tableEntry(j).isZero()) {
				  pos.vClear();
				  neg.vClear();
				for (int k = 0; k < function->variables().size(); k++) {
					neg.vAdd(csp_to_sat_variables[function->variables()[k]->id()][function->variables()[k]->addr_value()]);
					is_relevant[function->variables()[k]->id()]=true;
				}
				instance.vAddClause(new RClause(pos,neg,1));
			}
		}
	}
	for(int i=0;i<variables.size();i++){
		if (is_relevant[i]){
			pos.vClear();
			neg.vClear();
			for(int j=0;j<variables[i]->domain_size();j++){
				pos.vAdd(csp_to_sat_variables[i][j]);
			}
			instance.vAddClause(new RClause(pos,neg,1));
			pos.vClear();
			for(int j=0;j<variables[i]->domain_size();j++){
				for(int k=j+1;k<variables[i]->domain_size();k++){
					neg.vClear();
					neg.vAdd(csp_to_sat_variables[i][j]);
					neg.vAdd(csp_to_sat_variables[i][k]);
					instance.vAddClause(new RClause(pos,neg,1));
				}
			}
		}
	}
}
void GM::convertToSAT()
{

	// First map the variables between SAT and the belief network
	// For each variable we create a clause
	// Create a value for each variable value assignment
	int count=0;
	csp_to_sat_variables=vector<vector<int> > (variables.size());
	int num_con=0;
	for(int i=0;i<variables.size();i++)
	{
		for(int j=0;j<variables[i]->domain_size();j++)
		{
			sat_to_csp_variables.push_back(i);
			csp_to_sat_variables[i].push_back(count);
			count++;
		}
	}
	for(int i=0;i<variables.size();i++)
	{
		num_con++;
		for(int j=0;j<csp_to_sat_variables[i].size();j++)
		{
			for(int k=j+1;k<csp_to_sat_variables[i].size();k++)
			{
				num_con++;
			}
		}
	}
	//// Add all evidence variables as clauses
	//for(int i=0;i<num_variables;i++)
	//	if(variables[i]->is_evidence)
	//		num_con++;
	/*for( int i=0;i<num_constraints;i++)
	{
	for(int j=0;j<constraints[i]->table.size();j++)
	{
	Variable::setAddressValues(constraints[i]->scope,j);
	if(!constraints[i]->table[constraints[i]->getEntry()])
	{
	num_con++;
	}
	}
	}*/
	//num_con+=num_constraints;
	cout<<"Counting constraints = ";
	//Find the number of constraints that can be derived from the cpts
	int num_det_functions=0;
	for(int i=0;i<functions.size();i++)
	{
	  bool is_det=false;
		Function* function =functions[i];
		for(int j=0;j<function->tableSize();j++)
		{
		  if(function->tableEntry(j).isZero()){
				num_con++;
				is_det=true;
		  }
		}
		if (is_det)
		  num_det_functions++;
	}
	cout<<num_det_functions<<endl;
	//cerr<<"Writing to file\n";
	ofstream outfile ("temp.cnf");
	outfile<<"p cnf "<<sat_to_csp_variables.size()<<" "<<num_con<<endl;

	//Now write constraints to ostream
	for(int i=0;i<variables.size();i++)
	{


		// A constraint making sure that each variable is assigned a value
		for(int j=0;j<csp_to_sat_variables[i].size();j++)
		{
			outfile<<csp_to_sat_variables[i][j]+1<<" ";
		}
		outfile<<"0 \n";
		// A constraint making sure that each variable is assigned a value
		for(int j=0;j<csp_to_sat_variables[i].size();j++)
		{
			for(int k=j+1;k<csp_to_sat_variables[i].size();k++)
			{
				outfile<<-(csp_to_sat_variables[i][j]+1)<<" "<<-(csp_to_sat_variables[i][k]+1)<<" 0 "<<endl;
			}
		}

	}
	for(int i=0;i<functions.size();i++)
	{
		Function* function = functions[i];

		for(int j=0;j<function->tableSize();j++)
		{
			Variable::setAddress(function->variables(),j);
			if(function->tableEntry(j).isZero())
			{

				for(int k=0;k<function->variables().size();k++)
				{
					/*if(function->scope[k]->addr_value==0)
					{
					outfile<<(function->scope[k]->id+1)<<" ";
					}
					else
					{
					outfile<<-(function->scope[k]->id+1)<<" ";
					}*/
					outfile<<-(csp_to_sat_variables[function->variables()[k]->id()][function->variables()[k]->addr_value()]+1)<<" ";
				}
				outfile<<"0\n";
			}
		}
	}

	// Write evidence
	/*for(int i=0;i<num_variables;i++)
	if(variables[i]->is_evidence)
	{
	assert(variables[i]->value!=INVALID_VALUE);
	outfile<<csp_to_sat_variables[i][variables[i]->value]+1<<" 0\n";
	}*/
	/*for( int i=0;i<num_constraints;i++)
	{
	for(int j=0;j<constraints[i]->table.size();j++)
	{
	Variable::setAddressValues(constraints[i]->scope,j);
	if(!constraints[i]->table[constraints[i]->getEntry()])
	{
	for(int k=0;k<constraints[i]->scope_size;k++)
	{
	int var=constraints[i]->scope[k]->id;
	int val=constraints[i]->scope[k]->addr_value;
	outfile<<-(csp_to_sat_variables[var][val]+1)<<" ";
	}
	outfile<<" 0\n";
	}
	}
	}*/
	outfile.close();

}
/*
bool GM::readVIB(char* bayes_file)
{
	cout<<"Reading the Bayesian Network..."<<flush;
	//Open the file
	ifstream infile(bayes_file);
	if(!infile.good())
	{
		cerr<<"Empty Bayesnet file\n";
		return false;
	}
	int num_variables;
	// Read number of variables
	infile>>num_variables;
	//Read the values of each variable
	variables=vector<Variable*>(num_variables);

	//Read variable information
	for(int i=0;i<num_variables;i++)
	{
		int num_values;
		infile>>num_values;
		vector<int> domain;
		for(int j=0;j<num_values;j++)
		{
			domain.push_back(j);
		}
		variables[i]=new Variable(i,domain);
	}

	int num_cpts;
	//Read #conditional probability tables
	infile>>num_cpts;

	functions=vector<Function*> (num_cpts);

	// Currently we only allow num cpts = numvariables
	assert(num_cpts==num_variables);
	int max_parents=0;
	//Read the cpts
	for(int i=0;i<num_cpts;i++)
	{
		functions[i]=new CPT();
		CPT* cpt=dynamic_cast<CPT*> (functions[i]);
		cpt->id()=i;

		//Read the variables in the function
		int num_variables_infunc;
		infile>>num_variables_infunc;
		int marg_variable;
		infile>>marg_variable;
		cpt->setMargVariable(variables[marg_variable]);
		//func_variables.push_back(variables[marg_variable]);
		for(int j=1;j<num_variables_infunc;j++)
		{
			int temp;
			infile>>temp;
			cpt->cond_variables().push_back(variables[temp]);
		}
		if(max_parents < num_variables_infunc)
			max_parents=num_variables_infunc;
		// Currently in vib format we allow specification of functions in order
		assert(marg_variable==i);

		cpt->variables()=cpt->cond_variables();
		cpt->variables().push_back(cpt->marg_variable());
		sort(cpt->variables().begin(),cpt->variables().end(),less_than_comparator_variable);

		//int num_values=Variable::getDomainSize(sorted_func_variables);
		////int marg_num_values=Variable::getNumValues(marg_variables);
		//functions[i]->variables()=sorted_func_variables;
		int cond_num_values=Variable::getDomainSize(cpt->cond_variables());
		int num_values=Variable::getDomainSize(cpt->variables());
		cpt->table()=vector<Double> (num_values);

		for(int j=0;j<cond_num_values;j++)
		{
			Variable::setAddressVIB(cpt->cond_variables(),j);
			for(int k=0;k<cpt->marg_variable()->domain_size();k++)
			{
				cpt->marg_variable()->addr_value()=k;
				double value;
				infile>>value;

				int entry=Variable::getAddress(cpt->variables());
				if(value > DBL_MIN)
				{		
					cpt->table()[entry]=Double(value);
				}
			}
		}
		sort(cpt->cond_variables().begin(),cpt->cond_variables().end(),less_than_comparator_variable);
	}

	// Construct the graph of the bayesian network
	//cout<<"Max parents = "<<max_parents+1<<endl;
	cout<<"Done\n";
	return true;
}
void GM::readEvidence1(char* evidencefile)
{
	mult_factor=Double((double)1.0);
	int num_evidence;
	ifstream evidfile(evidencefile);
	char buffer[1000];
	evidfile.getline(buffer,1000);
	evidfile>>num_evidence;
	for(int i=0;i<num_evidence;i++)
	{
		int var,val;
		evidfile>>var>>val;
		assert(var < variables.size());
		assert(val < variables[var]->domain_size());
		Function *func=new Function();
		func->variables().push_back(variables[var]);
		func->table()=vector<Double>(variables[var]->domain_size());
		func->table()[val]=Double((double)1.0);
		functions.push_back(func);
	}
}

void GM::readUAIO8Evidence(char* evidencefile)
{
	mult_factor=Double((double)1.0);
	vector<Variable*> evidence_variables;

	int num_evidence;
	ifstream evidfile(evidencefile);
	// Just ignore the first line

	//char buffer[1000];
	//evidfile.getline(buffer,1000);
	evidfile>>num_evidence;
	for(int i=0;i<num_evidence;i++)
	{
		int var,val;
		evidfile>>var>>val;
		evidence_variables.push_back(variables[var]);
		variables[var]->value()=val;
		variables[var]->addr_value()=val;
	}
	sort(evidence_variables.begin(),evidence_variables.end(),less_than_comparator_variable);
	//cerr<<"num-evidence = "<<evidence_variables.size()<<endl;
	vector<int> variable_mapping(variables.size());
	vector<bool> is_evidence(variables.size());
	for(int i=0;i<variables.size();i++)
	{
		is_evidence[i]=false;
	}
	for(int i=0;i<evidence_variables.size();i++)
		is_evidence[evidence_variables[i]->id()]=true;

	int var_count=0;
	for(int i=0;i<variable_mapping.size();i++)
	{
		if(!is_evidence[i])
		{		
			variable_mapping[i]=var_count;
			var_count++;
		}
		else
		{
			variable_mapping[i]=INVALID_VALUE;
		}
	}

	vector<bool> deleted_functions(functions.size());
	//update functions
	for(int i=0;i<functions.size();i++)
	{
		vector<Variable*> func_evid_vars;
		deleted_functions[i]=false;
		do_set_intersection(functions[i]->variables(),evidence_variables,func_evid_vars,less_than_comparator_variable);
		if(func_evid_vars.empty())
			continue;
		// All variables are evidence variables
		if(func_evid_vars.size()==functions[i]->variables().size())
		{
			int address=Variable::getAddress(functions[i]->variables());
			mult_factor*=functions[i]->table()[address];

			deleted_functions[i]=true;
			continue;
		}
		vector<Variable*> other_variables;
		do_set_difference(functions[i]->variables(),func_evid_vars,other_variables,less_than_comparator_variable);
		int other_num_values=Variable::getDomainSize(other_variables);
		vector<Double> new_table(other_num_values);
		for(int j=0;j<other_num_values;j++)
		{
			Variable::setAddress(other_variables,j);
			int entry=Variable::getAddress(functions[i]->variables());
			new_table[j]=functions[i]->table()[entry];
		}

		functions[i]->table()=new_table;
		functions[i]->variables()=other_variables;
	}
	//for(int i=0;i<functions.size();i++)
	//{
	//	vector<Variable*> func_evid_vars;
	//	//deleted_functions[i]=false;
	//	do_set_intersection(functions[i]->variables(),evidence_variables,func_evid_vars,less_than_comparator_variable);
	//	if(!func_evid_vars.empty())
	//	{
	//		cerr<<"Something wrong in assigning evidence\n";
	//	}
	//}
	cerr<<"Mult-factor = "<<mult_factor<<endl;
	//update functions;
	vector<Function*> new_functions;
	int count=0;
	for(int i=0;i<functions.size();i++)
	{
		if(deleted_functions[i])
		{
			Function* func_pt=functions[i];
			functions[i]=NULL;
			delete(func_pt);
		}
		else
		{
			functions[i]->id()=count++;
			new_functions.push_back(functions[i]);
		}
	}
	functions=new_functions;
	//update variables
	vector<Variable*> new_variables;
	var_to_ids=vector<int> (variables.size()-num_evidence);
	for(int i=0;i<variables.size();i++)
	{
		if(is_evidence[i])
		{
			Variable* var_pt=variables[i];
			variables[i]==NULL;
			delete(var_pt);
		}
		else
		{
			variables[i]->id()=variable_mapping[i];
			var_to_ids[variables[i]->id()]=i;
			new_variables.push_back(variables[i]);
		}
	}
	variables=new_variables;
	//for(int i=0;i<variables.size();i++)
	//	cout<<variables[i]->id()<<" ";
	//cout<<endl;
}

void GM::readEvidenceErgo(char* evidencefile)
{
	mult_factor=Double((double)1.0);
	vector<Variable*> evidence_variables;

	int num_evidence;
	ifstream evidfile(evidencefile);
	// Just ignore the first line

	char buffer[1000];
	evidfile.getline(buffer,1000);
	evidfile>>num_evidence;
	for(int i=0;i<num_evidence;i++)
	{
		int var,val;
		evidfile>>var>>val;
		evidence_variables.push_back(variables[var]);
		variables[var]->value()=val;
		variables[var]->addr_value()=val;
	}
	sort(evidence_variables.begin(),evidence_variables.end(),less_than_comparator_variable);
	//cerr<<"num-evidence = "<<evidence_variables.size()<<endl;
	vector<int> variable_mapping(variables.size());
	vector<bool> is_evidence(variables.size());
	for(int i=0;i<variables.size();i++)
	{
		is_evidence[i]=false;
	}
	for(int i=0;i<evidence_variables.size();i++)
		is_evidence[evidence_variables[i]->id()]=true;

	int var_count=0;
	for(int i=0;i<variable_mapping.size();i++)
	{
		if(!is_evidence[i])
		{		
			variable_mapping[i]=var_count;
			var_count++;
		}
		else
		{
			variable_mapping[i]=INVALID_VALUE;
		}
	}

	vector<bool> deleted_functions(functions.size());
	//update functions
	for(int i=0;i<functions.size();i++)
	{
		vector<Variable*> func_evid_vars;
		deleted_functions[i]=false;
		do_set_intersection(functions[i]->variables(),evidence_variables,func_evid_vars,less_than_comparator_variable);
		if(func_evid_vars.empty())
			continue;
		// All variables are evidence variables
		if(func_evid_vars.size()==functions[i]->variables().size())
		{
			int address=Variable::getAddress(functions[i]->variables());
			mult_factor*=functions[i]->table()[address];

			deleted_functions[i]=true;
			continue;
		}
		vector<Variable*> other_variables;
		do_set_difference(functions[i]->variables(),func_evid_vars,other_variables,less_than_comparator_variable);
		int other_num_values=Variable::getDomainSize(other_variables);
		vector<Double> new_table(other_num_values);
		for(int j=0;j<other_num_values;j++)
		{
			Variable::setAddress(other_variables,j);
			int entry=Variable::getAddress(functions[i]->variables());
			new_table[j]=functions[i]->table()[entry];
		}

		functions[i]->table()=new_table;
		functions[i]->variables()=other_variables;
	}
	//for(int i=0;i<functions.size();i++)
	//{
	//	vector<Variable*> func_evid_vars;
	//	//deleted_functions[i]=false;
	//	do_set_intersection(functions[i]->variables(),evidence_variables,func_evid_vars,less_than_comparator_variable);
	//	if(!func_evid_vars.empty())
	//	{
	//		cerr<<"Something wrong in assigning evidence\n";
	//	}
	//}
	cerr<<"Mult-factor = "<<mult_factor<<endl;
	//update functions;
	vector<Function*> new_functions;
	int count=0;
	for(int i=0;i<functions.size();i++)
	{
		if(deleted_functions[i])
		{
			Function* func_pt=functions[i];
			functions[i]=NULL;
			delete(func_pt);
		}
		else
		{
			functions[i]->id()=count++;
			new_functions.push_back(functions[i]);
		}
	}
	functions=new_functions;
	//update variables
	vector<Variable*> new_variables;
	var_to_ids=vector<int> (variables.size()-num_evidence);
	for(int i=0;i<variables.size();i++)
	{
		if(is_evidence[i])
		{
			Variable* var_pt=variables[i];
			variables[i]==NULL;
			delete(var_pt);
		}
		else
		{
			variables[i]->id()=variable_mapping[i];
			var_to_ids[variables[i]->id()]=i;
			new_variables.push_back(variables[i]);
		}
	}
	variables=new_variables;
	//for(int i=0;i<variables.size();i++)
	//	cout<<variables[i]->id()<<" ";
	//cout<<endl;
}

void GM::readEvidenceVIB(char* evidencefile)
{
	mult_factor=Double((double)1.0);
	vector<Variable*> evidence_variables;

	int num_evidence;
	ifstream evidfile(evidencefile);
	// Just ignore the first line

	char buffer[1000];
	//evidfile.getline(buffer,1000);
	evidfile>>num_evidence;
	for(int i=0;i<num_evidence;i++)
	{
		int var,val;
		evidfile>>var>>val;
		evidence_variables.push_back(variables[var]);
		cout<<var<<" "<<val<<endl;
		variables[var]->value()=val;
		variables[var]->addr_value()=val;
	}
	sort(evidence_variables.begin(),evidence_variables.end(),less_than_comparator_variable);
	//cerr<<"num-evidence = "<<evidence_variables.size()<<endl;
	vector<int> variable_mapping(variables.size());
	vector<bool> is_evidence(variables.size());
	for(int i=0;i<variables.size();i++)
	{
		is_evidence[i]=false;
	}
	for(int i=0;i<evidence_variables.size();i++)
		is_evidence[evidence_variables[i]->id()]=true;

	int var_count=0;
	for(int i=0;i<variable_mapping.size();i++)
	{
		if(!is_evidence[i])
		{		
			variable_mapping[i]=var_count;
			var_count++;
		}
		else
		{
			variable_mapping[i]=INVALID_VALUE;
		}
	}

	vector<bool> deleted_functions(functions.size());
	//update functions
	for(int i=0;i<functions.size();i++)
	{
		vector<Variable*> func_evid_vars;
		deleted_functions[i]=false;
		do_set_intersection(functions[i]->variables(),evidence_variables,func_evid_vars,less_than_comparator_variable);
		if(func_evid_vars.empty())
			continue;
		// All variables are evidence variables
		if(func_evid_vars.size()==functions[i]->variables().size())
		{
			int address=Variable::getAddress(functions[i]->variables());
			mult_factor*=functions[i]->table()[address];

			deleted_functions[i]=true;
			continue;
		}
		vector<Variable*> other_variables;
		do_set_difference(functions[i]->variables(),func_evid_vars,other_variables,less_than_comparator_variable);
		int other_num_values=Variable::getDomainSize(other_variables);
		vector<Double> new_table(other_num_values);
		for(int j=0;j<other_num_values;j++)
		{
			Variable::setAddress(other_variables,j);
			int entry=Variable::getAddress(functions[i]->variables());
			new_table[j]=functions[i]->table()[entry];
		}

		functions[i]->table()=new_table;
		functions[i]->variables()=other_variables;
	}
	for(int i=0;i<functions.size();i++)
	{
		vector<Variable*> func_evid_vars;
		//deleted_functions[i]=false;
		do_set_intersection(functions[i]->variables(),evidence_variables,func_evid_vars,less_than_comparator_variable);
		if(!func_evid_vars.empty())
		{
			cerr<<"Something wrong in assigning evidence\n";
		}
	}
	cerr<<"Mult-factor = "<<mult_factor<<endl;
	//update functions;
	vector<Function*> new_functions;
	int count=0;
	for(int i=0;i<functions.size();i++)
	{
		if(deleted_functions[i])
		{
			Function* func_pt=functions[i];
			functions[i]=NULL;
			delete(func_pt);
		}
		else
		{
			functions[i]->id()=count++;
			new_functions.push_back(functions[i]);
		}
	}
	functions=new_functions;
	//update variables
	vector<Variable*> new_variables;
	for(int i=0;i<variables.size();i++)
	{
		if(is_evidence[i])
		{
			Variable* var_pt=variables[i];
			variables[i]==NULL;
			delete(var_pt);
		}
		else
		{
			variables[i]->id()=variable_mapping[i];
			new_variables.push_back(variables[i]);
		}
	}
	variables=new_variables;
	//for(int i=0;i<variables.size();i++)
	//	cout<<variables[i]->id()<<" ";
	//cout<<endl;
}

void GM::writeConstraintGraph(char* cgraph)

{
	ofstream outfile(cgraph);

	vector<vector<bool> > adjgraph(this->variables.size());
	for(int i=0;i<variables.size();i++)
	{
		adjgraph[i]=vector<bool> (variables.size());
	}

	int num_edges=0;
	//Now go through each function
	for(int i=0;i<functions.size();i++)
	{
		for(int j=0;j<functions[i]->table().size();j++)
		{
			if(functions[i]->table()[j].isZero())
			{
				for(int k=0;k<functions[i]->variables().size();k++)
				{
					int var1=functions[i]->variables()[k]->id();
					for(int o=k+1;o<functions[i]->variables().size();o++)
					{
						int var2=functions[i]->variables()[o]->id();
						adjgraph[var1][var2]=true;
						adjgraph[var2][var1]=true;
						num_edges++;
					}
				}
				break;
			}
		}
	}

	//Write graph to the file
	outfile<<variables.size()<<" "<<num_edges<<endl;
	for(int i=0;i<variables.size();i++)
	{
		for(int j=i+1;j<variables.size();j++)
		{
			if(adjgraph[i][j])
				outfile<<i+1<<" "<<j+1<<endl;
		}
	}
}

void GM::readUAI08(const char* infilename)
{
	ifstream infile(infilename);
	int num_variables;
	string tmp_string;
	infile>>tmp_string;
	if(tmp_string.compare("BAYES")==0)
	{
		type=BAYES;
	}
	else if(tmp_string.compare("MARKOV")==0)
	{
		type=MARKOV;
	}

	if(type == BAYES)
	{
		cerr<<"Reading bayesnet\n";
		infile>>num_variables;
		// Read domains
		variables=vector<Variable*> (num_variables);
		for(int i=0;i<num_variables;i++)
		{
			int domain_size;
			infile>>domain_size;
			vector<int> domain(domain_size);
			for(int j=0;j<domain_size;j++)
				domain[j]=j;
			variables[i]=new Variable(i,domain);
			variables[i]->orig_id=i;
		}
		copy_of_variables=variables;
		int num_functions;
		infile>>num_functions;
		vector<vector<Variable*> > parents (num_variables);
		vector<int> func_order(num_functions);
		for(int i=0;i<num_functions;i++)
		{
			// Read parents of variables
			int num_parents;
			infile>>num_parents;
			num_parents--;
			vector<Variable*> curr_parents(num_parents);
			for(int j=0;j<num_parents;j++)
			{
				int temp;
				infile>>temp;
				curr_parents[j]=variables[temp];
			}
			int var;
			infile>>var;
			func_order[i]=var;
			parents[var]=curr_parents;
		}
		functions=vector<Function*> (num_functions);
		for(int i=0;i<num_functions;i++)
		{

			int var=func_order[i];
			int num_probabilities;
			infile>>num_probabilities;

			functions[var]=new CPT();
			CPT* cpt=dynamic_cast<CPT*> (functions[var]);
			cpt->id()=var;
			cpt->setMargVariable(variables[var]);
			cpt->cond_variables()=parents[var];
			cpt->variables()=cpt->cond_variables();
			cpt->variables().push_back(cpt->marg_variable());
			sort(cpt->variables().begin(),cpt->variables().end(),less_than_comparator_variable);
			int cond_num_values=Variable::getDomainSize(parents[var]);
			assert(num_probabilities == (cond_num_values*cpt->marg_variable()->domain_size()));
			cpt->table()=vector<Double> (num_probabilities);
			for(int j=0;j<cond_num_values;j++)
			{
				Variable::setAddressVIB(cpt->cond_variables(),j);
				for(int k=0;k<cpt->marg_variable()->domain_size();k++)
				{
					cpt->marg_variable()->addr_value()=k;
					double value;
					infile>>value;

					int entry=Variable::getAddress(cpt->variables());
					if(value > DBL_MIN)
					{		
						cpt->table()[entry]=Double(value);
						addToWCSP(cpt->table()[entry]);
					}
					else
					{
						mode=DET;
					}
				}
			}

			sort(cpt->cond_variables().begin(),cpt->cond_variables().end(),less_than_comparator_variable);

		}

	}
	else if(type==MARKOV)
	{
		cerr<<"Reading Markov network\n";
		infile>>num_variables;
		// Read domains
		variables=vector<Variable*> (num_variables);
		for(int i=0;i<num_variables;i++)
		{
			int domain_size;
			infile>>domain_size;
			vector<int> domain(domain_size);
			for(int j=0;j<domain_size;j++)
				domain[j]=j;
			variables[i]=new Variable(i,domain);
			variables[i]->orig_id=i;
		}
		copy_of_variables=variables;
		int num_functions;
		infile>>num_functions;
		vector<vector<Variable*> > scope (num_functions);
		for(int i=0;i<num_functions;i++)
		{
			// Read parents of variables
			int num_vars_in_func;
			infile>>num_vars_in_func;
			scope[i]=vector<Variable*>(num_vars_in_func);
			for(int j=0;j<num_vars_in_func;j++)
			{
				int temp;
				infile>>temp;
				scope[i][j]=variables[temp];
			}
		}
		functions=vector<Function*> (num_functions);
		for(int i=0;i<num_functions;i++)
		{
			int var=i;
			int num_probabilities;
			infile>>num_probabilities;

			functions[var]=new Function(var,scope[i]);
			Function* cpt=functions[var];


			
			//cout<<num_probabilities<<": "<<variables[var]->id()<<" |";
			//for(int j=0;j<parents[var].size();j++)
			//cout<<parents[var][j]->id()<<",";
			//cout<<endl;

			sort(cpt->variables().begin(),cpt->variables().end(),less_than_comparator_variable);

			//int num_values=Variable::getDomainSize(sorted_func_variables);
			////int marg_num_values=Variable::getNumValues(marg_variables);
			//functions[i]->variables()=sorted_func_variables;
			int num_values=Variable::getDomainSize(scope[var]);
			assert(num_probabilities == num_values);
			cpt->table()=vector<Double> (num_probabilities);
			for(int j=0;j<num_probabilities;j++)
			{
				Variable::setAddressVIB(scope[var],j);
				double value;
				infile>>value;
				int entry=Variable::getAddress(cpt->variables());
				if(value > DBL_MIN)
				{		
					cpt->table()[entry]=Double(value);
					addToWCSP(cpt->table()[entry]);
				}
				else
				{
					mode=DET;
				}
			}
		}
	}
}
void GM::readErgo(char* infilename)
{
	cerr<<"Reading Bayesnet.."<<flush;
	ifstream infile(infilename);
	char check_char,buffer[1000];
	int num_variables;
	// Read num variables
	while(!infile.eof())
	{
		infile>>check_char;
		if(check_char == '/')
		{
			infile.getline(buffer,1000);
			continue;
		}
		infile.putback(check_char);
		infile>>num_variables;
		break;
	}
	// Read domains
	variables=vector<Variable*> (num_variables);
	int var=0;
	while(!infile.eof())
	{
		infile>>check_char;
		if(check_char == '/')
		{
			infile.getline(buffer,1000);
			continue;
		}
		infile.putback(check_char);
		int domain_size;
		infile>>domain_size;
		vector<int> domain(domain_size);
		for(int i=0;i<domain_size;i++)
			domain[i]=i;
		variables[var]=new Variable(var,domain);
		++var;
		if(var==num_variables)
			break;
	}

	// Read parents of variables
	vector<vector<Variable*> > parents (num_variables);
	var=0;
	while(!infile.eof())
	{
		infile>>check_char;
		if(check_char == '/')
		{
			infile.getline(buffer,1000);
			continue;
		}
		infile.putback(check_char);
		int num_parents;
		infile>>num_parents;
		if(num_parents>0)
		{
			parents[var]=vector<Variable*>(num_parents);
		}
		for(int i=0;i<num_parents;i++)
		{
			int var_num;
			infile>>var_num;
			parents[var][i]=variables[var_num];
		}
		++var;
		if(var==num_variables)
			break;
	}

	// Read the probabilities
	var=0;
	functions=vector<Function*> (num_variables);
	cout<<"\tReading cpts"<<flush;
	while(!infile.eof())
	{
		infile>>check_char;
		if(check_char == '/')
		{
			infile.getline(buffer,1000);
			continue;
		}
		infile.putback(check_char);
		int num_probabilities;
		infile>>num_probabilities;

		functions[var]=new CPT();
		CPT* cpt=dynamic_cast<CPT*> (functions[var]);
		cpt->id()=var;


		cpt->setMargVariable(variables[var]);
		//func_variables.push_back(variables[marg_variable]);

		cpt->cond_variables()=parents[var];
		cpt->variables()=cpt->cond_variables();
		cpt->variables().push_back(cpt->marg_variable());
		sort(cpt->variables().begin(),cpt->variables().end(),less_than_comparator_variable);

		//int num_values=Variable::getDomainSize(sorted_func_variables);
		////int marg_num_values=Variable::getNumValues(marg_variables);
		//functions[i]->variables()=sorted_func_variables;
		int cond_num_values=Variable::getDomainSize(parents[var]);
		assert(num_probabilities == (cond_num_values*cpt->marg_variable()->domain_size()));
		cpt->table()=vector<Double> (num_probabilities);
		for(int j=0;j<cond_num_values;j++)
		{
			Variable::setAddressVIB(cpt->cond_variables(),j);
			for(int k=0;k<cpt->marg_variable()->domain_size();k++)
			{
				cpt->marg_variable()->addr_value()=k;
				double value;
				infile>>value;

				int entry=Variable::getAddress(cpt->variables());
				if(value > DBL_MIN)
				{		
					cpt->table()[entry]=Double(value);
				}
			}
		}

		sort(cpt->cond_variables().begin(),cpt->cond_variables().end(),less_than_comparator_variable);
		++var;
		if(var==num_variables)
			break;
	}
	infile.close();
	cerr<<"Done\n";
}
*/

void GM::readMLN(const char* infilename)
{
	type=MARKOV;
	ifstream infile(infilename);
	int num_variables;
	cerr<<"Reading Markov network\n";
	infile>>num_variables;
	// Read domains
	variables=vector<Variable*> (num_variables);
	for(int i=0;i<num_variables;i++)
	{
		int domain_size;
		infile>>domain_size;
		vector<int> domain(domain_size);
		for(int j=0;j<domain_size;j++)
			domain[j]=j;
		variables[i]=new Variable(i,domain);
		variables[i]->orig_id=i;
	}
	copy_of_variables=variables;
	int num_functions;
	infile>>num_functions;
	vector<vector<Variable*> > scope (num_functions);
	functions=vector<Function*> (num_functions);
	for(int i=0;i<num_functions;i++)
	{
		int num_vars_in_func;
		infile>>num_vars_in_func;
		scope[i]=vector<Variable*>(num_vars_in_func);
		for(int j=0;j<num_vars_in_func;j++)
		{
			int curr_var;
			infile>>curr_var;
			int var_id=((curr_var > 0)? (curr_var-1):(-curr_var-1));
			scope[i][j]=variables[var_id];
			if(curr_var>0)
				variables[var_id]->addr_value()=0;
			else
				variables[var_id]->addr_value()=1;
		}
		double clauseW;
		infile>>clauseW;
		Double weightWhenFalse = Double();
		if(clauseW > DBL_MIN)
			weightWhenFalse = Double(clauseW);
		else
			mode=DET;
		//cout << "IsZero(" << whenWhenFalse <<"/"<<clauseW <<") = " <<whenWhenFalse.isZero() << "/" << (clauseW == 0)<<endl;
		sort(scope[i].begin(),scope[i].end(),less_than_comparator_variable);
		int tableFalseEntry = Variable::getAddress(scope[i]);
		functions[i]=new Function(i ,scope[i], weightWhenFalse, tableFalseEntry);
		//for(int j = 0; j<functions[i]->table().size(); j++)
		//	cout <<functions[i]->table()[j] <<"("<<functions[i]->table()[j].isZero()<<") ";
		//cout <<endl;

	}
}

void GM::convertToCN(GM& gm)
{
	gm.variables=this->variables;
	for(int i=0;i<functions.size();i++)
	{
		Function* f = functions[i];
		for(int j=0;j<f->tableSize();j++)
		{
			if(f->tableEntry(j).isZero())
			{
				gm.functions.push_back(f);
				break;
			}
		}
	}
}

/*
// Reduce the Bayesian network by constraint processing
void GM::reduceBN(GM* gm, int i_bound, vector<int>& order)
{
	GM csp;
	convertToCN(csp);
	DRC cp_algo(csp,i_bound,order);

	// Do domain consistency

	for(int i=0;i<functions.size();i++)
	{
		cp_algo.reduceFunction(functions[i]);
	}
}
*/
void GM::reduce(int i_bound)
{

	//First remove zeros from the network
	GM csp;
	cout<<"Converting to csp\n";
	convertToCN(csp);
	vector<int> order;
	Graph graph;
	cout<<"Making graph\n";
	graph.makeGraph(&csp);
	graph.getMinFillOrdering(order);
	JG jg(csp,i_bound,10,order,LS);
	cout<<"Creating join graph\n";
	jg.propagate();
	cout<<"Propagation done\n";
	vector<Function> all_functions(variables.size());
	cout<<"Init domain sizes: ";
	for(int i=0;i<variables.size();i++)
	{
		all_functions[i].variables().push_back(variables[i]);
		all_functions[i].tableInit(variables[i]->domain_size());
		for(int j=0;j<variables[i]->domain_size();j++)
		{
			all_functions[i].tableEntry(j)=Double(1.0);
		}
		cout<<variables[i]->domain_size()<<" ";
	}
	cout<<endl;
	//Reduce domains
	for(int i=0;i<jg.nodes.size();i++)
	{
		//First get Marginal for the variable
		for(int j=0;j<jg.nodes[i]->variables().size();j++)
		{
			vector<Variable*> marg_var;
			marg_var.push_back(jg.nodes[i]->variables()[j]);
			Function function;
			jg.nodes[i]->getMarginal(marg_var,function);
			assert(function.tableSize() == marg_var[0]->domain_size());
			bool all_zeros=true;
			if(all_functions[marg_var[0]->id()].tableSize() == 0)
			{
				all_functions[marg_var[0]->id()]=function;
				for(int k=0;k<function.tableSize();k++)
				{
					if(!all_functions[marg_var[0]->id()].tableEntry(k).isZero())
						all_zeros=false;
				}
			}
			else
			{

				for(int k=0;k<function.tableSize();k++)
				{
					//cout<<function.table()[k]<<"\t"<<all_functions[marg_var[0]->id()].table()[k]<<endl;
					if(function.tableEntry(k).isZero())
					{
						all_functions[marg_var[0]->id()].tableEntry(k)=Double();
					}
					else
					{
						if(!all_functions[marg_var[0]->id()].tableEntry(k).isZero())
							all_zeros=false;
					}
				}

			}
			assert(!all_zeros);
			for(int k=0;k<functions.size();k++)
			{
				vector<Variable*> temp_variables;
				do_set_intersection(jg.nodes[i]->variables(),functions[k]->variables(),temp_variables,less_than_comparator_variable);
				if(temp_variables.empty())
					continue;
				jg.nodes[i]->getMarginal(temp_variables,function);
				functions[k]->project(function);
			}
		}
	}
	cout<<"Adjusting variables\n";
	//return;
	//Adjust the functions and variables
	// Adjust variables
	vector<map<int,int> > map_domains_new2old(variables.size());
	vector<int> new_domain_size(variables.size());
	vector<Variable*> new_variables(variables.size());
	cout<<"Final domain sizes: ";
	for(int i=0;i<all_functions.size();i++)
	{
		new_domain_size[i]=0;
		for(int j=0;j<all_functions[i].tableSize();j++)
		{
			if(!all_functions[i].tableEntry(j).isZero())
			{
				map_domains_new2old[i][new_domain_size[i]]=j;
				new_domain_size[i]++;
			}
		}
		assert(new_domain_size[i]>0);
		cout<<new_domain_size[i]<<" ";
		vector<int> domains(new_domain_size[i]);
		for(int j=0;j<domains.size();j++)
			domains[j]=j;
		new_variables[i]=new Variable(i,domains);

	}
	cout<<endl;
	for(int i=0;i<functions.size();i++)
	{
		vector<Variable*> new_func_variables;
		for(int j=0;j<functions[i]->variables().size();j++)
		{
			int id=functions[i]->variables()[j]->id();
			new_func_variables.push_back(new_variables[id]);
		}
		vector<Double> new_table(Variable::getDomainSize(new_func_variables));
		for(int j=0;j<new_table.size();j++)
		{
			Variable::setAddress(new_func_variables,j);
			for(int k=0;k<new_func_variables.size();k++)
			{
				int id=new_func_variables[k]->id();
				int val=new_func_variables[k]->addr_value();
				functions[i]->variables()[k]->addr_value()=map_domains_new2old[id][val];
			}
			int entry=Variable::getAddress(functions[i]->variables());
			new_table[j]=functions[i]->tableEntry(entry);
		}
		delete(functions[i]);
		functions[i]=new Function(i,new_func_variables);
		functions[i]->tableSet(new_table);
	}
	for(int i=0;i<variables.size();i++)
		delete(variables[i]);
	variables=new_variables;
}

/*void GM::writeErgo(char* outfile)
{
	ofstream out(outfile);
	assert(functions.size()==variables.size());
	out<<variables.size()<<endl;
	//Write domains
	for(int i=0;i<variables.size();i++)
		out<<variables[i]->domain_size()<<" ";
	out<<endl;
	//Print parents
	for(int i=0;i<functions.size();i++)
	{
		out<<functions[i]->variables().size()-1<<" ";
		bool found=false;
		for(int j=0;j<functions[i]->variables().size();j++)
		{
			if(functions[i]->variables()[j]->id()==i)
			{
				found=true;
			}
			else
			{
				out<<functions[i]->variables()[j]->id()<<" ";
			}
		}
		assert(found==true);
		out<<endl;
	}
	//Write functions
	for(int i=0;i<functions.size();i++)
	{
		out<<functions[i]->table().size()<<endl;
		vector<Variable*> other_variables;
		Variable* curr_variable=variables[i];
		assert(curr_variable->id()==i);
		vector<Variable*> temp_variables;
		temp_variables.push_back(variables[i]);
		do_set_difference(functions[i]->variables(),temp_variables,other_variables,less_than_comparator_variable);

		for(int k=0;k<Variable::getDomainSize(other_variables);k++)
		{
			Variable::setAddressVIB(other_variables,k);
			for(int j=0;j<variables[i]->domain_size();j++)
			{
				variables[i]->addr_value()=j;
				int entry=Variable::getAddress(functions[i]->variables());
				out<<functions[i]->table()[entry]<<" ";
			}
			out<<endl;
		}
		out<<endl;
	}
	out.close();
}

void GM::eliminate(int i_bound)
{
	vector<set<Function*,less_than_comparator_function> > buckets(variables.size());
	vector<vector<Variable*> > bucket_vars(variables.size());
	vector<bool> deleted_funcs(functions.size());
	vector<set<int> > where_var_present(variables.size());
	vector<bool> deleted_vars(variables.size());
	// Initialize buckets
	int max_id=functions.size();
	for(int i=0;i<functions.size();i++)
	{
		deleted_funcs[i]=false;
		functions[i]->id()=i;
		for(int j=0;j<functions[i]->variables().size();j++)
		{
			buckets[functions[i]->variables()[j]->id()].insert(functions[i]);
			do_set_union(functions[i]->variables(),bucket_vars[functions[i]->variables()[j]->id()],bucket_vars[functions[i]->variables()[j]->id()],less_than_comparator_variable);
			for(int k=0;k<functions[i]->variables().size();k++)
			{
				if(j==k)
					continue;
				where_var_present[functions[i]->variables()[j]->id()].insert(functions[i]->variables()[k]->id());
			}
		}
	}
	cout<<"Eliminate:Added functions\n";
	// Eliminate the variable with the minimum degree
	while(true)
	{
		int min=variables.size();
		vector<int> min_vars;
		//Find the variable with minimum degree
		for(int i=0;i<bucket_vars.size();i++)
		{
			if(bucket_vars[i].empty())
				continue;
			if((int) bucket_vars[i].size() < min)
			{
				min=bucket_vars[i].size();
				min_vars.clear();
				min_vars.push_back(i);
			}
			else if((int) bucket_vars[i].size()==min)
			{
				min_vars.push_back(i);
			}
		}
		if(min > i_bound || min==(int)variables.size())
			break;
		if(min_vars.empty())
			break;
		int select=min_vars[rand()%min_vars.size()];
		cout<<"Selected var = "<<select<<endl;

		vector<Variable*> marg_vars,other_vars;
		marg_vars.push_back(variables[select]);
		do_set_difference(bucket_vars[select],marg_vars,other_vars,less_than_comparator_variable);
		if(other_vars.empty())
		{
			bucket_vars[select].clear();
			buckets[select].clear();
			continue;
		}

		deleted_vars[select]=true;
		//Multiply all functions to form a new function

		Function* new_func=new Function();

		functions.push_back(new_func);
		deleted_funcs.push_back(false);

		vector<Function*> all_functions;
		for(set<Function*,less_than_comparator_function>::iterator i=buckets[select].begin();i!=buckets[select].end();i++)
		{
			all_functions.push_back(*i);
		}
		Function::dummy_multiplyMarginalize(other_vars,all_functions,*new_func);
		new_func->id()=max_id++;
		cout<<"\tElim:New func created\n";
		//Update buckets, bucket_vars,deleted_funcs,where_var_present
		for(set<Function*,less_than_comparator_function>::iterator i=buckets[select].begin();i!=buckets[select].end();i++)
		{
			assert((*i)!=NULL);
			assert((*i)->id() < deleted_funcs.size());
			deleted_funcs[(*i)->id()]=true;
			cout<<"num vars= "<<(*i)->variables().size()<<endl;
			for(int j=0;j<(*i)->variables().size();j++)
			{
				if((*i)->variables()[j]->id()==select)
					continue;
				cout<<"Erasing "<<(*i)->variables()[j]->id()<<"\n";
				buckets[(*i)->variables()[j]->id()].erase(*i);
				cout<<"Erased\n";
			}
		}
		cout<<"\tElim: Updated bucks\n";
		for(int i=0;i<bucket_vars[select].size();i++)
		{
			int id=bucket_vars[select][i]->id();
			do_set_difference(bucket_vars[id],marg_vars,bucket_vars[id],less_than_comparator_variable);
		}

		//Put the new function in the appropriate bucket
		//functions.push_back(new_func);

		for(int i=0;i<new_func->variables().size();i++)
		{
			buckets[new_func->variables()[i]->id()].insert(new_func);
			do_set_union(bucket_vars[new_func->variables()[i]->id()],new_func->variables(),bucket_vars[new_func->variables()[i]->id()],less_than_comparator_variable);
		}
		bucket_vars[select].clear();
		buckets[select].clear();
	}

	// Update Variables and functions
	vector<Function*> new_functions;
	int id=0;
	for(int i=0;i<functions.size();i++)
	{
		if(!deleted_funcs[i])
		{
			new_functions.push_back(functions[i]);
			functions[i]->id()=id++;
		}
		else
		{
			delete(functions[i]);
		}
	}
	functions=new_functions;

	vector<Variable*> new_variables;
	id=0;
	for(int i=0;i<variables.size();i++)
	{
		if(!deleted_vars[i])
		{
			new_variables.push_back(variables[i]);
			variables[i]->id()=id++;
		}
		else
		{
			delete(variables[i]);
		}
	}
	variables=new_variables;
}
*/
//int main(int argc, char* argv[])
//{
//	GM gm;
//	gm.readCNF(argv[1]);
//	//gm.print();
//	vector<int> order;
//	for(int i=0;i<gm.variables.size();i++)
//		order.push_back(i);
//	JG jg(gm,1,50,order);
//	cerr<<"Propagating..\n";
//	jg.propagate();
//	cerr<<"Done\n";
//	cerr<<"Printing marginals\n";
//	for(int i=0;i<gm.variables.size();i++)
//	{
//		vector<Variable*> curr_variable;
//		curr_variable.push_back(gm.variables[i]);
//		vector<Double> marg_table;
//		for(int j=0;j<jg.nodes.size();j++)
//		{
//			if(do_set_inclusion(curr_variable,jg.nodes[j]->variables(),less_than_comparator_variable))
//			{
//				jg.nodes[j]->getMarginal(curr_variable,marg_table);
//				break;
//			}
//		}
//		cout<<"Marginal of "<<i<<" is ";
//		for(int i=0;i<marg_table.size();i++)
//		{
//			cout<<marg_table[i]<<" ";
//		}
//		cout<<endl;
//	}
//}

void GM::getIrrelevantNodes(vector<int>& evidence, vector<int>& irrelevant_nodes)
{
}
void GM::removeIrrelevantNetwork(vector<int>& evidence)
{

	vector<bool> processed(variables.size());
	set<int> unprocessed_nodes;
	//set<int> all_unprocessed_nodes;
	set<int> relevant_nodes;
	if(type==BAYES)
	{
		Graph graph;
		graph.makeDirectedGraph(this);

		for(int i=0;i<evidence.size();i++)
		{
			unprocessed_nodes.insert(evidence[i]);
			//all_unprocessed_nodes.insert(evidence[i]);
			relevant_nodes.insert(evidence[i]);
		}
		while(!unprocessed_nodes.empty())
		{
			int v=*(unprocessed_nodes.begin());

			unprocessed_nodes.erase(v);
			for(list<int>::const_iterator i=graph.getParentList()[v].begin();i!=graph.getParentList()[v].end();i++)
			{
				if(!processed[*i] ){
					relevant_nodes.insert(*i);
					unprocessed_nodes.insert(*i);
				}
			}
			processed[v]=true;
			//cout<<unprocessed_nodes.size()<<" "<<flush;

		}
		cerr<<"Number of Relevant nodes = "<<relevant_nodes.size()<<" out of "<<variables.size()<<endl;
	}
	else
	{
		for(int i=0;i<processed.size();i++)
		{
			processed[i]=true;
			relevant_nodes.insert(i);
		}
	}
	mult_factor=Double(1.0);
	vector<Function*> new_functions;
	for(int i=0;i<functions.size();i++)
	{
		bool deleted=false;
		for(int j=0;j<functions[i]->variables().size();j++)
		{
			if(!processed[functions[i]->variables()[j]->id()])
			{
				deleted=true;
				break;
			}
		}
		if(deleted)
		{
			delete(functions[i]);
			continue;
		}
		functions[i]->removeEvidence();
		if(functions[i]->variables().empty())
		{
			mult_factor*=functions[i]->tableEntry(0);
			delete(functions[i]);
			continue;
		}
		new_functions.push_back(functions[i]);
	}
	functions=new_functions;
	vector<Variable*> new_variables;
	int count=0;
	for(int i=0;i<variables.size();i++)
	{
		if(processed[i] && variables[i]->value()==INVALID_VALUE)
		{
			new_variables.push_back(variables[i]);
			variables[i]->id()=count;
			count++;
		}
		else
		{
			variables[i]->id()=INVALID_VALUE;
		}
	}
	variables=new_variables;
}

void GM::setEvidenceBeliefsUAI08(vector<int>& evidence)
{
	mult_factor=Double(1.0);
	vector<Function*> new_functions;
	for(int i=0;i<functions.size();i++)
	{
		functions[i]->removeEvidence();
		if(functions[i]->variables().empty())
		{
			mult_factor*=functions[i]->tableEntry(0);
			delete(functions[i]);
			continue;
		}
		new_functions.push_back(functions[i]);
	}
	functions=new_functions;
	vector<Variable*> new_variables;
	int count=0;
	for(int i=0;i<variables.size();i++)
	{
		if(variables[i]->value()==INVALID_VALUE)
		{
			new_variables.push_back(variables[i]);
			variables[i]->id()=count;
			count++;
		}
		else{
			vector<bool> new_domains(variables[i]->domain_size());
			for(int j=0;j<variables[i]->domain_size();j++){
				if (variables[i]->value()==j){
					new_domains[j]=true;
				}
				else{
					new_domains[j]=false;
				}
			}
			variables[i]->updateDomain(new_domains);
			variables[i]->value()=0;
			variables[i]->id()=INVALID_VALUE;
		}
	}
	variables=new_variables;
}

void GM::reduceDomains()
{
	RBSampleSearch rss(cout);
	vector<vector<bool> > new_domains;
	rss.reduce(*this,new_domains);

	//Update variables
	for(int i=0;i<variables.size();i++)
		variables[i]->updateDomain(new_domains[i]);

	// Update functions
	for(int i=0;i<functions.size();i++)
		functions[i]->reduceDomains();

	// Remove variables which have just one value
	for(int i=0;i<variables.size();i++){
		if((int)variables[i]->domain_size()==(int)1)
		{
			variables[i]->value()=0;
		}
	}
	vector<Function*> new_functions;
	for(int i=0;i<functions.size();i++)
	{
		/*bool deleted=false;
		for(int j=0;j<functions[i]->variables().size();j++)
		{
		if(!processed[functions[i]->variables()[j]->id()])
		{
		deleted=true;
		break;
		}
		}
		if(deleted)
		{
		delete(functions[i]);
		continue;
		}*/
		//cout<<"Func "<<i<<" Table size = "<<functions[i]->table().size()<<endl;
		if(functions[i]->tableSize() == 0 || functions[i]->variables().empty())
		{
			delete(functions[i]);
			continue;
		}
		//cout<<"Removing function\n";
		functions[i]->removeEvidence();
		//cout<<"Evidence removed\n";
		if(functions[i]->variables().empty())
		{
			if(!functions[i]->tableSize() == 0)
			{
				mult_factor*=functions[i]->tableEntry(0);
			}
			delete(functions[i]);
			continue;
		}
		new_functions.push_back(functions[i]);
	}
	//cout<<"Functions inited\n";
	functions=new_functions;
	vector<Variable*> new_variables;
	int count=0;
	for(int i=0;i<variables.size();i++)
	{
		if(variables[i]->value()==INVALID_VALUE)
		{
			new_variables.push_back(variables[i]);
			variables[i]->id()=count;
			count++;
		}
		else{
			variables[i]->id()=INVALID_VALUE;
		}
	}
	variables=new_variables;
	cerr<<"Domains reduced\n";
}
/*void GM::writeSAT(char* satfilename)
{
	ofstream out(satfilename);
	out<<"p cnf "<<variables.size()<<" "<<clauses.size()<<endl;
	for(int i=0;i<clauses.size();i++){
		for(int j=0;j<clauses[i].size();j++){
			int Var=var(clauses[i][j]);
			Lit p=Lit(Var);
			int val=(clauses[i][j] == Lit(Var))?(Var+1):(-(Var+1));
			out<<val<<" ";
		}
		out<<"0\n";
	}
}
void GM::reduceSAT(const char* satfilename)
{
	RBSampleSearchSAT rss;
	vector<vector<bool> > new_domains;
	rss.reduce(satfilename,*this,new_domains);

	//Hack: Delete all functions
	for(int i=0;i<functions.size();i++){
		delete(functions[i]);
	}
	functions.clear();
	
	
	// Update variables
	copy_of_variables=variables;
	vector<Variable*> new_variables;
	for(int i=0;i<variables.size();i++){
		variables[i]->value()=INVALID_VALUE;
		if(!(new_domains[i][0] && new_domains[i][1])){
			int val=(new_domains[i][0])?(0):(1);
			variables[i]->value()=val;
		}
		else{
			new_variables.push_back(variables[i]);
		}
		variables[i]->orig_id=i;
		//variables[i]->id();
	}
	//Update the ids of the variables
	for(int i=0;i<new_variables.size();i++){
		new_variables[i]->id()=i;
	}
	

	// Update Clauses
	vector<vector<Lit> > new_clauses;
	for(int i=0;i<clauses.size();i++){
		// If clause is satisfied delete it
		vector<Lit> new_clause;
		bool is_sat=false;
		for(int j=0;j<clauses[i].size();j++){
			int Var=var(clauses[i][j]);
			if (variables[Var]->value()==INVALID_VALUE){
				Lit q=Lit(Var);
				Lit p = (clauses[i][j]==q)? (Lit(variables[Var]->id())):(~Lit(variables[Var]->id()));
				new_clause.push_back(p);
				continue;
			}
			Lit p = (variables[Var]->value()==0)?(~Lit(Var)):(Lit(Var));
			if (p==clauses[i][j]){
				//Clause is satisfied
				is_sat=true;
				break;
			}
		}
		if (is_sat)
			continue;
		assert(!new_clause.empty());
		new_clauses.push_back(new_clause);
	}
	
	variables=new_variables;
	clauses=new_clauses;
	// construct new functions
	for(int i=0;i<clauses.size();i++){
		Function* func= new Function(i);
		if (clauses[i].size() > 15)
			continue;
		for(int j=0;j<clauses[i].size();j++){
			func->variables().push_back(variables[var(clauses[i][j])]);
			Lit q= Lit(var(clauses[i][j]));
			if(clauses[i][j]==q){
				variables[var(clauses[i][j])]->addr_value()=0;
			}
			else
			{
				variables[var(clauses[i][j])]->addr_value()=1;
			}
		}
		sort(func->variables().begin(),func->variables().end(),less_than_comparator_variable);
		int table_size=Variable::getDomainSize(func->variables());
		func->table()=vector<Double> (table_size);
		for(int i=0;i<table_size;i++)
		{
			func->table()[i]=Double(1.0);
		}
		if (damp > 0.0){
			func->table()[Variable::getAddress(func->variables())]=Double(damp);
		} else {
			func->table()[Variable::getAddress(func->variables())]=Double();
		}
		functions.push_back(func);
	}
}
*/
void GM::getMinFillOrdering(vector<int>& order, vector<set<int> >& clusters, double& estimate)
{
	estimate=0.0;
	order=vector<int>(variables.size());
	clusters=vector<set<int> > (variables.size());
	vector<vector<bool> > adj_matrix(variables.size());
	for(int i=0;i<variables.size();i++)
	{
		adj_matrix[i]=vector<bool> (variables.size());
	}
	vector<set<int> > graph(variables.size());
	vector<bool> processed(variables.size());
	for(int i=0;i<functions.size();i++)
	{
		for(int j=0;j<functions[i]->variables().size();j++)
		{
			for(int k=j+1;k<functions[i]->variables().size();k++)
			{
				int a=functions[i]->variables()[j]->id();
				int b=functions[i]->variables()[k]->id();
				graph[a].insert(b);
				graph[b].insert(a);
				adj_matrix[a][b]=true;
				adj_matrix[b][a]=true;
			}
		}
	}
	list<int> zero_list;
	//cerr<<"minfill: Inited\n";
	for(int i=0;i<variables.size();i++){
		// Find variable with the minimum number of edges added
		//cerr<<"Processing variable "<<i<<" out of "<<variables.size()<<endl;
		double min=DBL_MAX;
		int min_id=-1;
		bool first=true;
		if (zero_list.empty()){
			for(int j=0;j<variables.size();j++){
				if(processed[j])
					continue;
				double curr_min=0.0;
				for(set<int>::iterator a=graph[j].begin();a!=graph[j].end();a++)
				{
					set<int>::iterator b=a;
					b++;
					for(;b!=graph[j].end();b++)
					{
						if(!adj_matrix[*a][*b])
						{
							curr_min+=(variables[*a]->domain_size()*variables[*b]->domain_size());
							if (curr_min > min)
								break;
						}
					}
					if (curr_min > min)
						break;
				}
				if(first)
				{
					min_id=j;
					min=curr_min;
					first=false;
				}
				else
				{
					if(min > curr_min)
					{
						min=curr_min;
						min_id=j;
					}
					else if (curr_min< DBL_MIN)
					{
						zero_list.push_back(j);
					}
				}
			}
		}
		else
		{
			min_id=zero_list.front();
			zero_list.pop_front();
		}

		//cout<<"order["<<i<<"]= "<<min_id<<" "<<flush;
		assert(min_id!=-1);
		order[i]=min_id;
		// Now form the cluster 
		clusters[i]=graph[min_id];
		clusters[i].insert(min_id);

		// Trinagulate min id and remove it from the graph
		for(set<int>::iterator a=graph[min_id].begin();a!=graph[min_id].end();a++)
		{
			set<int>::iterator b=a;
			b++;
			for(;b!=graph[min_id].end();b++)
			{
				if(!adj_matrix[*a][*b])
				{
					adj_matrix[*a][*b]=true;
					adj_matrix[*b][*a]=true;
					graph[*a].insert(*b);
					graph[*b].insert(*a);
				}
			}
		}
		for(set<int>::iterator a=graph[min_id].begin();a!=graph[min_id].end();a++)
		{
			graph[*a].erase(min_id);
			adj_matrix[*a][min_id]=false;
			adj_matrix[min_id][*a]=false;
		}
		graph[min_id].clear();
		processed[min_id]=true;
	}

	// compute the estimate
	int max_cluster_size=0;
	for(int i=0;i<clusters.size();i++)
	{
		if((int)clusters[i].size() > max_cluster_size)
			max_cluster_size=(int)clusters[i].size();
		double curr_estimate=1.0;
		for(set<int>::iterator j=clusters[i].begin();j!=clusters[i].end();j++)
		{
			curr_estimate*=(double)variables[*j]->domain_size();
		}
		estimate+=curr_estimate;
	}
	cerr<<"Max cluster size =" <<max_cluster_size<<endl;
	cerr<<"Estimate  = "<<estimate<<endl;
}

void GM::getMinDegreeOrdering(vector<int>& order, vector<set<int> >& clusters, double& estimate)
{
	estimate=0.0;
	order=vector<int>(variables.size());
	clusters=vector<set<int> > (variables.size());
	vector<vector<bool> > adj_matrix(variables.size());
	for(int i=0;i<variables.size();i++)
	{
		adj_matrix[i]=vector<bool> (variables.size());
	}
	vector<set<int> > graph(variables.size());
	vector<bool> processed(variables.size());
	vector<set<int> > minimums(variables.size());
	vector<int> min_pos(variables.size());
	for(int i=0;i<functions.size();i++)
	{
		for(int j=0;j<functions[i]->variables().size();j++)
		{
			for(int k=j+1;k<functions[i]->variables().size();k++)
			{
				int a=functions[i]->variables()[j]->id();
				int b=functions[i]->variables()[k]->id();
				graph[a].insert(b);
				graph[b].insert(a);
				adj_matrix[a][b]=true;
				adj_matrix[b][a]=true;
			}
		}
	}

	// Initialize minimums
	for(int i=0;i<variables.size();i++){
		minimums[graph[i].size()].insert(i);
		min_pos[i]=graph[i].size();
	}
	cout<<"mindegree inited\n";
	//cerr<<"minfill: Inited\n";
	for(int i=0;i<variables.size();i++)
	{

		// Find variable with the minimum degree
		double min;
		int min_id=-1;
		bool first=true;
		for(int j=0;j<variables.size();j++)
		{
			if (minimums[j].empty()){
				continue;
			}

			min_id=*(minimums[j].begin());
			if (processed[min_id]){
				cerr<<"Something wrong\n";
			}

			minimums[j].erase(min_id);
			min_pos[min_id]=INVALID_VALUE;
			break;
		}
		//cout<<"order["<<i<<"]= "<<min_id<<" "<<flush;
		assert(min_id!=-1);
		order[i]=min_id;

		// Now form the cluster 
		clusters[i]=graph[min_id];
		clusters[i].insert(min_id);

		// Becase min_id will be removed from the graph, the degrees of its neighbors will be reduced by 1
		// therefore update minimums
		for(set<int>::iterator a=graph[min_id].begin();a!=graph[min_id].end();a++)
		{
			assert(!processed[*a]);
			minimums[min_pos[*a]].erase(*a);
			min_pos[*a]--;
			minimums[min_pos[*a]].insert(*a);
		}
		// Trinagulate min id and remove it from the graph
		for(set<int>::iterator a=graph[min_id].begin();a!=graph[min_id].end();a++)
		{
			set<int>::iterator b=a;
			b++;
			for(;b!=graph[min_id].end();b++)
			{
				if(!adj_matrix[*a][*b])
				{
					adj_matrix[*a][*b]=true;
					adj_matrix[*b][*a]=true;
					graph[*a].insert(*b);
					graph[*b].insert(*a);

					// update minimums for *a and *b
					minimums[min_pos[*a]++].erase(*a);
					minimums[min_pos[*a]].insert(*a);
					minimums[min_pos[*b]++].erase(*b);
					minimums[min_pos[*b]].insert(*b);
				}
			}
		}
		for(set<int>::iterator a=graph[min_id].begin();a!=graph[min_id].end();a++)
		{
			graph[*a].erase(min_id);
			adj_matrix[*a][min_id]=false;
			adj_matrix[min_id][*a]=false;
		}
		graph[min_id].clear();
		processed[min_id]=true;
	}

	// compute the estimate
	int max_cluster_size=0;
	for(int i=0;i<clusters.size();i++)
	{
		if((int)clusters[i].size() > max_cluster_size)
			max_cluster_size=(int)clusters[i].size();
		double curr_estimate=1.0;
		for(set<int>::iterator j=clusters[i].begin();j!=clusters[i].end();j++)
		{
			curr_estimate*=(double)variables[*j]->domain_size();
		}
		estimate+=curr_estimate;
	}
	cerr<<"Max cluster size =" <<max_cluster_size<<endl;
	cerr<<"Estimate  = "<<estimate<<endl;
}
// Because we lose track of topological ordering once we reduce the network, we store it somewhere else
// in suggestion
void GM::getTopologicalOrdering(vector<int>& order, vector<set<int> >& clusters, double& estimate,vector<int>& suggestion)
{
	estimate=0.0;
	order=vector<int>(variables.size());
	clusters=vector<set<int> > (variables.size());
	vector<vector<bool> > adj_matrix(variables.size());
	int count=order.size()-1;
	//int count=0;
	for(int i=0;i<suggestion.size();i++){
		int var=suggestion[i];
		if (copy_of_variables[var]->id()==INVALID_VALUE){
			continue;
		}
		order[count--]=copy_of_variables[var]->id();
		//order[count++]=copy_of_variables[var]->id();
	}
	for(int i=0;i<variables.size();i++)
	{
		adj_matrix[i]=vector<bool> (variables.size());
	}
	vector<set<int> > graph(variables.size());

	for(int i=0;i<functions.size();i++)
	{
		for(int j=0;j<functions[i]->variables().size();j++)
		{
			for(int k=j+1;k<functions[i]->variables().size();k++)
			{
				int a=functions[i]->variables()[j]->id();
				int b=functions[i]->variables()[k]->id();
				graph[a].insert(b);
				graph[b].insert(a);
				adj_matrix[a][b]=true;
				adj_matrix[b][a]=true;
			}
		}
	}
	cout<<"topo inited\n";
	//cerr<<"minfill: Inited\n";
	for(int i=0;i<variables.size();i++)
	{
		int min_id=order[i];
		// Now form the cluster 
		clusters[i]=graph[min_id];
		clusters[i].insert(min_id);

		// Trinagulate min id and remove it from the graph
		for(set<int>::iterator a=graph[min_id].begin();a!=graph[min_id].end();a++)
		{
			set<int>::iterator b=a;
			b++;
			for(;b!=graph[min_id].end();b++)
			{
				if(!adj_matrix[*a][*b])
				{
					adj_matrix[*a][*b]=true;
					adj_matrix[*b][*a]=true;
					graph[*a].insert(*b);
					graph[*b].insert(*a);					
				}
			}
		}
		for(set<int>::iterator a=graph[min_id].begin();a!=graph[min_id].end();a++)
		{
			graph[*a].erase(min_id);
			adj_matrix[*a][min_id]=false;
			adj_matrix[min_id][*a]=false;
		}
		graph[min_id].clear();
	}
	cout<<"topo done\n";
	// compute the estimate
	int max_cluster_size=0;
	for(int i=0;i<clusters.size();i++)
	{
		if((int)clusters[i].size() > max_cluster_size)
			max_cluster_size=(int)clusters[i].size();
		double curr_estimate=1.0;
		for(set<int>::iterator j=clusters[i].begin();j!=clusters[i].end();j++)
		{
			curr_estimate*=(double)variables[*j]->domain_size();
		}
		estimate+=curr_estimate;
	}
	cerr<<"Max cluster size =" <<max_cluster_size<<endl;
	cerr<<"Estimate  = "<<estimate<<endl;

}
void GM::getLexOrdering(vector<int>& order, vector<set<int> >& clusters, double& estimate)
{
	estimate=0.0;
	order=vector<int>(variables.size());
	clusters=vector<set<int> > (variables.size());
	vector<vector<bool> > adj_matrix(variables.size());
	for(int i=0;i<variables.size();i++)
	{
		adj_matrix[i]=vector<bool> (variables.size());
	}
	vector<set<int> > graph(variables.size());

	for(int i=0;i<functions.size();i++)
	{
		for(int j=0;j<functions[i]->variables().size();j++)
		{
			for(int k=j+1;k<functions[i]->variables().size();k++)
			{
				int a=functions[i]->variables()[j]->id();
				int b=functions[i]->variables()[k]->id();
				graph[a].insert(b);
				graph[b].insert(a);
				adj_matrix[a][b]=true;
				adj_matrix[b][a]=true;
			}
		}
	}
	cout<<"lex inited\n";
	//cerr<<"minfill: Inited\n";
	for(int i=0;i<variables.size();i++)
	{
		int min_id=i;
		order[i]=min_id;

		// Now form the cluster 
		clusters[i]=graph[min_id];
		clusters[i].insert(min_id);

		// Trinagulate min id and remove it from the graph
		for(set<int>::iterator a=graph[min_id].begin();a!=graph[min_id].end();a++)
		{
			set<int>::iterator b=a;
			b++;
			for(;b!=graph[min_id].end();b++)
			{
				if(!adj_matrix[*a][*b])
				{
					adj_matrix[*a][*b]=true;
					adj_matrix[*b][*a]=true;
					graph[*a].insert(*b);
					graph[*b].insert(*a);					
				}
			}
		}
		for(set<int>::iterator a=graph[min_id].begin();a!=graph[min_id].end();a++)
		{
			graph[*a].erase(min_id);
			adj_matrix[*a][min_id]=false;
			adj_matrix[min_id][*a]=false;
		}
		graph[min_id].clear();
	}
	cout<<"Lex done\n";
	// compute the estimate
	int max_cluster_size=0;
	for(int i=0;i<clusters.size();i++)
	{
		if((int)clusters[i].size() > max_cluster_size)
			max_cluster_size=(int)clusters[i].size();
		double curr_estimate=1.0;
		for(set<int>::iterator j=clusters[i].begin();j!=clusters[i].end();j++)
		{
			curr_estimate*=(double)variables[*j]->domain_size();
		}
		estimate+=curr_estimate;
	}
	cerr<<"Max cluster size =" <<max_cluster_size<<endl;
	cerr<<"Estimate  = "<<estimate<<endl;
}
void GM::getClusters(vector<int>&order, vector<set<int> >&clusters)
{
	//order=vector<int>(variables.size());
	assert(order.size()==variables.size());
	clusters=vector<set<int> > (variables.size());
	bool adj_matrix[variables.size()][variables.size()];
	vector<set<int> > graph(variables.size());
	vector<bool> processed(variables.size());
	for(int i=0;i<functions.size();i++)
	{
		for(int j=0;j<this->functions[i]->variables().size();j++)
			for(int k=0;k<this->functions[i]->variables().size();k++)
			{
				if(j==k)
					continue;
				int a=functions[i]->variables()[j]->id();
				int b=functions[i]->variables()[k]->id();
				graph[a].insert(b);
				graph[b].insert(a);
				adj_matrix[a][b]=true;
				adj_matrix[b][a]=true;
			}
	}

	//cout<<"minfill: Inited\n";
	int treewidth=0;
	for(int i=0;i<variables.size();i++)
	{
		int min_id=order[i];
		// Now form the cluster 
		clusters[i]=graph[min_id];
		clusters[i].insert(min_id);
		if(treewidth < (int)clusters[i].size())
			treewidth=(int)clusters[i].size();
		// Trinagulate min id and remove it from the graph
		for(set<int>::iterator a=graph[min_id].begin();a!=graph[min_id].end();a++)
		{
			set<int>::iterator b=a;
			b++;
			for(;b!=graph[min_id].end();b++)
			{
				if(!adj_matrix[*a][*b])
				{
					adj_matrix[*a][*b]=true;
					adj_matrix[*b][*a]=true;
					graph[*a].insert(*b);
					graph[*b].insert(*a);
				}
			}	
		}
		// Redundant
		for(set<int>::iterator a=graph[min_id].begin();a!=graph[min_id].end();a++)
		{
			set<int>::iterator b=a;
			b++;
			for(;b!=graph[min_id].end();b++)
			{
				adj_matrix[*a][*b]=true;
				adj_matrix[*b][*a]=true;
				graph[*a].insert(*b);
				graph[*b].insert(*a);
			}
		}
		for(set<int>::iterator a=graph[min_id].begin();a!=graph[min_id].end();a++)
		{
			graph[*a].erase(min_id);
			adj_matrix[*a][min_id]=false;
			adj_matrix[min_id][*a]=false;
		}
		//cout<<"s"<<endl;
		graph[min_id].clear();
		processed[min_id]=true;
	}
	cerr<<"Treewidth is = "<<treewidth-1<<endl;
}

void GM::rearrangeOrdering(std::vector<int> &order, std::vector<set<int> > &clusters, std::vector<int> &new_order, double& log_limit)
{
	new_order=vector<int>();

	vector<vector<int> > var2clusters (variables.size());
	//vector<set<int> > temp_clusters(clusters);
	vector<int> var2estimate(variables.size());
	vector<double> cluster2estimate(clusters.size());
	for(int i=0;i<var2estimate.size();i++)
		var2estimate[i]=0;
	//double log_limit=log(limit);
	set<int> faulting_clusters;
	vector<bool> processed(variables.size());
	for(int i=0;i<clusters.size();i++)
	{
		processed[i]=false;
		double curr_estimate=0.0;
		for(set<int>::iterator j=clusters[i].begin();j!=clusters[i].end();j++)
		{
			var2clusters[*j].push_back(i);
			curr_estimate+=log((double)variables[*j]->domain_size());
		}
		cluster2estimate[i]=curr_estimate;

		if(curr_estimate > log_limit)
		{
			faulting_clusters.insert(i);
			for(set<int>::iterator j=clusters[i].begin();j!=clusters[i].end();j++)
			{
				var2estimate[*j]++;
			}
		}
	}

	int count=0;
	while(!faulting_clusters.empty())
	{
		if(new_order.size()==order.size())
			break;
		double min;
		int min_id=-1;
		bool first=true;
		//cout<<"Removing variable "<<count++<<endl;
		for(int i=0;i<variables.size();i++)
		{
			if(processed[i])
				continue;
			if(first)
			{
				first=false;
				min=(double)variables[i]->domain_size()/(double)var2estimate[i];
				min_id=i;
			}
			else
			{
				if(min > (double)variables[i]->domain_size()/(double)var2estimate[i])
				{
					min=(double)variables[i]->domain_size()/(double)var2estimate[i];
					min_id=i;
				}
			}
		}
		assert(min_id!=-1);
		processed[min_id]=true;
		new_order.push_back(min_id);

		double div=log((double)variables[min_id]->domain_size());
		//Update cluster2estimate
		for(int i=0;i<var2clusters[min_id].size();i++)
		{
			int cluster_id=var2clusters[min_id][i];
			if(faulting_clusters.find(cluster_id)!=faulting_clusters.end())
			{
				cluster2estimate[cluster_id]-=div;
				if(cluster2estimate[cluster_id]<log_limit)
				{
					faulting_clusters.erase(cluster_id);
					for(set<int>::iterator j=clusters[cluster_id].begin();j!=clusters[cluster_id].end();j++)
					{
						int var_id=*j;
						if(!processed[var_id])
						{
							assert(var2estimate[var_id]>0);
							var2estimate[var_id]--;
						}
					}
				}
			}
		}
	}
}
/*void GM::printMarginalsUAI08(std::vector<Function> &marginals_)
{
	vector<vector<Double> > marginals(marginals_.size());
	for(int i=0;i<marginals_.size();i++)
	{
		marginals[i]=vector<Double>(marginals_[i].table().size());
		Double norm_const;
		for(int j=0;j<marginals_[i].table().size();j++)
		{
			norm_const+=marginals_[i].table()[j];
			marginals[i][j]=marginals_[i].table()[j];
		}
		for(int j=0;j<marginals[i].size();j++)
			marginals[i][j]/=norm_const;
	}
	cout.setf(ios::fixed,ios::floatfield);
	cout<<"m "<<copy_of_variables.size()<<" ";
	for(int i=0;i<copy_of_variables.size();i++)
	{
		cout<<copy_of_variables[i]->old_domain.size()<<" ";
		if(copy_of_variables[i]->value()==INVALID_VALUE)
		{
			int curr_id=copy_of_variables[i]->id();
			vector<Double> var_marginals(copy_of_variables[i]->old_domain.size());
			int old_num=0;
			for(int j=0;j<marginals[curr_id].size();j++)
			{
				int new2old=copy_of_variables[i]->mapping[j];
				var_marginals[new2old]=marginals[curr_id][j];
			}
			for(int j=0;j<var_marginals.size();j++){
				cout<<var_marginals[j]<<" ";
			}
		}
		else
		{
			assert(copy_of_variables[i]->domain_size()==1);
			assert(copy_of_variables[i]->value()==0);
			int val=copy_of_variables[i]->mapping[0];
			for(int j=0;j<copy_of_variables[i]->old_domain.size();j++){
				if (j==val){
					cout<<"1 ";
				}
				else{
					cout<<"0 ";
				}
			}
//			for(int j=0;j<copy_of_variables[i]->old_domain.size();j++)
//			{
//				if(j==copy_of_variables[i]->value())
//					cout<<1<<" ";
//				else
//					cout<<0<<" ";
//			}
		}
	}
	cout<<endl;
}

void GM::printMarginalsUAI08(std::vector<vector<Double> > &marginals_)
{
	vector<vector<Double> > marginals=marginals_;
	//Normalize Marginals
	for(int i=0;i<marginals.size();i++)
	{
		Double norm_const;
		for(int j=0;j<marginals[i].size();j++)
			norm_const+=marginals[i][j];
		for(int j=0;j<marginals[i].size();j++)
			marginals[i][j]/=norm_const;
	}
	cout.setf(ios::fixed,ios::floatfield);
	cout<<"m "<<copy_of_variables.size()<<" ";
	for(int i=0;i<copy_of_variables.size();i++)
	{
		cout<<copy_of_variables[i]->old_domain.size()<<" ";
		if(copy_of_variables[i]->value()==INVALID_VALUE)
		{
			int curr_id=copy_of_variables[i]->id();
			vector<Double> var_marginals(copy_of_variables[i]->old_domain.size());
			int old_num=0;
			for(int j=0;j<marginals[curr_id].size();j++)
			{
				int new2old=copy_of_variables[i]->mapping[j];
				var_marginals[new2old]=marginals[curr_id][j];
			}
			for(int j=0;j<var_marginals.size();j++){
				cout<<var_marginals[j]<<" ";
			}
		}
		else
		{
			assert(copy_of_variables[i]->domain_size()==1);
			assert(copy_of_variables[i]->value()==0);
			int val=copy_of_variables[i]->mapping[0];
			for(int j=0;j<copy_of_variables[i]->old_domain.size();j++){
				if (j==val){
					cout<<"1 ";
				}
				else{
					cout<<"0 ";
				}
			}
		}
	}
	cout<<endl;
}
*/
void GM::printMarginalsUAI10(std::vector<vector<Double> > &marginals_, ostream& out)
{
	vector<vector<Double> > marginals=marginals_;
	//Normalize Marginals
	for(int i=0;i<marginals.size();i++)
	{
		Double norm_const;
		for(int j=0;j<marginals[i].size();j++)
			norm_const+=marginals[i][j];
		for(int j=0;j<marginals[i].size();j++)
			marginals[i][j]/=norm_const;
	}
	out<<copy_of_variables.size()<<" ";
	for(int i=0;i<copy_of_variables.size();i++)
	{
		out<<copy_of_variables[i]->old_domain.size()<<" ";
		if(copy_of_variables[i]->value()==INVALID_VALUE)
		{
			int curr_id=copy_of_variables[i]->id();
			vector<Double> var_marginals(copy_of_variables[i]->old_domain.size());
			int old_num=0;
			for(int j=0;j<marginals[curr_id].size();j++)
			{
				int new2old=copy_of_variables[i]->mapping[j];
				var_marginals[new2old]=marginals[curr_id][j];
			}
			for(int j=0;j<var_marginals.size();j++){
				out<<var_marginals[j]<<" ";
			}
		}
		else
		{
			assert(copy_of_variables[i]->domain_size()==1);
			assert(copy_of_variables[i]->value()==0);
			int val=copy_of_variables[i]->mapping[0];
			for(int j=0;j<copy_of_variables[i]->old_domain.size();j++){
				if (j==val){
					out<<"1 ";
				}
				else{
					out<<"0 ";
				}
			}
		}
	}
	out<<endl;
}
void GM::printMarginalsUAI10(std::vector<Function> &marginals_, ostream& out)
{
	vector<vector<Double> > marginals(marginals_.size());
	for (int i = 0; i < marginals_.size(); i++) {
		marginals[i] = vector<Double> (marginals_[i].tableSize());
		Double norm_const;
		for (int j = 0; j < marginals_[i].tableSize(); j++) {
			norm_const += marginals_[i].tableEntry(j);
			marginals[i][j] = marginals_[i].tableEntry(j);
		}
		for (int j = 0; j < marginals[i].size(); j++)
			marginals[i][j] /= norm_const;
	}
	out<<copy_of_variables.size()<<" ";
	for(int i=0;i<copy_of_variables.size();i++)
	{
		out<<copy_of_variables[i]->old_domain.size()<<" ";
		if(copy_of_variables[i]->value()==INVALID_VALUE)
		{
			int curr_id=copy_of_variables[i]->id();
			vector<Double> var_marginals(copy_of_variables[i]->old_domain.size());
			int old_num=0;
			for(int j=0;j<marginals[curr_id].size();j++)
			{
				int new2old=copy_of_variables[i]->mapping[j];
				var_marginals[new2old]=marginals[curr_id][j];
			}
			for(int j=0;j<var_marginals.size();j++){
				out<<var_marginals[j]<<" ";
			}
		}
		else
		{
			assert(copy_of_variables[i]->domain_size()==1);
			assert(copy_of_variables[i]->value()==0);
			int val=copy_of_variables[i]->mapping[0];
			for(int j=0;j<copy_of_variables[i]->old_domain.size();j++){
				if (j==val){
					out<<"1 ";
				}
				else{
					out<<"0 ";
				}
			}
		}
	}
	out<<endl;
}
/*
void GM::addToWCSP(Double& value)
{
	if((int)wcsp_probs.size() >= 10)
		return;
	for(int i=0;i<wcsp_probs.size();i++){
		if(wcsp_probs[i]==value)
			return;
	}
	wcsp_probs.push_back(value);
}

void GM::makeCopy(GM& gm)
{
	gm.mode=mode;
	gm.type=type;
	gm.variables=vector<Variable*>(variables.size());
	for(int i=0;i<variables.size();i++){
		gm.variables[i]=new Variable(i,variables[i]->domain());
		if (variables[i]->value()!=INVALID_VALUE){
			gm.variables[i]->value()=variables[i]->value();
		}
		gm.variables[i]->orig_id=i;
	}
	gm.copy_of_variables=gm.variables;
	if (type==BAYES){
		gm.functions = vector<Function*> (functions.size());
		for (int i = 0; i < functions.size(); i++) {
			CPT* cpt = static_cast<CPT*> (functions[i]);
			vector<Variable*> curr_func_variables;
			for (int j = 0; j < functions[i]->variables().size(); j++) {
				curr_func_variables.push_back(
						gm.variables[functions[i]->variables()[j]->id()]);
			}
			Variable* curr_marg_variable =
					gm.variables[cpt->marg_variable()->id()];
			assert(curr_marg_variable->id()==i);
			gm.functions[i] = new CPT(i, curr_func_variables,
					curr_marg_variable);

			gm.functions[i]->table() = functions[i]->table();
		}
	}
	else{
			gm.functions = vector<Function*> (functions.size());
			for (int i = 0; i < functions.size(); i++) {
				vector<Variable*> curr_func_variables;
				for (int j = 0; j < functions[i]->variables().size(); j++) {
					curr_func_variables.push_back(
							gm.variables[functions[i]->variables()[j]->id()]);
				}
				gm.functions[i] = new Function(i,curr_func_variables);
				gm.functions[i]->table() = functions[i]->table();
			}
	}
}
*/
void GM::getMinFillOrdering_randomized(vector<int>& order,
		vector<set<int> >& clusters, double& estimate, int& max_cluster_size) {
	estimate = 0.0;
	max_cluster_size = 0;
	order = vector<int> (variables.size());
	clusters = vector<set<int> > (variables.size());
	vector<vector<bool> > adj_matrix(variables.size());

	// Create the interaction graph of the functions in this graphical model - i.e.
	// create a graph structure such that an edge is drawn between variables in the
	// model that appear in the same function
	for (int i = 0; i < variables.size(); i++) {
		adj_matrix[i] = vector<bool> (variables.size());
	}
	vector<set<int> > graph(variables.size());
	vector<bool> processed(variables.size());
	for (int i = 0; i < functions.size(); i++) {
		for (int j = 0; j < functions[i]->variables().size(); j++) {
			for (int k = j + 1; k < functions[i]->variables().size(); k++) {
				int a = functions[i]->variables()[j]->id();
				int b = functions[i]->variables()[k]->id();
				graph[a].insert(b);
				graph[b].insert(a);
				adj_matrix[a][b] = true;
				adj_matrix[b][a] = true;
			}
		}
	}
	list<int> zero_list;
	//cerr<<"minfill: Inited\n";

	// For i = 1 to number of variables in the model
	// 1) Identify the variables that if deleted would add the fewest number of edges to the
	//    interaction graph
	// 2) Choose a variable, pi(i), from among this set
	// 3) Add an edge between every pair of non-adjacent neighbors of pi(i)
	// 4) Delete pi(i) from the interaction graph
	for (int i = 0; i < variables.size(); i++) {
		// Find variables with the minimum number of edges added
		double min = DBL_MAX;
		int min_id = -1;
		bool first = true;

		// Flag indicating whether the variable to be removed is from the
		// zero list - i.e. adds no edges to interaction graph when deleted
		bool fromZeroList = false;

		// Vector to keep track of the ID of each minimum fill variable
		vector<int> minFillIDs;

		// If there are no variables that, when deleted, add no edges...
		if (zero_list.empty()) {

			// For each unprocessed (non-deleted) variable
			for (int j = 0; j < variables.size(); j++) {
				if (processed[j])
					continue;
				double curr_min = 0.0;
				for (set<int>::iterator a = graph[j].begin(); a
						!= graph[j].end(); a++) {
					set<int>::iterator b = a;
					b++;
					for (; b != graph[j].end(); b++) {
						if (!adj_matrix[*a][*b]) {
							curr_min += (variables[*a]->domain_size()
									* variables[*b]->domain_size());
							if (curr_min > min)
								break;
						}
					}
					if (curr_min > min)
						break;
				}

				// Store the first non-deleted variable as a potential minimum
				if (first) {
					minFillIDs.push_back(j);
					min = curr_min;
					first = false;
				} else {
					// If this is a new minimum...
					if (min > curr_min) {
						min = curr_min;
						minFillIDs.clear();
						minFillIDs.push_back(j);
					}
					// Otherwise, if the number of edges removed is also a minimum, but
					// the minimum is zero
					else if (curr_min < DBL_MIN) {
						zero_list.push_back(j);
					}
					// Else if this is another potential min_fill
					else if (min == curr_min) {
						minFillIDs.push_back(j);
					}
				}
			}
		}
		// Else...delete variables from graph that don't add any edges
		else {
			min_id = zero_list.front();
			zero_list.pop_front();
			fromZeroList = true;
		}

		// If not from zero_list, choose one of the variables at random
		// from the set of min fill variables
		if (!fromZeroList) {
			int indexInVector = rand() % (int) minFillIDs.size();
			min_id = minFillIDs[indexInVector];
		}

		//cout<<"order["<<i<<"]= "<<min_id<<" "<<flush;
		assert(min_id!=-1);
		order[i] = min_id;
		// Now form the cluster
		clusters[i] = graph[min_id];
		clusters[i].insert(min_id);

		// Trinagulate min id and remove it from the graph
		for (set<int>::iterator a = graph[min_id].begin(); a
				!= graph[min_id].end(); a++) {
			set<int>::iterator b = a;
			b++;
			for (; b != graph[min_id].end(); b++) {
				if (!adj_matrix[*a][*b]) {
					adj_matrix[*a][*b] = true;
					adj_matrix[*b][*a] = true;
					graph[*a].insert(*b);
					graph[*b].insert(*a);
				}
			}
		}
		for (set<int>::iterator a = graph[min_id].begin(); a
				!= graph[min_id].end(); a++) {
			graph[*a].erase(min_id);
			adj_matrix[*a][min_id] = false;
			adj_matrix[min_id][*a] = false;
		}
		graph[min_id].clear();
		processed[min_id] = true;
	}

	// compute the estimate
	for (int i = 0; i < clusters.size(); i++) {
		if ((int) clusters[i].size() > max_cluster_size)
			max_cluster_size = (int) clusters[i].size();
		double curr_estimate = 1.0;
		for (set<int>::iterator j = clusters[i].begin(); j != clusters[i].end(); j++) {
			curr_estimate *= (double) variables[*j]->domain_size();
		}
		estimate += curr_estimate;
	}
	//cerr<<"Max cluster size =" <<max_cluster_size<<endl;
	//cerr<<"Estimate  = "<<estimate<<endl;
}

void GM::rearrangeOrdering_randomized(std::vector<int> &order,
		std::vector<set<int> > &clusters, std::vector<int> &new_order,
		double& log_limit) {
	new_order.clear();
	vector<vector<int> > var2clusters(variables.size());
	vector<int> var2estimate(variables.size());
	vector<double> cluster2estimate(clusters.size());
	for (int i = 0; i < var2estimate.size(); i++)
		var2estimate[i] = 0;

	set<int> faulting_clusters;
	vector<bool> processed(variables.size());

	for (int i = 0; i < clusters.size(); i++) {
		processed[i] = false;
		double curr_estimate = 0.0;
		for (set<int>::iterator j = clusters[i].begin(); j != clusters[i].end(); j++) {
			var2clusters[*j].push_back(i);
			curr_estimate += log((double) variables[*j]->domain_size());
		}

		cluster2estimate[i] = curr_estimate;
		if (curr_estimate > log_limit) {
			faulting_clusters.insert(i);
			for (set<int>::iterator j = clusters[i].begin(); j
					!= clusters[i].end(); j++) {
				var2estimate[*j]++;
			}
		}
	}
	int count = 0;
	myRandom random;
	while (!faulting_clusters.empty()) {
		if (new_order.size() == order.size())
			break;
		double min;
		int min_id = -1;
		bool first = true;

		// Vector to keep track of the ID of each minimum
		vector<int> minIDs;

		//cout<<"Removing variable "<<count++<<endl;
		for (int i = 0; i < variables.size(); i++) {
			if (processed[i])
				continue;
			if (first) {
				first = false;
				min = (double) variables[i]->domain_size()
						/ (double) var2estimate[i];
				minIDs.push_back(i);
			} else {
				double curr_min = (double) variables[i]->domain_size()
						/ (double) var2estimate[i];
				if (min > curr_min) {
					min = curr_min;
					minIDs.clear();
					minIDs.push_back(i);
				} else if (min == curr_min) {
					minIDs.push_back(i);
				}
			}
		}

		//int indexInVector = rand() % (int) minIDs.size();
		int indexInVector = random.getInt((int) minIDs.size());
		min_id = minIDs[indexInVector];

		assert(min_id!=-1);
		processed[min_id] = true;
		new_order.push_back(min_id);
		double div = log( variables[min_id]->domain_size());
		//Update cluster2estimate
		for (int i = 0; i < var2clusters[min_id].size(); i++) {
			int cluster_id = var2clusters[min_id][i];
			if (faulting_clusters.find(cluster_id) != faulting_clusters.end()) {
				cluster2estimate[cluster_id] -= div;
				if (cluster2estimate[cluster_id] < log_limit) {
					faulting_clusters.erase(cluster_id);
					for (set<int>::iterator j = clusters[cluster_id].begin(); j
							!= clusters[cluster_id].end(); j++) {
						int var_id = *j;
						if (!processed[var_id]) {
							assert(var2estimate[var_id]>0);
							var2estimate[var_id]--;
						}
					}
				}
			}
		}
	}
}


void GM::getMinDegreeOrdering_randomized(vector<int>& order, vector<set<int> >& clusters, double& estimate, int& max_cluster_size)
{
	estimate=0.0;
	order=vector<int>(variables.size());
	clusters=vector<set<int> > (variables.size());
	vector<vector<bool> > adj_matrix(variables.size());
	max_cluster_size=0;
	for(int i=0;i<variables.size();i++)
	{
		adj_matrix[i]=vector<bool> (variables.size());
	}
	vector<set<int> > graph(variables.size());
	vector<bool> processed(variables.size());
	vector<set<int> > minimums(variables.size());
	vector<int> min_pos(variables.size());
	myRandom random;
	for(int i=0;i<functions.size();i++)
	{
		for(int j=0;j<functions[i]->variables().size();j++)
		{
			for(int k=j+1;k<functions[i]->variables().size();k++)
			{
				int a=functions[i]->variables()[j]->id();
				int b=functions[i]->variables()[k]->id();
				graph[a].insert(b);
				graph[b].insert(a);
				adj_matrix[a][b]=true;
				adj_matrix[b][a]=true;
			}
		}
	}

	// Initialize minimums
	for(int i=0;i<variables.size();i++){
		minimums[graph[i].size()].insert(i);
		min_pos[i]=graph[i].size();
	}

	//cerr<<"minfill: Inited\n";
	for(int i=0;i<variables.size();i++)
	{

		// Find variable with the minimum degree
		double min;
		int min_id=-1;
		bool first=true;
		for(int j=0;j<variables.size();j++)
		{
			if (minimums[j].empty()){
				continue;
			}
			int rand_pos=random.getInt(minimums[j].size());
			set<int>::iterator set_iter=minimums[j].begin();
			for(int k=0;k<rand_pos;k++)
				set_iter++;
			min_id=*(set_iter);
			if (processed[min_id]){
				cerr<<"Something wrong\n";
			}

			minimums[j].erase(min_id);
			min_pos[min_id]=INVALID_VALUE;
			break;
		}
		//cout<<"order["<<i<<"]= "<<min_id<<" "<<flush;
		assert(min_id!=-1);
		order[i]=min_id;

		// Now form the cluster
		clusters[i]=graph[min_id];
		clusters[i].insert(min_id);

		// Becase min_id will be removed from the graph, the degrees of its neighbors will be reduced by 1
		// therefore update minimums
		for(set<int>::iterator a=graph[min_id].begin();a!=graph[min_id].end();a++)
		{
			assert(!processed[*a]);
			minimums[min_pos[*a]].erase(*a);
			min_pos[*a]--;
			minimums[min_pos[*a]].insert(*a);
		}
		// Trinagulate min id and remove it from the graph
		for(set<int>::iterator a=graph[min_id].begin();a!=graph[min_id].end();a++)
		{
			set<int>::iterator b=a;
			b++;
			for(;b!=graph[min_id].end();b++)
			{
				if(!adj_matrix[*a][*b])
				{
					adj_matrix[*a][*b]=true;
					adj_matrix[*b][*a]=true;
					graph[*a].insert(*b);
					graph[*b].insert(*a);

					// update minimums for *a and *b
					minimums[min_pos[*a]++].erase(*a);
					minimums[min_pos[*a]].insert(*a);
					minimums[min_pos[*b]++].erase(*b);
					minimums[min_pos[*b]].insert(*b);
				}
			}
		}
		for(set<int>::iterator a=graph[min_id].begin();a!=graph[min_id].end();a++)
		{
			graph[*a].erase(min_id);
			adj_matrix[*a][min_id]=false;
			adj_matrix[min_id][*a]=false;
		}
		graph[min_id].clear();
		processed[min_id]=true;
	}

	// compute the estimate
	//int max_cluster_size=0;
	for(int i=0;i<clusters.size();i++)
	{
		if((int)clusters[i].size() > max_cluster_size)
			max_cluster_size=(int)clusters[i].size();
		double curr_estimate=1.0;
		for(set<int>::iterator j=clusters[i].begin();j!=clusters[i].end();j++)
		{
			curr_estimate*=(double)variables[*j]->domain_size();
		}
		estimate+=curr_estimate;
	}
}


void GM::getLexOrdering(vector<int>& order, vector<set<int> >& clusters, double& estimate, int& max_cluster_size, int old_max_cluster_size)
{
	estimate=0.0;
	order=vector<int>(variables.size());
	clusters=vector<set<int> > (variables.size());
	vector<vector<bool> > adj_matrix(variables.size());
	for(int i=0;i<variables.size();i++)
	{
		adj_matrix[i]=vector<bool> (variables.size());
	}
	vector<set<int> > graph(variables.size());

	for(int i=0;i<functions.size();i++)
	{
		for(int j=0;j<functions[i]->variables().size();j++)
		{
			for(int k=j+1;k<functions[i]->variables().size();k++)
			{
				int a=functions[i]->variables()[j]->id();
				int b=functions[i]->variables()[k]->id();
				graph[a].insert(b);
				graph[b].insert(a);
				adj_matrix[a][b]=true;
				adj_matrix[b][a]=true;
			}
		}
	}
	cout<<"lex inited\n";
	//cerr<<"minfill: Inited\n";
	for(int i=0;i<variables.size();i++)
	{
		int min_id=i;
		order[i]=min_id;

		// Now form the cluster
		clusters[i]=graph[min_id];
		clusters[i].insert(min_id);

		// Trinagulate min id and remove it from the graph
		for(set<int>::iterator a=graph[min_id].begin();a!=graph[min_id].end();a++)
		{
			set<int>::iterator b=a;
			b++;
			for(;b!=graph[min_id].end();b++)
			{
				if(!adj_matrix[*a][*b])
				{
					adj_matrix[*a][*b]=true;
					adj_matrix[*b][*a]=true;
					graph[*a].insert(*b);
					graph[*b].insert(*a);
				}
			}
		}
		if ((int)graph[min_id].size() > old_max_cluster_size){
			max_cluster_size=variables.size();
			return;
		}
		for(set<int>::iterator a=graph[min_id].begin();a!=graph[min_id].end();a++)
		{
			graph[*a].erase(min_id);
			adj_matrix[*a][min_id]=false;
			adj_matrix[min_id][*a]=false;
		}
		graph[min_id].clear();
	}

	// compute the estimate
	max_cluster_size=0;
	for(int i=0;i<clusters.size();i++)
	{
		if((int)clusters[i].size() > max_cluster_size)
			max_cluster_size=(int)clusters[i].size();
		double curr_estimate=1.0;
		for(set<int>::iterator j=clusters[i].begin();j!=clusters[i].end();j++)
		{
			curr_estimate*=(double)variables[*j]->domain_size();
		}
		estimate+=curr_estimate;
	}

}
}
