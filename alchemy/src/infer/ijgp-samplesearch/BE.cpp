#include "BE.h"
namespace ss{

BE::BE(std::vector<Variable*> &variables, std::vector<Function*> &functions, std::vector<int> &order)
{
	log_pe=LogDouble(1.0);
	vector<vector<LogFunction*> > buckets (order.size());

	vector<int> var_in_pos(order.size());
	for(int i=0;i<var_in_pos.size();i++)
		var_in_pos[order[i]]=i;

	// First put the functions in the proper buckets
	for(int i=0;i<functions.size();i++)
	{
		int pos=order.size();
		LogFunction* function=new LogFunction(*functions[i]);
		if(function->variables().empty())
		{
			//cerr<<"Deleting function\n";
			assert((int)function->logTableSize()==1);
			//cerr<<function->log_table[0].toDouble()<<endl;
			log_pe+=function->logTableEntry(0);
			delete(function);
			continue;
		}
		for(int j=0;j<function->variables().size();j++)
		{
			if(var_in_pos[function->variables()[j]->id()] < pos)
				pos=var_in_pos[function->variables()[j]->id()];
		}
		assert(pos!=(int)order.size());
		/*{
			assert((int)function->log_table.size()==1);
			cerr<<function->log_table[0].toDouble()<<" "<<endl;
			log_pe+=function->log_table[0];
			delete(function);
			continue;
		}*/
		buckets[pos].push_back(function);
	}
	
	//cout<<"Now processing buckets\n";
	//Process buckets
	for(int i=0;i<buckets.size();i++)
	{
		if(buckets[i].empty())
			continue;

		vector<Variable*> bucket_variables;
		for(int j=0;j<buckets[i].size();j++)
		{
			do_set_union(bucket_variables,buckets[i][j]->variables(),bucket_variables,less_than_comparator_variable);
		}
		//cout<<bucket_variables.size()<<" "<<flush;
		//cout<<"buck-vars.size ="<<bucket_variables.size()<<endl;
		//cerr<<"Processing bucket "<<i<<" out of "<<buckets.size()<<endl;
		/*if((int)bucket_variables.size()==1)
		{
			Double temp;
			for(int k=0;k<bucket_variables[0]->domain_size();k++)
			{
				Double mult(1.0);
				for(int j=0;j<buckets[i].size();j++)
					mult*=buckets[i][j]->log_table[k].toDouble();
				temp+=mult;
			}
			cerr<<temp<<endl;
			log_pe+=LogDouble(temp);
			continue;
		}*/

		// Compute variables required for marginalization
		//cerr<<bucket_variables.size()<<endl;
		vector<Variable*> bucket_variable;
		bucket_variable.push_back(variables[order[i]]);
		vector<Variable*> marg_variables;
		do_set_difference(bucket_variables,bucket_variable,marg_variables,less_than_comparator_variable);

		LogFunction* function= new LogFunction();
		/*cout<<"Start mult\n";
		for(int j=0;j<buckets[i].size();j++)
		{
			cout<<"\t";
			buckets[i][j]->print(cout);
		}*/
		LogFunction::multiplyAndMarginalize(marg_variables,buckets[i],*function,false);
		//cout<<"End mult\n";

		if(function->variables().empty())
		{
			
			assert((int)function->logTableSize()==1);
			//cerr<<function->log_table[0].toDouble()<<endl;
			log_pe+=function->logTableEntry(0);
			delete(function);
			continue;
		}
		//Put the function in the appropriate bucket
		int pos=order.size();
		//function->print(cout);
		assert(!function->logTableSize() == 0);

		for(int j=0;j<function->variables().size();j++)
		{
			if(var_in_pos[function->variables()[j]->id()] < pos)
				pos=var_in_pos[function->variables()[j]->id()];
		}
		assert(pos!=(int)order.size());
		/*if(pos==(int)order.size())
		{
			assert((int)function->log_table.size()==1);
			log_pe+=function->log_table[0];
			continue;
		}*/
		assert(pos > i);
		buckets[pos].push_back(function);
		for(int j=0;j<buckets[i].size();j++)
		{
			delete(buckets[i][j]);
		}
		buckets[i].clear();
	}
	for(int i=0;i<buckets.size();i++){
		for(int j=0;j<buckets[i].size();j++){
			if (buckets[i][j]!=NULL){
				delete(buckets[i][j]);
			}
		}
	}
	buckets.clear();
}


BESample::BESample(std::vector<Variable*> &variables, std::vector<Function*> &functions, std::vector<int> &order,myRandom& random)
{
	log_pe=LogDouble(1.0);
	vector<vector<LogFunction*> > buckets (order.size());

	vector<int> var_in_pos(order.size());
	for(int i=0;i<var_in_pos.size();i++)
		var_in_pos[order[i]]=i;

	// First put the functions in the proper buckets
	for(int i=0;i<functions.size();i++)
	{
		int pos=order.size();
		LogFunction* function=new LogFunction(*functions[i]);
		if(function->variables().empty())
		{
			//cerr<<"Deleting function\n";
			assert((int)function->logTableSize()==1);
			//cerr<<function->log_table[0].toDouble()<<endl;
			log_pe+=function->logTableEntry(0);
			delete(function);
			continue;
		}
		for(int j=0;j<function->variables().size();j++)
		{
			if(var_in_pos[function->variables()[j]->id()] < pos)
				pos=var_in_pos[function->variables()[j]->id()];
		}
		assert(pos!=(int)order.size());
		/*{
			assert((int)function->log_table.size()==1);
			cerr<<function->log_table[0].toDouble()<<" "<<endl;
			log_pe+=function->log_table[0];
			delete(function);
			continue;
		}*/
		buckets[pos].push_back(function);
	}
	
	//cout<<"Now processing buckets\n";
	//Process buckets
	for(int i=0;i<buckets.size();i++)
	{
		if(buckets[i].empty())
			continue;

		vector<Variable*> bucket_variables;
		for(int j=0;j<buckets[i].size();j++)
		{
			do_set_union(bucket_variables,buckets[i][j]->variables(),bucket_variables,less_than_comparator_variable);
		}
		//cout<<bucket_variables.size()<<" "<<flush;
		//cout<<"buck-vars.size ="<<bucket_variables.size()<<endl;
		//cerr<<"Processing bucket "<<i<<" out of "<<buckets.size()<<endl;
		/*if((int)bucket_variables.size()==1)
		{
			Double temp;
			for(int k=0;k<bucket_variables[0]->domain_size();k++)
			{
				Double mult(1.0);
				for(int j=0;j<buckets[i].size();j++)
					mult*=buckets[i][j]->log_table[k].toDouble();
				temp+=mult;
			}
			cerr<<temp<<endl;
			log_pe+=LogDouble(temp);
			continue;
		}*/

		// Compute variables required for marginalization
		//cerr<<bucket_variables.size()<<endl;
		vector<Variable*> bucket_variable;
		bucket_variable.push_back(variables[order[i]]);
		vector<Variable*> marg_variables;
		do_set_difference(bucket_variables,bucket_variable,marg_variables,less_than_comparator_variable);

		LogFunction* function= new LogFunction();
		/*cout<<"Start mult\n";
		for(int j=0;j<buckets[i].size();j++)
		{
			cout<<"\t";
			buckets[i][j]->print(cout);
		}*/
		LogFunction::multiplyAndMarginalize(marg_variables,buckets[i],*function,false);
		//cout<<"End mult\n";

		if(function->variables().empty())
		{
			
			assert((int)function->logTableSize()==1);
			//cerr<<function->log_table[0].toDouble()<<endl;
			log_pe+=function->logTableEntry(0);
			delete(function);
			continue;
		}
		//Put the function in the appropriate bucket
		int pos=order.size();
		//function->print(cout);
		assert(!function->logTableSize() == 0);

		for(int j=0;j<function->variables().size();j++)
		{
			if(var_in_pos[function->variables()[j]->id()] < pos)
				pos=var_in_pos[function->variables()[j]->id()];
		}
		assert(pos!=(int)order.size());
		/*if(pos==(int)order.size())
		{
			assert((int)function->log_table.size()==1);
			log_pe+=function->log_table[0];
			continue;
		}*/
		assert(pos > i);
		buckets[pos].push_back(function);
		/*
		for(int j=0;j<buckets[i].size();j++)
		{
			delete(buckets[i][j]);
		}
		buckets[i].clear();
		*/
	}

	/* Generate a sample from the buckets */
	for(int i=buckets.size()-1;i>-1;i--){
		int curr_var=order[i];
		if(variables[curr_var]->value()!=INVALID_VALUE){
			continue;
		}
		vector<Double> marginal(variables[curr_var]->domain_size());
		for (int j=0;j<marginal.size();j++)
			marginal[j]=Double(1.0);
		for(int j=0;j<buckets[i].size();j++)
		{
			for(int k=0;k<buckets[i][j]->variables().size();k++)
			{
				if (buckets[i][j]->variables()[k]->id()==curr_var)
					continue;
				assert(buckets[i][j]->variables()[k]->value()!=INVALID_VALUE);
			}
			for(int k=0;k<marginal.size();k++)
			{
				int entry;
				variables[curr_var]->addr_value()=k;
				entry=Variable::getAddress(buckets[i][j]->variables());
				marginal[k]*=buckets[i][j]->logTableEntry(entry).toDouble();
			}
		}
		Double norm_const;
		for(int j=0;j<marginal.size();j++){
			norm_const+=marginal[j];
		}
		double rand_num = random.getDouble();
		Double cdf;
		for(int j=0;j<marginal.size();j++){
			marginal[j]/=norm_const;
			cdf+=marginal[j];
			if (rand_num <= cdf.value()){
				variables[curr_var]->value()=j;
				break;
			}
		}
		assert(variables[curr_var]->value()!=INVALID_VALUE);
	}
	/* Delete all the buckets */
	for(int i=0;i<buckets.size();i++){
		for(int j=0;j<buckets[i].size();j++){
			if (buckets[i][j]!=NULL){
				delete(buckets[i][j]);
			}
		}
	}
	buckets.clear();
}


struct SATFunction
{
	vector<Variable*>& all_variables;
	vector<Variable*> variables;
	vector<vector<Lit>* > clauses;
	vector<double> table;
	vector<SATFunction*> sat_functions;
	SATFunction(vector<Variable*>& all_variables_): all_variables(all_variables_){}
	~SATFunction(){}
	//SATFunction():all_variables(NULL){ }
	void addFunction(SATFunction* func){
		sat_functions.push_back(func);
		do_set_union(variables,func->variables,variables,less_than_comparator_variable);
	}
	
	void initVariables(){
		variables=vector<Variable*>();
		set<int> vars;
		for(int i=0;i<clauses.size();i++){
			vector<Lit>& c = *clauses[i];
			for(int j=0;j<c.size();j++){
				int Var=var(c[j]);
				if (all_variables[Var]->value()==INVALID_VALUE){
					vars.insert(Var);
				}
			}
		}
		for(set<int>::iterator i=vars.begin();i!=vars.end();i++){
			variables.push_back(all_variables[*i]);
		}
		sort(variables.begin(),variables.end(),less_than_comparator_variable);
	}
	bool isSatisfied(){
		for(int i=0;i<clauses.size();i++){
			bool is_sat=false;
			vector<Lit>& c = *clauses[i];
			for(int j=0;j<c.size();j++){
				int Var=var(c[j]);
				Lit q=(all_variables[Var]->addr_value()==0)?((Lit(Var))):(~(Lit(Var)));
				if (c[j] == q)
				{
					is_sat=true;
					break;
				}
			}
			if (!is_sat){
				return false;
			}
		}
		return true;
	}
	void multiplyAndMarginalize(vector<Variable*>& marg_variables, SATFunction& other_func){
		
		assert((int)(variables.size()-marg_variables.size())==1);
		other_func.variables=marg_variables;
		other_func.table=vector<double>(Variable::getDomainSize(marg_variables));
		int num_values=Variable::getDomainSize(variables);
		for(int i=0;i<other_func.table.size();i++){
			other_func.table[i]=0.0;
		}
		for(int i=0;i<num_values;i++){
			Variable::setAddress(variables,i);
			double mult=1.0;
			if (isSatisfied()){
				for(int j=0;j<sat_functions.size();j++){
					int entry=Variable::getAddress(sat_functions[j]->variables);
					mult*=sat_functions[j]->table[entry];
				}
				int entry=Variable::getAddress(other_func.variables);
				other_func.table[entry]+=mult;
			}
			//if (!isSatisfied() || !other_func.isSatisfied()){
			//	new_table[i]=(double)0.0;
			//	continue;
			//}
			
		}
	}
//	void multiply(SATFunction& other_func){
//		if (table.empty()){
//			initTable();
//		}
//		assert(other_func.clauses.empty());
//		vector<Variable*> new_variables;
//		do_set_union(variables,other_func.variables,new_variables,less_than_comparator_variable);
//		int num_values=Variable::getDomainSize(new_variables);
//		vector<double> new_table(num_values);
//		for(int i=0;i<num_values;i++){
//			Variable::setAddress(new_variables,i);
//			//if (!isSatisfied() || !other_func.isSatisfied()){
//			//	new_table[i]=(double)0.0;
//			//	continue;
//			//}
//			int entry1=Variable::getAddress(variables);
//			int entry2=Variable::getAddress(other_func.variables);
//			new_table[i]=table[entry1]*other_func.table[entry2];
//		}
//		variables=new_variables;
//		table=new_table;
//		
//	}
//	void marginalize(vector<Variable*>& marg_variables, SATFunction& sat_function){
//		if (table.empty())
//		{
//			initTable();
//		}
//		assert(((int)variables.size()-(int)marg_variables.size())==1);
//		if (marg_variables.empty()){
//			sat_function.variables=vector<Variable*>();
//			sat_function.table=vector<double>(1);
//			sat_function.table[0]=0.0;
//			for(int i=0;i<table.size();i++){
//				Variable::setAddress(variables,i);
//				if (isSatisfied()){
//					sat_function.table[0]+=table[i];
//				}
//			}
//			return;
//		}
//		
//		assert(do_set_inclusion(marg_variables,variables,less_than_comparator_variable));
//		//sat_function=SATFunction(all_variables);
//		sat_function.variables=marg_variables;
//		sat_function.table=vector<double>(Variable::getDomainSize(sat_function.variables));
//		for(int i=0;i<sat_function.table.size();i++){
//			sat_function.table[i]=0.0;
//		}
//		for(int i=0;i<table.size();i++){
//			Variable::setAddress(variables,i);
//			int entry=Variable::getAddress(sat_function.variables);
//			//if (isSatisfied())
//			sat_function.table[entry]+=table[i];
//		}
//	}
	void addClause(vector<Lit>& clause){
		clauses.push_back(&clause);
	}
};

bool isClauseSatisfied(vector<Variable*>& all_variables, vector<Lit>& clause)
{
	for(int i=0;i<clause.size();i++){
		int Var=var(clause[i]);
		if (all_variables[Var]->value()!=INVALID_VALUE){
			Lit q = (all_variables[Var]->value()==0)? (~(Lit(Var))): (Lit(Var));
			if (q == clause[i])
				return true;
		}
	}
	return false;
}
double superhack(vector<Variable*>& all_variables,vector<vector<Lit> >& clauses, vector<int>& order)
{
	//create functions
	vector<Function*> functions;
	vector<int> old2new(all_variables.size());
	vector<Variable*> new_variables;
	for(int i=0;i<clauses.size();i++){
		if (!isClauseSatisfied(all_variables,clauses[i])){
			Function* function=new Function();
			for(int j=0;j<clauses[i].size();j++){
				int id=var(clauses[i][j]);
				if (all_variables[id]->value()==INVALID_VALUE){
					function->variables().push_back(all_variables[id]);
					if(clauses[i][j]==Lit(id)){
						all_variables[id]->addr_value()=0;
					}
					else{
						all_variables[id]->addr_value()=1;
					}
				}
			}
			sort(function->variables().begin(),function->variables().end(),less_than_comparator_variable);
			function->tableInit(Variable::getDomainSize(function->variables()));
			for(int j=0;j<function->tableSize();j++){
				function->tableEntry(j)=Double(1.0);
			}
			function->tableEntry(Variable::getAddress(function->variables()))=Double();
			functions.push_back(function);
		}
	}
	vector<Variable*> covered_variables;
	for(int i=0;i<functions.size();i++){
		do_set_union(covered_variables,functions[i]->variables(),covered_variables,less_than_comparator_variable);
	}
	int count=0;
	for(int i=0;i<all_variables.size();i++){
		old2new[i]=-1;
	}
	if (!covered_variables.empty()){
		for(int i=0;i<all_variables.size();i++){
			if (all_variables[i]->id()==covered_variables[count]->id()){
				old2new[i]=count;
			//new2old[count]=i;
				count++;
				if(count >= (int) covered_variables.size())
					break;
			}
		}
	}
	double pe=(double)1.0;
	for(int i=0;i<all_variables.size();i++){
		if (old2new[i]==-1 && all_variables[i]->value()==INVALID_VALUE){
			pe*=(double)2.0;
		}
	}
	if (covered_variables.empty()){
		return pe;
	}
	//Create new variables, update the functions and order
	for(int i=0;i<covered_variables.size();i++){
		new_variables.push_back(new Variable(i,covered_variables[i]->domain()));
	}
	vector<int> new_order;
	count=0;
	for(int i=0;i<order.size();i++){
		int var=order[i];
		if(old2new[var] > -1){
			new_order.push_back(old2new[var]);
		}
	}
	
	for(int i=0;i<functions.size();i++){
		vector<Variable*> new_func_variables;
		for(int j=0;j<functions[i]->variables().size();j++){
			int var=functions[i]->variables()[j]->id();
			assert(old2new[var]>-1);
			new_func_variables.push_back(new_variables[old2new[var]]);
		}
		functions[i]->variables()=new_func_variables;
	}
	//BE be(all_variables,functions,order);
	BE be(new_variables,functions,new_order);
	for(int i=0;i<functions.size();i++){
		delete(functions[i]);
	}
	for(int i=0;i<new_variables.size();i++){
		delete(new_variables[i]);
	}
	pe*=be.log_pe.toDouble().value();
	return pe;
}
BESAT::BESAT(vector<Variable*>& all_variables,vector<vector<Lit> >& clauses, vector<int>& order)
{
	log_pe=log(superhack(all_variables,clauses,order));
	return;
	//cout<<"Func "<<exp(log_pe)<<endl;
	
	log_pe=0.0;
	vector<SATFunction*> buckets (order.size());
//	//First reset all the variables and set them appropriately
//	for(int i=0;i<all_variables.size();i++){
//		all_variables[i]->value()=INVALID_VALUE;
//	}
//	for(int i=0;i<assumptions.size();i++){
//		int Var=var(assumptions[i]);
//		if(!sign(assumptions[i])){
//			all_variables[Var]->value()=0;
//		}
//		else{
//			all_variables[Var]->value()=1;
//		}
//	}
	
	// Set the bucket functions
	for(int i=0;i<order.size();i++){
		buckets[i]=new SATFunction(all_variables);
	}
	
	vector<int> var_in_pos(order.size());
	for(int i=0;i<var_in_pos.size();i++)
		var_in_pos[order[i]]=i;

	// First put the functions in the proper buckets
	for(int i=0;i<clauses.size();i++)
	{
		int pos=order.size();
		vector<Lit>& c=clauses[i];
		if (isClauseSatisfied(all_variables,c)){
			continue;
		}
		for(int j=0;j<c.size();j++)
		{
			
			int Var=var(c[j]);
			if(var_in_pos[Var] < pos)
				pos=var_in_pos[Var];
		}
		assert(pos!=(int)order.size());
		/*{
			assert((int)function->log_table.size()==1);
			cerr<<function->log_table[0].toDouble()<<" "<<endl;
			log_pe+=function->log_table[0];
			delete(function);
			continue;
		}*/
	
		buckets[pos]->addClause(clauses[i]);
	}
	
	
	for(int i=0;i<order.size();i++){
		buckets[i]->initVariables();
		//buckets[i]->initTable();
	}
	
	//Process buckets
	for(int i=0;i<buckets.size();i++)
	{
		if(all_variables[order[i]]->value()!=INVALID_VALUE){
				delete(buckets[i]);
				continue;
		}
		if(buckets[i]->variables.empty()){
			delete(buckets[i]);
			log_pe+=log(2.0);
			continue;
		}
		vector<Variable*> bucket_variable;
		bucket_variable.push_back(all_variables[order[i]]);
		vector<Variable*> marg_variables;
		do_set_difference(buckets[i]->variables,bucket_variable,marg_variables,less_than_comparator_variable);
		SATFunction* tmp_function=new SATFunction(all_variables);
		buckets[i]->multiplyAndMarginalize(marg_variables,*tmp_function);
		if(tmp_function->variables.empty())
		{
			
			assert((int)tmp_function->table.size()==1);
			log_pe+=log(tmp_function->table[0]);
			delete(tmp_function);
			continue;
		}
		//Put the function in the appropriate bucket
		int pos=order.size();
		//function->print(cout);
		//assert(!tmp_function->table.empty());

		for(int j=0;j<tmp_function->variables.size();j++)
		{
			if(var_in_pos[tmp_function->variables[j]->id()] < pos)
				pos=var_in_pos[tmp_function->variables[j]->id()];
		}
		assert(pos!=(int)order.size());
		/*if(pos==(int)order.size())
		{
			assert((int)function->log_table.size()==1);
			log_pe+=function->log_table[0];
			continue;
		}*/
		assert(pos > i);
		buckets[pos]->addFunction(tmp_function);
		
		//for(int j=0;j<buckets.size();j++)
		//{
		for(int j=0;j<buckets[i]->sat_functions.size();j++){
			delete(buckets[i]->sat_functions[j]);
		}
		delete(buckets[i]);	
		//}
	}
	
}

/*
BESampleSAT::BESampleSAT(vector<Variable*>& all_variables, vector<vector<Lit> >& clauses, vector<int>& order, myRandom& random)
{
	typedef vector<Lit>   myClause;
	vector<vector<Function> > buckets (order.size());
	vector<vector<myClause> > sat_buckets(order.size());

	vector<int> var_in_pos(order.size());
	for(int i=0;i<var_in_pos.size();i++)
		var_in_pos[order[i]]=i;

	// First put the clauses in appropriate buckets
	for(int i=0;i<clauses.size();i++)
	{
		int pos=order.size();
		if(clauses[i].empty())
			continue;
		// Reduce the clause
		myClause new_clause;
		bool is_satisfied=false;
		for(int j=0;j<clauses[i].size();j++){
			int Var=var(clauses[i][j]);
			assert(Var < (int) all_variables.size());
			if (all_variables[Var]->value()==INVALID_VALUE){
				new_clause.push_back(clauses[i][j]);
				if(var_in_pos[Var] < pos)
					pos=var_in_pos[Var];
			}
			else{
				Lit p=(all_variables[Var]->value()==0)? (~Lit(Var)): (Lit(Var));
				if (clauses[i][j]==p){
					is_satisfied=true;
					break;
				}
			}
		}
		
		if (is_satisfied)
			continue;
		//assert(!isClauseSatisfied(all_variables,clauses[i]));
		assert(!new_clause.empty());
		assert(pos!=(int)order.size());
		sat_buckets[pos].push_back(new_clause);
	}

	// Process the buckets
	for(int i=0;i<buckets.size();i++){
		int curr_var=order[i];
		if (all_variables[curr_var]->value()!=INVALID_VALUE){
			continue;
		}
		//Create a function out of the clauses
		set<int> sat_bucket_vars;
		for(int j=0;j<sat_buckets[i].size();j++){
			for(int k=0;k<sat_buckets[i][j].size();k++){
				sat_bucket_vars.insert(var(sat_buckets[i][j][k]));
			}
		}
		Function func;
		for(set<int>::iterator j=sat_bucket_vars.begin();j!=sat_bucket_vars.end();j++){
			func.variables().push_back(all_variables[*j]);
		}
		sort(func.variables().begin(),func.variables().end(),less_than_comparator_variable);
		int num_values=Variable::getDomainSize(func.variables());
		func.tableInit(num_values);
		for(int j=0;j<num_values;j++){
			Variable::setAddress(func.variables(),j);
			bool is_satisfied=true;
			for(int k=0;k<sat_buckets[i].size();k++){
				is_satisfied=false;
				for(int a=0;a<sat_buckets[i][k].size();a++){
					int this_var=var(sat_buckets[i][k][a]);
					Lit q = (all_variables[this_var]->addr_value()==0)?(~Lit(this_var)):(Lit(this_var));
					if (q==sat_buckets[i][k][a]){
						is_satisfied=true;
						break;
					}
				}
				if(!is_satisfied){
					break;
				}
			}
			if (is_satisfied){
				func.tableEntry(j)=Double(1.0);
			}
		}

		if (!func.variables().empty()){
			buckets[i].push_back(func);
		}

		if (buckets[i].empty()){
			continue;
		}

		// Otherwise Create a message and put it in the appropriate bucket
		Function new_func;
		vector<Function*> all_functions(buckets[i].size());
		for(int j=0;j<buckets[i].size();j++){
			all_functions[j]=&buckets[i][j];
		}
		vector<Variable*> marg_variables;
		for(int j=0;j<buckets[i].size();j++){
			do_set_union(marg_variables,buckets[i][j].variables(),marg_variables,less_than_comparator_variable);
		}
		vector<Variable*> curr_variable;
		curr_variable.push_back(all_variables[curr_var]);
		do_set_difference(marg_variables,curr_variable,marg_variables,less_than_comparator_variable);
		Function::dummy_multiplyMarginalize(marg_variables,all_functions,new_func);

		all_functions.clear();
		// Put the new function in the appropriate bucket
		//Put the function in the appropriate bucket
		if (new_func.variables().empty()){
			continue;
		}
		int pos=order.size();
		for(int j=0;j<new_func.variables().size();j++)
		{
			if(var_in_pos[new_func.variables()[j]->id()] < pos)
				pos=var_in_pos[new_func.variables()[j]->id()];
		}
		assert(pos!=(int)order.size());
		//if(pos==(int)order.size())
		//{
		//	assert((int)function->log_table.size()==1);
		//	log_pe+=function->log_table[0];
		//	continue;
		//}
		assert(pos > i);
		buckets[pos].push_back(new_func);
	}

	// Now sample
	for(int i=buckets.size()-1;i>-1;i--){
		int curr_var=order[i];
		if(all_variables[curr_var]->value()!=INVALID_VALUE){
			continue;
		}
		Double marg1(1.0),marg2(1.0);
		for(int j=0;j<buckets[i].size();j++){
			for(int k=0;k<buckets[i][j].variables().size();k++){
				if (buckets[i][j].variables()[k]->id()==curr_var)
					continue;
				assert(buckets[i][j].variables()[k]->value()!=INVALID_VALUE);
			}
			int entry;
			all_variables[curr_var]->addr_value()=0;
			entry=Variable::getAddress(buckets[i][j].variables());
			marg1*=buckets[i][j].tableEntry(entry);
			all_variables[curr_var]->addr_value()=1;
			entry=Variable::getAddress(buckets[i][j].variables());
			marg2*=buckets[i][j].tableEntry(entry);
		}
		Double norm_const;
		norm_const=marg1+marg2;
		Double rand_num(random.getDouble());
		if(rand_num < (marg1/(norm_const))){
			all_variables[curr_var]->value()=0;
		}
		else{
			all_variables[curr_var]->value()=1;
		}
	}
}
*/
/*
BucketProp::BucketProp(std::vector<Variable*> &variables, std::vector<Function*> &functions, std::vector<int> &order)
{
cout << "in BE "<< endl;
	log_pe=LogDouble(1.0);
	vector<vector<LogFunction*> > buckets (order.size());

	vector<int> var_in_pos(order.size());
	for(int i=0;i<var_in_pos.size();i++)
		var_in_pos[order[i]]=i;

	// First put the functions in the proper buckets
	for(int i=0;i<functions.size();i++)
	{
		int pos=order.size();
		LogFunction* function=new LogFunction(*functions[i]);
		if(function->variables().empty())
		{
			//cerr<<"Deleting function\n";
			assert((int)function->logTableSize()==1);
			//cerr<<function->log_table[0].toDouble()<<endl;
			log_pe+=function->logTableEntry(0);
			delete(function);
			continue;
		}
		for(int j=0;j<function->variables().size();j++)
		{
			if(var_in_pos[function->variables()[j]->id()] < pos)
				pos=var_in_pos[function->variables()[j]->id()];
		}
		assert(pos!=(int)order.size());
		//{
		//	assert((int)function->log_table.size()==1);
		//	cerr<<function->log_table[0].toDouble()<<" "<<endl;
		//	log_pe+=function->log_table[0];
		//	delete(function);
		//	continue;
		//}
		buckets[pos].push_back(function);
	}

	//cout<<"Now processing buckets\n";
	//Process buckets
	vector<pair<int,int> > messages;
	vector<int> to_not_include;
	vector<vector<Variable*> > all_bucket_vars (buckets.size());
	for(int i=0;i<buckets.size();i++)
	{
		if(buckets[i].empty())
			continue;

		vector<Variable*> bucket_variables;
		for(int j=0;j<buckets[i].size();j++)
		{
			do_set_union(bucket_variables,buckets[i][j]->variables(),bucket_variables,less_than_comparator_variable);
		}
		// Compute variables required for marginalization
		vector<Variable*> bucket_variable;
		bucket_variable.push_back(variables[order[i]]);
		vector<Variable*> marg_variables;
		do_set_difference(bucket_variables,bucket_variable,marg_variables,less_than_comparator_variable);
		all_bucket_vars[i]=bucket_variables;
		LogFunction* function= new LogFunction();
		LogFunction::multiplyAndMarginalize(marg_variables,buckets[i],*function,false);
		if(function->variables().empty())
		{
			assert((int)function->logTableSize()==1);
			log_pe+=function->logTableEntry(0);
			delete(function);
			continue;
		}
		//Put the function in the appropriate bucket
		int pos=order.size();
		assert(!function->logTableSize() == 0);
		for(int j=0;j<function->variables().size();j++)
		{
			if(var_in_pos[function->variables()[j]->id()] < pos)
				pos=var_in_pos[function->variables()[j]->id()];
		}
		assert(pos!=(int)order.size());
		assert(pos > i);
		messages.push_back(pair<int,int> (i,pos));
		to_not_include.push_back(buckets[pos].size());
		buckets[pos].push_back(function);
	}
	for(int i=messages.size()-1;i>-1;i--){
		int from_node=messages[i].second;
		int to_node=messages[i].first;
		vector<Variable*> marg_variables;
		do_set_intersection(all_bucket_vars[from_node],all_bucket_vars[to_node],marg_variables,less_than_comparator_variable);
		LogFunction* function= new LogFunction();
		// Create a vector of all functions except the to_not_include function
		vector<LogFunction*> curr_bucket_functions;
		bool found=false;
		for(int j=0;j<buckets[from_node].size();j++){
			if(j==to_not_include[i]){
				found=true;
			}
			curr_bucket_functions.push_back(buckets[from_node][j]);
		}
		LogFunction::multiplyAndMarginalize(marg_variables,curr_bucket_functions,*function,true);
		if (function->variables().empty()) {
			assert((int)function->logTableSize()==1);
			delete (function);
			continue;
		}
		buckets[to_node].push_back(function);
	}
	marginals=vector<Function> (order.size());
	for(int i=0;i<order.size();i++){
		int var=order[i];
		vector<Variable*> marg_variable;
		marg_variable.push_back(variables[var]);
		if (variables[order[i]]->value()!=INVALID_VALUE){
			marginals[var].variables()=marg_variable;
			marginals[var].tableInit(variables[var]->domain_size());
			marginals[var].tableEntry(variables[var]->value())=Double(1.0);
		}
		else{
			LogFunction::multiplyAndMarginalize(marg_variable,buckets[i],marginals[var],true);
			if (marginals[var].tableSize() == 0){
				marginals[var].variables()=marg_variable;
				marginals[var].tableInit(variables[var]->domain_size());
				for(int j=0;j<variables[var]->domain_size();j++){
					marginals[var].tableEntry(j)=Double((long double)1.0/(long double)variables[var]->domain_size());
				}
			}
		}
	}
	for(int i=0;i<buckets.size();i++){
		for(int j=0;j<buckets[i].size();j++){
			if (buckets[i][j]!=NULL){
				delete(buckets[i][j]);
			}
		}
	}
	buckets.clear();
}
*/
}

