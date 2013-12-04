#include "DRC.h"
#include <cassert>

bool allzeros(Function& func)
{
	for(int i=0;i<func.table().size();i++)
		if(!func.table()[i].isZero())
			return false;
	return true;
}
DRC::DRC(GM& gm_, int i_bound ,vector<int>& order): gm(gm_)
{

	//Assert that all variables are not set a value
	for(int i=0;i<gm.variables.size();i++)
		assert(gm.variables[i]->value()==INVALID_VALUE);
	// Create a mapping from variables to their position in the ordering
	vector<int> mapped_order(order.size());
	for(int i=0;i<order.size();i++)
		mapped_order[order[i]]=i;

	
	// Make sure the order is over the bayesian network
	assert(order.size()==gm.variables.size());
	
	// Put functions in the bucket as you would do for bucket elimination
	buckets=vector<vector<Function> > (order.size());
	for(int i=0;i<gm.functions.size();i++)
	{
		int min_size=(int)gm.variables.size();
		for(int j=0;j<gm.functions[i]->variables().size();j++)
		{
			int curr_var=gm.functions[i]->variables()[j]->id();
			assert(gm.functions[i]->variables()[j]->id() < order.size());
			if(mapped_order[curr_var] < min_size)
			{
				min_size=mapped_order[curr_var];
			}		
		}
		buckets[order[min_size]].push_back(Function(*gm.functions[i]));
	}
	cerr<<"buckets created\n";
	// Run mini-bucket scheme which will result in a directionally consistent network
	for(int i=0;i<order.size();i++)
	{
		int var=order[i];
		vector<Function> curr_bucket;
		for(int j=0;j<buckets[var].size();j++)
		{
			bool found=false;
			int loc=0;
			// Try to put the function in a mini-bucket
			for(int k=0;k<curr_bucket.size();k++)
			{
				vector<Variable*> variables;
				do_set_union(curr_bucket[k].variables(),buckets[var][j].variables(),variables,less_than_comparator_variable);
				if ((int)variables.size() <= i_bound)
				{
					found=true;
					loc=k;
					break;
				}
			}
			// If there already exists a mini-bucket for the function, put the function in it
			if(found)
			{
				Function func;
				Function::dummy_product(curr_bucket[loc],buckets[var][j],func);
				if(allzeros(func))
				{
					cerr<<"All zeros \n";
					curr_bucket[loc].print();
					buckets[var][j].print();
					func.print();
					exit(1);
				}
				curr_bucket[loc]=func;
			}
			// Else create a new mini-bucket and put the function in it
			else
			{
				curr_bucket.push_back(buckets[var][j]);
			}
		}
		/*
		cout<<"Size = "<<curr_bucket.size()<<endl;
		*/
		//if(curr_bucket.size() > 1)
		//{
		//	for(int j=0;j<curr_bucket.size();j++)
		//	{
		//		curr_bucket[j].print(cout);
		//	}
		//}
		buckets[var]=curr_bucket;
		// Create new functions by marginalizing out the bucket variable from the function and put the function in appropriate bucket
		for(int j=0;j<curr_bucket.size();j++)
		{
			
			vector<Variable*> marg_variables;
			vector<Variable*> curr_variable;
			curr_variable.push_back(gm.variables[var]);
			do_set_difference(curr_bucket[j].variables(),curr_variable,marg_variables,less_than_comparator_variable);
			if(marg_variables.empty())
				continue;
			int min_size=(int)gm.variables.size();

			for(int k=0;k<marg_variables.size();k++)
			{
				int curr_var=marg_variables[k]->id();
				if(mapped_order[curr_var] < min_size)
				{
					min_size=mapped_order[curr_var];
				}		
			}
			assert(mapped_order[var] < min_size);
			assert(min_size < (int)gm.variables.size());
			buckets[order[min_size]].push_back(Function());
			vector<Function*> funcs;
			funcs.push_back(&curr_bucket[j]);
			int size=(int)buckets[order[min_size]].size()-1;
			Function::dummy_multiplyMarginalize(marg_variables,funcs,buckets[order[min_size]][size]);
			if(allzeros(buckets[order[min_size]][size]))
				{
					cerr<<"All zeros marg\n";
					curr_bucket[j].print();
					//buckets[var][j].print();
					//func.print();
					exit(1);
				}
			//Function::multiplyAndMarginalize(marg_variables,funcs,buckets[order[min_size]][size]);
		}
	}
	//for(int i=0;i<gm.variables.size();i++)
	//	assert(buckets[i].size()<=1);
	cerr<<"DRC done\n";
}

// Return consistent if the current assignment is consistent based on the mini-buckets assoc
bool DRC::isConsistent(int var, int value)
{
	
	assert(gm.variables[var]->value()==INVALID_VALUE);
	gm.variables[var]->value()=value;
	
	//Check if the current value is set
	for(int i=0;i<buckets[var].size();i++)
	{
		//buckets[var][i].print();
		int entry=Variable::getAddress(buckets[var][i].variables());
		for(int j=0;j<buckets[var][i].variables().size();j++)
			assert(buckets[var][i].variables()[j]->value()!=INVALID_VALUE);
		if(buckets[var][i].table()[entry].isZero())
		{
			gm.variables[var]->value()=INVALID_VALUE;
			//ret_val=false;
			return false;
		}
	}
	gm.variables[var]->value()=INVALID_VALUE;
	return true;
}

void DRC::domainConsistency(int var,vector<bool>& new_domain)
{
	new_domain=vector<bool> (gm.variables[var]->domain_size());
	for(int i= 0; i<new_domain.size();i++)
		new_domain[i]=true;
	vector<Variable*> marg_variable;
	marg_variable.push_back(gm.variables[var]);
	for(int i=0;i<buckets[var].size();i++)
	{
		Function function;
		buckets[var][i].marginalize(marg_variable,function);
		for(int j=0;j<function.table().size();j++)
		{
			if(function.table()[j].isZero())
			{
				new_domain[j]=false;
			}
		}
	}
	for(int i=0;i<new_domain.size();i++)
	{
		if(!new_domain[i])
		{
			cout<<"Domain of variable "<<var<<"Reduced\n";
			return;
		}
	}
}

void DRC::reduceFunction(Function* function)
{
	bool reduced=false;
	int num_zeros=0;
	for(int i=0;i<buckets.size();i++)
	{
		for(int j=0;j<buckets[i].size();j++)
		{
			vector<Variable*> temp_variables;
			do_set_intersection(function->variables(),buckets[i][j].variables(),temp_variables,less_than_comparator_variable);
			if(temp_variables.empty())
				continue;
			Function temp_func;
			buckets[i][j].marginalize(temp_variables,temp_func);
			function->project(temp_func);
		}
	}
	
}