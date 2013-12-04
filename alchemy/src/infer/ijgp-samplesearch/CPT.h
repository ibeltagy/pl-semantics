#ifndef CPT_H_
#define CPT_H_

#include <vector>
#include "Function.h"
#include <cassert>

using namespace std;
struct CPT: public Function
{
protected:
	vector<Variable*> cond_variables_;
	Variable* marg_variable_;
public:
	CPT() { }
	CPT(int& id,vector<Variable*>& variables, Variable* marg_variable): Function(id,variables),marg_variable_(marg_variable)
	{
		for(vector<Variable*>::iterator curr=variables.begin();curr!=variables.end();curr++)
		{
			if(*curr==marg_variable_)
				continue;
			cond_variables_.push_back(*curr);
		}
	}
	CPT(Function& function, Variable* marg_variable__, vector<Variable*>& cond_variables__)
	{
		cond_variables_=cond_variables__;
		marg_variable_=marg_variable__;
		variables_=function.variables();
		table_=function.table();
		vector<Variable*> test_vars;
		do_set_intersection(function.variables(),cond_variables_,test_vars,less_than_comparator_variable);
		assert((int)test_vars.size()==(int)cond_variables_.size());
		do_set_difference(function.variables(),cond_variables_,test_vars,less_than_comparator_variable);
		assert((int) test_vars.size() >0);
		assert(test_vars[0]->id()==marg_variable_->id());
		int cond_num_values=Variable::getDomainSize(cond_variables_);
		table_=vector<Double> (function.table().size());
		for(int i=0;i<cond_num_values;i++)
		{
			Variable::setAddress(cond_variables_,i);
			Double norm_const;
			for(int j=0;j<marg_variable_->domain_size();j++)
			{
				marg_variable_->addr_value()=j;
				int address=Variable::getAddress(variables_);
				norm_const+=function.table()[address];
			}
			for(int j=0;j<marg_variable_->domain_size();j++)
			{
				marg_variable_->addr_value()=j;
				int address=Variable::getAddress(variables_);
				table_[address]=function.table()[address]/norm_const;
			}
		}
	}
	~CPT(){ }
	Variable* marg_variable() { return marg_variable_;}
	void setMargVariable(Variable* marg_var) { marg_variable_=marg_var;}
	vector<Variable*>& cond_variables() { return cond_variables_;}
	void getSample(pair<int,Double>& sample,Double& rand_number);
	void epsilonCorrection(Double epsilon=Double(0.1))
	{
		int cond_num_values=Variable::getDomainSize(cond_variables_);
		//table_=vector<Double> (function.table().size());
		for(int i=0;i<cond_num_values;i++)
		{
			Variable::setAddress(cond_variables_,i);
			Double norm_const;
			for(int j=0;j<marg_variable_->domain_size();j++)
			{
				marg_variable_->addr_value()=j;
				int address=Variable::getAddress(variables_);
				if(table()[address].isZero())
					continue;
				if(table()[address]<epsilon)
				{
					table()[address]=epsilon;
				}
				norm_const+=table()[address];
			}
			for(int j=0;j<marg_variable_->domain_size();j++)
			{
				marg_variable_->addr_value()=j;
				int address=Variable::getAddress(variables_);
				table_[address]/=norm_const;
			}
		}
	}
	void reduceDomains()
	{
		int new_num_values=Variable::getDomainSize(variables_);
		vector<Double> new_table(new_num_values);
		for(int i=0;i<new_num_values;i++)
		{
			Variable::setAddress(variables_,i);
			int address=0;
			int multiplier=1;
			for(int j=0;j<variables_.size();j++)
			{
				address+=(multiplier*variables_[j]->mapping[variables_[j]->addr_value()]);
				multiplier*=(int)variables_[j]->old_domain.size();
			}
			new_table[i]=table_[address];
		}
		table_=new_table;
		//cout<<table_.size()<<endl;
	}
	void removeEvidence()
	{

		vector<Variable*> other_variables;
		for(int i=0;i<variables_.size();i++)
			if(variables_[i]->value()==INVALID_VALUE)
				other_variables.push_back(variables_[i]);
		//cout<<other_variables.size()<<endl;
		int other_num_values=Variable::getDomainSize(other_variables);
		vector<Double> new_table(other_num_values);
		for(int j=0;j<other_num_values;j++)
		{
			
			Variable::setAddress(other_variables,j);
			int entry=Variable::getAddress(variables_);
	
			new_table[j]=table()[entry];
		}

		variables_=other_variables;
		table_=new_table;
		do_set_intersection(cond_variables_,other_variables,cond_variables_,less_than_comparator_variable);
		if(marg_variable_==NULL)
			return;
		vector<Variable*> marg_variables;
		marg_variables.push_back(marg_variable_);
		do_set_intersection(marg_variables,other_variables,marg_variables,less_than_comparator_variable);
		if(marg_variables.empty())
			marg_variable_=NULL;
		

	}
};

#endif
