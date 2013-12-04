#include "SF.h"
#include <cassert>
using namespace std;

SF::SF(CPT& cpt)
{
	variables_=cpt.variables();
	cond_variables_=cpt.cond_variables();
	marg_variable_=cpt.marg_variable();
	table_=cpt.table();
	int cond_num_values=Variable::getDomainSize(cpt.cond_variables());
	sampling_table=vector<vector<Double> > (cond_num_values);
	for(int i=0;i<cond_num_values;i++)
	{
		Variable::setAddress(cond_variables_,i);
		Double prev_val;
		sampling_table[i]=vector<Double> (marg_variable()->domain_size());
		for(int j=0;j<marg_variable()->domain_size();j++)
		{
			marg_variable()->addr_value()=j;
			int address=Variable::getAddress(variables_);
			prev_val+=cpt.table()[address];
			sampling_table[i][j]=prev_val;
		}
	}
}
void SF::getSample(int& value, Double& weight,myRandom& random)
{
	//Make sure that all cond variables are assigned a value
	//for(int i=0;i<cond_variables().size();i++)
		//assert(cond_variables()[i]->value()!=INVALID_VALUE);
	int address=Variable::getAddress(cond_variables());
	Double sampled_value(random.getDouble());
	if(address >=sampling_table.size())
	{
		cout<<"Address= "<<address<<endl;
		cout<<marg_variable_->id()<<" | ";
		cout<<"Cond-variable-num-values = "<<Variable::getDomainSize(cond_variables_)<<endl;
		this->print();
	}
	assert(address<sampling_table.size());
	vector<Double>::iterator pos;
	pos=lower_bound(sampling_table[address].begin(),sampling_table[address].end(),sampled_value);
	value=pos-sampling_table[address].begin();
	if(value >= (int) marg_variable_->domain_size())
	  value=(int)marg_variable_->domain_size()-1;
	marg_variable_->addr_value()=value;
	weight=table_[Variable::getAddress(variables_)];
	//cerr<<marg_variable_->id()<<" "<<value<<" "<<weight<<endl;
}
