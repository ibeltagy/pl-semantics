#include "GrayCode.h"
#include <ext/hash_map>
using namespace std;
using namespace __gnu_cxx;

GrayCode::GrayCode(const vector<Variable*>& variables_,const vector<Function*>& functions_): 
variables(variables_),
functions(functions_),
j(0),
F(vector<int> (variables_.size()+1)),
O(vector<int> (variables_.size())),
A(vector<int> (variables_.size())),
M(vector<int> (variables_.size())),
vars_to_functions(vector<vector<int> > (variables_.size())),
vars_to_functions_multipliers(vector<vector<int> > (variables_.size())),
address_of_functions(vector<int> (functions_.size())),
first(true)
{
	
	//cout<<"\t\t\t initing gray code\n";
	for(int i=0;i<variables.size();i++)
	{
		A[i]=0;
		F[i]=i;
		O[i]=1;
		M[i]=variables[i]->domain_size();
	}
	F[variables.size()]=(int)variables.size();
	hash_map<int,int> co_ordinates(variables.size());
	for(int i=0;i<variables.size();i++)
	{
		co_ordinates[variables[i]->id()]=i;
		variables[i]->addr_value()=0;
	}
	for(int i=0;i<functions.size();i++)
	{
		int multiplier=1;
		address_of_functions[i]=0;
		for(int j=0;j<functions[i]->variables().size();j++)
		{
			int var_id=co_ordinates[functions[i]->variables()[j]->id()];
			vars_to_functions[var_id].push_back(i);
			vars_to_functions_multipliers[var_id].push_back(multiplier);
			multiplier*=functions[i]->variables()[j]->domain_size();
		}
	}
}

void GrayCode::incrementAddress()
{
	if(first)
	{
		first=false;
		return;
	}
	j=F[0];
	F[0]=0;
	if(j==(int)variables.size())
	{
		// Code should not reach here
		cerr<<"Should not reach here\n";
		return;
	}
	A[j]=A[j]+O[j];
	if(A[j]==0 || A[j]==(M[j]-1))
	{
		O[j]=-O[j];
		F[j]=F[j+1];
		F[j+1]=j+1;
	}
	
	// Now multiply val by appropriate values
	for(int k=0;k<vars_to_functions[j].size();k++)
	{
		int func_id=vars_to_functions[j][k];
		address_of_functions[func_id]-=(vars_to_functions_multipliers[j][k]*variables[j]->addr_value());
		address_of_functions[func_id]+=(vars_to_functions_multipliers[j][k]*A[j]);
	}
	
	
}
GrayCodeP::GrayCodeP(const vector<Variable*>& variables_,const vector<Function*>& functions_, Function* function):
GrayCode(variables_,functions_),f(function),address_of_f(0),vars_to_f_multipliers(vector<int> (variables_.size()))
{
	f->variables()=variables;
	f->table()=vector<Double> (Variable::getDomainSize(f->variables()));
	int mult=1;
	for(int i=0;i<variables.size();i++)
	{
		vars_to_f_multipliers[i]=mult;
		variables[i]->addr_value()=0;
		mult*=variables[i]->domain_size();
	}
}
void GrayCodeP::moveForward()
{
	if(first)
	{
		incrementAddress();
		f->table()[address_of_f]=Double(1.0);
		for(int i=0;i<functions.size();i++)
		{
			f->table()[address_of_f]*=functions[i]->table()[address_of_functions[i]];
		}
		return;
	}
	incrementAddress();
	address_of_f-=(vars_to_f_multipliers[j]*variables[j]->addr_value());
	address_of_f+=(vars_to_f_multipliers[j]*A[j]);
	f->table()[address_of_f]=Double(1.0);
	for(int i=0;i<functions.size();i++)
	{
		f->table()[address_of_f]*=functions[i]->table()[address_of_functions[i]];
	}
	variables[j]->addr_value()=A[j];
}

GrayCodePM::GrayCodePM(const vector<Variable*>& variables_,const vector<Function*>& functions_, Function* function_, 
		   const vector<Variable*>& marg_variables): GrayCode(variables_,functions_),
		   f(function_),
		   address_of_f(0),
		   vars_to_f_multipliers(vector<int> (variables_.size()))
{
	//cout<<"\t\t\t initing gray codepm \n";
	f->variables()=marg_variables;
	f->table()=vector<Double> (Variable::getDomainSize(f->variables()));
	hash_map<int,int> co_ordinates;
	for(int i=0;i<variables.size();i++)
	{
		co_ordinates[variables[i]->id()]=i;
		vars_to_f_multipliers[i]=INVALID_VALUE;
		variables[i]->addr_value()=0;
	}
	int mult=1;
	for(int i=0;i<f->variables().size();i++)
	{
		int var_id=co_ordinates[f->variables()[i]->id()];
		vars_to_f_multipliers[var_id]=mult;
		mult*=f->variables()[i]->domain_size();
	}
}

void GrayCodePM::moveForward()
{
	if(first)
	{
		incrementAddress();
		Double value(1.0);
		for(int i=0;i<functions.size();i++)
		{
			value*=functions[i]->table()[address_of_functions[i]];
		}
		f->table()[address_of_f]+=value;
		return;
	}
	incrementAddress();
	if(vars_to_f_multipliers[j]>0)
	{
		address_of_f-=(vars_to_f_multipliers[j]*variables[j]->addr_value());
		address_of_f+=(vars_to_f_multipliers[j]*A[j]);
	}
	Double value(1.0);
	for(int i=0;i<functions.size();i++)
	{
		value*=functions[i]->table()[address_of_functions[i]];
	}
	f->table()[address_of_f]+=value;
	variables[j]->addr_value()=A[j];
}
GrayCodeD::GrayCodeD(const vector<Variable*>& variables_,const vector<Function*>& functions_, Function* function):
GrayCode(variables_,functions_),f(function),address_of_f(0),vars_to_f_multipliers(vector<int> (variables_.size()))
{
	f->variables()=variables;
	f->table()=vector<Double> (Variable::getDomainSize(f->variables()));
	int mult=1;
	for(int i=0;i<variables.size();i++)
	{
		variables[i]->addr_value()=0;
		vars_to_f_multipliers[i]=mult;
		mult*=variables[i]->domain_size();
	}
}
void GrayCodeD::moveForward()
{
	if(first)
	{
		incrementAddress();
		for(int i=0;i<functions.size();i++)
		{
			if(i==0)
				f->table()[address_of_f]=functions[i]->table()[address_of_functions[i]];
			else
				f->table()[address_of_f]/=functions[i]->table()[address_of_functions[i]];
		}
		return;
	}
	incrementAddress();
	address_of_f-=(vars_to_f_multipliers[j]*variables[j]->addr_value());
	address_of_f+=(vars_to_f_multipliers[j]*A[j]);
	
	for(int i=0;i<functions.size();i++)
	{
		if(i==0)
			f->table()[address_of_f]=functions[i]->table()[address_of_functions[i]];
		else
			f->table()[address_of_f]/=functions[i]->table()[address_of_functions[i]];
	}
	variables[j]->addr_value()=A[j];
}
