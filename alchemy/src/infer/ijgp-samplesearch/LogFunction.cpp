#include "LogFunction.h"
#include "Function.h"

void LogFunction::print(ostream& out)
{
	for(int i=0;i<variables_.size();i++)
		out<<variables_[i]->id()<<" ("<<variables_[i]->domain_size()<<") ";
	out<<endl;
}
LogFunction::LogFunction(Function& function)
{
	if(function.variables().empty() && function.table().empty())
	{
		variables_=vector<Variable*>();
		log_table=vector<LogDouble>(1);
		log_table[0]=LogDouble(1.0);
		return;
	}
	//Remove all variables that have been assignend
	for(int i=0;i<function.variables().size();i++)
	{
		if(function.variables()[i]->value()==INVALID_VALUE)
			variables_.push_back(function.variables()[i]);
	}
	if(variables_.empty())
	{
		log_table=vector<LogDouble> (1);
		Double tab;
		log_table[0]=LogDouble(function.table()[Variable::getAddress(function.variables())]);
		return;
	}
	if(variables_.size()==function.variables().size())
	{
		log_table=vector<LogDouble>(function.table().size());
		for(int i=0;i<log_table.size();i++)
			log_table[i]=LogDouble(function.table()[i]);
	}
	else
	{
		int domain_size=Variable::getDomainSize(variables_);
		log_table=vector<LogDouble>(domain_size);
		int num_variables=variables_.size();
		int g[num_variables];
		int a[num_variables];
		int f[num_variables+1];
		int o[num_variables];
		int m[num_variables];

		int multiplier=1;
		for(int i=0;i<num_variables;i++)
		{
			a[i]=0;
			g[i]=multiplier;
			f[i]=i;
			o[i]=1;
			m[i]=variables_[i]->domain_size();
			multiplier*=m[i];
		}
		f[num_variables]=num_variables;
		int h[num_variables];
		int func_address=0;
		multiplier=1;
		int k=0;
		for(int i=0;i<function.variables().size();i++)
		{
			if(function.variables()[i]->value()!=INVALID_VALUE)
				func_address+=(multiplier*function.variables()[i]->value());
			else
			{
				assert(k<num_variables);
				h[k++]=multiplier;
			}
			multiplier*=function.variables()[i]->domain_size();
		}
		int address=0;
		
		while(1)
		{
			log_table[address]=LogDouble(function.table()[func_address]);
			int j=f[0];
			f[0]=0;

			if(j==num_variables) break;
			int old_aj=a[j];
			a[j]=a[j]+o[j];
			if(a[j]==0 || a[j]==(m[j]-1))
			{
				o[j]=-o[j];
				f[j]=f[j+1];
				f[j+1]=j+1;
			}
			address-=(g[j]*old_aj);
			address+=(g[j]*a[j]);
			func_address-=(h[j]*old_aj);
			func_address+=(h[j]*a[j]);
		}
	}
}
void LogFunction::multiplyAndMarginalize(vector<Variable*>& marg_variables_,vector<LogFunction*>& functions, Function& out_function, bool normalize)
{
	//cout<<"Mult\n";
	// Compute the appropriate variables from the two functions
	vector<Variable*> variables;
	vector<Variable*> marg_variables;
	int num_functions=functions.size();
	if(num_functions==0)
		return;
	for(int i=0;i<num_functions;i++)
	{
		do_set_union(variables,functions[i]->variables(),variables,less_than_comparator_variable);
	}
	int num_variables=variables.size();
	if(num_variables==0)
		return;
	do_set_intersection(variables,marg_variables_,marg_variables,less_than_comparator_variable);
	int num_marg_variables=marg_variables.size();
	/*if(num_marg_variables==0)
	{
		out_function.variables()=vector<Variable*>();
		Double tab;

		for(int i=0;i<functions.size();i++)
		{

		}
	}*/

	// 6 arrays for graycoding using Knuth's algorithm
	vector<vector< pair<int,int> > > g(num_variables);
	int c[num_functions];
	int a[num_variables];
	int f[num_variables+1];
	int o[num_variables];
	int m[num_variables];
	int t[num_variables];
	LogDouble mult(1.0);
	int address=0;

	// Init variables for graycoding
	for(int i=0;i<num_variables;i++)
	{
		a[i]=0;
		f[i]=i;
		o[i]=1;
		t[i]=0;
		m[i]=variables[i]->domain_size();
		variables[i]->temp_value=i;
	}
	for(int i=0;i<num_functions;i++)
		c[i]=0;
	f[num_variables]=num_variables;
	for(int i=0;i<num_functions;i++)
	{
		int multiplier=1;
		assert(functions[i]!=NULL);
		for(int j=0;j<functions[i]->variables().size();j++)
		{
			g[functions[i]->variables()[j]->temp_value].push_back(pair<int,int>(i,multiplier));
			multiplier*=functions[i]->variables()[j]->domain_size();
		}
		if(!functions[i]->log_table.empty())
		{
			mult+=functions[i]->log_table[0];
		}
	}
	int multiplier=1;
	//cout<<"mult here\n";
	for(int i=0;i<num_marg_variables;i++)
	{
		t[marg_variables[i]->temp_value]=multiplier;
		multiplier*=marg_variables[i]->domain_size();
	}
	//cout<<"mult initing log function\n";
	//Gray  code algorithm
	//Initialize LogFunction
	out_function.variables()=marg_variables;
	out_function.table()=vector<Double> (Variable::getDomainSize(marg_variables));
	//out_function.log_table=vector<LogDouble> (out_function.table().size());
	//cout<<"Log function inited\n";
	while(1)
	{
		//cout<<address<<endl;
		// Step 1: Visit
		out_function.table()[address]+=mult.toDouble();
		// Step 2: Choose j
		int j=f[0];
		f[0]=0;
		
		if(j==num_variables) break;
		int old_aj=a[j];
		a[j]=a[j]+o[j];
		if(a[j]==0 || a[j]==(m[j]-1))
		{
			o[j]=-o[j];
			f[j]=f[j+1];
			f[j+1]=j+1;
		}
		if(!mult.is_zero)
		{
			for(int i=0;i<g[j].size();i++)
			{
				int index=g[j][i].first;
				int multiplier=g[j][i].second;
				mult-=functions[index]->log_table[c[index]];
				c[index]-=multiplier*old_aj;
				c[index]+=multiplier*a[j];
				mult+=functions[index]->log_table[c[index]];
			}
		}
		else
		{
			for(int i=0;i<g[j].size();i++)
			{	
				int index=g[j][i].first;
				int multiplier=g[j][i].second;
				c[index]-=multiplier*old_aj;
				c[index]+=multiplier*a[j];
			}
			mult=LogDouble(1.0);
			for(int i=0;i<num_functions;i++)
				mult+=functions[i]->log_table[c[i]];
		}
		if(t[j]>0)
		{
			address-=t[j]*old_aj;
			address+=t[j]*a[j];
		}
	}
	if(normalize)
		out_function.normalize();
	//out_function.toLogTable();
	//cout<<"mult done\n";
}
void LogFunction::multiplyAndMarginalize(std::vector<Variable*> &marg_variables_, std::vector<LogFunction*> &functions, LogFunction &out_function, bool normalize)
{
	//cout<<"Mult\n";
	// Compute the appropriate variables from the two functions
	vector<Variable*> variables;
	vector<Variable*> marg_variables;
	int num_functions=functions.size();
	if(num_functions==0)
		return;
	for(int i=0;i<num_functions;i++)
	{
		do_set_union(variables,functions[i]->variables(),variables,less_than_comparator_variable);
	}
	int num_variables=variables.size();
	if(num_variables==0)
		return;
	do_set_intersection(variables,marg_variables_,marg_variables,less_than_comparator_variable);
	int num_marg_variables=marg_variables.size();
	//if(num_marg_variables==0)
	//	return;

	// 6 arrays for graycoding using Knuth's algorithm
	vector<vector< pair<int,int> > > g(num_variables);
	int c[num_functions];
	int a[num_variables];
	int f[num_variables+1];
	int o[num_variables];
	int m[num_variables];
	int t[num_variables];
	LogDouble mult(1.0);
	int address=0;

	// Init variables for graycoding
	for(int i=0;i<num_variables;i++)
	{
		a[i]=0;
		f[i]=i;
		o[i]=1;
		t[i]=0;
		m[i]=variables[i]->domain_size();
		variables[i]->temp_value=i;
	}
	for(int i=0;i<num_functions;i++)
		c[i]=0;
	f[num_variables]=num_variables;
	for(int i=0;i<num_functions;i++)
	{
		int multiplier=1;
		assert(functions[i]!=NULL);
		for(int j=0;j<functions[i]->variables().size();j++)
		{
			g[functions[i]->variables()[j]->temp_value].push_back(pair<int,int>(i,multiplier));
			multiplier*=functions[i]->variables()[j]->domain_size();
		}
		if(!functions[i]->log_table.empty())
		{
			mult+=functions[i]->log_table[0];
		}
	}
	int multiplier=1;
	//cout<<"mult here\n";
	for(int i=0;i<num_marg_variables;i++)
	{
		t[marg_variables[i]->temp_value]=multiplier;
		multiplier*=marg_variables[i]->domain_size();
	}
	//cout<<"mult initing log function\n";
	//Gray  code algorithm
	//Initialize LogFunction
	out_function.variables()=marg_variables;
	out_function.table()=vector<Double> (Variable::getDomainSize(marg_variables));
	out_function.log_table=vector<LogDouble> (out_function.table().size());
	//cout<<"Log function inited\n";
	while(1)
	{
		//cout<<address<<endl;
		// Step 1: Visit
		out_function.table()[address]+=mult.toDouble();
		// Step 2: Choose j
		int j=f[0];
		f[0]=0;
		
		if(j==num_variables) break;
		int old_aj=a[j];
		a[j]=a[j]+o[j];
		if(a[j]==0 || a[j]==(m[j]-1))
		{
			o[j]=-o[j];
			f[j]=f[j+1];
			f[j+1]=j+1;
		}
		if(!mult.is_zero)
		for(int i=0;i<g[j].size();i++)
		{
			int index=g[j][i].first;
			int multiplier=g[j][i].second;
			mult-=functions[index]->log_table[c[index]];
			c[index]-=multiplier*old_aj;
			c[index]+=multiplier*a[j];
			mult+=functions[index]->log_table[c[index]];
		}
		else
		{
			for(int i=0;i<g[j].size();i++)
			{	
				int index=g[j][i].first;
				int multiplier=g[j][i].second;
				c[index]-=multiplier*old_aj;
				c[index]+=multiplier*a[j];
			}
			mult=LogDouble(1.0);
			for(int i=0;i<num_functions;i++)
				mult+=functions[i]->log_table[c[i]];
		}
		if(t[j]>0)
		{
			address-=t[j]*old_aj;
			address+=t[j]*a[j];
		}
	}
	if(normalize)
		out_function.normalize();
	out_function.toLogTable();
	//cout<<"mult done\n";
}