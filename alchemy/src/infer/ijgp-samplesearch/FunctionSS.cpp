#include "FunctionSS.h"
#include "myRandom.h"

#include <cassert>

namespace ss{

void Function::reduceDomains()
{
	int new_num_values=Variable::getDomainSize(variables_);

#ifdef WITH_TABLE
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
#else
	vector<int> old_domain_addr_vals = vector<int>(variables_.size());
	int address=tableFalseEntry_;
	for(int i=0;i<variables_.size();i++)
	{
		old_domain_addr_vals[i] = address%variables_[i]->old_domain.size();
		address/=variables_[i]->old_domain.size();
	}

	int new_tableFalseEntry=0;
	int multiplier=1;
	for(int i=0;i<variables_.size();i++)
	{
		new_tableFalseEntry += (multiplier*variables_[i]->inverse_mapping[old_domain_addr_vals[i]]);
		multiplier *= variables_[i]->domain_size();
	}
	//cout << tableSize_<<", "<< new_num_values << ", "<< tableFalseEntry_ << ", " << new_tableFalseEntry <<endl;
	tableFalseEntry_ = new_tableFalseEntry;
	tableSize_ = new_num_values;

#endif
}
void Function::removeEvidence()
{
	vector<Variable*> other_variables;
	for(int i=0;i<variables_.size();i++)
		if(variables_[i]->value()==INVALID_VALUE)
			other_variables.push_back(variables_[i]);

	int other_num_values=Variable::getDomainSize(other_variables);
#ifdef WITH_TABLE
	//cout <<variables_.size() <<", " << other_variables.size() << ", "<< tableSize_ <<", "<<other_num_values<< ", " << tableFalseEntry_ <<" -->" ;
	vector<Double> new_table(other_num_values);
	for(int j=0;j<other_num_values;j++)
	{
		Variable::setAddress(other_variables,j);
		int entry=Variable::getAddress(variables_);
		new_table[j]=tableEntry(entry);
		//cout << new_table[j] << ", " ;
	}
	//cout << endl;
	table_=new_table;
#else
	//cout <<variables_.size() <<", " << other_variables.size() << ", "<< tableSize_ <<", "<<other_num_values<< ", " << tableFalseEntry_ <<", " << endl;
	//Variable::setAddress(variables_,tableFalseEntry_);
	//cout << Variable::getAddress(other_variables) << endl;
	//tableFalseEntry_ = Variable::getAddress(other_variables);
	tableSize_ = other_num_values;
#endif
	variables_=other_variables;
}


/*
void Function::product(Function& function)
{
#ifdef WITH_TABLE
	if(function.tableSize() == 0 || function.variables().empty())
		return;
	vector<Variable*> new_variables;
	do_set_union(variables(),function.variables(),new_variables,less_than_comparator_variable);
	int num_values=Variable::getDomainSize(new_variables);
	if(new_variables.size()==variables().size())
	{
		for(int i=0;i<num_values;i++)
		{
			Variable::setAddress(variables(),i);
			int func_entry=Variable::getAddress(function.variables());
			tableEntry(i)*=function.tableEntry(func_entry);
		}
	}
	else
	{
		vector<Double> old_table;
		old_table=table_;
		table_=vector<Double> (num_values);
		for(int i=0;i<num_values;i++)
		{
			Variable::setAddress(new_variables,i);
			int entry=Variable::getAddress(variables());
			int func_entry=Variable::getAddress(function.variables());
			tableEntry(i)=Double(function.tableEntry(func_entry)*old_table[entry]);
		}
		variables_=new_variables;
	}
	normalize();
#else
	assert(false && "Function::product withoutTable is not implemented yet");
#endif
}
void Function::dummy_divide(Function& f1, Function& f2, Function& f3)
{
	f3=f1;
	f3.divide(f2);
	f3.normalize();
}

void Function::divide(Function& function)
{
	if(function.tableSize() == 0 || function.variables().empty())
		return;
	vector<Variable*> new_variables;
	do_set_union(variables(),function.variables(),new_variables,less_than_comparator_variable);
	if(new_variables.size() > variables().size())
	{
		return;
	}
	for(int i=0;i<tableSize();i++)
	{
			Variable::setAddress(variables(),i);
			int func_entry=Variable::getAddress(function.variables());
			tableEntry(i)/=function.tableEntry(func_entry);
	}
	normalize();
}

void Function::marginalize(vector<Variable*>& marg_variables_,Function& function)
{
	do_set_intersection(variables_,marg_variables_,function.variables(),less_than_comparator_variable);
	if(function.variables().empty())
		return;
	function.tableInit(Variable::getDomainSize(function.variables()));
	for(int i=0;i<this->tableSize();i++)
	{
		Variable::setAddress(variables_,i);
		int entry=Variable::getAddress(function.variables());
		function.tableEntry(entry)+=this->tableEntry(i);
	}
	function.normalize();
}
*/
void Function::normalize()
{
	Double norm_const;
	for(int i=0;i<this->tableSize();i++)
	{
		norm_const+=this->tableEntry(i);
	}
	for(int i=0;i<this->tableSize();i++)
	{
		this->tableEntry(i) /=norm_const;
	}
}

void Function::project(Function& function)
{
#ifdef WITH_TABLE
cout << "in project" << endl;
	assert(function.tableSize() <= this->tableSize());
	if(function.tableSize() == 0 || function.variables().empty())
		return;
	vector<Variable*> new_variables;
	do_set_union(variables(),function.variables(),new_variables,less_than_comparator_variable);
	assert(new_variables.size() == variables_.size());
	int num_values=Variable::getDomainSize(new_variables);
	if(new_variables.size()==variables().size())
	{
		for(int i=0;i<num_values;i++)
		{
			Variable::setAddress(variables(),i);
			int func_entry=Variable::getAddress(function.variables());
			if(function.tableEntry(func_entry).isZero())
			{
				tableEntry(i)=Double();
			}
		}
	}
	else
	{
		//vector<Double> old_table;

		//old_table=table_;
		//table_=vector<Double> (num_values);

		for(int i=0;i<num_values;i++)
		{

			Variable::setAddress(variables_,i);
			int func_entry=Variable::getAddress(function.variables());

			if(function.tableEntry(func_entry).isZero())
			{

				tableEntry(i)=Double();
			}

		}
	}
#else
	assertStacktrace(false && "Function::project is not implemented yet");
#endif

}
void Function::dummy_multiplyMarginalize(vector<Variable*>& marg_variables_, vector<Function*>& functions, Function& f)
{
#ifdef WITH_TABLE
cout << "in dummy_multiplyMarginalize " << endl;
	// First iterate through the 
	vector<Variable*> variables;
	vector<Variable*> marg_variables;
	//cout<<"Computing marginals\n";
	for(int i=0;i<functions.size();i++)
	{
		do_set_union(variables,functions[i]->variables(),variables,less_than_comparator_variable);
	}
	
	do_set_intersection(variables,marg_variables_,marg_variables,less_than_comparator_variable);
	int num_values=Variable::getDomainSize(variables);
	f=Function();
	f.variables()=marg_variables;
	f.tableInit(Variable::getDomainSize(marg_variables));
	for(int i=0;i<num_values;i++)
	{
		Variable::setAddress(variables,i);
		Double value(1.0);
		for(int j=0;j<functions.size();j++)
		{
			
			int func_entry=Variable::getAddress(functions[j]->variables());
			assert(func_entry < (int)functions[j]->tableSize());
			value*=functions[j]->tableEntry(func_entry);
		}
		assert(f.tableSize() > Variable::getAddress(marg_variables));
		
		f.tableEntry(Variable::getAddress(marg_variables))+=value;
		
	}
	
	Double norm_const;
	for(int i=0;i<f.tableSize();i++)
	{
		norm_const+=f.tableEntry(i);
	}
	for(int i=0;i<f.tableSize();i++)
	{
		f.tableEntry(i)/=norm_const;
	}
#else
	assertStacktrace(false && "Function::dummy_multiplyMarginalize is not implemented yet");
#endif
}

void Function::dummy_product(Function& f1, Function& f2, Function& f3)
{
#ifdef WITH_TABLE
cout << "in dummy_product 2" << endl;
	do_set_union(f1.variables(),f2.variables(),f3.variables(),less_than_comparator_variable);
	int num_values=Variable::getDomainSize(f3.variables());
	f3.tableInit(num_values);
	for(int i=0;i<num_values;i++)
	{
		Variable::setAddress(f3.variables(),i);
		int add1,add2;
		add1=Variable::getAddress(f1.variables());
		add2=Variable::getAddress(f2.variables());
		//cout<<add1<<" "<<add2<<" val = ";
		//cout<<f1.table()[add1]<<" "<<f2.table()[add2]<<" ";
		f3.tableEntry(i)=f1.tableEntry(add1);
		f3.tableEntry(i)*=f2.tableEntry(add2);
		//cout<<f3.table()[i]<<endl;
	}
#else
	assertStacktrace(false && "Function::dummy_product is not implemented yet");
#endif

}


/*
void Function::multiplyAndMarginalize(vector<Variable*>& marg_variables_, vector<Function*>& functions, Function& new_func)
{
	vector<Variable*> variables;
	vector<Variable*> marg_variables;
	for(int i=0;i<functions.size();i++)
	{
		do_set_union(variables,functions[i]->variables(),variables,less_than_comparator_variable);
	}
	do_set_intersection(variables,marg_variables_,marg_variables,less_than_comparator_variable);
	//cout<<"num-marg-vars = "<<marg_variables.size()<<" ";
	if(marg_variables.empty()||variables.empty())
		return;

	int num_variables=variables.size();
	int num_functions=functions.size();
	int num_marg_variables=marg_variables.size();
	
	// Compute gray index for all variables and functions
	vector<vector<pair<int,int> > > gray_index(num_variables);
	int old_temp_value[num_variables];
	for(int i=0;i<num_variables;i++)
	{
		old_temp_value[i]=variables[i]->temp_value;
		variables[i]->temp_value=i;
	}
	for(int i=0;i<num_functions;i++)
	{
		int multiplier=1;
		for(int j=0;j<functions[i]->variables().size();j++){
			gray_index[functions[i]->variables()[j]->temp_value].push_back(pair<int,int>(i,multiplier));
			multiplier*=functions[i]->variables()[j]->domain_size();
		}
	}

	//Initialize the data structure for gray code
	int a[num_variables];
	int f[num_variables+1];
	int o[num_variables+1];
	int m[num_variables];

	for(int i=0;i<num_variables;i++)
	{
		a[i]=0;
		f[i]=i;
		o[i]=1;
		m[i]=variables[i]->domain_size();
	}
	f[num_variables]=num_variables;
	new_func.variables()=marg_variables;
	new_func.tableInit(Variable::getDomainSize(marg_variables));
	int func_address[num_functions];
	int marg_address=0;
	Double mult(1.0);
	for(int i=0;i<num_functions;i++)
	{
		if(!functions[i]->tableSize() == 0)
			mult*=functions[i]->tableEntry(0);
		func_address[i]=0;
	}
	int domain_size=Variable::getDomainSize(variables);
	int gray_marg_index[num_variables];
	
	for(int i=0;i<num_variables;i++)
		gray_marg_index[i]=0;
	int multiplier=1;
	for(int i=0;i<new_func.variables().size();i++)
	{
		gray_marg_index[new_func.variables()[i]->temp_value]=multiplier;
		multiplier*=new_func.variables()[i]->domain_size();
	}
	for(int i=0;i<domain_size;i++)
	{
		new_func.tableEntry(marg_address)+=mult;
		//cout<<marg_address<<" "<<mult<<" ";
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
		for(int k=0;k<gray_index[j].size();k++)
		{
			int index=gray_index[j][k].first;
			int addr_multiplier=gray_index[j][k].second;
			mult/=functions[index]->tableEntry(func_address[index]);
			func_address[index]-=addr_multiplier*old_aj;
			func_address[index]+=addr_multiplier*a[j];
			mult*=functions[index]->tableEntry(func_address[index]);
		}
		// Hack
		mult=Double(1.0);
		for(int k=0;k<num_functions;k++)
		{
			mult*=functions[k]->tableEntry(func_address[k]);
		}
		//End Hack
		if(gray_marg_index[j]>0)
		{
			marg_address-=gray_marg_index[j]*old_aj;
			marg_address+=gray_marg_index[j]*a[j];
		}
	}
	new_func.normalize();
	//cout<<endl;
	//cout<<"\tGray code\n";
	//if(variables.empty())
	//	return;
	//GrayCodePM gray_code(variables,functions,&f,marg_variables);
	//int num_values=Variable::getDomainSize(variables);

	////cout<<"\tMoving forward\n";

	//for(int i=0;i<num_values;i++)
	//{
	//	
	//	gray_code.moveForward();
	//}
	//Double norm_const;
	//for(int i=0;i<f.table().size();i++)
	//{
	//	norm_const+=f.table()[i];
	//}
	//for(int i=0;i<f.table().size();i++)
	//{
	//	f.table()[i]/=norm_const;
	//}
	//cout<<"Done\n";
}*/
/*
void Function::divide(Function& f1, Function& f2, Function& f3)
{
	vector<Variable*> variables;
	vector<Function*> functions;
	functions.push_back(&f1);functions.push_back(&f2);
	for(int i=0;i<functions.size();i++)
	{
		do_set_union(variables,functions[i]->variables(),variables,less_than_comparator_variable);
	}
	
	GrayCodeD gray_code(variables,functions,&f3);
	int num_values=Variable::getDomainSize(variables);
	for(int i=0;i<num_values;i++)
	{
		gray_code.moveForward();
	}
}

void Function::product(vector<Function*>& functions, Function& f)
{
	vector<Variable*> variables;
	for(int i=0;i<functions.size();i++)
	{
		do_set_union(variables,functions[i]->variables(),variables,less_than_comparator_variable);
	}
	int num_values=Variable::getDomainSize(variables);
	GrayCodeP gray_code(variables,functions,&f);
	for(int i=0;i<num_values;i++)
	{
		gray_code.moveForward();
	}
}
*/


void Function::print(ostream& out)
{
	out<<"Variables: ";
	for(int i=0;i<variables_.size();i++)
		out<<variables_[i]->id()<<" ";
	out<<endl;
	for(int i=0;i<this->tableSize();i++)
		out<<tableEntry(i)<<" ";
	out<<endl;
}
double Function::MSE(Function& function)
{
	double error=0.0;
	if(function.tableSize()!=this->tableSize())
		return -1;
	for(int i=0;i<this->tableSize();i++)
	{
		error+=((function.tableEntry(i).value()-this->tableEntry(i).value())*(function.tableEntry(i).value()-this->tableEntry(i).value()));
	}
	return error;

}
double Function::MSE(Function* f1, Function* f2)
{
	double error=0.0;
	if(f1->tableSize()!=f2->tableSize())
		return -1;
	for(int i=0;i<f1->tableSize();i++)
	{
		error+=((f1->tableEntry(i).value()-f2->tableEntry(i).value())*(f1->tableEntry(i).value()-f2->tableEntry(i).value()));
	}
	return error;
}
}
