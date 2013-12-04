#include "HashFunction.h"
#include "myRandom.h"

HashFunction::HashFunction(Function& function)
{
	variables()=function.variables();
	sparse_table_=sparsetable<Double> (function.table().size());
	for(int i=0;i<function.table().size();i++)
	{
		if(function.table()[i].isZero())
			continue;
		Variable::setAddress(function.variables(),i);
		relations_.push_back(vector<int>());
		for(int j=0;j<variables().size();j++)
		{
			relations_[relations_.size()-1].push_back(variables()[j]->addr_value());
		}
		sparse_table_[i]=function.table()[i];
	}
}
void HashFunction::print(ostream& out)
{
	out<<"Variables: ";
	for(int i=0;i<variables_.size();i++)
		out<<variables_[i]->id()<<" ";
	out<<endl;
	for(int i=0;i<table_.size();i++)
		out<<sparse_table_[i]<<" ";
	out<<"Relations: ";
	for(int i=0;i<relations_.size();i++)
	{
		out<<"[ ";
		for(int j=0;j<relations_[i].size();j++)
			out<<relations_[i][j]<<" ";
		out<<"] ";
	}
	out<<endl;
}
void HashFunction::product(HashFunction& function, HashFunction& out_function)
{
	if(variables().empty())
	{
		out_function=function;
		return;
	}
	out_function=HashFunction();
	vector<Variable*> common_variables;
	do_set_intersection(variables(),function.variables(),common_variables,less_than_comparator_variable);
	do_set_union(variables(),function.variables(),out_function.variables(),less_than_comparator_variable);

	out_function.stable()=sparsetable<Double>(Variable::getDomainSize(out_function.variables()));
	HashFunction *h1,*h2;
	if(function.relations().size()>=this->relations().size())
	{
		h1=this;
		h2=&function;
	}
	else
	{
		h1=&function;
		h2=this;
	}
	vector<int> common_indices_h1;
	int j=0;
	for(int i=0;i<h1->variables().size();i++)
	{
		if((int)common_variables.size()==j)
			break;
		if(h1->variables()[i]->id()==common_variables[j]->id())
		{
			j++;
			common_indices_h1.push_back(i);
		}
	}
	vector<int> common_indices_h2;
	j=0;
	for(int i=0;i<h2->variables().size();i++)
	{
		if((int)common_variables.size()==j)
			break;
		if(h2->variables()[i]->id()==common_variables[j]->id())
		{
			j++;
			common_indices_h2.push_back(i);
		}
	}
	// Hash on the common_variables
	sparse_hash_map<int,vector<int> > common_var_address;

	for(int i=0;i<h1->relations().size();i++)
	{
		for(int j=0;j<common_indices_h1.size();j++)
		{
			h1->variables()[common_indices_h1[j]]->addr_value()=h1->relations()[i][common_indices_h1[j]];
		}
		int curr_address=Variable::getAddress(common_variables);

		common_var_address[curr_address].push_back(i);
	}

	// Iterate on the larger relation
	for(int i=0;i<h2->relations().size();i++)
	{
		for(int j=0;j<h2->variables().size();j++)
		{
			h2->variables()[j]->addr_value()=h2->relations()[i][j];
		}
		int curr_address=Variable::getAddress(common_variables);

		Double val1=h2->stable()[Variable::getAddress(h2->variables())];


		if(common_var_address.find(curr_address)!=common_var_address.end())
		{
			for(int a=0;a<common_var_address[curr_address].size();a++)
			{
				int k=common_var_address[curr_address][a];
				for(int b=0;b<h1->variables().size();b++)
				{
					h1->variables()[b]->addr_value()=h1->relations()[k][b];
				}
				vector<int> final_relation(out_function.variables().size());
				for(int b=0;b<out_function.variables().size();b++)
				{
					final_relation[b]=out_function.variables()[b]->addr_value();
				}
				Double val2=h1->stable()[Variable::getAddress(h1->variables())];
				out_function.relations().push_back(final_relation);
				out_function.stable()[Variable::getAddress(out_function.variables())]=val1*val2;
			}
		}
	}
}

void HashFunction::product(vector<HashFunction*>& functions, HashFunction& out_function)
{
	out_function=HashFunction();
	if(functions.empty())
		return;
	HashFunction h1,h2;

	bool is_h1=false;
	for(int i=0;i<functions.size();i++)
	{
		if(i%2==0)
		{
			h1.product(*functions[i],h2);
			is_h1=false;
		}
		else
		{
			h2.product(*functions[i],h1);
			is_h1=true;
		}
	}
	if(is_h1)
		out_function=h1;
	else
		out_function=h2;
}
void HashFunction::multiplyAndMarginalize(vector<Variable*>& marg_variables,vector<HashFunction*>& functions, HashFunction* out_function)
{
	//cout<<"num func ="<<functions.size()<<endl;
	if(out_function==NULL)
		cerr<<"Something wrong\n";
	*out_function=HashFunction();
	if(functions.empty())
		return;
	HashFunction h1,h2;

	bool is_h1=false;
	for(int i=0;i<functions.size();i++)
	{
		//cout<<"product "<<i<<endl;
		if(i%2==0)
		{
			h1.product(*functions[i],h2);
			is_h1=false;
		}
		else
		{
			h2.product(*functions[i],h1);
			is_h1=true;
		}
	}
	// Credit fraud number 18004807230

	HashFunction& prod=(is_h1)?(h1):(h2);
	out_function->variables()=marg_variables;
	vector<int> marg_relation(marg_variables.size());
	out_function->stable()=sparsetable<Double>(Variable::getDomainSize(marg_variables));
	for(int i=0;i<prod.relations().size();i++)
	{
		for(int j=0;j<prod.relations()[i].size();j++)
		{
			prod.variables()[j]->addr_value()=prod.relations()[i][j];
		}
		int prod_address=Variable::getAddress(prod.variables());
		int address=Variable::getAddress(marg_variables);
		if(out_function->stable().get(address).isZero())
		{
			for(int j=0;j<marg_variables.size();j++)
				marg_relation[j]=marg_variables[j]->addr_value();
			out_function->relations().push_back(marg_relation);
		}
		Double temp=out_function->stable().get(address);
		temp+=prod.stable().get(prod_address);
		out_function->stable().set(address,temp);
	}
	Double norm_const;
	for(sparsetable<Double>::nonempty_iterator curr=out_function->stable().nonempty_begin();
		curr!=out_function->stable().nonempty_end();curr++)
	{
		norm_const+=*curr;
	}
	for(sparsetable<Double>::nonempty_iterator curr=out_function->stable().nonempty_begin();
		curr!=out_function->stable().nonempty_end();curr++)
	{
		*curr/=norm_const;
	}

}
//int main(int argc, char* argv[])
//{
//	int num_vars,p;
//	sscanf(argv[1],"%d",&num_vars);
//	sscanf(argv[2],"%d",&p);
//	Function f1,f2,f3,f4;
//	HashFunction h1,h2,h3,h4;
//	myRandom random;
//	vector<Variable*> variables;
//	for(int i=0;i<num_vars;i++)
//	{
//		vector<int> domain(4);
//		for(int j=0;j<4;j++)
//			domain[j]=j;
//		variables.push_back(new Variable(i,domain));
//	}
//
//	cout<<"Created variables\n";
//	for(int i=0;i<num_vars;i++)
//	{
//		if(random.getInt(2) ==0)
//		{
//			f1.variables().push_back(variables[i]);
//			h1.variables().push_back(variables[i]);
//		}
//		if(random.getInt(2) ==0)
//		{
//			f2.variables().push_back(variables[i]);
//			h2.variables().push_back(variables[i]);
//		}
//	}
//	vector<Variable*> vars;
//	do_set_difference(variables,f1.variables(),vars,less_than_comparator_variable);
//	do_set_union(f2.variables(),vars,f2.variables(),less_than_comparator_variable);
//	h2.variables()=f2.variables();
//	//do_set_difference(variables,f1.variables(),f2.variables(),less_than_comparator_variable);
//	f1.table()=vector<Double> (Variable::getDomainSize(f1.variables()));
//	f2.table()=vector<Double> (Variable::getDomainSize(f2.variables()));
//
//	//do_set_difference(variables,h1.variables(),h2.variables(),less_than_comparator_variable);
//	h1.stable()=sparsetable<Double>(Variable::getDomainSize(h1.variables()));
//	h2.stable()=sparsetable<Double>(Variable::getDomainSize(h2.variables()));
//	Double norm_const;
//	cout<<"Creating f1\n";
//	for(int i=0;i<f1.table().size();i++)
//	{
//		if(random.getInt(100) < p)
//		{
//			f1.table()[i]=Double(random.getDouble());
//			h1.stable()[i]=f1.table()[i];
//			Variable::setAddress(h1.variables(),i);
//			vector<int> temp(h1.variables().size());
//			for(int j=0;j<h1.variables().size();j++)
//			{
//				temp[j]=h1.variables()[j]->addr_value();
//			}
//			h1.relations().push_back(temp);
//		}
//		norm_const+=f1.table()[i];
//	}
//
//	for(int i=0;i<f1.table().size();i++)
//	{
//		f1.table()[i]/=norm_const;
//		Double t=h1.stable()[i];
//		h1.stable()[i]=t/norm_const;
//	}
//	cout<<"Created F1\n";
//	norm_const=Double();
//
//	for(int i=0;i<f2.table().size();i++)
//	{
//		if(random.getInt(100) < p)
//		{
//			f2.table()[i]=Double(random.getDouble());
//			h2.stable()[i]=f2.table()[i];
//			Variable::setAddress(h2.variables(),i);
//			vector<int> temp(h2.variables().size());
//			for(int j=0;j<h2.variables().size();j++)
//			{
//				temp[j]=h2.variables()[j]->addr_value();
//			}
//			h2.relations().push_back(temp);
//		}
//		norm_const+=f2.table()[i];
//	}
//	for(int i=0;i<f2.table().size();i++)
//	{
//		f2.table()[i]/=norm_const;
//		Double t=h2.stable()[i];
//		h2.stable()[i]=t/norm_const;
//	}
//	do_set_union(f1.variables(),f2.variables(),f3.variables(),less_than_comparator_variable);
//	cerr<<"Domain size ="<<Variable::getDomainSize(f3.variables());
//	cerr<<"Computing product\n";
//	time_t start_time,end_time;
//	start_time=time(NULL);
//	vector<Function*> functions;
//	functions.push_back(&f1);functions.push_back(&f2);
//	//for(int i=0;i<300;i++)
//	//{
//	Function::product(functions,f3);
//	//}
//
//	end_time=time(NULL);
//	cerr<<"Time1 = "<<(end_time-start_time)<<endl;
//	f3=Function();
//	start_time=time(NULL);
//	//for(int i=0;i<300;i++)
//	Function::dummy_product(f1,f2,f4);
//	end_time=time(NULL);
//	cerr<<"Time2 = "<<(end_time-start_time)<<endl;
//	//cerr<<"Error= "<<f3.MSE(f4)<<endl;
//
//	f4=Function();
//	start_time=time(NULL);
//	//h1.print();
//	//h2.print();
//	h1.product(h2,h3);
//	//h3.print(cout);
//	//for(int i=0;i<300;i++)
//	end_time=time(NULL);
//	cerr<<"Time3 = "<<(end_time-start_time)<<endl;
//	cerr<<"Error = ";
//	double error=0.0;
//	for(int i=0;i<f3.table().size();i++)
//	{
//		Double temp=h3.stable()[i];
//		error+=((f3.table()[i].value()-temp.value())*(f3.table()[i].value()-temp.value()));
//	}
//	cerr<<error;
//	cerr<<endl;
//	//f1.print();
//	//f2.print();
//	//f3.print();
//	//f4.print();
//
//}