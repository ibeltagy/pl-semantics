#include "JG.h"
#include "myRandom.h"

bool isConsistent(Function& function)
{
	if(function.table().empty())
		return true;
	for(int i=0;i<function.table().size();i++)
	{
		if(!function.table()[i].isZero())
			return true;
	}
	return false;
}
void JGNodeLS::initialize()
{
	
	for(int i=0;i<edges_.size();i++)
	{
		edges_[i]->message1()=Function();
		edges_[i]->message2()=Function();
	}
	
	function=Function();
	for(int i=0;i<original_functions.size();i++)
	{
	
		do_set_union(original_functions[i]->variables(),function.variables(),function.variables(),less_than_comparator_variable);
	
	}
	
	//Remove variables that have been assigned a value
	vector<Variable*> removed_variables;
	for(int i=0;i<function.variables().size();i++)
	{
		if(function.variables()[i]->value()!=INVALID_VALUE)
		{
			removed_variables.push_back(function.variables()[i]);
		}
	}
	
	do_set_difference(function.variables(),removed_variables,function.variables(),less_than_comparator_variable);
	function.table()=vector<Double> (Variable::getDomainSize(function.variables()));
	Double norm_const;
	
	for(int i=0;i<function.table().size();i++)
	{
		function.table()[i]=Double(1.0);
		Variable::setAddress(function.variables(),i);
		for(int j=0;j<original_functions.size();j++)
		{
			int entry=Variable::getAddress(original_functions[j]->variables());
			function.table()[i]*=original_functions[j]->table()[entry];
		}
		norm_const+=function.table()[i];
	}
	for(int i=0;i<function.table().size();i++)
		function.table()[i]/=norm_const;
	
}
void JGNodeLS::addFunction(Function& function_)
{
	original_functions.push_back(&function_);
}
void JGNodeLS::updateVariables()
{
	variables_=function.variables();
}
void JGNodeLS::getCF(vector<Variable*>& cond_variables, Variable* marg_variable, CPT& cf)
{
	Function out_function;
	vector<Variable*> all_variables;
	all_variables=cond_variables;
	all_variables.push_back(marg_variable);
	sort(all_variables.begin(),all_variables.end(),less_than_comparator_variable);
	function.marginalize(all_variables,out_function);
	vector<Variable*> cond_variables_;
	do_set_intersection(cond_variables,function.variables(),cond_variables_,less_than_comparator_variable);
	vector<Variable*> marg_vars; marg_vars.push_back(marg_variable);
	assert(do_set_inclusion(marg_vars,function.variables(),less_than_comparator_variable));
	cf=CPT(out_function,marg_variable,cond_variables_);
	
}
void JGNodeLS::getMarginal(vector<Variable*>& marg_variables, Function& function_)
{
	function.marginalize(marg_variables,function_);
}


JGEdgeLS::JGEdgeLS(JGNodeLS* ls_node1__, JGNodeLS* ls_node2__): JGEdge(), ls_node1_(ls_node1__),ls_node2_(ls_node2__)
{
	node1_=ls_node1_;
	node2_=ls_node2_;
}
/*
	(f1) -----> (f2)...This message is m2
	(f1) <----- (f2)...This message is m1

	RESULT:
	Formula: f2=(f2/m2)*\sum_{vars(f1) \ vars(m2)}(f1/m1)
	new_m2=\sum_{vars(f1) \ vars(m2)}(f1/m1)

	Algorithm:
	f2=f2/m2;
	m2=\sum_{vars(f1) \ vars(m2)}(f1/m1);
	f2=f2*m2;
*/
void JGEdgeLS::sendMessage1to2()
{
	Function& f1=ls_node1_->function;
	Function& f2=ls_node2_->function;
	Function& m1=message2();
	Function& m2=message1();
	f2.divide(m2);
	Function f3;
	Function::dummy_divide(f1,m1,f3);
	f3.marginalize(variables(),m2);
	f2.product(m2);
	assert(isConsistent(m2));
}

void JGEdgeLS::sendMessage2to1()
{
	Function& f1=ls_node2_->function;
	Function& f2=ls_node1_->function;
	Function& m1=message1();
	Function& m2=message2();

	f2.divide(m2);
	/*f1.divide(m1);
	f1.marginalize(variables(),m2);*/
	Function f3;
	Function::dummy_divide(f1,m1,f3);
	f3.marginalize(variables(),m2);
	f2.product(m2);
	assert(isConsistent(m2));
}