#include "JG.h"
#include "myRandom.h"
void JGNodeHSS::addFunction(Function& function)
{
	functions.push_back(new HashFunction(function));
}
void JGNodeHSS::getCF(vector<Variable*>& cond_variables,Variable* marg_variable, CPT& cf)
{
	vector<HashFunction*> all_functions;
	all_functions=functions;
	for(int i=0;i<edges_.size();i++)
	{
		if(edges_[i]->node1()==this)
		{
			HashFunction* h;
			Function* f;
			f=&(edges_[i]->message2());
			h=dynamic_cast<HashFunction*>(f);
			all_functions.push_back(h);
		}
		else
		{
			HashFunction* h;
			Function* f;
			f=&(edges_[i]->message1());
			h=dynamic_cast<HashFunction*>(f);
			all_functions.push_back(h);
			
		}
	}
	vector<Variable*> all_variables;
	all_variables=cond_variables;
	all_variables.push_back(marg_variable);
	sort(all_variables.begin(),all_variables.end(),less_than_comparator_variable);

	HashFunction temp;
	HashFunction::multiplyAndMarginalize(all_variables,all_functions,&temp);
	cf.variables()=all_variables;
	cf.cond_variables()=cond_variables;
	cf.setMargVariable(marg_variable);
	cf.table()=vector<Double> (Variable::getDomainSize(all_variables));
	for(int i=0;i<cf.table().size();i++)
		cf.table()[i]=temp.stable().get(i);
	//Normalize the conditional function
	int multiplier=1;
	for(int i=0;i<all_variables.size();i++)
	{
		if(all_variables[i]==marg_variable)
			break;
		multiplier*=all_variables[i]->domain_size();
	}
	int cond_num_values=Variable::getDomainSize(cond_variables);
	for(int i=0;i<cond_num_values;i++)
	{
		Double norm_const;
		for(int j=0;j<marg_variable->domain_size();j++)
		{
			int address=i+multiplier*j;
			norm_const+=cf.table()[address];
		}
		for(int j=0;j<marg_variable->domain_size();j++)
		{
			int address=i+multiplier*j;
			cf.table()[address]/=norm_const;
		}
	}
}

void JGNodeHSS::getMarginal(vector<Variable*>& marg_variables, Function& function)
{
	vector<HashFunction*> all_functions;
	all_functions=functions;
	for(int i=0;i<edges_.size();i++)
	{
		if(edges_[i]->node1()==this)
		{
			HashFunction* h;
			Function* f;
			f=&(edges_[i]->message2());
			h=dynamic_cast<HashFunction*>(f);
			all_functions.push_back(h);
		}
		else
		{
			HashFunction* h;
			Function* f;
			f=&(edges_[i]->message1());
			h=dynamic_cast<HashFunction*>(f);
			all_functions.push_back(h);
			
		}
	}
	vector<Variable*> all_variables;
	all_variables=marg_variables;
	sort(all_variables.begin(),all_variables.end(),less_than_comparator_variable);

	HashFunction temp;
	HashFunction::multiplyAndMarginalize(all_variables,all_functions,&temp);
	function.variables()=all_variables;
	function.table()=vector<Double> (Variable::getDomainSize(all_variables));
	for(int i=0;i<function.table().size();i++)
		function.table()[i]=temp.stable().get(i);
}



JGEdgeHSS::JGEdgeHSS(JGNodeHSS* hss_node1__, JGNodeHSS* hss_node2__): JGEdge(),hss_node1_(hss_node1__),hss_node2_(hss_node2__)
{
	if(node1_to_node2_message_!=NULL)
	{
		delete(node1_to_node2_message_);
	}
	if(node2_to_node1_message_!=NULL)
	{
		delete(node2_to_node1_message_);
	}
	node1_to_node2_message_=new HashFunction();
	node2_to_node1_message_=new HashFunction();
	node1_=hss_node1_;
	node2_=hss_node2_;
}
void JGEdgeHSS::sendMessage1to2()
{
	vector<HashFunction*> all_functions;
	all_functions=hss_node1_->functions;
	for(int i=0;i<hss_node1_->edges().size();i++)
	{
		JGEdge* curr_edge=hss_node1_->edges()[i];
		if(curr_edge==this)
			continue;
		if(curr_edge->node1()==hss_node1_)
		{
			if(!curr_edge->message2().variables().empty())
			{
				all_functions.push_back(dynamic_cast<HashFunction*>(&curr_edge->message2()));
			}
		}
		else
		{
			if(!curr_edge->message1().variables().empty())
			{
				all_functions.push_back(dynamic_cast<HashFunction*>(&curr_edge->message1()));
			}
		}
	}
	//cout<<"Calling multiply and marginalize hss ";
	//cout<<hss_node1_->id()<<" to "<<hss_node2_->id()<<endl;
	HashFunction::multiplyAndMarginalize(this->variables(),all_functions,dynamic_cast<HashFunction*>(node1_to_node2_message_));
}
void JGEdgeHSS::sendMessage2to1()
{
	vector<HashFunction*> all_functions;
	all_functions=hss_node2_->functions;
	for(int i=0;i<hss_node2_->edges().size();i++)
	{
		JGEdge* curr_edge=hss_node2_->edges()[i];
		if(curr_edge==this)
			continue;
		if(curr_edge->node1()==hss_node2_)
		{
			if(!curr_edge->message2().variables().empty())
				all_functions.push_back(dynamic_cast<HashFunction*>(&curr_edge->message2()));
		}
		else
		{
			if(!curr_edge->message2().variables().empty())
				all_functions.push_back(dynamic_cast<HashFunction*>(&curr_edge->message1()));
		}
	}
	//cout<<"Calling multiply and marginalize ";
	//cout<<hss_node2_->id()<<" to "<<hss_node1_->id()<<endl;
	HashFunction::multiplyAndMarginalize(this->variables(),all_functions,dynamic_cast<HashFunction*>(node2_to_node1_message_));
}
