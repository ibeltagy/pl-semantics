#include "AOTree.h"
#include "GM.h"
#include <fstream>
#include <stack>
#include <queue>

//bool checkParent(AOTreeNode* node)
//{
//	AOTreeNode* parent=node->parent;
//	map<int,vector<AOTreeNode*> >::iterator iter=parent->children.begin();
//	for(;iter!=parent->children.end();iter++)
//	{
//		vector<AOTreeNode*>::iterator child_iter=iter->second.begin();
//		for(;child_iter!=iter->second.end();child_iter++)
//		{
//			if((*child_iter)==node)
//				return true;
//		}
//	}
//	return false;
//}
AOTree::AOTree(PseudoTree& pseudo_tree_,GM& gm,vector<int>& sampling_order):pseudo_tree(pseudo_tree_),root(new AOTreeNode()),
leaves(vector<AOTreeNode*> ())
{
	cout<<"Size of root = "<<sizeof(root)<<endl;
	//cout<<"Initing AOTREE\n";
	ordered_functions=vector<vector<Function*> > (gm.variables.size());
	vector<int> mapped_order(sampling_order.size());
	for(int i=0;i<sampling_order.size();i++)
		mapped_order[sampling_order[i]]=i;
	for(int i=0;i<gm.functions.size();i++)
	{
		int max=-1;
		for(int j=0;j<gm.functions[i]->variables().size();j++)
		{
			int mapped_var=mapped_order[gm.functions[i]->variables()[j]->id()];
			if(mapped_var > max)
			{
				max=mapped_var;
			}
		}
		assert(max > -1);
		ordered_functions[sampling_order[max]].push_back(gm.functions[i]);
	}
	/*cout<<"Printing var_funcs\n";
	for(int i=0;i<sampling_order.size();i++)
	{
		int var=sampling_order[i];
		cout<<"Var = "<<var<<": ";
		vector<Variable*> temp;
		for(int j=0;j<ordered_functions[var].size();j++)
		{
			do_set_union(temp,ordered_functions[var][j]->variables(),temp,less_than_comparator_variable);
			
		}
		for(int j=0;j<temp.size();j++)
		{
			cout<<temp[j]->id()<<" ";
		}
		cout<<endl;
	}*/
}

Double AOTree::getP(int variable)
{
	//cout<<variable<<" ";
	Double value(1.0);
	for(int i=0;i<ordered_functions[variable].size();i++)
	{
		int entry=Variable::getValueAddress(ordered_functions[variable][i]->variables());
		value*=ordered_functions[variable][i]->table()[entry];
	}
	//cout<<"Ac -weight = "<<value<<endl;
	return value;
}
void AOTree::addAssignment(vector<Double>& Q)
{		
	stack<PseudoTreeNode*> s;
	s.push(pseudo_tree.root);
	stack<AOTreeNode*> aos;
	aos.push(root);
	

	while(!s.empty())
	{
		PseudoTreeNode* ps_node=s.top();
		s.pop();

		AOTreeNode* ao_node=aos.top();
		aos.pop();
		
		assert(ao_node!=NULL);
		if(ao_node->clone_node==NULL)
		{
			ao_node->clone_node=ps_node;
		}
		int val=ps_node->variable->value();
		//Update weight and number
		
		map<int,int>::iterator iter=ao_node->val2index.find(val);
		int index=-1;
		// If the value is not found update the map and other fields
		if(iter==ao_node->val2index.end())
		{
			index=ao_node->number.size();
			assert(ao_node->number.size() == ao_node->children.size());
			assert(ao_node->children.size() == ao_node->weights.size());
			ao_node->val2index[val]=index;

			ao_node->number.push_back(0);
			ao_node->weights.push_back(getP(ps_node->variable->id())/Q[ps_node->variable->id()]);
			//Update the children
			//if(!ps_node->children.empty())
			//{
				ao_node->children.push_back(vector<AOTreeNode*>(ps_node->children.size()));
				for(int j=0;j<ps_node->children.size();j++)
				{
					ao_node->children[index][j]=new AOTreeNode();
					
				}
			//}
		}
		
		index=(index==-1)?(iter->second):(index);
		assert(index < ps_node->variable->domain_size());

		//Update the number of times the appropriate children will be visited
		++ao_node->number[index];

		// Put all the children of the Pseudotree and the AOTree on the stack
		for(int i=0;i<ps_node->children.size();i++)
		{
			s.push(ps_node->children[i]);
			aos.push(ao_node->children[index][i]);
			//ao_node->children[index][i]->parent=ao_node;	
		}
		if(ps_node->children.empty())
		{
			if(!ao_node->added)
			{
				leaves.push_back(ao_node);	
				ao_node->added=true;
			}
		}
	}
	
	assert(aos.empty());
	/*s.push(pseudo_tree.root);
	while(!s.empty())
	{
		PseudoTreeNode* ps_node=s.top();
		s.pop();
		cout<<"V = "<<ps_node->variable->id()<<" Children = ";
		for(int i=0;i<ps_node->children.size();i++)
		{
			s.push(ps_node->children[i]);
			cout<<ps_node->children[i]->variable->id()<<" ";
		}
		cout<<endl;
	}*/
}

Double AOTree::computeWeight()
{
	cout<<"Size of root = "<<sizeof(root)<<endl;
	queue<AOTreeNode*> q;
	int num_mult_nodes=0;
	for(int i=0;i<leaves.size();i++)
	{
		leaves[i]->value=Double();
	}
	//cout<<"Leaves: ";
	// Initialize leaves
	for(int i=0;i<leaves.size();i++)
	{
		//assert(leaves[i]->num_samples.isZero());
		assert(leaves[i]->clone_node->children.empty());
		Double tot_num,tot_weight;
		for( map<int,int>::iterator j = leaves[i]->val2index.begin();j!=leaves[i]->val2index.end();j++)
		{
			int index=j->second;
			Double temp((double)leaves[i]->number[index]);
			tot_num+=temp;
			//leaves[i]->num_samples+=temp;
			tot_weight+=(leaves[i]->weights[index]*temp);
			
			
		}
		leaves[i]->value=tot_weight/tot_num;
		//// Put the parents of the leaves in the queue
		//if(leaves[i]->parent!=NULL)
		//{
		//	cout<<"V's parent = "<<leaves[i]->parent->clone_node->variable->id()<<" "<<flush;
		//	if(!leaves[i]->parent->added)
		//	{
		//		//q.push(leaves[i]->parent);
		//		leaves[i]->parent->added=true;
		//	}
		//}
		
	}
	stack<AOTreeNode*> s;
	s.push(root);

	
	while(!s.empty())
	{
		AOTreeNode* ao_node=s.top();
		s.pop();

		if(ao_node->added)
		{
			if(ao_node->clone_node->children.empty())
				continue;
			//Process the node
			ao_node->value=Double();
			Double num_samples;
			for(map<int,int>::iterator i = ao_node->val2index.begin();i!=ao_node->val2index.end();i++)
			{
				int index=i->second;
				Double and_node_weight(1.0);
				//Double and_number(1.0);
				for(int j=0;j<ao_node->children[index].size();j++)
				{
					//cout<<"V's child ="<<ao_node->children[index][j]->clone_node->variable->id()<<" "<<flush;
					assert(ao_node->children[index][j]->added);

					//and_number*=ao_node->children[index][j]->num_samples;
					and_node_weight*=ao_node->children[index][j]->value;
				}
				Double temp(and_node_weight);
				temp*=ao_node->weights[index];
				Double curr_num_samples ((double) ao_node->number[index]);
				temp*=curr_num_samples;
				num_samples+=curr_num_samples;
				ao_node->value+=temp;
			}
			ao_node->value/=num_samples;
		}
		else
		{
			ao_node->added=true;
			s.push(ao_node);
			for(map<int,int>::iterator i = ao_node->val2index.begin();i!=ao_node->val2index.end();i++)
			{
				int index=i->second;
				for(int j=0;j<ao_node->children[index].size();j++)
				{
					s.push(ao_node->children[index][j]);
				}
			}
			
		}

	}	
	//	//Put the parent in the queue
	//	if(ao_node->parent!=NULL)
	//	{
	//		if(!ao_node->parent->added)
	//		{
	//			q.push(ao_node->parent);
	//			ao_node->parent->added=true;
	//		}
	//	}
	//}
	return root->value;///root->num_samples;
}
