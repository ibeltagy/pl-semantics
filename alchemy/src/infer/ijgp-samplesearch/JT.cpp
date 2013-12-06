#include "JT.h"
namespace ss{

JT::JT(vector<Variable*>& variables_, vector<Function*>& functions, vector<set<int> >& clusters, vector<int>& order): variables(variables_)
{
	vector<int> var_in_pos(order.size());
	for(int i=0;i<var_in_pos.size();i++)
		var_in_pos[order[i]]=i;

	nodes=vector<JGNode*>(variables.size());
	var_to_node=vector<JGNode*> (variables.size());
	for(int i=0;i<variables.size();i++)
	{
		nodes[i]=new JGNodeLSS();
		var_to_node[order[i]]=nodes[i];
	}
	//Put the functions in the appropriate nodes
	// First put the functions in the proper buckets
	for(int i=0;i<functions.size();i++)
	{
		int pos=order.size();
		Function* function=functions[i];
		if(function->variables().empty())
		{
			continue;
		}
		for(int j=0;j<function->variables().size();j++)
		{
			if(var_in_pos[function->variables()[j]->id()] < pos)
				pos=var_in_pos[function->variables()[j]->id()];
		}
		assert(pos!=(int)order.size());
		/*{
			assert((int)function->log_table.size()==1);
			cerr<<function->log_table[0].toDouble()<<" "<<endl;
			log_pe+=function->log_table[0];
			delete(function);
			continue;
		}*/
		nodes[pos]->addFunction(*function);
	}

	// Now create the edges and message order
	for(int i=0;i<clusters.size();i++)
	{
		set<int> curr_cluster=clusters[i];
		assert(curr_cluster.find(order[i])!=curr_cluster.end());
		curr_cluster.erase(order[i]);
		// Find where to put this cluster
		int pos=order.size();
		for(set<int>::iterator j= curr_cluster.begin();j!=curr_cluster.end();j++)
		{
			if(var_in_pos[*j] < pos)
				pos=var_in_pos[*j];
		}
		if(pos!=(int)order.size())
		{
			edges.push_back(new JGEdgeLSS(dynamic_cast<JGNodeLSS*> (nodes[i]),dynamic_cast<JGNodeLSS*> (nodes[pos])));
			set<int> temp;
			set_intersection(clusters[i].begin(),clusters[i].end(),clusters[pos].begin(),clusters[pos].end(),inserter(temp,temp.begin()));
			for(set<int>::iterator j=temp.begin();j!=temp.end();j++)
				edges[edges.size()-1]->variables().push_back(variables[*j]);
			message_order.push_back(edges[edges.size()-1]);
			nodes[i]->edges().push_back(edges[edges.size()-1]);
			nodes[pos]->edges().push_back(edges[edges.size()-1]);
			//cerr<<"Edge added\n";
		}
	}
}

void JT::propagate()
{
	for(int i=0;i<nodes.size();i++)
	{
		nodes[i]->initialize();
		//for(int j=0;j<nodes[i]->edges().size();j++)
		//{
		//	nodes[i]->edges()[j]->initialize();
		//}
	}
	for(int i=0;i<message_order.size();i++)
		message_order[i]->sendMessage1to2();
	for(int i=message_order.size()-1;i>-1;i--)
		message_order[i]->sendMessage2to1();

	marginals=vector<Function> (variables.size());
	for(int i=0;i<variables.size();i++)
	{
		vector<Variable*> marg_variables;
		marg_variables.push_back(variables[i]);
		var_to_node[i]->getMarginal(marg_variables,marginals[i]);
	}
}
}
