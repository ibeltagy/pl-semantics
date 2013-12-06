#ifndef SS_AOGRAPH_H_
#define SS_AOGRAPH_H_
#include <iostream>
#include <vector>
#include <map>
#include "GM.h"
#include "Graph.h"
#include <gmpxx.h>

using namespace std;

namespace ss{

struct SparseFunction
{
	vector<Variable*> cond_variables;
	Variable* marg_variable;
	vector<Variable*> all_variables;
	vector<Double> table;
	vector<Double> num_times;
	map<int,int> val2addr;
	map<int,int> addr2val;

	SparseFunction(){ }
	void getMessage(SparseFunction& message)
	{
		message=SparseFunction();
		message.cond_variables=cond_variables;
		message.all_variables=cond_variables;
		message.marg_variable=NULL;
		
		for(int i=0;i<table.size();i++)
		{
			assert(addr2val.find(i)!=addr2val.end());
			int val=addr2val[i];
			Variable::setAddress(all_variables,val);
			int message_val=Variable::getAddress(cond_variables);
			// If message_val already exists in the message then update it
			if(message.val2addr.find(message_val)!=message.val2addr.end())
			{
				int message_addr=message.val2addr[message_val];
				message.table[message_addr]+=(table[i]*num_times[i]);
				message.num_times[message_addr]+=num_times[i];
			}
			else // Add entry1 to the message
			{
				int message_addr=message.table.size();
				message.table.push_back(table[i]*num_times[i]);
				message.num_times.push_back(num_times[i]);
				message.addr2val[message_addr]=message_val;
				message.val2addr[message_val]=message_addr;
			}
		}

		// Now compute average i.e. divide all entries in the message table by num_times;
		for(int i=0;i<message.table.size();i++)
		{
			message.table[i]/=message.num_times[i];
		}
	}

	void multiply(SparseFunction& message)
	{
		for(int i=0;i<table.size();i++)
		{
			assert(addr2val.find(i)!=addr2val.end());
			int val=addr2val[i];
			Variable::setAddress(all_variables,val);
			int message_val=Variable::getAddress(message.all_variables);
			if(message.val2addr.find(message_val)==message.val2addr.end())
			{
				cout<<"Message val = "<<message_val<<" val = "<<val<<endl;
				message.print();
				this->print();
			}
			assert(message.val2addr.find(message_val)!=message.val2addr.end());
			int message_addr=message.val2addr[message_val];
			table[i]*=message.table[message_addr];
			//assert(num_times[i]==message.num_times[i]);
		}
	}
	void addAssignment(Double& weight)
	{
		//for(int i=0;i<all_variables.size();i++)
		//{
			int entry=Variable::getAddress(all_variables);
			//If the entry is already present add weight to it
			if(val2addr.find(entry)!=val2addr.end())
			{
				int addr=val2addr[entry];
				assert(addr < (int)table.size());
				//table[addr]+=weight;
				num_times[addr]+=Double(1.0);
			}
			else
			{
				int addr=(int)table.size();
				table.push_back(weight);
				num_times.push_back(Double(1.0));
				val2addr[entry]=addr;
				addr2val[addr]=entry;
			}
		//}
	}
	void print()
	{
		cout<<"_______________________________________\n";
		cout<<"Vars = ";
		if(marg_variable!=NULL)
		{
			cout<<marg_variable->id()<<"| ";
		}
		for(int i=0;i<cond_variables.size();i++)
		{
			cout<<cond_variables[i]->id()<<" ";
		}
		cout<<endl;
		cout<<"Tab-size = "<<table.size()<<endl;
		for(int i=0;i<table.size();i++)
			cout<<table[i]<<" ";
		cout<<endl;
		cout<<endl;
		cout<<"____________________________________\n";
	}
};
struct AOGraphNode
{
	SparseFunction function;
	vector<Function*> bucket_functions;
	AOGraphNode* parent;
	AOGraphNode():parent(NULL){ }
	void addAssignment(Double& q_value)
	{
		//Make sure that all variables are assigned a value
		for(int i=0;i<function.all_variables.size();i++)
			assert(function.all_variables[i]->value()!=INVALID_VALUE);
		Double weight(1.0);
		for(int i=0;i<bucket_functions.size();i++)
		{
			int entry=Variable::getAddress(bucket_functions[i]->variables());
			weight*=bucket_functions[i]->table()[entry];
		}
		weight/=q_value;
		function.addAssignment(weight);
	}
	void sendMessage()
	{
		SparseFunction message;
		function.getMessage(message);
		if(parent!=NULL)
			parent->function.multiply(message);
	}
};
class AOGraph
{
public:
	vector<AOGraphNode> all_nodes;
	AOGraph(GM& gm, Graph& g, vector<int>& order)
	{
		Graph triangulated_graph;
		g.getTriangulatedGraph(triangulated_graph,order);

		// Read the cliques from the triangulated graph
		vector<vector<int> > cliques;
		triangulated_graph.getCliques(order,cliques);

		vector<int> mapped_order(order.size());
		for(int i=0;i<order.size();i++)
			mapped_order[order[i]]=i;
		// Now form the AOGraph nodes
		all_nodes=vector<AOGraphNode>(order.size());
		for(int i=0;i<all_nodes.size();i++)
		{
			for(int j=0;j<cliques[i].size();j++)
			{
				if(j==0)
				{
					all_nodes[i].function.all_variables.push_back(gm.variables[cliques[i][j]]);
					all_nodes[i].function.marg_variable=gm.variables[cliques[i][j]];
					assert(order[i]==cliques[i][j]);
				}
				else
				{
					all_nodes[i].function.all_variables.push_back(gm.variables[cliques[i][j]]);
					all_nodes[i].function.cond_variables.push_back(gm.variables[cliques[i][j]]);
				}
			}

			sort(all_nodes[i].function.all_variables.begin(),all_nodes[i].function.all_variables.end(),less_than_comparator_variable);
			sort(all_nodes[i].function.cond_variables.begin(),all_nodes[i].function.cond_variables.end(),less_than_comparator_variable);
		}

		// Set the appropriate parents
		for(int i=0;i<all_nodes.size();i++)
		{
			
			int min_id=gm.variables.size();
			for(int j=0;j<all_nodes[i].function.cond_variables.size();j++)
			{

				int id=mapped_order[all_nodes[i].function.cond_variables[j]->id()];
				if(id < min_id)
				{
					min_id=id;
				}
			}
			if(min_id != (int)gm.variables.size())
			{
				assert(all_nodes[min_id].parent==NULL);
				all_nodes[i].parent=&all_nodes[min_id];
			}
		}

		// Set the bucket_functions
		for(int i=0;i<gm.functions.size();i++)
		{
			int min_id=gm.variables.size();
			for(int j=0;j<gm.functions[i]->variables().size();j++)
			{
				int id=mapped_order[gm.functions[i]->variables()[j]->id()];
				if(id < min_id)
				{
					min_id=id;
				}
			}
			assert(min_id != (int)gm.variables.size());
			all_nodes[min_id].bucket_functions.push_back(gm.functions[i]);
		}
	}

	void addAssignment(vector<Double>& q_value)
	{
		for(int i=0;i<all_nodes.size();i++)
		{
			assert(all_nodes[i].function.marg_variable!=NULL);
			int var_id=all_nodes[i].function.marg_variable->id();
			all_nodes[i].addAssignment(q_value[var_id]);
		}
	}

	Double pe()
	{
		/*cerr<<"Computine pe\n";
		for(int i=0;i<all_nodes.size();i++)
		{
			all_nodes[i].function.print();
		}*/
		
		for(int i=0;i<all_nodes.size();i++)
		{
			all_nodes[i].sendMessage();
		}
		//cout<<"Messages sent\n";
		SparseFunction func;
		all_nodes[all_nodes.size()-1].function.getMessage(func);
		assert((int)func.table.size()==1);
		return func.table[0];
	}
};
}
#endif
