#ifndef SS_AOGRAPH_BIG_INT_H_
#define SS_AOGRAPH_BIG_INT_H_
#include <iostream>
#include <vector>
#include <map>
#include <queue>
#include "GM.h"
#include "Graph.h"
#include <gmpxx.h>
#include <ext/new_allocator.h>
#include <ext/hash_map>

namespace ss{

using namespace __gnu_cxx;
//#include <google/sparse_hash_map>

//using google::sparse_hash_map; 

//#include "my_hash_map.H"

using namespace std;

struct LessThan
{
	bool operator()(const Int& r, const Int& p)
	{
		return r<p;
	}
};

struct HashInt
{
	size_t operator()(const Int& r) const
	{
		return (r.get_ui()%1000);

		//return (size_t)r%1000;
		
	}
};

struct SparseFunctionBigInt
{
	vector<Variable*> cond_variables;
	Variable* marg_variable;
	vector<Variable*> all_variables;
	vector<Double> table;
	vector<Double> num_times;
	map<Int,int,LessThan, __gnu_cxx::new_allocator<Int> > val2addr;
	map<int,Int,less<int>, __gnu_cxx::new_allocator<int> > addr2val;
	//sparse_hash_map<Int,int,HashInt> val2addr;
	//sparse_hash_map<int,Int,hash<int> > addr2val;
	//my_hash_map<Int,int,HashInt> val2addr;
	//my_hash_map<int,Int,hash<int> > addr2val;
	SparseFunctionBigInt()
	{ 
		table.clear();
		cond_variables.clear();
		all_variables.clear();
		marg_variable=NULL;
		num_times.clear();
		val2addr.clear();
		addr2val.clear();
		//my_hash_map<Int,int,HashInt> temp1;
		//my_hash_map<int,Int,hash<int> > temp2;
		//val2addr=temp1;
		//addr2val=temp2;
	}
	void getMessage(SparseFunctionBigInt& message)
	{
		message=SparseFunctionBigInt();
		message.cond_variables=cond_variables;
		message.all_variables=cond_variables;
		message.marg_variable=NULL;
		
		for(int i=0;i<table.size();i++)
		{
			assert(addr2val.find(i)!=addr2val.end());
			Int val=addr2val[i];
			Variable::setAddressBigInt(all_variables,val);
			Int message_val;
			Variable::getAddressBigInt(cond_variables,message_val);
			// If message_val already exists in the message then update it
			map<Int,int>::iterator j=message.val2addr.find(message_val);
			if(j!=message.val2addr.end())
			{
				int message_addr=(*j).second;
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
	void print_marg()
	{
		ofstream out("margfile");
		vector<Double> new_table(table.size());
		set<Int,LessThan> vals;
		Double norm_const;
		for(int i=0;i<table.size();i++)
		{
			new_table[i]=table[i];
			Int val=addr2val[i];
			vals.insert(val);
			norm_const+=table[i];
		}
		set<Int,LessThan>::iterator j=vals.begin();
		out<<marg_variable->id()<<" "<<marg_variable->domain_size()<<" ";
		for(int i=0;i<table.size();i++)
		{
			int k=val2addr[*j];
			new_table[k]/=norm_const;
			out<<new_table[k]<<" ";
			j++;
		}
		out<<endl;
		out.close();
	}
	void multiply(SparseFunctionBigInt& message)
	{
		for(int i=0;i<table.size();i++)
		{
			assert(addr2val.find(i)!=addr2val.end());
			Int val=addr2val[i];
			Variable::setAddressBigInt(all_variables,val);
			Int message_val;
			Variable::getAddressBigInt(message.all_variables,message_val);
			//if(message.val2addr.find(message_val)==message.val2addr.end())
			//{
			//	cout<<"Message val = "<<message_val<<" val = "<<val<<endl;
			//	message.print();
			//	this->print();
			//}
			//assert(message.val2addr.find(message_val)!=message.val2addr.end());
			int message_addr=message.val2addr[message_val];
			table[i]*=message.table[message_addr];
			//assert(num_times[i]==message.num_times[i]);
		}
	}
	void addAssignment(Double& weight)
	{
		//for(int i=0;i<all_variables.size();i++)
		//{
			Int entry;
			Variable::getAddressBigInt(all_variables,entry);
			//If the entry is already present add weight to it
			map<Int,int>::iterator j=val2addr.find(entry);
			if(j!=val2addr.end())
			{
				int addr=(*j).second;
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
		/*
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
		*/
	}
};
struct AOGraphNodeBigInt
{
	SparseFunctionBigInt function;
	vector<Function*> bucket_functions;
	AOGraphNodeBigInt* parent;
	vector<AOGraphNodeBigInt*> children;
	int id;
	AOGraphNodeBigInt():parent(NULL),bucket_functions(vector<Function*>()),function(SparseFunctionBigInt()){ }
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
	void sendMessage(bool to_delete=true)
	{
		SparseFunctionBigInt message;
		function.getMessage(message);
		if(parent!=NULL)
			parent->function.multiply(message);
		if(to_delete) message=SparseFunctionBigInt();
	}

	void populate(vector<vector<int> >& samples, vector<vector<Double> >& q)
	{
		for(int i=0;i<samples.size();i++)
		{
			for(int j=0;j<function.all_variables.size();j++)
			{
				function.all_variables[j]->value()=samples[i][function.all_variables[j]->id()];
			}
			addAssignment(q[i][function.marg_variable->id()]);
		}
		for(int j=0;j<function.all_variables.size();j++)
		{
				function.all_variables[j]->value()=INVALID_VALUE;
		}
	}
};
class AOGraphBigInt
{
public:
	vector<AOGraphNodeBigInt> all_nodes;
	vector<vector<int> > samples;
	vector<vector<Double> > q;
	
	AOGraphBigInt(GM& gm, Graph& g, vector<int>& order)
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
		all_nodes=vector<AOGraphNodeBigInt>(order.size());
		for(int i=0;i<all_nodes.size();i++)
		{
			all_nodes[i].id=i;
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
				all_nodes[min_id].children.push_back(&all_nodes[i]);
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

		// Print num-children
		int max_children=-1;
		for(int i=0;i<all_nodes.size();i++)
		{
			if(max_children < (int)all_nodes[i].children.size())
				max_children=all_nodes[i].children.size();
			//cout<<i<<" num-children  = "<<all_nodes[i].children.size()<<endl;
		}
		cout<<"Max children = "<<max_children<<endl;
	}

	void addAssignment(vector<Double>& q_value)
	{
		vector<int> sample(all_nodes.size());
		for(int i=0;i<all_nodes.size();i++)
		{
			assert(all_nodes[i].function.marg_variable->value()!=INVALID_VALUE);
			sample[all_nodes[i].function.marg_variable->id()]=all_nodes[i].function.marg_variable->value();
		}
		samples.push_back(sample);
		q.push_back(q_value);

		//for(int i=0;i<all_nodes.size();i++)
		//{
		//	assert(all_nodes[i].function.marg_variable!=NULL);
		//	int var_id=all_nodes[i].function.marg_variable->id();
		//	all_nodes[i].addAssignment(q_value[var_id]);
		//}
	}

	Double pe()
	{
		// Create rev_ordering
		vector<int> reverse_order;
		queue<int> qu;
		qu.push(all_nodes.size()-1);
		while(!qu.empty())
		{
			int curr=qu.front();
			qu.pop();
			cout<<curr<<"---";
			reverse_order.push_back(curr);
			//Push the children in the stack
			for(int i=0;i<all_nodes[curr].children.size();i++)
			{
				qu.push(all_nodes[curr].children[i]->id);
				cout<<all_nodes[curr].children[i]->id<<" ";
			}
			cout<<endl;
		}
		for(int i=0;i<reverse_order.size();i++)
			cout<<reverse_order[reverse_order.size()-i-1];
		cout<<endl;
		/*cerr<<"Computine pe\n";
		for(int i=0;i<all_nodes.size();i++)
		{
			all_nodes[i].function.print();
		}*/
		cout<<"Computing P(e)"<<endl;
		set<int> all_ids;

		for(int j=0;j<all_nodes.size();j++)
		{
			int i=reverse_order[all_nodes.size()-j-1];
			// First populate all_nodes[i] and all_nodes[i].parent
			if(all_nodes[i].function.table.empty())
			{
				all_nodes[i].populate(samples,q);
			}
			if(all_nodes[i].parent!=NULL)
			{
				if(all_nodes[i].parent->function.table.empty())
				{
					all_nodes[i].parent->populate(samples,q);
				}
				all_ids.insert(all_nodes[i].parent->id);
			}
			if(i==((int)all_nodes.size()-1))
			{
				all_nodes[i].sendMessage(false);
			}
			else
			{
				all_nodes[i].sendMessage();
			}
			if(i!=((int)all_nodes.size()-1))
			{
				all_nodes[i]=AOGraphNodeBigInt();
			}
			all_ids.erase(i);
			cout<<j<<" "<<all_ids.size()<<endl;
		}
		cout<<"Messages sent\n";
		all_nodes[all_nodes.size()-1].function.print_marg();
		SparseFunctionBigInt func;
		all_nodes[all_nodes.size()-1].function.getMessage(func);
		assert((int)func.table.size()==1);
		return func.table[0];
	}
};
}
#endif




//#ifndef AOGRAPH_BIG_INT_H_
//#define AOGRAPH_BIG_INT_H_
//#include <iostream>
//#include <vector>
//#include <map>
//#include <queue>
//#include "GM.h"
//#include "Graph.h"
//#include <gmpxx.h>
//#include <ext/new_allocator.h>
//#include <ext/hash_map>
//
//using namespace __gnu_cxx;
////#include <google/sparse_hash_map>
//
////using google::sparse_hash_map; 
//
////#include "my_hash_map.H"
//
//using namespace std;
//static inline int memReadStat(int field)
//{
//	char    name[256];
//	pid_t pid = getpid();
//	sprintf(name, "/proc/%d/statm", pid);
//	FILE*   in = fopen(name, "rb");
//	if (in == NULL) return 0;
//	int     value;
//	for (; field >= 0; field--)
//		fscanf(in, "%d", &value);
//	fclose(in);
//	return value;
//}
//static inline uint64_t memUsed() { return (uint64_t)memReadStat(0) * (uint64_t)getpagesize(); }
//
//struct LessThan
//{
//	bool operator()(const Int& r, const Int& p)
//	{
//		return r<p;
//	}
//};
//
//struct HashInt
//{
//	size_t operator()(const Int& r) const
//	{
//		return (r.get_ui()%1000);
//
//		//return (size_t)r%1000;
//		
//	}
//};
//
//struct SparseFunctionBigInt
//{
//	vector<Variable*> cond_variables;
//	Variable* marg_variable;
//	vector<Variable*> all_variables;
//	vector<Double> table;
//	vector<Double> num_times;
//	//map<Int,int,LessThan, __gnu_cxx::new_allocator<Int> > val2addr;
//	//map<int,Int,less<int>, __gnu_cxx::new_allocator<int> > addr2val;
//	vector<Int> valueOfAddr;
//	
//	//sparse_hash_map<Int,int,HashInt> val2addr;
//	//sparse_hash_map<int,Int,hash<int> > addr2val;
//	//my_hash_map<Int,int,HashInt> val2addr;
//	//my_hash_map<int,Int,hash<int> > addr2val;
//	SparseFunctionBigInt()
//	{ 
//		table.clear();
//		cond_variables.clear();
//		all_variables.clear();
//		marg_variable=NULL;
//		num_times.clear();
//		valueOfAddr.clear();
//		//addr2val.clear();
//		//my_hash_map<Int,int,HashInt> temp1;
//		//my_hash_map<int,Int,hash<int> > temp2;
//		//val2addr=temp1;
//		//addr2val=temp2;
//	}
//	void destroy()
//	{
//		table.clear();
//		cond_variables.clear();
//		all_variables.clear();
//		marg_variable=NULL;
//		num_times.clear();
//		valueOfAddr.clear();
//	}
//	bool isValPresent(Int& j)
//	{
//		return (getVal2Addr(j)!=-1);
//	}
//	bool isAddrPresent(int i)
//	{
//		return (i < (int)(valueOfAddr.size()));
//	}
//	int getVal2Addr(Int& j)
//	{
//		for(int i=0;i<valueOfAddr.size();i++)
//		{
//			if(valueOfAddr[i]==j)
//			{
//				return i;
//			}
//		}
//		return INVALID_VALUE;
//	}
//	Int& getAddr2Val(int j)
//	{
//		return valueOfAddr[j];
//	}
//	void AddVal(Int& i)
//	{
//		valueOfAddr.push_back(i);
//	}
//	void getMessage(SparseFunctionBigInt& message)
//	{
//		message=SparseFunctionBigInt();
//		message.cond_variables=cond_variables;
//		message.all_variables=cond_variables;
//		message.marg_variable=NULL;
//		
//		for(int i=0;i<table.size();i++)
//		{
//			//assert(addr2val.find(i)!=addr2val.end());
//			//Int val=addr2val[i];
//			Int val=getAddr2Val(i);
//			Variable::setAddressBigInt(all_variables,val);
//			Int message_val;
//			Variable::getAddressBigInt(cond_variables,message_val);
//			// If message_val already exists in the message then update it
//			//if(message.val2addr.find(message_val)!=message.val2addr.end())
//			if(message.isValPresent(message_val))
//			{
//				//int message_addr=message.val2addr[message_val];
//				int message_addr=message.getVal2Addr(message_val);
//				assert(message_addr != INVALID_VALUE);
//				message.table[message_addr]+=(table[i]*num_times[i]);
//				message.num_times[message_addr]+=num_times[i];
//			}
//			else // Add entry1 to the message
//			{
//				int message_addr=message.table.size();
//				message.table.push_back(table[i]*num_times[i]);
//				message.num_times.push_back(num_times[i]);
//				message.AddVal(message_val);
//				//message.addr2val[message_addr]=message_val;
//				//message.val2addr[message_val]=message_addr;
//			}
//		}
//
//		// Now compute average i.e. divide all entries in the message table by num_times;
//		for(int i=0;i<message.table.size();i++)
//		{
//			message.table[i]/=message.num_times[i];
//		}
//		message.num_times.clear();
//	}
//	void multiply(SparseFunctionBigInt& message)
//	{
//		for(int i=0;i<table.size();i++)
//		{
//			//assert(addr2val.find(i)!=addr2val.end());
//			assert(isAddrPresent(i));
//			//Int val=addr2val[i];
//			Int val=getAddr2Val(i);
//			Variable::setAddressBigInt(all_variables,val);
//			Int message_val;
//			Variable::getAddressBigInt(message.all_variables,message_val);
//			//if(message.val2addr.find(message_val)==message.val2addr.end())
//			if(!message.isValPresent(message_val))
//			{
//				cout<<"Message val = "<<message_val<<" val = "<<val<<endl;
//				message.print();
//				this->print();
//				exit(-1);
//			}
//			//assert(message.val2addr.find(message_val)!=message.val2addr.end());
//			//int message_addr=message.val2addr[message_val];
//			int message_addr=message.getVal2Addr(message_val);
//			table[i]*=message.table[message_addr];
//			//assert(num_times[i]==message.num_times[i]);
//		}
//		message.destroy();
//	}
//	void addAssignment(Double& weight)
//	{
//		//for(int i=0;i<all_variables.size();i++)
//		//{
//			Int entry;
//			Variable::getAddressBigInt(all_variables,entry);
//			//If the entry is already present add weight to it
//			//if(val2addr.find(entry)!=val2addr.end())
//			if(isValPresent(entry))
//			{
//				//int addr=val2addr[entry];
//				int addr=this->getVal2Addr(entry);
//				assert(addr < (int)table.size());
//				//table[addr]+=weight;
//				num_times[addr]+=Double(1.0);
//			}
//			else
//			{
//				int addr=(int)table.size();
//				table.push_back(weight);
//				num_times.push_back(Double(1.0));
//				AddVal(entry);
//			//	val2addr[entry]=addr;
//			//	addr2val[addr]=entry;
//			}
//		//}
//	}
//	void print()
//	{
//		/*
//		cout<<"_______________________________________\n";
//		cout<<"Vars = ";
//		if(marg_variable!=NULL)
//		{
//			cout<<marg_variable->id()<<"| ";
//		}
//		for(int i=0;i<cond_variables.size();i++)
//		{
//			cout<<cond_variables[i]->id()<<" ";
//		}
//		cout<<endl;
//		cout<<"Tab-size = "<<table.size()<<endl;
//		for(int i=0;i<table.size();i++)
//			cout<<table[i]<<" ";
//		cout<<endl;
//		cout<<endl;
//		cout<<"____________________________________\n";
//		*/
//	}
//};
//struct AOGraphNodeBigInt
//{
//	SparseFunctionBigInt function;
//	vector<Function*> bucket_functions;
//	AOGraphNodeBigInt* parent;
//	vector<AOGraphNodeBigInt*> children;
//	int id;
//	AOGraphNodeBigInt():parent(NULL),bucket_functions(vector<Function*>()),function(SparseFunctionBigInt()){ }
//	void destroy()
//	{
//		function.destroy();
//		bucket_functions.clear();
//		parent=NULL;
//		children.clear();
//	}
//	void addAssignment(Double& q_value)
//	{
//		//Make sure that all variables are assigned a value
//		for(int i=0;i<function.all_variables.size();i++)
//			assert(function.all_variables[i]->value()!=INVALID_VALUE);
//		Double weight(1.0);
//		for(int i=0;i<bucket_functions.size();i++)
//		{
//			int entry=Variable::getAddress(bucket_functions[i]->variables());
//			weight*=bucket_functions[i]->table()[entry];
//		}
//		weight/=q_value;
//		function.addAssignment(weight);
//	}
//	void sendMessage()
//	{
//		SparseFunctionBigInt message;
//		function.getMessage(message);
//		if(parent!=NULL)
//			parent->function.multiply(message);
//		
//	}
//
//	void populate(vector<vector<int> >& samples, vector<vector<Double> >& q)
//	{
//		for(int i=0;i<samples.size();i++)
//		{
//			for(int j=0;j<function.all_variables.size();j++)
//			{
//				function.all_variables[j]->value()=samples[i][function.all_variables[j]->id()];
//			}
//			addAssignment(q[i][function.marg_variable->id()]);
//		}
//		for(int j=0;j<function.all_variables.size();j++)
//		{
//				function.all_variables[j]->value()=INVALID_VALUE;
//		}
//	}
//};
//class AOGraphBigInt
//{
//public:
//	vector<AOGraphNodeBigInt> all_nodes;
//	vector<vector<int> > samples;
//	vector<vector<Double> > q;
//	
//	AOGraphBigInt(GM& gm, Graph& g, vector<int>& order)
//	{
//		Graph triangulated_graph;
//		g.getTriangulatedGraph(triangulated_graph,order);
//
//		// Read the cliques from the triangulated graph
//		vector<vector<int> > cliques;
//		triangulated_graph.getCliques(order,cliques);
//
//		vector<int> mapped_order(order.size());
//		for(int i=0;i<order.size();i++)
//			mapped_order[order[i]]=i;
//		// Now form the AOGraph nodes
//		all_nodes=vector<AOGraphNodeBigInt>(order.size());
//		for(int i=0;i<all_nodes.size();i++)
//		{
//			all_nodes[i].id=i;
//			for(int j=0;j<cliques[i].size();j++)
//			{
//				if(j==0)
//				{
//					all_nodes[i].function.all_variables.push_back(gm.variables[cliques[i][j]]);
//					all_nodes[i].function.marg_variable=gm.variables[cliques[i][j]];
//					assert(order[i]==cliques[i][j]);
//				}
//				else
//				{
//					all_nodes[i].function.all_variables.push_back(gm.variables[cliques[i][j]]);
//					all_nodes[i].function.cond_variables.push_back(gm.variables[cliques[i][j]]);
//				}
//			}
//
//			sort(all_nodes[i].function.all_variables.begin(),all_nodes[i].function.all_variables.end(),less_than_comparator_variable);
//			sort(all_nodes[i].function.cond_variables.begin(),all_nodes[i].function.cond_variables.end(),less_than_comparator_variable);
//		}
//
//		// Set the appropriate parents
//		for(int i=0;i<all_nodes.size();i++)
//		{
//			
//			int min_id=gm.variables.size();
//			for(int j=0;j<all_nodes[i].function.cond_variables.size();j++)
//			{
//
//				int id=mapped_order[all_nodes[i].function.cond_variables[j]->id()];
//				if(id < min_id)
//				{
//					min_id=id;
//				}
//			}
//			if(min_id != (int)gm.variables.size())
//			{
//				assert(all_nodes[min_id].parent==NULL);
//				all_nodes[i].parent=&all_nodes[min_id];
//				all_nodes[min_id].children.push_back(&all_nodes[i]);
//			}
//		}
//
//		// Set the bucket_functions
//		for(int i=0;i<gm.functions.size();i++)
//		{
//			int min_id=gm.variables.size();
//			for(int j=0;j<gm.functions[i]->variables().size();j++)
//			{
//				int id=mapped_order[gm.functions[i]->variables()[j]->id()];
//				if(id < min_id)
//				{
//					min_id=id;
//				}
//			}
//			assert(min_id != (int)gm.variables.size());
//			all_nodes[min_id].bucket_functions.push_back(gm.functions[i]);
//		}
//
//		// Print num-children
//		int max_children=-1;
//		for(int i=0;i<all_nodes.size();i++)
//		{
//			if(max_children < (int)all_nodes[i].children.size())
//				max_children=all_nodes[i].children.size();
//			//cout<<i<<" num-children  = "<<all_nodes[i].children.size()<<endl;
//		}
//		cout<<"Max children = "<<max_children<<endl;
//	}
//
//	void addAssignment(vector<Double>& q_value)
//	{
//		vector<int> sample(all_nodes.size());
//		for(int i=0;i<all_nodes.size();i++)
//		{
//			assert(all_nodes[i].function.marg_variable->value()!=INVALID_VALUE);
//			sample[all_nodes[i].function.marg_variable->id()]=all_nodes[i].function.marg_variable->value();
//		}
//		samples.push_back(sample);
//		q.push_back(q_value);
//
//		//for(int i=0;i<all_nodes.size();i++)
//		//{
//		//	assert(all_nodes[i].function.marg_variable!=NULL);
//		//	int var_id=all_nodes[i].function.marg_variable->id();
//		//	all_nodes[i].addAssignment(q_value[var_id]);
//		//}
//	}
//
//	Double pe()
//	{
//		// Create rev_ordering
//		vector<int> reverse_order;
//		queue<int> qu;
//		qu.push(all_nodes.size()-1);
//		while(!qu.empty())
//		{
//			int curr=qu.front();
//			qu.pop();
//			cout<<curr<<"---";
//			reverse_order.push_back(curr);
//			//Push the children in the stack
//			for(int i=0;i<all_nodes[curr].children.size();i++)
//			{
//				qu.push(all_nodes[curr].children[i]->id);
//				cout<<all_nodes[curr].children[i]->id<<" ";
//			}
//			cout<<endl;
//		}
//		for(int i=0;i<reverse_order.size();i++)
//			cout<<reverse_order[reverse_order.size()-i-1];
//		cout<<endl;
//		/*cerr<<"Computine pe\n";
//		for(int i=0;i<all_nodes.size();i++)
//		{
//			all_nodes[i].function.print();
//		}*/
//		cout<<"Computing P(e)"<<endl;
//		set<int> all_ids;
//
//		for(int j=0;j<all_nodes.size();j++)
//		{
//			int i=reverse_order[all_nodes.size()-j-1];
//			// First populate all_nodes[i] and all_nodes[i].parent
//			if(all_nodes[i].function.table.empty())
//			{
//				all_nodes[i].populate(samples,q);
//			}
//			if(all_nodes[i].parent!=NULL)
//			{
//				if(all_nodes[i].parent->function.table.empty())
//				{
//					all_nodes[i].parent->populate(samples,q);
//				}
//				all_ids.insert(all_nodes[i].parent->id);
//			}
//			all_nodes[i].sendMessage();
//			if(i!=((int)all_nodes.size()-1))
//			{
//				cout<<"Deleting\n";
//				all_nodes[i].function.destroy();
//			}
//			all_ids.erase(i);
//			cout<<j<<" ";
//			int count=0;
//			for(int k=0;k<all_nodes.size();k++)
//				if(!all_nodes[k].function.table.empty())
//					count++;
//			cout<<count<<endl;
//			cout<<memUsed()/1048576.0<<endl;
//		}
//		cout<<"Messages sent\n";
//		SparseFunctionBigInt func;
//		all_nodes[all_nodes.size()-1].function.getMessage(func);
//		assert((int)func.table.size()==1);
//		return func.table[0];
//	}
//};
//#endif
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
////#ifndef AOGRAPH_BigInt_H_
////#define AOGRAPH_BigInt_H_
////#include <iostream>
////#include <vector>
////#include <map>
////#include "GM.h"
////#include "Graph.h"
////#include <gmp.h>
////
////
////using namespace std;
////
////char* BigIntToString(mpz_t& _xCount) 
////{
////    // Caller responsible for deleting the returned string.
////    int buf_size = mpz_sizeinbase (_xCount, 10) + 2;
////    char* buffer = new char[buf_size];
////    mpz_get_str(buffer, 10, _xCount);
////    return buffer;
////}
////class MapBigInt2Int
////{
////	map<string,int> string2int;
////public:
////	MapBigInt2Int(){}
////	bool ispresent(mpz_t& r)
////	{
////		char* buffer;
////		buffer=BigIntToString(r);
////		if(string2int.find(buffer)!=string2int.end())
////		{
////			delete(buffer);
////			return true;
////		}
////		delete(buffer);
////		return false;
////	}
////	int get(mpz_t& r)
////	{
////		char* buffer;
////		buffer=BigIntToString(r);
////		assert(ispresent(r));
////		int temp=string2int[buffer];
////		delete(buffer);
////		return temp;
////	}
////	void set(mpz_t& r, int q)
////	{
////		char* buffer;
////		buffer=BigIntToString(r);
////		string2int[buffer]=q;
////		//delete(buffer);
////	}
////};
////
////class MapInt2BigInt
////{
////	map<int,string> int2string;
////public:
////	MapInt2BigInt(){}
////	bool ispresent(int r)
////	{
////		return (int2string.find(r)!=int2string.end());
////	}
////	void get(int r, mpz_t& q)
////	{
////		assert(ispresent(r));
////		string buffer=int2string[r];
////		mpz_set_str(q,buffer.c_str(),10);
////	}
////	void set(int r, mpz_t& q)
////	{
////		char* buffer;
////		buffer=BigIntToString(q);
////		int2string[r]=buffer;
////		//delete(buffer);
////	}
////};
////
////
////
////
////
////struct SparseFunctionBigInt
////{
////	vector<Variable*> cond_variables;
////	Variable* marg_variable;
////	vector<Variable*> all_variables;
////	vector<Double> table;
////	vector<Double> num_times;
////	//map<mpz_t,string> val2addr;
////	//map<string,mpz_t> addr2val;
////	MapBigInt2Int val2addr;
////	MapInt2BigInt addr2val;
////
////	SparseFunctionBigInt(){ }
////	void getMessage(SparseFunctionBigInt& message)
////	{
////		message=SparseFunctionBigInt();
////		message.cond_variables=cond_variables;
////		message.all_variables=cond_variables;
////		message.marg_variable=NULL;
////		
////		for(int i=0;i<table.size();i++)
////		{
////			//assert(addr2val.find(i)!=addr2val.end());
////			//int val=addr2val[i];
////			mpz_t val;
////			addr2val.get(i,val);
////			Variable::setAddressBigInt(all_variables,val);
////			//int message_val=Variable::getAddress(cond_variables);
////			mpz_t message_val;
////			Variable::getAddressBigInt(cond_variables,message_val);
////			// If message_val already exists in the message then update it
////			//if(message.val2addr.find(message_val)!=message.val2addr.end())
////			if(message.val2addr.ispresent(message_val))
////			{
////				//int message_addr=message.val2addr[message_val];
////				int message_addr=message.val2addr.get(message_val);
////				message.table[message_addr]+=(table[i]*num_times[i]);
////				message.num_times[message_addr]+=num_times[i];
////			}
////			else // Add entry1 to the message
////			{
////				int message_addr=message.table.size();
////				message.table.push_back(table[i]*num_times[i]);
////				message.num_times.push_back(num_times[i]);
////				//message.addr2val[message_addr]=message_val;
////				//message.val2addr[message_val]=message_addr;
////				message.addr2val.set(message_addr,message_val);
////				message.val2addr.set(message_val,message_addr);
////			}
////		}
////
////		// Now compute average i.e. divide all entries in the message table by num_times;
////		for(int i=0;i<message.table.size();i++)
////		{
////			message.table[i]/=message.num_times[i];
////		}
////	}
////
////	void multiply(SparseFunctionBigInt& message)
////	{
////		for(int i=0;i<table.size();i++)
////		{
////			//assert(addr2val.find(i)!=addr2val.end());
////			//int val=addr2val[i];
////			mpz_t val;
////			addr2val.get(i,val);
////			Variable::setAddressBigInt(all_variables,val);
////			//int message_val=Variable::getAddress(message.all_variables);
////			mpz_t message_val;
////			Variable::getAddressBigInt(message.all_variables,message_val);
////			//if(message.val2addr.find(message_val)==message.val2addr.end())
////			//{
////			//	cout<<"Message val = "<<message_val<<" val = "<<val<<endl;
////			//	message.print();
////			//	this->print();
////			//}
////			//assert(message.val2addr.find(message_val)!=message.val2addr.end());
////			//int message_addr=message.val2addr[message_val];
////			int message_addr=message.val2addr.get(message_val);
////			table[i]*=message.table[message_addr];
////			//assert(num_times[i]==message.num_times[i]);
////		}
////	}
////	void addAssignment(Double& weight)
////	{
////		//for(int i=0;i<all_variables.size();i++)
////		//{
////			//int entry=Variable::getAddress(all_variables);
////			mpz_t entry;
////			Variable::getAddressBigInt(all_variables,entry);
////			//If the entry is already present add weight to it
////			//if(val2addr.find(entry)!=val2addr.end())
////			if(val2addr.ispresent(entry))
////			{
////				//int addr=val2addr[entry];
////				int addr=val2addr.get(entry);
////				assert(addr < (int)table.size());
////				//table[addr]+=weight;
////				num_times[addr]+=Double(1.0);
////			}
////			else
////			{
////				int addr=(int)table.size();
////				table.push_back(weight);
////				num_times.push_back(Double(1.0));
////				//val2addr[entry]=addr;
////				//addr2val[addr]=entry;
////				val2addr.set(entry,addr);
////				addr2val.set(addr,entry);
////			}
////		//}
////	}
////	void print()
////	{
////		/*cout<<"_______________________________________\n";
////		cout<<"Vars = ";
////		if(marg_variable!=NULL)
////		{
////			cout<<marg_variable->id()<<"| ";
////		}
////		for(int i=0;i<cond_variables.size();i++)
////		{
////			cout<<cond_variables[i]->id()<<" ";
////		}
////		cout<<endl;
////		cout<<"Tab-size = "<<table.size()<<endl;
////		for(int i=0;i<table.size();i++)
////			cout<<table[i]<<" ";
////		cout<<endl;
////		cout<<endl;
////		cout<<"____________________________________\n";*/
////	}
////};
////struct AOGraphNodeBigInt
////{
////	SparseFunctionBigInt function;
////	vector<Function*> bucket_functions;
////	AOGraphNodeBigInt* parent;
////	AOGraphNodeBigInt():parent(NULL){ }
////	void addAssignment(Double& q_value)
////	{
////		//Make sure that all variables are assigned a value
////		for(int i=0;i<function.all_variables.size();i++)
////			assert(function.all_variables[i]->value()!=INVALID_VALUE);
////		Double weight(1.0);
////		for(int i=0;i<bucket_functions.size();i++)
////		{
////			int entry=Variable::getAddress(bucket_functions[i]->variables());
////			weight*=bucket_functions[i]->table()[entry];
////		}
////		weight/=q_value;
////		function.addAssignment(weight);
////	}
////	void sendMessage()
////	{
////		SparseFunctionBigInt message;
////		function.getMessage(message);
////		if(parent!=NULL)
////			parent->function.multiply(message);
////	}
////};
////class AOGraphBigInt
////{
////public:
////	vector<AOGraphNodeBigInt> all_nodes;
////	AOGraphBigInt(GM& gm, Graph& g, vector<int>& order)
////	{
////		Graph triangulated_graph;
////		g.getTriangulatedGraph(triangulated_graph,order);
////
////		// Read the cliques from the triangulated graph
////		vector<vector<int> > cliques;
////		triangulated_graph.getCliques(order,cliques);
////
////		vector<int> mapped_order(order.size());
////		for(int i=0;i<order.size();i++)
////			mapped_order[order[i]]=i;
////		// Now form the AOGraph nodes
////		all_nodes=vector<AOGraphNodeBigInt>(order.size());
////		for(int i=0;i<all_nodes.size();i++)
////		{
////			for(int j=0;j<cliques[i].size();j++)
////			{
////				if(j==0)
////				{
////					all_nodes[i].function.all_variables.push_back(gm.variables[cliques[i][j]]);
////					all_nodes[i].function.marg_variable=gm.variables[cliques[i][j]];
////					assert(order[i]==cliques[i][j]);
////				}
////				else
////				{
////					all_nodes[i].function.all_variables.push_back(gm.variables[cliques[i][j]]);
////					all_nodes[i].function.cond_variables.push_back(gm.variables[cliques[i][j]]);
////				}
////			}
////
////			sort(all_nodes[i].function.all_variables.begin(),all_nodes[i].function.all_variables.end(),less_than_comparator_variable);
////			sort(all_nodes[i].function.cond_variables.begin(),all_nodes[i].function.cond_variables.end(),less_than_comparator_variable);
////		}
////
////		// Set the appropriate parents
////		for(int i=0;i<all_nodes.size();i++)
////		{
////			
////			int min_id=gm.variables.size();
////			for(int j=0;j<all_nodes[i].function.cond_variables.size();j++)
////			{
////
////				int id=mapped_order[all_nodes[i].function.cond_variables[j]->id()];
////				if(id < min_id)
////				{
////					min_id=id;
////				}
////			}
////			if(min_id != (int)gm.variables.size())
////			{
////				assert(all_nodes[min_id].parent==NULL);
////				all_nodes[i].parent=&all_nodes[min_id];
////			}
////		}
////
////		// Set the bucket_functions
////		for(int i=0;i<gm.functions.size();i++)
////		{
////			int min_id=gm.variables.size();
////			for(int j=0;j<gm.functions[i]->variables().size();j++)
////			{
////				int id=mapped_order[gm.functions[i]->variables()[j]->id()];
////				if(id < min_id)
////				{
////					min_id=id;
////				}
////			}
////			assert(min_id != (int)gm.variables.size());
////			all_nodes[min_id].bucket_functions.push_back(gm.functions[i]);
////		}
////	}
////
////	void addAssignment(vector<Double>& q_value)
////	{
////		for(int i=0;i<all_nodes.size();i++)
////		{
////			assert(all_nodes[i].function.marg_variable!=NULL);
////			int var_id=all_nodes[i].function.marg_variable->id();
////			all_nodes[i].addAssignment(q_value[var_id]);
////		}
////	}
////
////	Double pe()
////	{
////		/*cerr<<"Computine pe\n";
////		for(int i=0;i<all_nodes.size();i++)
////		{
////			all_nodes[i].function.print();
////		}*/
////		
////		for(int i=0;i<all_nodes.size();i++)
////		{
////			all_nodes[i].sendMessage();
////		}
////		cout<<"Messages sent\n";
////		SparseFunctionBigInt func;
////		all_nodes[all_nodes.size()-1].function.getMessage(func);
////		assert((int)func.table.size()==1);
////		return func.table[0];
////	}
////};
////#endif
//
//
////
////
////
////
////
////
////
////
////
////
////
////
////
////
////
////
////
////
////
////
//////#ifndef AOGRAPH_BigInt_H_
//////#define AOGRAPH_BigInt_H_
//////#include <iostream>
//////#include <vector>
//////#include <map>
//////#include "GM.h"
//////#include "Graph.h"
//////#include <gmp.h>
//////
//////
//////using namespace std;
//////
//////char* BigIntToString(mpz_t& _xCount) 
//////{
//////    // Caller responsible for deleting the returned string.
//////    int buf_size = mpz_sizeinbase (_xCount, 10) + 2;
//////    char* buffer = new char[buf_size];
//////    mpz_get_str(buffer, 10, _xCount);
//////    return buffer;
//////}
//////class MapBigInt2Int
//////{
//////	map<string,int> string2int;
//////public:
//////	MapBigInt2Int(){}
//////	bool ispresent(mpz_t& r)
//////	{
//////		char* buffer;
//////		buffer=BigIntToString(r);
//////		if(string2int.find(buffer)!=string2int.end())
//////		{
//////			delete(buffer);
//////			return true;
//////		}
//////		delete(buffer);
//////		return false;
//////	}
//////	int get(mpz_t& r)
//////	{
//////		char* buffer;
//////		buffer=BigIntToString(r);
//////		assert(ispresent(r));
//////		int temp=string2int[buffer];
//////		delete(buffer);
//////		return temp;
//////	}
//////	void set(mpz_t& r, int q)
//////	{
//////		char* buffer;
//////		buffer=BigIntToString(r);
//////		string2int[buffer]=q;
//////		//delete(buffer);
//////	}
//////};
//////
//////class MapInt2BigInt
//////{
//////	map<int,string> int2string;
//////public:
//////	MapInt2BigInt(){}
//////	bool ispresent(int r)
//////	{
//////		return (int2string.find(r)!=int2string.end());
//////	}
//////	void get(int r, mpz_t& q)
//////	{
//////		assert(ispresent(r));
//////		string buffer=int2string[r];
//////		mpz_set_str(q,buffer.c_str(),10);
//////	}
//////	void set(int r, mpz_t& q)
//////	{
//////		char* buffer;
//////		buffer=BigIntToString(q);
//////		int2string[r]=buffer;
//////		//delete(buffer);
//////	}
//////};
//////
//////
//////
//////
//////
//////struct SparseFunctionBigInt
//////{
//////	vector<Variable*> cond_variables;
//////	Variable* marg_variable;
//////	vector<Variable*> all_variables;
//////	vector<Double> table;
//////	vector<Double> num_times;
//////	//map<mpz_t,string> val2addr;
//////	//map<string,mpz_t> addr2val;
//////	MapBigInt2Int val2addr;
//////	MapInt2BigInt addr2val;
//////
//////	SparseFunctionBigInt(){ }
//////	void getMessage(SparseFunctionBigInt& message)
//////	{
//////		message=SparseFunctionBigInt();
//////		message.cond_variables=cond_variables;
//////		message.all_variables=cond_variables;
//////		message.marg_variable=NULL;
//////		
//////		for(int i=0;i<table.size();i++)
//////		{
//////			//assert(addr2val.find(i)!=addr2val.end());
//////			//int val=addr2val[i];
//////			mpz_t val;
//////			addr2val.get(i,val);
//////			Variable::setAddressBigInt(all_variables,val);
//////			//int message_val=Variable::getAddress(cond_variables);
//////			mpz_t message_val;
//////			Variable::getAddressBigInt(cond_variables,message_val);
//////			// If message_val already exists in the message then update it
//////			//if(message.val2addr.find(message_val)!=message.val2addr.end())
//////			if(message.val2addr.ispresent(message_val))
//////			{
//////				//int message_addr=message.val2addr[message_val];
//////				int message_addr=message.val2addr.get(message_val);
//////				message.table[message_addr]+=(table[i]*num_times[i]);
//////				message.num_times[message_addr]+=num_times[i];
//////			}
//////			else // Add entry1 to the message
//////			{
//////				int message_addr=message.table.size();
//////				message.table.push_back(table[i]*num_times[i]);
//////				message.num_times.push_back(num_times[i]);
//////				//message.addr2val[message_addr]=message_val;
//////				//message.val2addr[message_val]=message_addr;
//////				message.addr2val.set(message_addr,message_val);
//////				message.val2addr.set(message_val,message_addr);
//////			}
//////		}
//////
//////		// Now compute average i.e. divide all entries in the message table by num_times;
//////		for(int i=0;i<message.table.size();i++)
//////		{
//////			message.table[i]/=message.num_times[i];
//////		}
//////	}
//////
//////	void multiply(SparseFunctionBigInt& message)
//////	{
//////		for(int i=0;i<table.size();i++)
//////		{
//////			//assert(addr2val.find(i)!=addr2val.end());
//////			//int val=addr2val[i];
//////			mpz_t val;
//////			addr2val.get(i,val);
//////			Variable::setAddressBigInt(all_variables,val);
//////			//int message_val=Variable::getAddress(message.all_variables);
//////			mpz_t message_val;
//////			Variable::getAddressBigInt(message.all_variables,message_val);
//////			//if(message.val2addr.find(message_val)==message.val2addr.end())
//////			//{
//////			//	cout<<"Message val = "<<message_val<<" val = "<<val<<endl;
//////			//	message.print();
//////			//	this->print();
//////			//}
//////			//assert(message.val2addr.find(message_val)!=message.val2addr.end());
//////			//int message_addr=message.val2addr[message_val];
//////			int message_addr=message.val2addr.get(message_val);
//////			table[i]*=message.table[message_addr];
//////			//assert(num_times[i]==message.num_times[i]);
//////		}
//////	}
//////	void addAssignment(Double& weight)
//////	{
//////		//for(int i=0;i<all_variables.size();i++)
//////		//{
//////			//int entry=Variable::getAddress(all_variables);
//////			mpz_t entry;
//////			Variable::getAddressBigInt(all_variables,entry);
//////			//If the entry is already present add weight to it
//////			//if(val2addr.find(entry)!=val2addr.end())
//////			if(val2addr.ispresent(entry))
//////			{
//////				//int addr=val2addr[entry];
//////				int addr=val2addr.get(entry);
//////				assert(addr < (int)table.size());
//////				//table[addr]+=weight;
//////				num_times[addr]+=Double(1.0);
//////			}
//////			else
//////			{
//////				int addr=(int)table.size();
//////				table.push_back(weight);
//////				num_times.push_back(Double(1.0));
//////				//val2addr[entry]=addr;
//////				//addr2val[addr]=entry;
//////				val2addr.set(entry,addr);
//////				addr2val.set(addr,entry);
//////			}
//////		//}
//////	}
//////	void print()
//////	{
//////		/*cout<<"_______________________________________\n";
//////		cout<<"Vars = ";
//////		if(marg_variable!=NULL)
//////		{
//////			cout<<marg_variable->id()<<"| ";
//////		}
//////		for(int i=0;i<cond_variables.size();i++)
//////		{
//////			cout<<cond_variables[i]->id()<<" ";
//////		}
//////		cout<<endl;
//////		cout<<"Tab-size = "<<table.size()<<endl;
//////		for(int i=0;i<table.size();i++)
//////			cout<<table[i]<<" ";
//////		cout<<endl;
//////		cout<<endl;
//////		cout<<"____________________________________\n";*/
//////	}
//////};
//////struct AOGraphNodeBigInt
//////{
//////	SparseFunctionBigInt function;
//////	vector<Function*> bucket_functions;
//////	AOGraphNodeBigInt* parent;
//////	AOGraphNodeBigInt():parent(NULL){ }
//////	void addAssignment(Double& q_value)
//////	{
//////		//Make sure that all variables are assigned a value
//////		for(int i=0;i<function.all_variables.size();i++)
//////			assert(function.all_variables[i]->value()!=INVALID_VALUE);
//////		Double weight(1.0);
//////		for(int i=0;i<bucket_functions.size();i++)
//////		{
//////			int entry=Variable::getAddress(bucket_functions[i]->variables());
//////			weight*=bucket_functions[i]->table()[entry];
//////		}
//////		weight/=q_value;
//////		function.addAssignment(weight);
//////	}
//////	void sendMessage()
//////	{
//////		SparseFunctionBigInt message;
//////		function.getMessage(message);
//////		if(parent!=NULL)
//////			parent->function.multiply(message);
//////	}
//////};
//////class AOGraphBigInt
//////{
//////public:
//////	vector<AOGraphNodeBigInt> all_nodes;
//////	AOGraphBigInt(GM& gm, Graph& g, vector<int>& order)
//////	{
//////		Graph triangulated_graph;
//////		g.getTriangulatedGraph(triangulated_graph,order);
//////
//////		// Read the cliques from the triangulated graph
//////		vector<vector<int> > cliques;
//////		triangulated_graph.getCliques(order,cliques);
//////
//////		vector<int> mapped_order(order.size());
//////		for(int i=0;i<order.size();i++)
//////			mapped_order[order[i]]=i;
//////		// Now form the AOGraph nodes
//////		all_nodes=vector<AOGraphNodeBigInt>(order.size());
//////		for(int i=0;i<all_nodes.size();i++)
//////		{
//////			for(int j=0;j<cliques[i].size();j++)
//////			{
//////				if(j==0)
//////				{
//////					all_nodes[i].function.all_variables.push_back(gm.variables[cliques[i][j]]);
//////					all_nodes[i].function.marg_variable=gm.variables[cliques[i][j]];
//////					assert(order[i]==cliques[i][j]);
//////				}
//////				else
//////				{
//////					all_nodes[i].function.all_variables.push_back(gm.variables[cliques[i][j]]);
//////					all_nodes[i].function.cond_variables.push_back(gm.variables[cliques[i][j]]);
//////				}
//////			}
//////
//////			sort(all_nodes[i].function.all_variables.begin(),all_nodes[i].function.all_variables.end(),less_than_comparator_variable);
//////			sort(all_nodes[i].function.cond_variables.begin(),all_nodes[i].function.cond_variables.end(),less_than_comparator_variable);
//////		}
//////
//////		// Set the appropriate parents
//////		for(int i=0;i<all_nodes.size();i++)
//////		{
//////			
//////			int min_id=gm.variables.size();
//////			for(int j=0;j<all_nodes[i].function.cond_variables.size();j++)
//////			{
//////
//////				int id=mapped_order[all_nodes[i].function.cond_variables[j]->id()];
//////				if(id < min_id)
//////				{
//////					min_id=id;
//////				}
//////			}
//////			if(min_id != (int)gm.variables.size())
//////			{
//////				assert(all_nodes[min_id].parent==NULL);
//////				all_nodes[i].parent=&all_nodes[min_id];
//////			}
//////		}
//////
//////		// Set the bucket_functions
//////		for(int i=0;i<gm.functions.size();i++)
//////		{
//////			int min_id=gm.variables.size();
//////			for(int j=0;j<gm.functions[i]->variables().size();j++)
//////			{
//////				int id=mapped_order[gm.functions[i]->variables()[j]->id()];
//////				if(id < min_id)
//////				{
//////					min_id=id;
//////				}
//////			}
//////			assert(min_id != (int)gm.variables.size());
//////			all_nodes[min_id].bucket_functions.push_back(gm.functions[i]);
//////		}
//////	}
//////
//////	void addAssignment(vector<Double>& q_value)
//////	{
//////		for(int i=0;i<all_nodes.size();i++)
//////		{
//////			assert(all_nodes[i].function.marg_variable!=NULL);
//////			int var_id=all_nodes[i].function.marg_variable->id();
//////			all_nodes[i].addAssignment(q_value[var_id]);
//////		}
//////	}
//////
//////	Double pe()
//////	{
//////		/*cerr<<"Computine pe\n";
//////		for(int i=0;i<all_nodes.size();i++)
//////		{
//////			all_nodes[i].function.print();
//////		}*/
//////		
//////		for(int i=0;i<all_nodes.size();i++)
//////		{
//////			all_nodes[i].sendMessage();
//////		}
//////		cout<<"Messages sent\n";
//////		SparseFunctionBigInt func;
//////		all_nodes[all_nodes.size()-1].function.getMessage(func);
//////		assert((int)func.table.size()==1);
//////		return func.table[0];
//////	}
//////};
//////#endif

