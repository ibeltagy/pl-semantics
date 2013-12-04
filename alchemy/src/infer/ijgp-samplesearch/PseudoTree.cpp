#include "AOTree.h"
#include "GM.h"
#include <fstream>
#include <stack>
#include <queue>

#include <boost/config.hpp>
#include <boost/graph/graph_utility.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/pending/disjoint_sets.hpp>
#include <boost/graph/connected_components.hpp>
#include <boost/graph/copy.hpp>
#include "Graph.h"
#include "Util.h"

using namespace boost;

typedef adjacency_list <vecS, vecS, undirectedS,property<vertex_name_t, int> > BG;
typedef graph_traits<BG>::vertex_descriptor Vertex;
typedef graph_traits<BG>::vertices_size_type size_type;

int getFirstVertex(BG& bg)
{
	property_map <BG, vertex_name_t>::type name_map=get(vertex_name,bg);
	graph_traits<BG>::vertex_iterator i,end;
	for (tie(i, end) = vertices(bg); i != end; ++i)
	{
		return name_map[*i];
	}
	return -1;
}

void PseudoTree::print(ostream& out)
{
	stack<PseudoTreeNode*> s;
	s.push(root);
	int height=0;
	while(!s.empty())
	{
		PseudoTreeNode* ps=s.top();
		s.pop();
		out<<ps->variable->id()<<" "<<ps->children.size()<<endl;
		if(!ps->children.empty())
			height++;
		for(int i=0;i<ps->children.size();i++)
		{
			s.push(ps->children[i]);
		}
	}
	cerr<<"Height of Tree = "<<height<<endl;
}
void PseudoTree::generateAncestors(GM& gm, vector<vector<Variable*> >& ancestors)
{
	ancestors=vector<vector<Variable*> > (gm.variables.size());
	stack<PseudoTreeNode*> s;
	s.push(root);
	while(!s.empty())
	{
		PseudoTreeNode* ps=s.top();
		s.pop();
		for(int i=0;i<ps->children.size();i++)
		{
			s.push(ps->children[i]);
			ancestors[ps->children[i]->variable->id()]=ancestors[ps->variable->id()];
			ancestors[ps->children[i]->variable->id()].push_back(ps->variable);
		}
	}
	for(int i=0;i<ancestors.size();i++)
	{
		std::sort(ancestors[i].begin(),ancestors[i].end(),less_than_comparator_variable);
	}
}
/*
PseudoTree::PseudoTree(GM& gm, JG& jg, vector<int>& sampling_order,vector<int>& order)
{
	set<int> all_vars;
	vector<PseudoTreeNode*> all_nodes (gm.variables.size());
	for(int i=0;i<gm.variables.size();i++)
	{
		all_vars.insert(i);
	}
	vector<vector<int> > separators;
	vector<set<int> > all_hyp_vertices;
	vector<set<int> > all_hyp_edges (gm.variables.size());
	for(int i=0;i<jg.nodes.size();i++)
	{
		set<int> tmp;
		for(int j=0;j<jg.nodes[i]->variables().size();j++)
		{
			tmp.insert(jg.nodes[i]->variables()[j]->id());
			all_hyp_edges[jg.nodes[i]->variables()[j]->id()].insert(i);
		}
		all_hyp_vertices.push_back(tmp);
	}
	
	int node_count=0;
	while(!all_vars.empty())
	{

		//Count the number of vertices and edges
		int num_hyp_vertices=0,num_hyp_edges=0;
		vector<int> which_partition(all_hyp_vertices.size());
		vector<int> function_to_hyper_vertices(all_hyp_vertices.size());
		//map<int,int> hyp_edges_to_variable_index;
		for(int i=0;i<all_hyp_vertices.size();i++)
		{
			function_to_hyper_vertices[i]=-1;
			if(!all_hyp_vertices[i].empty())
			{
				//hyp_vertices_to_function_index[num_hyp_vertices]=i;
				function_to_hyper_vertices[i]=num_hyp_vertices;
				num_hyp_vertices++;
			}
			which_partition[i]=-1;
		}
		for(int i=0;i<all_hyp_edges.size();i++)
		{
			if(!all_hyp_edges[i].empty())
			{
				//hyp_edges_to_variable_index[num_hyp_edges]=i;
				num_hyp_edges++;
			}
		}
		ofstream out("tmp.hgr");
		out<<num_hyp_vertices<<" "<<num_hyp_edges<<endl;
		//Write the edges
		for(int i=0;i<all_hyp_edges.size();i++)
		{
			if(all_hyp_edges[i].empty())
				continue;
			for(set<int>::iterator j=all_hyp_edges[i].begin();j!=all_hyp_edges[i].end();j++)
			{
				assert(function_to_hyper_vertices[*j]!=-1);
				out<<function_to_hyper_vertices[*j]<<" ";
			}
			out<<endl;
		}
		out.close();

		char command[100];
		system("./shmetis tmp.hgr 3 10");
		ifstream in ("tmp.hgr.part.3");

		for(int i=0;i<all_hyp_vertices.size();i++)
		{
			if(!all_hyp_vertices[i].empty())
			{
				int tmp;
				in>>tmp;
				which_partition[i]=tmp;
			}
		}
		in.close();
		// Now go through all the hyperedges and select those that have hypervertices belonging to different partitions
		vector<int> curr_sep;
		for(int i=0;i<all_hyp_edges.size();i++)
		{
			if(!all_hyp_edges[i].empty())
			{
				int curr=-1;
				bool to_add=false;
				for(set<int>::iterator j=all_hyp_edges[i].begin();j!=all_hyp_edges[i].end();j++)
				{
					assert(which_partition[*j]!=-1);
					if(curr==-1)
						curr=*j;
					else if(curr!=(*j))
					{
						to_add=true;
						break;
					}
				}
				if(to_add)
					curr_sep.push_back(i);
			}
		}

		// Now that we have a separator, update the ordering
		for(int i=0;i<curr_sep.size();i++)
		{
			sampling_order.push_back(curr_sep[i]);
			all_vars.erase(curr_sep[i]);
			for(set<int>::iterator j=all_hyp_edges[curr_sep[i]].begin();j!=all_hyp_edges[curr_sep[i]].end();j++)
			{
				all_hyp_vertices[*j].erase(curr_sep[i]);
			}
			all_hyp_edges[curr_sep[i]]=set<int>();	
		}
	}

	//Set the order to reverse of sampling_order
	order=vector<int> (sampling_order.size());
	for(int i=0;i<sampling_order.size();i++)
		order[order.size()-i]=sampling_order[i];
}
*/

PseudoTree::PseudoTree(GM& gm,Graph& g ,vector<int>& order)
{
	//cout<<"num-vertices = "<<g.getNumberOfVertices()<<endl;
	vector<PseudoTreeNode*> all_nodes (gm.variables.size());
	for(int i=0;i<all_nodes.size();i++)
	{
		all_nodes[i]=new PseudoTreeNode();
		all_nodes[i]->variable=gm.variables[i];
	}
	BG bg(g.getNumberOfVertices());
	vector<int> mapped_order(order.size());
	for(int i=0;i<order.size();i++)
		mapped_order[order[i]]=i;
	const vector<vector<bool> >& adj_matrix=g.getAdjMatrix();
	for(int i=0;i<adj_matrix.size();i++)
	{
		for(int j=i+1;j<adj_matrix.size();j++)
		{
			if(adj_matrix[i][j])
			{
				add_edge(mapped_order[i],mapped_order[j],bg);
			}
		}
	}
	//cout<<"Constructed boost graph\n";

	property_map <BG, vertex_name_t>::type name_map=get(vertex_name,bg);
	graph_traits<BG>::vertex_iterator i,end;
	for (tie(i, end) = vertices(bg); i != end; ++i)
		name_map[*i] = order[*i];
	//print_graph(bg,name_map);
	//cout<<"Graph printed\n";
	boost::queue<BG*> q;
	vector<BG*> all_graphs;
	q.push(&bg);
	int count=0;
	while(!q.empty())
	{
		BG* q_g=q.top();
		q.pop();

		int var=getFirstVertex(*q_g);
		if(var==-1)
			continue;
		//cout<<"Var = "<<var<<" "<<flush;
		//cout<<"num-components = "<<num<<endl;
		clear_vertex(0,*q_g);
		remove_vertex(0,*q_g);

		std::vector<int> components(num_vertices(*q_g));
		int num = connected_components(*q_g, &components[0]);
		vector<vector<int> > vars_in_components(num);
		vector<vector<int> > vars_not_in_components(num);
		vector<int> all_vars;
		for(int i=0;i<components.size();i++)
		{
			vars_in_components[components[i]].push_back(i);
			all_vars.push_back(i);
		}
		for(int i=0;i<num;i++)
		{
			do_set_difference(all_vars,vars_in_components[i],vars_not_in_components[i]);
			//cout<<"Comp "<<i<<" "<<endl;
			//printVector(vars_in_components[i]);
			//printVector(vars_not_in_components[i]);
		}


		//Form graphs and add them to the queue
		for(int i=0;i<num;i++)
		{
			BG* new_q_g=new BG();
			all_graphs.push_back(new_q_g);
			copy_graph(*q_g,*new_q_g);
			
			//Remove variables not in the component from the new graph
			for(int j=vars_not_in_components[i].size()-1;j>-1;j--)
			{
				clear_vertex(vars_not_in_components[i][j],*new_q_g);
				remove_vertex(vars_not_in_components[i][j],*new_q_g);
			}

			int new_var=getFirstVertex(*new_q_g);
			if(new_var !=-1)
			{
				//cout<<new_var<<endl;
				q.push(new_q_g);
				assert(var < gm.variables.size());
				assert(new_var < gm.variables.size());
				all_nodes[var]->children.push_back(all_nodes[new_var]);
			}
		}
	}
	for(int i=0;i<all_graphs.size();i++)
	{
		delete(all_graphs[i]);
	}
	if(!all_nodes.empty())
		root=all_nodes[order[0]];

	//ifstream dfs(dfs_file);
	//vector<vector<int> > clusters;
	//char buffer[100];
	//cout<<"Reading DFS file\n";
	//char cCheck='P';
	//while(true)
	//{
	//	dfs>>cCheck;
	//	if(cCheck=='O')
	//	{
	//		dfs.getline(buffer,100);
	//		break;
	//	}
	//	else
	//	{
	//		dfs.putback(cCheck);
	//		int temp=0;
	//		vector<int> curr_cluster;
	//		while(temp!=-1)
	//		{
	//			dfs>>temp;
	//			if(temp>-1)
	//			{
	//				assert(ids_to_vars[temp]>-1);
	//				if(ids_to_vars[temp]!=-1)
	//				{

	//					curr_cluster.push_back(ids_to_vars[temp]);
	//				}
	//			}

	//		}
	//		clusters.push_back(curr_cluster);
	//	}
	//}
	//cout<<"Read clusters\n";
	////vector<int> order(gm.variables.size());
	////for(int i=0;i<gm.variables.size();i++)
	////{
	////	dfs>>order[i];
	////}
	////dfs.close();


	//vector<vector<PseudoTreeNode*> > all_nodes(clusters.size());
	////root=new PseudoTreeNode();
	//for(int i=0;i<clusters.size();i++)
	//{
	//	all_nodes[i]=vector<PseudoTreeNode*> (clusters[i].size());
	//	for(int j=0;j<all_nodes[i].size();j++)
	//	{
	//		all_nodes[i][j]=new PseudoTreeNode();
	//		if(j>0)
	//		{
	//			all_nodes[i][j-1]->children.push_back(all_nodes[i][j]);
	//		}

	//		all_nodes[i][j]->variable=gm.variables[clusters[i][j]];
	//	}
	//}
	//for(int i=0;i<clusters.size();i++)
	//{
	//	int temp=0;
	//	while(true)
	//	{
	//		dfs>>temp;
	//		if(temp==-1)
	//			break;
	//		assert(!all_nodes[i].empty());
	//		assert(!all_nodes[temp].empty());
	//		all_nodes[i][all_nodes[i].size()-1]->children.push_back(all_nodes[temp][0]);
	//	}
	//}
	//assert(!all_nodes[0].empty());
	//root=all_nodes[0][0];
	//dfs.close();
	//stack<PseudoTreeNode*> s;
	//s.push(root);
	//order=vector<int>();
	//cout<<"Printing vars\n";
	//while(!s.empty())
	//{
	//	PseudoTreeNode* curr=s.top();
	//	s.pop();
	//	if(curr->variable!=NULL)
	//	{
	//		order.push_back(curr->variable->id());
	//		cout<<curr->variable->id()<<endl;
	//	}
	//	for(int i=0;i<curr->children.size();i++)
	//	{
	//		s.push(curr->children[i]);
	//	}
	//}
	////cout<<"Order computed..Size = "<<order.size()<<" vars = "<<gm.variables.size()<<endl;
	//assert(order.size()==gm.variables.size());
	//vector <bool> check(order.size());
	//for(int i=0;i<order.size();i++)
	//{
	//	assert(order[i]<order.size());
	//	check[order[i]]=true;
	//}

	//for(int i=0;i<check.size();i++)
	//{
	//	if(!check[i])
	//	{
	//		cout<<"Error \n";
	//		exit(1);
	//	}
	//}
}
