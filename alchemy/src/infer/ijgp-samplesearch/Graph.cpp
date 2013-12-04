#include "Graph.h"
#include "CPT.h"
// Functions for class Graph
// The 3 constructors defined in the main class
Graph::Graph():num_vertices(0),directed(false),num_edges(0)
{}

void Graph::findRelevantNodes(vector<bool>& nodes, const vector<int>& evidence_nodes)
{
	nodes=vector<bool>(num_vertices);
	for(int i=0;i<num_vertices;i++)
		nodes[i]=false;
	for(int i=0;i<evidence_nodes.size();i++)
	{
		vector<bool> visited(num_vertices);
		int curr_node=evidence_nodes[i];
		for(int j=0;j<num_vertices;j++)
			visited[j]=false;
		list<int> the_list;
		the_list.push_back(evidence_nodes[i]);
		while(!the_list.empty())
		{
			//cerr<<curr_node<<endl;
			visited[curr_node]=true;
			list<int>::iterator curr,last;
			curr=parent_list[curr_node].begin();
			last=parent_list[curr_node].end();
			while(curr!=last)
			{
				if(!visited[*curr])
					the_list.push_back(*curr);
				curr++;
			}
			if(the_list.empty())
				break;
			do{
				the_list.pop_front();
				curr_node=*the_list.begin();
			}while(visited[curr_node] && !the_list.empty());
		}
		for(int j=0;j<visited.size();j++)
			if(visited[j])
				nodes[j]=true;
	}
}
Graph::Graph(const Graph& g):
num_vertices(g.getNumberOfVertices()),
directed(g.isDirected()),
num_edges(g.getNumberOfEdges()),
edge_list(g.getEdgeList()),
parent_list(g.getParentList()),
child_list(g.getChildList()),
adj_matrix(g.getAdjMatrix()),
vertex_ids(g.getIds())

{}

Graph::Graph(int num_vertices,const vector<vector<bool> > &adj_matrix, const bool directed=false):
num_vertices(num_vertices),adj_matrix(adj_matrix),directed(directed)
{
	int i,j;
	num_edges=0;
	if(directed)
	{
		parent_list = vector<list<int> > (num_vertices);
		child_list = vector<list<int> > (num_vertices);
		for(i=0;i<num_vertices;i++)
		{
			for(j=0;j<num_vertices;j++)
			{
				if(adj_matrix[i][j])
				{
					parent_list[j].push_back(i);
					child_list[i].push_back(j);
					num_edges++;
				}
			}
		}
	}
	else
	{
		edge_list = vector<list<int> > (num_vertices);
		for(i=0;i<num_vertices;i++)
		{
			for(j=0;j<num_vertices;j++)
			{
				if(adj_matrix[i][j])
				{
					edge_list[i].push_back(j);
					num_edges++;
				}
			}
		}
	}
	vertex_ids=vector<int>(num_vertices);
	//set the vertex ids
	for(i=0;i<num_vertices;i++)
	{
		vertex_ids[i]=i;
	}
}

Graph::Graph(int num_vertices,const vector<vector<bool> > &adj_matrix, const vector<int>& vertex_ids_,const bool directed=false):num_vertices(num_vertices),adj_matrix(adj_matrix),directed(directed),vertex_ids(vertex_ids_)
{
	int i,j;
	num_edges=0;
	if(directed)
	{
		parent_list = vector<list<int> > (num_vertices);
		child_list = vector<list<int> > (num_vertices);
		for(i=0;i<num_vertices;i++)
		{
			for(j=0;j<num_vertices;j++)
			{
				if(adj_matrix[i][j])
				{
					parent_list[j].push_back(i);
					child_list[i].push_back(j);
					num_edges++;
				}
			}
		}
	}
	else
	{
		edge_list = vector<list<int> > (num_vertices);
		for(i=0;i<num_vertices;i++)
		{
			for(j=0;j<num_vertices;j++)
			{
				if(adj_matrix[i][j])
				{
					edge_list[i].push_back(j);
					num_edges++;
				}
			}
		}
	}
}
void Graph::getLeaves(vector<int>& leaves)
{
  leaves=vector<int>();
  for(int i=0;i<num_vertices;i++){
    if(child_list[i].empty()){
      leaves.push_back(i);
    }
  }
}
void Graph::makeDirectedGraph(GM* gm)
{
	num_vertices=gm->variables.size();
	directed=true;
	//adj_matrix =vector<vector<bool> >(gm->variables.size());
	//for(int i=0;i<gm->variables.size();i++)
	//	adj_matrix[i]=vector<bool> (gm->variables.size());
	num_edges=0;
	parent_list = vector<list<int> > (num_vertices);
	child_list = vector<list<int> > (num_vertices);
	for(int i=0;i<gm->functions.size();i++)
	{
		CPT* cpt;
		cpt=dynamic_cast<CPT*>(gm->functions[i]);
		assert(cpt!=NULL);
		int marg_var=cpt->marg_variable()->id();
		for(int j=0;j<cpt->cond_variables().size();j++)
		{
			assert(cpt->cond_variables()[j]->id() < num_vertices);
			parent_list[marg_var].push_back(cpt->cond_variables()[j]->id());
			child_list[cpt->cond_variables()[j]->id()].push_back(marg_var);
			num_edges++;
			//adj_matrix[cpt->cond_variables()[j]->id()][marg_var]=true;
		}
	}
	
	/*for(int i=0;i<num_vertices;i++)
	{
		for(int j=0;j<num_vertices;j++)
		{
			if(adj_matrix[i][j])
			{
				parent_list[j].push_back(i);
				child_list[i].push_back(j);
				num_edges++;
			}
		}
	}*/

	vertex_ids=vector<int>(num_vertices);
	//set the vertex ids
	for(int i=0;i<num_vertices;i++)
	{
		vertex_ids[i]=i;
	}
	cerr<<"Directed Graph constructed\n";
}
void Graph::makeGraph(GM* gm)
{
	if(gm==NULL)
		return;
	directed=false;
	num_vertices=gm->variables.size();
	adj_matrix=vector<vector<bool> > (num_vertices);
	for(int i=0;i<num_vertices;i++)
		adj_matrix[i]=vector<bool> (num_vertices);
	for(int i=0;i<gm->functions.size();i++)
	{
		for(int j=0;j<gm->functions[i]->variables().size();j++)
		{
			int var1=gm->functions[i]->variables()[j]->id();
			for(int k=j+1;k<gm->functions[i]->variables().size();k++)
			{
				int var2=gm->functions[i]->variables()[k]->id();
				adj_matrix[var1][var2]=true;
				adj_matrix[var2][var1]=true;
			}
		}
	}

	edge_list = vector<list<int> > (num_vertices);
	num_edges=0;
	for(int i=0;i<num_vertices;i++)
	{
		for(int j=0;j<num_vertices;j++)
		{
			if(adj_matrix[i][j])
			{
				edge_list[i].push_back(j);
				num_edges++;
			}
		}
	}
	vertex_ids=vector<int>(num_vertices);
	//set the vertex ids
	for(int i=0;i<num_vertices;i++)
	{
		vertex_ids[i]=i;
	}
	
}
const int Graph::getNumberOfVertices() const
{
	return num_vertices;
}

const int Graph::getNumberOfEdges() const
{
	return num_edges;
}
const list<int>& Graph::getEdgeList(int i) const
{
	return edge_list[i];
}
const int Graph::getNumberOfNeighbors(int vertex) const
{
	if(vertex >= num_vertices)
		return -1;

	if(directed)
	{
		return (parent_list[vertex].size() + child_list[vertex].size());
	}
	else
	{
		return edge_list[vertex].size();
	}
}

int const Graph::getNumberOfParents(int vertex) const
{
	if(directed)
	{
		return parent_list[vertex].size();
	}

	return -1;
}

const int Graph::getNumberOfChildren(int vertex) const
{
	if(directed)
	{
		return child_list[vertex].size();
	}
	return -1;
}

const vector<list<int> >& Graph::getEdgeList() const
{
	return edge_list;
}

const vector<list<int> >& Graph::getParentList() const
{
	return parent_list;
}
const list<int> & Graph::getParentList(int id) const
{
	int i;
	for(i=0;i<num_vertices;i++)
		if(vertex_ids[i]==id)
			break;
	if(i==num_vertices)
	{
		cout<<"something wrong\n";
		exit(1);
	}
	return parent_list[i];
}
const vector<list<int> >& Graph::getChildList() const
{
	return child_list;
}
const list<int> & Graph::getChildList(int id) const
{
	int i;
	for(i=0;i<num_vertices;i++)
		if(vertex_ids[i]==id)
			break;
	if(i==num_vertices)
	{
		cout<<"something wrong\n";
		exit(1);
	}
	return child_list[i];
}
const vector<vector<bool> >& Graph::getAdjMatrix() const
{
	return adj_matrix;
}

const list<int>& Graph::getParents(int vertex) const
{
	return parent_list[vertex];
}

const int Graph::getIdOfVetex(int i) const
{
	return vertex_ids[i];
}
const vector<int>& Graph::getIds() const
{
	return vertex_ids;
}
//Add an edge to the graph
int Graph::addEdge(int vertex1, int vertex2)
{
	if(directed)
	{
		if(adj_matrix[vertex1][vertex2])
			return -1;
		adj_matrix[vertex1][vertex2]=true;
		parent_list[vertex2].push_back(vertex1);
		child_list[vertex1].push_back(vertex2);
		num_edges++;
	}
	else
	{
		if(adj_matrix[vertex1][vertex2])
			return -1;
		adj_matrix[vertex1][vertex2]=true;
		edge_list[vertex1].push_back(vertex2);
		adj_matrix[vertex2][vertex1]=true;
		edge_list[vertex2].push_back(vertex1);
		num_edges++;
	}
	return 1;
}

int Graph::removeEdge(int vertex1, int vertex2)
{
	if(directed)
	{
		if(adj_matrix[vertex1][vertex2])
		{
			adj_matrix[vertex1][vertex2]=false;
			parent_list[vertex2].remove(vertex1);
			child_list[vertex1].remove(vertex2);
			num_edges--;
		}
		else
			return -1;
	}
	else
	{
		if(adj_matrix[vertex1][vertex2])
		{
			adj_matrix[vertex1][vertex2]=false;
			adj_matrix[vertex2][vertex1]=false;
			edge_list[vertex1].remove(vertex2);
			edge_list[vertex2].remove(vertex1);
		}
		else
			return -1;
	}
	return 1;
}

void Graph::moralize()
{
	if(isUnDirected())
		return;

	for(int i=0;i<num_vertices;i++)
	{
		list<int>::iterator current,current1,last;
		current = parent_list[i].begin();
		last = parent_list[i].end();
		while(current!=last)
		{
			adj_matrix[i][*current]=true;
			adj_matrix[*current][i]=true;
			current1=current;
			current1++;
			while(current1!=last)
			{
				adj_matrix[*current][*current1]=true;
				adj_matrix[*current1][*current]=true;
				current1++;
			}
			current++;
		}
	}
	num_edges=0;
	edge_list = vector<list<int> > (num_vertices);
	for(int i=0;i<num_vertices;i++)
	{
		for(int j=0;j<num_vertices;j++)
		{
			if(adj_matrix[i][j])
			{
				edge_list[i].push_back(j);
				num_edges++;
			}
		}
	}
	directed=false;
}
Graph Graph::moralizeGraph(const Graph &graph_in)
{
	Graph graph;
	if(graph_in.isUnDirected())
	{
		graph = Graph(graph_in);
		return graph;
	}
	int i;
	int num_vertices_in = graph_in.getNumberOfVertices();
	vector<vector<bool> > adj_matrix (num_vertices_in);
	for(i=0;i<num_vertices_in;i++)
		adj_matrix[i]=vector<bool> (num_vertices_in);

	for(i=0;i<num_vertices_in;i++)
	{
		list<int> parents = graph_in.getParents(i);
		list<int>::iterator current,current1,last;
		current = parents.begin();
		last = parents.end();
		while(current!=last)
		{
			adj_matrix[i][*current]=true;
			adj_matrix[*current][i]=true;
			current1=current;
			current1++;
			while(current1!=last)
			{
				adj_matrix[*current][*current1]=true;
				adj_matrix[*current1][*current]=true;
				current1++;
			}
			current++;
		}
	}

	graph = Graph(num_vertices_in,adj_matrix,graph_in.getIds(),false);
	return graph;
}

const bool Graph::isDirected() const
{
	return directed;
}

const bool Graph::isUnDirected() const
{

	return !directed;
}

void Graph::clearGraph()
{
	adj_matrix = vector<vector<bool> > (num_vertices);
	for(int i=0;i<num_vertices;i++)
		adj_matrix[i]=vector<bool>(num_vertices);

	edge_list = vector<list<int> > (num_vertices);
	parent_list = vector<list<int> > (num_vertices);
	child_list = vector<list<int> > (num_vertices);
	num_edges = 0;
}

bool Graph::operator==(const Graph &graph)
{
	int i,j;
	if(directed != graph.isDirected())
		return false;
	if(num_vertices == graph.getNumberOfVertices() && num_edges == graph.getNumberOfEdges())
	{
		vector<vector<bool> > adj_matrix_g = graph.getAdjMatrix();
		//check whether the two graphs have the same edges
		for(i=0;i<num_vertices;i++)
		{
			for(j=0;j<num_vertices;j++)
			{
				if(adj_matrix[i][j]!=adj_matrix_g[i][j])
				{
					return false;
				}
			}
		}
		//check whether the ids are equal
		for(i=0;i<num_vertices;i++)
		{
			if(vertex_ids[i]!=(graph.getIds())[i])
			{
				return false;
			}
		}
		return true;
	}
	return false;
}


bool Graph::operator!=(const Graph &graph)
{
	if(*this==graph)
		return false;
	return true;
}

int Graph::getMinFillOrdering(vector<int>&ordering)
{
	int i,j;
	space=0.0;
	int treewidth=0;
	vector<vector<bool> > fillin_adj_matrix (num_vertices);
	vector<list<int> > fillin_edge_list(num_vertices);

	for(i=0;i<num_vertices;i++)
		fillin_adj_matrix[i]=vector<bool> (num_vertices);

	for(i=0;i<num_vertices;i++)
		for(j=0;j<num_vertices;j++)
		{
			fillin_adj_matrix[i][j]=adj_matrix[i][j];
			if(fillin_adj_matrix[i][j])
				fillin_edge_list[i].push_back(j);
		}

		ordering = vector<int>(num_vertices);
		vector<int> minFill;
		vector<int> processed = vector<int> (num_vertices);
		int min=BigNum;
		int edgesAdded;
		int currVertex;
		list<int>::iterator curr,last,curr1;


		/* Initialize processed */
		for(i=0;i<num_vertices;i++)
			processed[i]=0;

		/* The min-fill algorithm */
		for(j=0;j<num_vertices;j++)
		{
			/* Find the vertices with the minimum fill-in value */
			/* fill-in value is the number of edges required to be added to make
			a graph chordal */
			min = BigNum;
			for(i=0;i<num_vertices;i++)
			{
				/* If the variable is already processed continue */
				if(processed[i]==1)
					continue;
				curr = fillin_edge_list[i].begin();
				last = fillin_edge_list[i].end();

				edgesAdded=0;
				while(curr!=last)
				{
					if(processed[*curr]==1)
					{
						curr++;
						continue;
					}
					curr1 = curr;
					++curr1;
					while(curr1!=last)
					{
						if(processed[*curr1]==1)
						{
							curr1++;
							continue;
						}
						if(fillin_adj_matrix[*curr][*curr1]==false)
							edgesAdded++;
						curr1++;

					}
					curr++;
				}

				if(edgesAdded<min)
				{
					min=edgesAdded;
					minFill.clear();
					minFill.push_back(i);
				}
				else if(edgesAdded==min)
				{
					minFill.push_back(i);
				}
			}
			/* Select a minumum fill-in vertex uniformly at random */
			currVertex = minFill[rand()%minFill.size()];

			/* Change the original graph by adding new edges */
			curr = fillin_edge_list[currVertex].begin();
			last = fillin_edge_list[currVertex].end();
			//double curr_space=1.0;
			while(curr!=last)
			{
				if(processed[*curr]==1)
				{
					curr++;
					continue;
				}
				
				curr1= curr;
				curr1++;
				while(curr1 !=last)
				{
					if(processed[*curr1]==1)
					{
						curr1++;
						continue;
					}
					if(!fillin_adj_matrix[*curr][*curr1])
					{
						fillin_adj_matrix[*curr][*curr1]=fillin_adj_matrix[*curr1][*curr]=true;
						fillin_edge_list[*curr].push_back(*curr1);
						fillin_edge_list[*curr1].push_back(*curr);
					}
					curr1++;
				}
				curr++;
			}
			int curr_num_edges=0;
			for(int k=0;k<num_vertices;k++)
				if(fillin_adj_matrix[currVertex][k] && processed[k]!=1)
					curr_num_edges++;
			if(treewidth < curr_num_edges)
				treewidth=curr_num_edges;
			/* Update the temporary variables processed and ordering */
			processed[currVertex]=1;
			ordering[j]=currVertex;
		}
		cerr<<"Treewidth = "<<treewidth<<endl;
		return treewidth;
}

void Graph::getConstrainedMinFillOrdering(vector<int> &ordering, vector<bool> &constraints)
{
	if(constraints.size()!=num_vertices)
	{
		cout<<"Error: Size of constraints and num_vertices not same\n";
		return;
	}
	int i,j;
	ordering=vector<int>(num_vertices);
	vector<int> constrained_vertices;
	vector<int> other_vertices;
	int num_constrained_vertices=0;
	int num_other_vertices=0;
	// Partition vertices into two sets
	for(i=0;i<num_vertices;i++)
	{
		if(constraints[i]==true)
		{
			constrained_vertices.push_back(i);
			num_constrained_vertices++;
		}
		else
		{
			other_vertices.push_back(i);
			num_other_vertices++;
		}
	}

	// Copy the original graph because we don't make changes to the original graph
	vector<vector<bool> > fillin_adj_matrix (num_vertices);
	vector<list<int> > fillin_edge_list(num_vertices);

	for(i=0;i<num_vertices;i++)
		fillin_adj_matrix[i]=vector<bool> (num_vertices);

	for(i=0;i<num_vertices;i++)
		for(j=0;j<num_vertices;j++)
		{
			fillin_adj_matrix[i][j]=adj_matrix[i][j];
			if(fillin_adj_matrix[i][j])
				fillin_edge_list[i].push_back(j);
		}

		// Initialize the orderings
		ordering = vector<int>(num_vertices);
		vector<int> minFill;
		vector<int> processed = vector<int> (num_vertices);
		int min=BigNum;
		int edgesAdded;
		int currVertex;
		list<int>::iterator curr,last,curr1;

		/* Initialize processed */
		for(i=0;i<num_vertices;i++)
			processed[i]=0;

		/* The min-fill algorithm */
		for(j=0;j<num_constrained_vertices;j++)
		{
			/* Find the vertices with the minimum fill-in value */
			/* fill-in value is the number of edges required to be added to make
			a graph chordal */
			min = BigNum;
			for(i=0;i<num_constrained_vertices;i++)
			{
				/* If the variable is already processed continue */
				if(processed[constrained_vertices[i]]==1)
					continue;
				curr = fillin_edge_list[constrained_vertices[i]].begin();
				last = fillin_edge_list[constrained_vertices[i]].end();

				edgesAdded=0;
				while(curr!=last)
				{
					if(processed[*curr]==1)
					{
						curr++;
						continue;
					}
					curr1 = curr;
					++curr1;
					while(curr1!=last)
					{
						if(processed[*curr1]==1)
						{
							curr1++;
							continue;
						}
						if(fillin_adj_matrix[*curr][*curr1]==false)
							edgesAdded++;
						curr1++;

					}
					curr++;
				}

				if(edgesAdded<min)
				{
					min=edgesAdded;
					minFill.clear();
					minFill.push_back(constrained_vertices[i]);
				}
				else if(edgesAdded==min)
				{
					minFill.push_back(constrained_vertices[i]);
				}
			}
			/* Select a minumum fill-in vertex uniformly at random */
			currVertex = minFill[rand()%minFill.size()];

			/* Change the original graph by adding new edges */
			curr = fillin_edge_list[currVertex].begin();
			last = fillin_edge_list[currVertex].end();
			while(curr!=last)
			{
				if(processed[*curr]==1)
				{
					curr++;
					continue;
				}
				curr1= curr;
				curr1++;
				while(curr1 !=last)
				{
					if(processed[*curr1]==1)
					{
						curr1++;
						continue;
					}
					if(!fillin_adj_matrix[*curr][*curr1])
					{
						fillin_adj_matrix[*curr][*curr1]=fillin_adj_matrix[*curr1][*curr]=true;
						fillin_edge_list[*curr].push_back(*curr1);
						fillin_edge_list[*curr1].push_back(*curr);
					}
					curr1++;
				}
				curr++;
			}
			/* Update the temporary variables processed and ordering */
			processed[currVertex]=1;
			ordering[j]=currVertex;
		}

		// Now add the other vertices

		for(j=0;j<num_other_vertices;j++)
		{
			/* Find the vertices with the minimum fill-in value */
			/* fill-in value is the number of edges required to be added to make
			a graph chordal */
			min = BigNum;
			for(i=0;i<num_other_vertices;i++)
			{
				/* If the variable is already processed continue */
				if(processed[other_vertices[i]]==1)
					continue;
				curr = fillin_edge_list[other_vertices[i]].begin();
				last = fillin_edge_list[other_vertices[i]].end();

				edgesAdded=0;
				while(curr!=last)
				{
					if(processed[*curr]==1)
					{
						curr++;
						continue;
					}
					curr1 = curr;
					++curr1;
					while(curr1!=last)
					{
						if(processed[*curr1]==1)
						{
							curr1++;
							continue;
						}
						if(fillin_adj_matrix[*curr][*curr1]==false)
							edgesAdded++;
						curr1++;

					}
					curr++;
				}

				if(edgesAdded<min)
				{
					min=edgesAdded;
					minFill.clear();
					minFill.push_back(other_vertices[i]);
				}
				else if(edgesAdded==min)
				{
					minFill.push_back(other_vertices[i]);
				}
			}
			/* Select a minumum fill-in vertex uniformly at random */
			currVertex = minFill[rand()%minFill.size()];

			/* Change the original graph by adding new edges */
			curr = fillin_edge_list[currVertex].begin();
			last = fillin_edge_list[currVertex].end();
			while(curr!=last)
			{
				if(processed[*curr]==1)
				{
					curr++;
					continue;
				}
				curr1= curr;
				curr1++;
				while(curr1 !=last)
				{
					if(processed[*curr1]==1)
					{
						curr1++;
						continue;
					}
					if(!fillin_adj_matrix[*curr][*curr1])
					{
						fillin_adj_matrix[*curr][*curr1]=fillin_adj_matrix[*curr1][*curr]=true;
						fillin_edge_list[*curr].push_back(*curr1);
						fillin_edge_list[*curr1].push_back(*curr);
					}
					curr1++;
				}
				curr++;
			}
			/* Update the temporary variables processed and ordering */
			processed[currVertex]=1;
			ordering[j+num_constrained_vertices]=currVertex;
		}
		cout<<"printing ordering\n";
		for(int i=0;i<ordering.size();i++)
			cout<<ordering[i]<<" ";
		cout<<endl;
}
void Graph::getTopologicalOrdering(vector<int>& ordering)
{
	// For each vertex call DFS search
	if(directed==false)
	{
		cout<<"Error while Computing topological ordering of a directed graph\n";
		cout<<endl;
		exit(1);
	}
	vector<bool> color (num_vertices);
	for(int i=0;i<num_vertices;i++)
		color[i]=false;
	ordering=vector<int>();
	bool all_colored=false;
	while(1)
	{
		for(int i=0;i<num_vertices;i++)
		{
			if(color[i])
				continue;
			bool toadd=true;
			// If the current variable has no incoming edges or all colored vertices
			// add it to the list
			list<int>::iterator curr,last;
			curr=parent_list[i].begin();
			last=parent_list[i].end();
			while(curr!=last)
			{
				if(color[*curr]==false)
				{
					//cout<<"making toadd false\n";
					toadd=false;
					break;
				}
				curr++;
			}
			//cout<<"variable "<<i<<endl;
			if(toadd)
			{
				color[i]=true;
				//cout<<"adding variable "<<i<<" ";
				ordering.push_back(i);
			}
		}
		if(ordering.size() >= num_vertices)
			break;
	}
}

void Graph::getTopologicalOrdering_OLD(vector<int>& ordering)
{
	// For each vertex call DFS search
	if(directed==false)
	{
		cout<<"Error while Computing topological ordering of a directed graph\n";
		cout<<endl;
		exit(1);
	}
	vector<bool> color (num_vertices);
	vector<int> ordering_;
	for(int i=0;i<num_vertices;i++)
	{
		//cout<<"trying "<<i<<"\n";
		if(color[i]==false)
			DFS_topological(i,color,ordering_);
	}
	ordering=vector<int>();
	for(int i=ordering_.size()-1;i>-1;i--)
		ordering.push_back(ordering_[i]);

}

void Graph::DFS_topological(int i, vector<bool>& color, vector<int>& ordering)
{
	color[i]=true;
	list<int>::iterator curr,last;
	// Replace edge_list by child_list
	//cout<<"size = "<<child_list[i].size()<<endl;
	curr=child_list[i].begin();
	last=child_list[i].end();
	while(curr!=last)
	{
		if(color[*curr]==false)
			DFS_topological(*curr,color,ordering);
		curr++;
	}
	//cout<<"adding "<<i<<"\n";
	ordering.push_back(i);
}

void Graph::getTriangulatedGraph(Graph&g, vector<int> &ordering)
{
	vector<vector<bool> > fillin_adj_matrix (num_vertices);
	vector<list<int> > fillin_edge_list(num_vertices);
	int i,j;
	for(i=0;i<num_vertices;i++)
		fillin_adj_matrix[i]=vector<bool> (num_vertices);

	for(i=0;i<num_vertices;i++)
		for(j=0;j<num_vertices;j++)
		{
			fillin_adj_matrix[i][j]=adj_matrix[i][j];
			if(fillin_adj_matrix[i][j])
				fillin_edge_list[i].push_back(j);
		}

		vector<bool> processed(num_vertices);
		for(i=0;i<num_vertices;i++)
		{
			list<int>::iterator curr,curr1,last;
			int vertex = ordering[i];
			curr = fillin_edge_list[vertex].begin();
			last = fillin_edge_list[vertex].end();
			while(curr!=last)
			{
				if(processed[*curr])
				{
					curr++;
					continue;
				}
				curr1=curr;
				curr1++;
				while(curr1!=last)
				{
					if(processed[*curr1])
					{
						curr1++;
						continue;
					}
					if(fillin_adj_matrix[*curr][*curr1]==false)
					{
						fillin_adj_matrix[*curr][*curr1]=true;
						fillin_adj_matrix[*curr1][*curr]=true;
						fillin_edge_list[*curr].push_back(*curr1);
						fillin_edge_list[*curr1].push_back(*curr);
					}
					curr1++;
				}
				curr++;
			}
			processed[vertex]=true;
		}

		g = Graph (num_vertices,fillin_adj_matrix,false);
#ifdef DEBUG_TRIANG
		cout<<"printing triangulated graph\n";
		g.print();
#endif

}
void Graph::getCliques( vector<int>& ordering, vector<vector<int> >& cliques)
{
	cliques=vector<vector<int> > (num_vertices);
	vector<bool> processed(num_vertices);
	int i;
	for(i=0;i<ordering.size();i++)
	{
		list<int>::iterator curr,last;
		int vertex = ordering[i];
		cliques[i].push_back(vertex);
		curr=edge_list[vertex].begin();
		last=edge_list[vertex].end();
		while(curr!=last)
		{
			if(processed[*curr]==false)
				cliques[i].push_back(*curr);
			curr++;
		}
		processed[vertex]=true;
	}
}
/*
void Graph::getBucketTree(TreeDecomposition &tree_decomp)
{
Graph bucket_tree;
vector<vector<bool> > bucket_adj_matrix (num_vertices);
int i,j;
for(i=0;i<num_vertices;i++)
{
bucket_adj_matrix[i]=vector<bool>(num_vertices);
}
tree_decomp.ordering = getMinFillOrdering();
vector<int> where_placed(num_vertices);
for(i=0;i<num_vertices;i++)
{
where_placed[tree_decomp.ordering[i]]=i;
}
Graph triangulated_graph;
getTriangulatedGraph(triangulated_graph,tree_decomp.ordering);
triangulated_graph.getCliques(tree_decomp.ordering,tree_decomp.cliques);
for(i=num_vertices-1;i>-1;i--)
{
int min = BigNum;
for(j=0;j<tree_decomp.cliques[i].size();j++)
{
if(where_placed[tree_decomp.cliques[i][j]] < min)
{
min = where_placed[tree_decomp.cliques[i][j]];
}
}

if(min!=BigNum)
{
bucket_adj_matrix[i][min]=true;
bucket_adj_matrix[min][i]=true;
}
}
tree_decomp.tree=Graph(num_vertices,bucket_adj_matrix,false);

}
void Graph::getConstrainedBucketTree(TreeDecomposition &tree_decomp, vector<bool>& constraints)
{
Graph bucket_tree;
vector<vector<bool> > bucket_adj_matrix (num_vertices);
int i,j;
for(i=0;i<num_vertices;i++)
{
bucket_adj_matrix[i]=vector<bool>(num_vertices);
}
getConstrainedMinFillOrdering(tree_decomp.ordering,constraints);
vector<int> where_placed(num_vertices);
for(i=0;i<num_vertices;i++)
{
where_placed[tree_decomp.ordering[i]]=i;
}
Graph triangulated_graph;
getTriangulatedGraph(triangulated_graph,tree_decomp.ordering);
triangulated_graph.getCliques(tree_decomp.ordering,tree_decomp.cliques);
for(i=num_vertices-1;i>-1;i--)
{
int min = BigNum;
for(j=0;j<tree_decomp.cliques[i].size();j++)
{
if(where_placed[tree_decomp.cliques[i][j]] < min)
{
min = where_placed[tree_decomp.cliques[i][j]];
}
}

if(min!=BigNum)
{
bucket_adj_matrix[i][min]=true;
bucket_adj_matrix[min][i]=true;
}
}
tree_decomp.tree=Graph(num_vertices,bucket_adj_matrix,false);

}
*/
/*
void Graph::getBucketTree(TreeDecomposition &tree_decomp)
{
int i,j,k;
vector<list<int> > bucket_tree_edge_list(num_vertices);
// Generate an ordering based on the min-fill heuristic
vector<int> ordering;
getMinFillOrdering(ordering);


// Triangulate the graph
Graph triangulated_graph;
getTriangulatedGraph(triangulated_graph,ordering);

// Read the cliques from the triangulated graph
vector<vector<int> > cliques;
triangulated_graph.getCliques(ordering,cliques);

// Form the bucket tree graph
for(i=0;i<num_vertices;i++)
{
int connect_node=-1;
for(j=i+1;j<num_vertices;j++)
{
for(k=0;k<cliques[i].size();k++)
{
if(cliques[i][k]==ordering[j])
{
connect_node=j;
break;
}
}
if(connect_node==j)
break;
}

if(connect_node!=-1)
{

bucket_tree_edge_list[i].push_back(connect_node);
bucket_tree_edge_list[connect_node].push_back(i);
}
}
// Create the bucket tree
Tree tree(num_vertices,bucket_tree_edge_list);

// Modify the cliques to reflect the ids
for(i=0;i<num_vertices;i++)
{
vector<int>::iterator curr,last;
curr=cliques[i].begin();
last=cliques[i].end();
while(curr!=last)
{
*curr=vertex_ids[*curr];
curr++;
}
}

// Now sort the cliques
for(i=0;i<cliques.size();i++)
sort(cliques[i].begin(),cliques[i].end());

// Create the tree_decomposition
tree_decomp=TreeDecomposition(cliques,tree);


}
void Graph::getConstrainedBucketTree(TreeDecomposition &tree_decomp, 
vector<bool>& constraints)
{
int i,j,k;
vector<list<int> > bucket_tree_edge_list(num_vertices);
// Generate an ordering based on the min-fill heuristic
vector<int> ordering;
getConstrainedMinFillOrdering(ordering,constraints);

// Triangulate the graph
Graph triangulated_graph;
getTriangulatedGraph(triangulated_graph,ordering);

// Read the cliques from the triangulated graph
vector<vector<int> > cliques;
triangulated_graph.getCliques(ordering,cliques);

// Form the bucket tree graph
for(i=0;i<num_vertices;i++)
{
int connect_node=-1;
for(j=i+1;j<num_vertices;j++)
{
for(k=0;k<cliques[i].size();k++)
{
if(cliques[i][k]==ordering[j])
{
connect_node=j;
break;
}
}
if(connect_node==j)
break;
}

if(connect_node!=-1)
{

bucket_tree_edge_list[i].push_back(connect_node);
bucket_tree_edge_list[connect_node].push_back(i);
}
}
// Create the bucket tree
Tree tree(num_vertices,bucket_tree_edge_list);

// Modify the cliques to reflect the ids
for(i=0;i<num_vertices;i++)
{
vector<int>::iterator curr,last;
curr=cliques[i].begin();
last=cliques[i].end();
while(curr!=last)
{
*curr=vertex_ids[*curr];
curr++;
}
}

// Create the tree_decomposition
tree_decomp=TreeDecomposition(cliques,tree);

}
*/
void Graph::print()
{
	int i,j;
	cout<<"printing graph\n";
	for(i=0;i<num_vertices;i++)
	{
		for(j=0;j<num_vertices;j++)
		{
			if(adj_matrix[i][j]==true)
				cout<<"1 ";
			else
				cout<<"0 ";
		}
		cout<<endl;
	}
}

/*
void Graph::getDBNBucketTree(TreeDecomposition &tree_decomp, const vector<int>&i_old_slice, const vector<int>&non_i_old_slice,
const vector<int>& i_new_slice,const bool if_ids)
{
int i,j,k;
// Construct a new graph and copy the old graph into it because we don't want to change the old graph
Graph new_graph(*this);

// a.1 Remove non-interface nodes from the graph, form a clique of interface nodes and moralize graph


if(if_ids==true)
{
// a.3. Remove non-interface nodes from the new graph
new_graph.removeVertices(non_i_old_slice);

// a.4 Moralize the new graph
new_graph=Graph::moralizeGraph(*this);

//a.5 Form a clique using only the interface nodes
new_graph.formClique(i_old_slice);
new_graph.formClique(i_new_slice);

// b. Construct a tree decomposition out of the new graph
new_graph.getBucketTree(tree_decomp);
}
else
{
vector<int> i_old_slice_,non_i_old_slice_,i_new_slice_;
for(i=0;i<i_old_slice.size();i++)
{
for(j=0;j<num_vertices;j++)
{
if(i_old_slice[i]==vertex_ids[j])
break;
}
if(j==num_vertices)
{
cout<<"something wrong with the ids\n";
exit(1);
}
i_old_slice_.push_back(j);
}
for(i=0;i<non_i_old_slice.size();i++)
{
for(j=0;j<num_vertices;j++)
{
if(non_i_old_slice[i]==vertex_ids[j])
break;
}
if(j==num_vertices)
{
cout<<"something wrong with the ids\n";
exit(1);
}
non_i_old_slice_.push_back(j);
}
for(i=0;i<i_new_slice.size();i++)
{
for(j=0;j<num_vertices;j++)
{
if(i_new_slice[i]==vertex_ids[j])
break;
}
if(j==num_vertices)
{
cout<<"something wrong with the ids\n";
exit(1);
}
i_new_slice_.push_back(j);
}
// a.3. Remove non-interface nodes from the new graph
new_graph.removeVertices(non_i_old_slice_);

// a.4 Moralize the new graph
new_graph=Graph::moralizeGraph(*this);

//a.5 Form a clique using only the interface nodes
new_graph.formClique(i_old_slice_);
new_graph.formClique(i_new_slice_);

// b. Construct a tree decomposition out of the new graph
new_graph.getBucketTree(tree_decomp);
}
}
*/
int Graph::removeVertices(const vector<int>&nodes)
{
	int i,j;
	vector<int> mapping;
	vector<bool> skip_node_id(num_vertices);
	for(i=0;i<nodes.size();i++)
		skip_node_id[nodes[i]]=true;
	int new_num_vertices=num_vertices-nodes.size();
	for(i=0;i<num_vertices;i++)
	{
		if(skip_node_id[i]==false)
			mapping.push_back(i);
	}
	vector<vector<bool> > new_adj_matrix(new_num_vertices);
	for(i=0;i<new_num_vertices;i++)
		new_adj_matrix[i]=vector<bool>(new_num_vertices);

	for(i=0;i<new_num_vertices;i++)
	{
		for(j=0;j<new_num_vertices;j++)
		{
			new_adj_matrix[i][j]=adj_matrix[mapping[i]][mapping[j]];
		}
	}

	vector<int> new_variable_ids;
	for(i=0;i<num_vertices;i++)
	{
		if(skip_node_id[i]==false)
			new_variable_ids.push_back(vertex_ids[i]);
	}
	vertex_ids=new_variable_ids;

	num_edges=0;
	if(directed)
	{
		num_vertices=new_num_vertices;
		parent_list = vector<list<int> > (new_num_vertices);
		child_list = vector<list<int> > (new_num_vertices);
		adj_matrix=new_adj_matrix;
		for(i=0;i<num_vertices;i++)
		{
			for(j=0;j<num_vertices;j++)
			{
				if(adj_matrix[i][j])
				{
					parent_list[j].push_back(i);
					child_list[i].push_back(j);
					num_edges++;
				}
			}
		}
	}
	else
	{
		num_vertices=new_num_vertices;
		edge_list = vector<list<int> > (num_vertices);
		adj_matrix=new_adj_matrix;
		for(i=0;i<num_vertices;i++)
		{
			for(j=0;j<num_vertices;j++)
			{
				if(adj_matrix[i][j])
				{
					edge_list[i].push_back(j);
					num_edges++;
				}
			}
		}
	}
}

void Graph::formClique(const vector<int>& nodes)
{
	int i,j;
	if(directed)
		return;
	for(i=0;i<nodes.size();i++)
		for(j=i+1;j<nodes.size();j++)
			addEdge(nodes[i],nodes[j]);
}
