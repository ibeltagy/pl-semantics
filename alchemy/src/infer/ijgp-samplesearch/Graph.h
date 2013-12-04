#ifndef GRAPH_H_
#define GRAPH_H_
#include "GM.h"
#include <iostream>
#include <utility>
#include <list>
#include <vector>


using namespace std;
const int BigNum=0x7FFFFFFF;
// We have defined 2 classes of graphs and operations on them
// 1. Class Graph which stores the graph as a edgelist and adjacency matrix
// 2. Class SparseGraph which stores the graph as a edgelist
class Graph
{
   	private:
	// A boolean variable indicating whether the graph is directed or not
	bool directed;
	// The number of vertices in the graph
	int num_vertices;
	// The number of edges in the graph
	int num_edges;
	 // Each Graph contains a adjacency matrix
	 vector<vector<bool> > adj_matrix;
	 // The edges in the graph specified as a list for each vertex
	 vector<list<int> > edge_list;
	 // For directed graph we store the edges to and from the graph as parent list and child list respectively
	 vector<list<int> > parent_list;
	 vector<list<int> > child_list;

	 
	 // The ids of the vertices in the graph
	     vector<int> vertex_ids;
	  public:

	  double space;
	     // 5 Constructors based on how the graph is specified and the default constructor
	 Graph();
	 Graph(int num_vertices, const vector<vector<bool> >& adj_matrix, const bool directed);
	 Graph(int num_vertices,const vector<vector<bool> >& adj_matrix,
	       const vector<int>& ids, const bool directed);
	 Graph(const Graph& graph);

	 void makeGraph(GM* gm);
	 void makeDirectedGraph(GM* gm);
	 // Moralize a directed graph
	 static Graph moralizeGraph(const Graph &graph);
	
	 // Moralize a graph
	 void moralize();
	
	 //Get Number of Vertices
	 const int getNumberOfVertices() const;
	
	 // Get Number of Edges
	 const int getNumberOfEdges() const;
	
	 // Return the number of neighbors of the current node
	 const int getNumberOfNeighbors(int vertex) const;
	
	 // Return the number of parents that a node has for a directed graph
	 const int getNumberOfParents( int vertex ) const;
	
	 // Return the number of children that a node has for a directed graph
	 const int getNumberOfChildren( int vertex ) const;
	
	 // Return the neighbors of a vertex
	 const list<int>& getEdgeList(int i) const;
	
	 // Return the Edge List
	 const vector<list<int> >& getEdgeList() const;
	
	 // Return the parentList
	 const vector<list<int> >& getParentList() const;
	
	 // Return the parentlist for a vertex
	 const list<int>& getParentList(int id) const;
	
	 // Return the childList
	 const vector<list<int> >& getChildList() const;
	
	 // Return the childlist for a vertex
	 const list<int> & getChildList(int id) const;
	
	 // Return the Adjacency matrix
	 const vector<vector<bool> >& getAdjMatrix() const;
	
	 // Return the list of parents of the current node
	 const list<int>& getParents(int vertex) const;
	
	 // Return the ids of the vertices
	 const vector<int>& getIds() const;
	
	 // Return the id of the vertex
	 const int getIdOfVetex(int ) const;
	
	 // Remove Vertices from the graph
	 int removeVertices(const vector<int>& nodes);
	
	 // Add an edge to the graph
	 int addEdge(int vertex1, int vertex2);
	
	 // Remove an edge from the graph
	 int  removeEdge(int vertex1, int vertex2);
	
	 // Returns true if directed and false if undirected
	 const bool isDirected() const;
	
	 // Returns false if directed and true if undirected
	 const bool isUnDirected() const;
	
	 // Clear graph by setting the adjacency matrix and the EdgeList of null vectors
	 void clearGraph();
	
	 // Check if the two graphs are the same
	 bool operator==(const Graph &graph);
	
	 // Check if the two graphs are different
	 bool operator!=(const Graph &graph);
	
	 // Orderings for a junction tree 2. min fill ordering
	 int getMinFillOrdering(vector<int>&ordering);
	
	 // Constrained minfill ordering
	 void getConstrainedMinFillOrdering(vector<int>&ordering, vector<bool>& constraints);
	
	 // Get cliques for the triangulated graph
	 void getCliques( vector<int>& ordering,vector<vector<int> >& );
	
	 // Get the triangulated graph based on a ordering
	 void getTriangulatedGraph(Graph&g, vector<int> &ordering);
	 // Get the topological ordering
	 void getTopologicalOrdering(vector<int>& ordering);
	 void getTopologicalOrdering_OLD(vector<int>& ordering);
	 void DFS_topological(int i, vector<bool>& color, vector<int>& ordering);
	 // Get the Bucket tree for the graph
     //void getBucketTree(TreeDecomposition&);
     //void getConstrainedBucketTree(TreeDecomposition &tree_decomp,
       //                                vector<bool>& constraints);
     //void getDBNBucketTree(TreeDecomposition &tree_decomp, const vector<int>&i_old_slice, const vector<int>&non_i_old_slice,
       //                        const vector<int>& i_new_slice,const bool if_ids=false);
	 void formClique(const vector<int>& nodes);
	 void findRelevantNodes(vector<bool>& nodes, const vector<int>& evidence_nodes);
	 
	 void print();
	 void getLeaves(vector<int>& leaves);
};
#endif /*GRAPH_H_*/
