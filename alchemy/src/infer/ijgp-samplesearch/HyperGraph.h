#ifndef HYPERGRAPH_H_
#define HYPERGRAPH_H_

#include <iostream>
#include <vector>
#include <set>
#include <map>
#include <fstream>

using namespace std;

class HyperGraph
{
private:
	
public:
	// Vertices are the functions
	vector<set<int> > vertices;
	// Edges are the variables
	vector<set<int> > edges;

	map<int,int> edge2varindex;
	map<int,int> vertex2funcindex;
	map<int,int> var2edgeindex;
	map<int,int> func2vertexindex;

	HyperGraph(){ }
	void write();
	void getSeparator(int num_components, const vector<int>& vertex_in_component, vector<int>& separator,vector<HyperGraph*>& components);
	void computeOrdering(int num_components,vector<int>& sampling_order);

	void readErgo(char* infilename);
};
#endif
