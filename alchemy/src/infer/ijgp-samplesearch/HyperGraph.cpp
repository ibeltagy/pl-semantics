#include "HyperGraph.h"
#include <cassert>
#include <queue>
#include <cstdlib>
#include <algorithm>
namespace ss{

struct HGTree
{
	HyperGraph* hg;
	vector<int> separator;
	vector<HyperGraph*> children;
	HGTree(){}
};
void HyperGraph::readErgo(char* infilename)
{
  ifstream infile(infilename);
  
  cerr<<"Reading Bayesnet ..."<<flush;
  char check_char,buffer[1000];
  int num_variables;
  // Read num variables
  while(!infile.eof())
{
    infile>>check_char;
    if(check_char == '/')
      {
	infile.getline(buffer,1000);
	continue;
      }
    infile.putback(check_char);
    infile>>num_variables;
    break;
  }

  cerr<<"..Read vars "<<flush;
  int var=0;
  while(!infile.eof()){
    infile>>check_char;
    if(check_char == '/'){
      infile.getline(buffer,1000);
      continue;
    }
    infile.putback(check_char);
    int d;
    infile>>d;
    ++var;
    if(var==num_variables)
      break;
  }
  
  cerr<<"..Read cpts "<<flush;
  // Read parents of variables
  vertices=vector<set<int> > (num_variables);
  edges =vector<set<int> > (num_variables);
  var=0;
  while(!infile.eof()){
    infile>>check_char;
    if(check_char == '/'){
      infile.getline(buffer,1000);
      continue;
    }
    infile.putback(check_char);
    int num_parents;
    infile>>num_parents;
    
    for(int i=0;i<num_parents;i++){
      int var_num;
      infile>>var_num;
      edges[var_num].insert(var);
      vertices[var].insert(var_num);
    }
    edges[var].insert(var);
    vertices[var].insert(var);
    ++var;
    if(var==num_variables)
      break;
  }
  for(int i=0;i<num_variables;i++){
    edge2varindex[i]=i;
    vertex2funcindex[i]=i;
    var2edgeindex[i]=i;
    func2vertexindex[i]=i;
  }
  cerr<<"...Done\n";
  infile.close();
}
//Write the hypergraph to a file tmp.hgr
void HyperGraph::write()
{
	ofstream out("tmp.hgr");

	out<<edges.size()<<" "<<vertices.size()<<endl;

	for(int i=0;i<edges.size();i++)
	{
		for(set<int>::iterator j=edges[i].begin();j!=edges[i].end();j++)
		{
			out<<func2vertexindex[*j]+1<<" ";
		}
		out<<endl;
	}
	out.close();
	
}
// Separate the hypergraph into multiple components
// Returns the separator and Hypergraph components
void HyperGraph::getSeparator(int num_components, const vector<int>& vertex_in_component, vector<int>& separator,vector<HyperGraph*>& components)
{
	separator=vector<int>();
	components=vector<HyperGraph*> (num_components);
	for(int i=0;i<num_components;i++)
	{
		components[i]=new HyperGraph();
	}
	//cout<<"num-edges ="<<edges.size()<<endl;
	for(int i=0;i<edges.size();i++)
	{
		int curr=-1;
		bool is_separator=false;
		// If the vertices (functions) at each edge (variable) are in different components then it is a separator
		
		for(set<int>::iterator j=edges[i].begin();j!=edges[i].end();j++)
		{
			int id=this->func2vertexindex[*j];
			if(curr==-1)
			{
				curr=vertex_in_component[id];
			}
			else if(curr!=vertex_in_component[id])
			{
				is_separator=true;
				break;
			}
		}
		assert(curr!=-1);
		if(is_separator)
		{
			separator.push_back(edge2varindex[i]);
		}
		else
		{
			
			components[curr]->edges.push_back(edges[i]);
			components[curr]->edge2varindex[components[curr]->edges.size()-1]=edge2varindex[i];
			components[curr]->var2edgeindex[edge2varindex[i]]=components[curr]->edges.size()-1;
		}
	}
	
	cerr<<"separator size= "<<separator.size()<<endl;
	// If the size of the separator is zero then add all vertices to the separator
	if(separator.empty())
	{
		for(int i=0;i<edges.size();i++)
		{
			separator.push_back(edge2varindex[i]);
		}
		for(int i=0;i<num_components;i++)
			delete(components[i]);
		components=vector<HyperGraph*>();
		return;
	}
	if(separator.size() == edges.size())
	{
		for(int i=0;i<num_components;i++)
			delete(components[i]);
		components=vector<HyperGraph*>();
		return;
	}
	//Now remove the separator edges from each vertex
	//vector<set<int> > temp_vertices=vertices;
	for(int i=0;i<vertices.size();i++)
	{
		set<int> result;
		set_difference(vertices[i].begin(),vertices[i].end(),separator.begin(),separator.end(),inserter(result,result.begin()));
		if(!result.empty())
		{
			components[vertex_in_component[i]]->vertices.push_back(result);
			components[vertex_in_component[i]]->vertex2funcindex[components[vertex_in_component[i]]->vertices.size()-1]=vertex2funcindex[i];
			components[vertex_in_component[i]]->func2vertexindex[vertex2funcindex[i]]=components[vertex_in_component[i]]->vertices.size()-1;
		}
	}

	// Ugly hack
	vector<HyperGraph*> temp_components;
	for(int i=0;i<components.size();i++)
		if(!components[i]->vertices.empty() && !components[i]->edges.empty())
			temp_components.push_back(components[i]);
		else
			delete(components[i]);
	components=temp_components;

}

void HyperGraph::computeOrdering(int num_components,vector<int>& sampling_order)
{
	sampling_order=vector<int>();

	queue<HyperGraph*> q;
	q.push(this);
	
	
	//HGTree* root;
	//HGTree* current_node;
	//root= new HGTree();
	//root->hg=this;
	//current_node=root;
	while(!q.empty())
	{
		HyperGraph* hg=q.front();
		q.pop();
		
		if(hg->edges.empty())
		{
			continue;
		}
		if((int) hg->edges.size()  <= num_components )
		{
			for(int i=0;i<hg->edges.size();i++)
			{
				sampling_order.push_back(hg->edge2varindex[i]);
				//current_node->separator.push_back(hg->edge2varindex[i]);
			}
			continue;
		}
			
		// Step 1 Write the current hypergraph to a file
		cout<<"Writing file\n";
		hg->write();
		cout<<"File written\n";
		// Step 2 Run Hmetis on the file
		
		char command[100];
		sprintf(command,"./shmetis tmp.hgr %d 30 > t",num_components);
		system(command);

		// Step 3 Read output of hmetis
		char partfilename[100];
		sprintf(partfilename,"tmp.hgr.part.%d",num_components);
		vector<int> vertex_in_component(hg->vertices.size());
		ifstream in(partfilename);
		for(int i=0;i<hg->vertices.size();i++)
		{
			in>>vertex_in_component[i];
			assert(vertex_in_component[i] < num_components);
		}
		in.close();

		//cerr<<"Read infile\n";

		vector<int> separator;
		// Step 4 Add the separator to the sampling order
		
		vector<HyperGraph*> components;
		hg->getSeparator(num_components,vertex_in_component,separator,components);

		//cerr<<"Got separator \n";

		//Update the sampling order
		for(int i=0;i<separator.size();i++)
		{
			sampling_order.push_back(separator[i]);
		}
		//std::copy(separator.begin(),separator.end(),sampling_order.end());

		//cerr<<"components added\n";
		// Put the components in the queue
		for(int i=0;i<components.size();i++)
		{
			q.push(components[i]);
		}
	}
}

/*
int main (int argc, char* argv[])
{
  HyperGraph g;
  g.readErgo(argv[1]);
  vector<int> sampling_order;
  g.computeOrdering(3,sampling_order);
  
  cerr<<"Order size = "<<sampling_order.size()<<" "<<g.edges.size()<<endl;
  for(int i=0;i<sampling_order.size();i++)
    cerr<<sampling_order[i]<<" ";
  cerr<<endl;
}
*/
}
