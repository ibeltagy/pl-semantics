#ifndef AOTREE_H_
#define AOTREE_H_

#include "GM.h"
#include "Graph.h"
#include <map>

struct PseudoTreeNode
{
	Variable* variable;
	vector<PseudoTreeNode*> children;
public:
	
	PseudoTreeNode():variable(NULL){}
};
class PseudoTree
{	
public:
	PseudoTreeNode* root;
	PseudoTree():root(NULL){}
	PseudoTree(GM& gm,Graph& g ,vector<int>& order);
	//PseudoTree(GM& gm, JG& jg, vector<int>& sampling_order,vector<int>& order);
	void generateAncestors(GM& gm, vector<vector<Variable*> >& ancestors);
	void print(ostream &out);
};
struct AOTreeNode
{
	bool added;
	PseudoTreeNode* clone_node;

	map<int,int> val2index;
	vector<vector<AOTreeNode*> > children;
	vector<int> number;
	vector<Double> weights;

	//AOTreeNode* parent;

	// The final value of computation will be stored here
	Double value;
	//Double num_samples;
	
	AOTreeNode():clone_node(NULL),added(false),number(vector<int>()),children(vector<vector<AOTreeNode*> > ()),
	weights(vector<Double> ()) { }
};
class AOTree
{
public:
	AOTreeNode* root;
private:
	vector<AOTreeNode*> leaves;
	PseudoTree& pseudo_tree;
	vector<vector<Function*> > ordered_functions;
	Double getP(int variable);
public:
	AOTree(PseudoTree& pseudo_tree_,GM& gm,vector<int>& sampling_order);
	void addAssignment(vector<Double>& Q);
	Double computeWeight();
};

#endif