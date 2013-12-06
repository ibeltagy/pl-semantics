#ifndef SS_JT_H_
#define SS_JT_H_

#include <vector>
#include "GM.h"
#include "Double.h"
#include "LogFunction.h"
#include "JG.h"

namespace ss{

struct JT
{
	vector<Function> marginals;
	vector<Variable*> variables;
	vector<JGNode*> nodes;
	vector<JGEdge*> edges;
	vector<JGEdge*> message_order;
	vector<JGNode*> var_to_node;
	JT(vector<Variable*>& variables_, vector<Function*>& functions, vector<set<int> >& clusters, vector<int>& order);
	~JT(){
		for(int i=0;i<edges.size();i++){
			if(edges[i]){
				delete(edges[i]);
			}
		}
		for(int i=0;i<nodes.size();i++){
			if(nodes[i]){
				delete(nodes[i]);
			}
		}
	}
	void propagate();
	
};
}
#endif
