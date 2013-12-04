#ifndef JG_H_
#define JG_H_

#include <iostream>
#include <fstream>
#include <vector>
#include <list>
#include "GM.h"
#include "Util.h"
#include "HashFunction.h"
#include "LogFunction.h"
#include "CPT.h"
#include "Graph.h"

using namespace std;

struct JGNode;
struct JGEdge;

// The base class for any discrete join graph node
// The propagation algorithm uses the functions addFunction() 

struct JGNode {
protected:
	vector<Variable*> variables_;
	vector<JGEdge*> edges_;
	int id_;
	bool deleted_;
public:
	// Default constructor
	JGNode() :
		id_(INVALID_VALUE), deleted_(false) {
	}
	// Access to some internal variables
	bool& deleted() {
		return deleted_;
	}
	virtual ~JGNode() {
	}
	vector<Variable*>& variables() {
		return variables_;
	}
	vector<JGEdge*>& edges() {
		return edges_;
	}
	//JGNode* neighbor(int i);
	int& id() {
		return id_;
	}
	// Main functions which are to be overloaded by various architectures

	virtual void addFunction(Function& function) {
	}
	virtual void getCF(vector<Variable*>& cond_variables,
			Variable* marg_variable, CPT& cpt) {
	}
	virtual void updateVariables() {
	}
	virtual void getMarginal(vector<Variable*>& marg_variables,
			Function& function) {
	}
	virtual void initialize() {
	}
};

// Join Graph Node using the Shenoy Shafer architecture
struct JGNodeSS: public JGNode {
public:
	vector<Function*> functions;
	void compileAllFunctions(vector<Function*>& all_functions);
public:
	JGNodeSS() :
		JGNode() {
	}
	~JGNodeSS() {
		for (int i = 0; i < functions.size(); i++) {
			if (functions[i]) {
				if (functions[i]->id() != INVALID_VALUE) {
					delete (functions[i]);
				}
			}
		}
	}
	void updateVariables();
	void getCF(vector<Variable*>& cond_variables, Variable* marg_variable,
			CPT& cf);
	void addFunction(Function& function);
	//void getMarginal(vector<Variable*>& marg_variables,vector<Double>& marg_table);
	void getMarginal(vector<Variable*>& marg_variables, Function& function);
	void initialize();
};

struct JGNodeLSS: public JGNode {
public:
	vector<Function*> original_functions;
	vector<LogFunction*> functions;
	void compileAllFunctions(vector<LogFunction*>& all_functions);
public:
	JGNodeLSS() :
		JGNode() {
	}
	~JGNodeLSS() {
		for (int i = 0; i < functions.size(); i++)
			if (functions[i])
				delete (functions[i]);
	}
	void updateVariables();
	void getCF(vector<Variable*>& cond_variables, Variable* marg_variable,
			CPT& cf);
	void addFunction(Function& function);
	//void getMarginal(vector<Variable*>& marg_variables,vector<Double>& marg_table);
	void getMarginal(vector<Variable*>& marg_variables, Function& function);
	void initialize();
};

// Join Graph Node using the Lauritzen Speigelhalter architecture
struct JGNodeLS: public JGNode {
public:
	Function function;
	vector<Function*> original_functions;
public:
	JGNodeLS() :
		JGNode() {
	}
	~JGNodeLS() {
	}
	//vector<Double>& table() { return table_;}
	void updateVariables();
	void initialize();
	void getCF(vector<Variable*>& cond_variables, Variable* marg_variable,
			CPT& cf);
	void addFunction(Function& function);
	//void getMarginal(vector<Variable*>& marg_variables,vector<Double>& marg_table);
	void getMarginal(vector<Variable*>& marg_variables, Function& function);
};

// Hash function with Shenoy Shafer
struct JGNodeHSS: public JGNode {
public:
	vector<HashFunction*> functions;
public:
	JGNodeHSS() :
		JGNode() {
	}
	~JGNodeHSS() {
	}
	void updateVariables() {
	}
	void getCF(vector<Variable*>& cond_variables, Variable* marg_variable,
			CPT& cf);
	void addFunction(Function& function);
	void getMarginal(vector<Variable*>& marg_variables,
			vector<Double>& marg_table);
	void getMarginal(vector<Variable*>& marg_variables, Function& function);
};
// Base class for Join Graph Edge
struct JGEdge {
protected:
	JGNode* node1_;
	JGNode* node2_;
	mutable vector<Variable*> variables_;
	Function* node1_to_node2_message_;
	Function* node2_to_node1_message_;
public:
	// Access to internal data structure
	JGEdge() :
		node1_to_node2_message_(new Function()), node2_to_node1_message_(
				new Function()) {
	}
	virtual ~JGEdge() {
		/*
		if (node1_to_node2_message_)
			delete (node1_to_node2_message_);
		if (node2_to_node1_message_)
			delete (node2_to_node1_message_);
			*/

	}
	virtual void initialize() {
	}
	JGNode* node1() {
		return node1_;
	}
	JGNode* node2() {
		return node2_;
	}
	vector<Variable*>& variables() const {
		return variables_;
	}
	Function& message1() {
		return *node1_to_node2_message_;
	}
	Function& message2() {
		return *node2_to_node1_message_;
	}

	// Functions

	void printMessages();
	virtual void sendMessage1to2() {
	}
	virtual void sendMessage2to1() {
	}
};

struct JGEdgeSS: public JGEdge {
protected:
	JGNodeSS* ss_node1_;
	JGNodeSS* ss_node2_;
public:
	JGEdgeSS() :
		JGEdge() {
	}
	~JGEdgeSS() {
		if (node1_to_node2_message_)
			delete (node1_to_node2_message_);
		if (node2_to_node1_message_)
			delete (node2_to_node1_message_);
	}
	JGEdgeSS(JGNodeSS* ss_node1__, JGNodeSS* ss_node2__);
	void initialize();
	void sendMessage1to2();
	void sendMessage2to1();
};

struct JGEdgeLSS: public JGEdge {
protected:
	JGNodeLSS* ss_node1_;
	JGNodeLSS* ss_node2_;
public:
	JGEdgeLSS() {
		node1_to_node2_message_ = new LogFunction();
		node2_to_node1_message_ = new LogFunction();
	}
	~JGEdgeLSS() {

		if (node1_to_node2_message_)
			delete (node1_to_node2_message_);
		if (node2_to_node1_message_)
			delete (node2_to_node1_message_);

	}
	JGEdgeLSS(JGNodeLSS* ss_node1__, JGNodeLSS* ss_node2__);
	void initialize();
	void sendMessage1to2();
	void sendMessage2to1();
};

struct JGEdgeLS: public JGEdge {
protected:
	JGNodeLS* ls_node1_;
	JGNodeLS* ls_node2_;
public:
	JGEdgeLS() :
		JGEdge() {
	}
	~JGEdgeLS() {
		if (node1_to_node2_message_)
			delete (node1_to_node2_message_);
		if (node2_to_node1_message_)
			delete (node2_to_node1_message_);
	}
	void initialize() {
	}
	JGEdgeLS(JGNodeLS* ls_node1__, JGNodeLS* ls_node2__);
	void sendMessage1to2();
	void sendMessage2to1();
};
struct JGEdgeHSS: public JGEdge {
protected:
	JGNodeHSS* hss_node1_;
	JGNodeHSS* hss_node2_;
public:
	JGEdgeHSS() {
		node1_to_node2_message_ = new HashFunction();
		node2_to_node1_message_ = new HashFunction();
	}
	~JGEdgeHSS() {
		if (node1_to_node2_message_)
			delete (node1_to_node2_message_);
		if (node2_to_node1_message_)
			delete (node2_to_node1_message_);
	}
	JGEdgeHSS(JGNodeHSS* hss_node1__, JGNodeHSS* hss_node2__);
	void initialize() {
	}
	void sendMessage1to2();
	void sendMessage2to1();
};
typedef enum {
	SS, LS, HSS, LSS, SSC, LSC, HSSC
} JG_TYPE;
struct JG {
private:
	int num_iterations_;
	int max_cluster_size;
	int i_bound_;
	//void reduce();
	JGNode* addNode(JG_TYPE type = SS);
	JGEdge* addEdge(JGNode* s1, JGNode* s2, JG_TYPE type = SS);
	vector<JGNode*> marginal_nodes;
	GM* copy_of_gm;
public:
	vector<Function> marginals;
	vector<JGNode*> nodes;
	void printGraph(ostream& out);
	JG(GM& gm, int i_bound, int num_iterations, vector<int>& order,
			JG_TYPE type = SS);
	~JG() {
		// Get the edges
		set<JGEdge*> all_edges;
		for (int j = 0; j < nodes.size(); j++) {
			for (int k = 0; k < nodes[j]->edges().size(); k++) {
				all_edges.insert(nodes[j]->edges()[k]);
			}
		}
		for(set<JGEdge*>::iterator i=all_edges.begin();i!=all_edges.end();i++){
			if (*i){
				delete(*i);
			}
		}
		for (int i = 0; i < nodes.size(); i++) {
			if (nodes[i]) {
				delete (nodes[i]);
			}
		}

	}
	void getGraph(GM& gm, Graph& graph);
	//void contractEdge(JGEdge* edge);
	//void putFunctions(GM& gm);
	//void minimize();
	int i_bound() {
		return i_bound_;
	}
	bool propagate();
	bool propagateDFS();
	void print(ostream& out);
	void clear();

	bool convergence_test();
	void updateMarginals(bool recompute=false);
};

#endif
