#include "JG.h"
#include "myRandom.h"
namespace ss{
JGNode* JG::addNode(JG_TYPE type) {
	switch (type) {
	case SS:
		return new JGNodeSS();
		break;
	case LS:
		return new JGNodeLS();
		break;
	case HSS:
		assert(false && "HSS not implemented");
		//return new JGNodeHSS();
		break;
	case LSS:
		return new JGNodeLSS();
		break;
	default:
		return new JGNodeSS();
	}
}

JGEdge* JG::addEdge(JGNode* s1, JGNode* s2, JG_TYPE type) {
	switch (type) {
	case SS:
		return new JGEdgeSS(dynamic_cast<JGNodeSS*> (s1),
				dynamic_cast<JGNodeSS*> (s2));
		break;
	case LS:
		return new JGEdgeLS(dynamic_cast<JGNodeLS*> (s1),
				dynamic_cast<JGNodeLS*> (s2));
		break;
	case HSS:
		assert(false && "HSS not implemented");
		//return new JGEdgeHSS(dynamic_cast<JGNodeHSS*> (s1),
		//		dynamic_cast<JGNodeHSS*> (s2));
		break;
	case LSS:
		return new JGEdgeLSS(dynamic_cast<JGNodeLSS*> (s1),
				dynamic_cast<JGNodeLSS*> (s2));
		break;
	default:
		return new JGEdgeSS(dynamic_cast<JGNodeSS*> (s1),
				dynamic_cast<JGNodeSS*> (s2));
		break;
	}
}

struct JGHelperFunction {
	vector<Variable*> variables;
	int parent_id;
	Function* f;
	JGHelperFunction(vector<Variable*>& variables_, int id_, Function* func_ =
			NULL) :
		variables(variables_), parent_id(id_), f(func_) {
	}
};

JG::JG(GM& gm, int i_bound__, int num_iterations__, vector<int>& order,
		JG_TYPE type) :
	num_iterations_(num_iterations__), i_bound_(i_bound__), copy_of_gm(&gm) {
	vector<int> mapped_order(order.size());
	for (int i = 0; i < order.size(); i++)
		mapped_order[order[i]] = i;
	assert(order.size()==gm.variables.size());
	//cerr<<"Creating buckets\n";
	// Create buckets
	vector<vector<JGHelperFunction> > buckets(order.size());
	for (int i = 0; i < gm.functions.size(); i++) {
		int min_size = (int) gm.variables.size();
		for (int j = 0; j < gm.functions[i]->variables().size(); j++) {
			if (gm.functions[i]->variables()[j]->id() >= order.size()) {
				gm.functions[i]->print();
				exit(1);
			}
			if (mapped_order[gm.functions[i]->variables()[j]->id()] < min_size) {
				min_size = mapped_order[gm.functions[i]->variables()[j]->id()];
			}
		}
		if (min_size == (int) gm.variables.size()) {
			for (int j = 0; j < gm.functions[i]->variables().size(); j++) {
				cerr << gm.functions[i]->variables()[j]->id() << endl;
			}

		}
		//cout<<min_size<<endl;
		buckets[min_size].push_back(JGHelperFunction(
				gm.functions[i]->variables(), INVALID_VALUE, gm.functions[i]));
		//cout<<"\tAdded\n";
	}
	//cout<<"buckets created\n";

	//Update i_bound
	/*
	 int max_func_size=0;

	 for(int i=0;i<gm.functions.size();i++)
	 {
	 int domain_size=0;
	 domain_size=Variable::getDomainSize(gm.functions[i]->variables());
	 if(max_func_size < domain_size)
	 {
	 max_func_size=domain_size;
	 }
	 }
	 int avg_domain_size=0;
	 for(int i=0;i<gm.variables.size();i++)
	 {
	 avg_domain_size+=gm.variables[i]->domain_size();
	 }
	 avg_domain_size/=(int)gm.variables.size();
	 cerr<<avg_domain_size<<" "<<max_func_size<<" ";
	 int i_bound=1;
	 for(int i=0;i<(i_bound_-1);i++)
	 {
	 i_bound*=avg_domain_size;
	 }
	 i_bound*=(max_func_size);
	 */
	//max_cluster_size=i_bound;
	cerr << "Dom size i-bound = " << i_bound() << endl;
	int old_nodes = 0;
	int id = 0;
	// Run schematic mini-buckets
	for (int i = 0; i < order.size(); i++) {
		//cerr<<i<<endl;
		// Form mini-buckets by using the first fill rule
		for (int j = 0; j < buckets[i].size(); j++) {
			bool found = false;
			int loc = INVALID_VALUE;
			for (int k = old_nodes; k < nodes.size(); k++) {
				vector<Variable*> temp_variables;
				do_set_union(buckets[i][j].variables, nodes[k]->variables(),
						temp_variables, less_than_comparator_variable);
				//int domain_size=Variable::getDomainSize(temp_variables);
				//if((int)domain_size <= i_bound)
				if ((int) temp_variables.size() <= i_bound()) {
					found = true;
					loc = k;
					break;
				}
			}
			// If not found create a mini-bucket
			if (!found) {

				nodes.push_back(addNode(type));
				nodes[nodes.size() - 1]->id() = id;
				id++;
				nodes[nodes.size() - 1]->variables() = buckets[i][j].variables;
				if (buckets[i][j].parent_id != INVALID_VALUE) {
					//cout<<"F1\n";
					//nodes[nodes.size()-1]->neighbors().push_back(nodes[buckets[i][j].parent_id]);
					//nodes[buckets[i][j].parent_id]->neighbors().push_back(nodes[nodes.size()-1]);
					JGEdge* edge = addEdge(nodes[nodes.size() - 1],
							nodes[buckets[i][j].parent_id], type);
					do_set_intersection(nodes[nodes.size() - 1]->variables(),
							nodes[buckets[i][j].parent_id]->variables(),
							edge->variables(), less_than_comparator_variable);
					//edge->setNode1(nodes[nodes.size()-1]);
					//edge->setNode2(nodes[buckets[i][j].parent_id]);
					do_set_intersection(nodes[nodes.size() - 1]->variables(),
							nodes[buckets[i][j].parent_id]->variables(),
							edge->variables(), less_than_comparator_variable);
					nodes[nodes.size() - 1]->edges().push_back(edge);
					nodes[buckets[i][j].parent_id]->edges().push_back(edge);

				} else {
					//cout<<"F2\n";
					nodes[nodes.size() - 1]->addFunction(*buckets[i][j].f);
				}
			} else {

				do_set_union(buckets[i][j].variables, nodes[loc]->variables(),
						nodes[loc]->variables(), less_than_comparator_variable);
				if (buckets[i][j].parent_id != INVALID_VALUE) {
					//cout<<"F3\n";
					/*nodes[loc]->neighbors().push_back(nodes[buckets[i][j].parent_id]);
					 nodes[buckets[i][j].parent_id]->neighbors().push_back(nodes[loc]);*/
					JGEdge* edge = addEdge(nodes[loc],
							nodes[buckets[i][j].parent_id], type);
					do_set_intersection(nodes[loc]->variables(),
							nodes[buckets[i][j].parent_id]->variables(),
							edge->variables(), less_than_comparator_variable);
					//edge->setNode1(nodes[loc]);
					//edge->setNode2(nodes[buckets[i][j].parent_id]);
					do_set_intersection(nodes[loc]->variables(),
							nodes[buckets[i][j].parent_id]->variables(),
							edge->variables(), less_than_comparator_variable);
					nodes[loc]->edges().push_back(edge);
					nodes[buckets[i][j].parent_id]->edges().push_back(edge);

				} else {
					//cout<<"F4\n";
					nodes[loc]->addFunction(*buckets[i][j].f);
				}

			}
		}
		vector<Variable*> curr_variable;
		curr_variable.push_back(gm.variables[order[i]]);

		// Connect the newly created nodes to each other
		for (int j = old_nodes; j < (int) nodes.size() - 1; j++) {
			//nodes[j]->neighbors().push_back(nodes[j+1]);
			//nodes[j+1]->neighbors().push_back(nodes[j]);
			JGEdge* edge = addEdge(nodes[j], nodes[j + 1], type);
			//edge->setNode1(nodes[j]);
			//edge->setNode2(nodes[j+1]);
			edge->variables() = curr_variable;
			nodes[j]->edges().push_back(edge);
			nodes[j + 1]->edges().push_back(edge);
		}

		if (i < (int) order.size() - 1) {
			for (int j = old_nodes; j < nodes.size(); j++) {

				vector<Variable*> temp_variables;
				do_set_difference(nodes[j]->variables(), curr_variable,
						temp_variables, less_than_comparator_variable);
				if (temp_variables.empty())
					continue;
				// Put the node in the appropriate bucket
				int min_size = (int) gm.variables.size();
				for (int k = 0; k < temp_variables.size(); k++) {

					if (min_size > mapped_order[temp_variables[k]->id()]) {
						min_size = mapped_order[temp_variables[k]->id()];
					}
				}
				assert(min_size<(int)gm.variables.size());
				//if(min_size >=(int) gm.variables.size())
				//	continue;
				buckets[min_size].push_back(JGHelperFunction(temp_variables, j));
			}
		}
		old_nodes = (int) nodes.size();
	}

	//print();
	// Minimize edges
	//minimize();
	// update node ids
	/*for(int i=0;i<nodes.size();i++)
	 {
	 nodes[i]->id()=i;
	 }*/
	//print();
	//// Set node and edge sizes
	//for(int i=0;i<nodes.size();i++)
	//{
	//	// Find domain size
	//	int domain_size=Variable::getDomainSize(nodes[i]->variables());
	//	nodes[i]->table()=vector<Double>(domain_size);
	//	for(int j=0;j<domain_size;j++)
	//	{
	//		nodes[i]->table()[j]=Double(1.0);
	//	}
	//	for(int j=0;j<nodes[i]->edges().size();j++)
	//	{
	//		int domain_size=Variable::getDomainSize(nodes[i]->edges()[j]->variables());
	//		nodes[i]->edges()[j]->message1()=vector<Double> (domain_size);
	//		nodes[i]->edges()[j]->message2()=vector<Double> (domain_size);
	//		for(int k=0;k<domain_size;k++)
	//		{
	//			nodes[i]->edges()[j]->message1()[k]=Double(1.0);
	//			nodes[i]->edges()[j]->message2()[k]=Double(1.0);
	//		}
	//	}
	//}
	//cerr<<"Now putting functions\n";
	////reduce();
	cout << "Join graph done\n";

}
//Perform propagation by performing along a DFS search tree of a join graph
bool JG::propagateDFS() {
	//Initialize
	for (int i = 0; i < nodes.size(); i++) {
		nodes[i]->initialize();
		/*
		for (int j = 0; j < nodes[i]->edges().size(); j++) {
			nodes[i]->edges()[j]->initialize();
		}
		*/
	}
	for (int i = 0; i < num_iterations_; i++) {
		cerr<<"Iteration "<<i<<" done\n";
		if (i>=8 && convergence_test()){
			break;
		}
		vector<bool> visited(nodes.size());
		//Create a random ordering
		vector<int> order(nodes.size());
		for (int j = 0; j < nodes.size(); j++)
			order[j] = j;
		myRandom random;
		for (int j = 0; j < nodes.size(); j++) {
			visited[j]=false;
			int i1 = random.getInt(nodes.size());
			int i2 = random.getInt(nodes.size());
			int temp = order[i1];
			order[i1] = order[i2];
			order[i2] = temp;
		}

		for (int j = 0; j < nodes.size(); j++) {
			int curr_node = order[j];
			if (visited[curr_node])
				continue;
			for (int k = 0; k < nodes[curr_node]->edges().size(); k++) {
				if (nodes[curr_node]->edges()[k]->node1()->id()
						== nodes[curr_node]->id()) {
					if (visited[nodes[curr_node]->edges()[k]->node2()->id()])
						continue;
					nodes[curr_node]->edges()[k]->sendMessage1to2();
					/*for(int a=0;a<nodes[j]->edges()[k]->node2()->table().size();a++)
					 {
					 cout<<nodes[j]->edges()[k]->node2()->table()[a]<<" ";
					 }
					 cout<<endl;*/
				} else {

					assert(nodes[curr_node]->edges()[k]->node2()->id()==nodes[curr_node]->id());
					if (visited[nodes[curr_node]->edges()[k]->node1()->id()])
						continue;
					nodes[curr_node]->edges()[k]->sendMessage2to1();
				}
			}
			visited[curr_node] = true;
		}
		for (int j = 0; j < nodes.size(); j++) {
			visited[j]=false;
		}
		for (int j = nodes.size()-1; j >-1; j--) {
			int curr_node = order[j];
			if (visited[curr_node])
				continue;
			for (int k = 0; k < nodes[curr_node]->edges().size(); k++) {
				if (nodes[curr_node]->edges()[k]->node1()->id() == nodes[curr_node]->id()) {
					if (visited[nodes[curr_node]->edges()[k]->node2()->id()])
						continue;
					nodes[curr_node]->edges()[k]->sendMessage1to2();
					/*for(int a=0;a<nodes[j]->edges()[k]->node2()->table().size();a++)
					 {
					 cout<<nodes[j]->edges()[k]->node2()->table()[a]<<" ";
					 }
					 cout<<endl;*/
				}
				else {

					assert(nodes[curr_node]->edges()[k]->node2()->id()==nodes[curr_node]->id());
					if (visited[nodes[curr_node]->edges()[k]->node1()->id()])
						continue;
					nodes[curr_node]->edges()[k]->sendMessage2to1();
				}
			}
			visited[curr_node] = true;
		}

	}
	for (int i = 0; i < nodes.size(); i++) {
		/*for(int j=0;j<nodes[i]->variables().size();j++)
		 cout<<nodes[i]->variables()[j]->id()<<" ";
		 cout<<"---";*/
		nodes[i]->updateVariables();
		/*for(int j=0;j<nodes[i]->variables().size();j++)
		 cout<<nodes[i]->variables()[j]->id()<<" ";
		 cout<<endl;*/
	}
	return true;
}
bool JG::propagate() {
	//return propagateDFS();
	//Initialize
	for (int i = 0; i < nodes.size(); i++) {
		nodes[i]->initialize();
		//for(int j=0;j<nodes[i]->edges().size();j++)
		//{
		//	nodes[i]->edges()[j]->initialize();
		//}
	}
	cerr << "Nodes initialized\n";
	for (int i = 0; i < num_iterations_; i++) {
		if (i>=10 && convergence_test()){
			break;
		}
		cerr << "Iteration " << i << " Done\n";
		for (int j = 0; j < nodes.size(); j++) {
			for (int k = 0; k < nodes[j]->edges().size(); k++) {
				if (nodes[j]->edges()[k]->node1()->id() == nodes[j]->id()) {
					nodes[j]->edges()[k]->sendMessage1to2();
					/*for(int a=0;a<nodes[j]->edges()[k]->node2()->table().size();a++)
					 {
					 cout<<nodes[j]->edges()[k]->node2()->table()[a]<<" ";
					 }
					 cout<<endl;*/
				} else {
					assert(nodes[j]->edges()[k]->node2()->id()==nodes[j]->id());
					nodes[j]->edges()[k]->sendMessage2to1();
				}
			}

		}
	}
	for (int i = 0; i < nodes.size(); i++) {
		/*for(int j=0;j<nodes[i]->variables().size();j++)
		 cout<<nodes[i]->variables()[j]->id()<<" ";
		 cout<<"---";*/
		nodes[i]->updateVariables();
		/*for(int j=0;j<nodes[i]->variables().size();j++)
		 cout<<nodes[i]->variables()[j]->id()<<" ";
		 cout<<endl;*/
	}
}
void JG::print(ostream& out) {
	out << "num nodes = " << nodes.size() << endl;
	for (int i = 0; i < nodes.size(); i++) {
		for (int j = 0; j < nodes[i]->variables().size(); j++) {
			out << nodes[i]->variables()[j]->id() << " ";
		}
		out << endl;
	}
}

void JG::printGraph(ostream& out) {
	out << "num nodes = " << nodes.size() << endl;
	for (int i = 0; i < nodes.size(); i++) {
		for (int j = 0; j < nodes[i]->variables().size(); j++) {
			out << nodes[i]->variables()[j]->id() << " ";
		}
		out << endl;
		JGNodeSS* ss_node;
		ss_node = dynamic_cast<JGNodeSS*> (nodes[i]);
		if (ss_node != NULL) {
			for (int j = 0; j < ss_node->functions.size(); j++) {
				out << "\t";
				ss_node->functions[j]->print(out);
			}
		}

	}

	for (int i = 0; i < nodes.size(); i++) {
		for (int j = 0; j < nodes[i]->edges().size(); j++) {
			JGEdge* edge = nodes[i]->edges()[j];
			out << edge->node1()->id() << " " << edge->node2()->id()
					<< "  vars = ";
			for (int k = 0; k < edge->variables().size(); k++)
				out << edge->variables()[k]->id() << " ";
			out << endl;

		}
	}
}

void JG::getGraph(GM& gm, Graph& graph) {
	vector<vector<bool> > adj_matrix(gm.variables.size());
	for (int i = 0; i < adj_matrix.size(); i++)
		adj_matrix[i] = vector<bool> (gm.variables.size());
	for (int i = 0; i < nodes.size(); i++) {
		for (int j = 0; j < nodes[i]->variables().size(); j++) {
			int var1 = nodes[i]->variables()[j]->id();
			for (int k = j + 1; k < nodes[i]->variables().size(); k++) {
				int var2 = nodes[i]->variables()[k]->id();
				assert(var1!=var2);
				adj_matrix[var1][var2] = true;
				adj_matrix[var2][var1] = true;
			}
		}
	}
	graph = Graph(gm.variables.size(), adj_matrix, false);
}

void JG::clear() {

	set<JGEdge*> all_edges;
	//First clear all the edges
	for (int i = 0; i < nodes.size(); i++) {
		if (nodes[i] == NULL)
			continue;
		for (int j = 0; j < nodes[i]->edges().size(); j++) {
			all_edges.insert(nodes[i]->edges()[j]);
		}
	}
	//cout<<"Edges deleted\n";

	// Now delete the nodes
	for (int i = 0; i < nodes.size(); i++) {
		delete (nodes[i]);
		//cout<<"Deleted\n";
	}
	for (set<JGEdge*>::iterator i = all_edges.begin(); i != all_edges.end(); i++) {
		if ((*i) == NULL)
			continue;
		delete (*i);
	}
}

void JG::updateMarginals(bool recompute)
{
	if (marginal_nodes.empty() || recompute){
		marginal_nodes=vector<JGNode*> (copy_of_gm->variables.size());
		int count=0;
		for (int i = 0; i < nodes.size(); i++) {
			for(int j=0;j<nodes[i]->variables().size();j++){
				if (!marginal_nodes[nodes[i]->variables()[j]->id()]){
					marginal_nodes[nodes[i]->variables()[j]->id()]=nodes[i];
					count++;
				}
			}
			if (count==(int)copy_of_gm->variables.size()){
				break;
			}
		}
	}

	marginals=vector<Function> (copy_of_gm->variables.size());
	for(int i=0;i<copy_of_gm->variables.size();i++){
		vector<Variable*> curr_variable;
		curr_variable.push_back(copy_of_gm->variables[i]);
		if(marginal_nodes[i]){
			marginal_nodes[i]->getMarginal(curr_variable,marginals[i]);
		}
		else {
			marginals[i].variables() = curr_variable;
			marginals[i].tableInit(copy_of_gm->variables[i]->domain_size());
			for (int j = 0; j < copy_of_gm->variables[i]->domain_size(); j++) {
				marginals[i].tableEntry(j) = Double((double) 1.0
						/ (double) copy_of_gm->variables[i]->domain_size());
			}
		}
	}
}
bool JG::convergence_test()
{
	vector<Function> old_marginals=marginals;
	updateMarginals();
	if (old_marginals.empty()){
		return false;
	}
	assert(marginals.size()==old_marginals.size());
	long double error=0.0;
	for(int i=0;i<marginals.size();i++){
		for(int j=0;j<marginals[i].tableSize();j++){
			error+=fabs(marginals[i].tableEntry(j).value()-old_marginals[i].tableEntry(j).value());
		}
	}
	error/=(long double)marginals.size();
	cout<<"Error = "<<error<<endl;
	if (error < (long double)0.00001){
		return true;
	}
	return false;
}
}
