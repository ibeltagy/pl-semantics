#ifndef FUNCTION_H_
#define FUNCTION_H_

#include <vector>
#include "Variable.h"
#include "Double.h"
#include "Heap.h"
#include "Alg.h"
#include "SolverTypes.h"


using namespace std;
struct Function
{
protected:
	int id_;
	vector<Variable*> variables_;
	vector<Double> table_;
public:
	Function():id_(INVALID_VALUE){}
	Function(const int& id): id_(id),variables_(vector<Variable*>()),table_(vector<Double>())
	{
	}
	Function(const int& id, const vector<Variable*>& variables): id_(id),variables_(variables), table_(vector<Double>(Variable::getDomainSize(variables)))
	{
	}
	/*
	Function(const Function& function): id_(function.id()),variables_(function.variables()),table_(function.table())
	{
	}*/
	virtual ~Function(){ }
	int& id() { return id_;}
	int id() const {return id_;}
	vector<Variable*>& variables()  { return variables_;}
	vector<Double>& table() { return table_;}
	double MSE(Function& function);
	void project(Function& function);
	void product(Function& function);
	void divide(Function& function);
	void marginalize(vector<Variable*>&marg_variables,Function& function);
	void normalize();
	void print(ostream& out=cout);
  void readCNF(istream& in, vector<Variable*>& all_variables,vector<Lit>& clause, double damp=0.0, int c_bound=15);
	void readVIB(istream& in,vector<Variable*>& all_variables);
	void readCF(istream& in,vector<Variable*>& all_variables);
	virtual void removeEvidence();
	virtual void reduceDomains();
	Double getWeight()
	{
		return table()[Variable::getAddress(variables_)];
	}

	static void dummy_product(Function& f1, Function& f2, Function& f3);
	static void dummy_divide(Function& f1, Function& f2, Function& f3);
	static void dummy_multiplyMarginalize( vector<Variable*>& marg_variables, vector<Function*>& functions, Function& function);
	static double MSE(Function* f1, Function* f2);
	static void product(vector<Function*>& functions, Function& function);
	static void marginalize(vector<Variable*>& marg_variables, vector<Function*>& functions, Function& function);
	static void multiplyAndMarginalize( vector<Variable*>& marg_variables, vector<Function*>& functions, Function& function);
	static void divide(Function& f1, Function& f2, Function& f3);
	
};


#endif
