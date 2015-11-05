#ifndef SS_FUNCTION_H_
#define SS_FUNCTION_H_

#include <iostream>
#include <vector>
#include "Variable.h"
#include "Double.h"
#include "Heap.h"
#include "Alg.h"
#include "SolverTypes.h"
#include "Util.h"

using namespace std;

namespace ss{

#define WITH_TABLE 1

struct Function
{
protected:
	int id_;
	vector<Variable*> variables_;
#ifdef WITH_TABLE 
	vector<Double> table_;
#endif
	int tableFalseEntry_;
	Double weightWhenFalse_;
	Double weightWhenTrue_;
	int tableSize_;	
public:
	Function():id_(INVALID_VALUE){}
	Function(const int& id): id_(id),variables_(vector<Variable*>())
#ifdef WITH_TABLE 
		,table_(vector<Double>())
#endif
	{
	}
	Function(const int& id, const vector<Variable*>& variables): id_(id),variables_(variables)
#ifdef WITH_TABLE 
		, table_(vector<Double>(Variable::getDomainSize(variables)))
#endif
	{
	}
	Function(const int& id, const vector<Variable*>& variables, Double weighWhenFalse, int tableFalseEntry): 
		id_(id),variables_(variables), 
#ifdef WITH_TABLE 
		table_(NULL),
#endif
		weightWhenFalse_(weighWhenFalse), tableFalseEntry_(tableFalseEntry), weightWhenTrue_(1)
	{
#ifdef WITH_TABLE 
		int num_values=Variable::getDomainSize(variables);
		table_=vector<Double> (num_values);
		for(int j=0;j<num_values;j++)
		{
			if(j == tableFalseEntry_)
				table_[j]=Double(weightWhenFalse_);
			else
				table_[j]=Double(1);
		}
#else

		tableSize_ = Variable::getDomainSize(variables);
#endif



	/*cout <<id_<<": " << weightWhenFalse_ << ", "<< tableFalseEntry_ << "/"<<Variable::getDomainSize(variables_)<<"-->";
	for(int i = 0; i<variables_.size(); i++)
		cout <<variables_[i]->id() <<"("<<variables_[i]->addr_value()<<") ";
	cout <<endl;*/
	}

	/*
	Function(const Function& function): id_(function.id()),variables_(function.variables()),table_(function.table())
	{
	}*/
	virtual ~Function(){ }
	int& id() { return id_;}
	int id() const {return id_;}
	vector<Variable*>& variables()  { return variables_;}
	//vector<Double>& table() { return table_;}

	//Double& tableEntry(int i) { return table_[i];}
	Double& tableEntry(int i) 
	{ 
#ifdef WITH_TABLE 
		return table_[i];
#else
		if (i == tableFalseEntry_)
			return weightWhenFalse_; 
		else return weightWhenTrue_;
#endif
	}

	void tableInit(int size) 
	{
#ifdef WITH_TABLE 
		table_ = vector<Double> (size);
#else
		tableFalseEntry_ = -1;
		weightWhenFalse_ = Double();
		weightWhenTrue_  = Double();
		tableSize_       = size;
		//printStacktrace();
#endif
	}
	void tableSet(vector<Double>& table) 
	{
#ifdef WITH_TABLE 
		table_ = table;
#else
		assert(false && "Function::tableSet is not implemented yet");
#endif
	}
	vector<Double>& tableGet() 
	{ 
#ifdef WITH_TABLE
		return table_;
#else
		assertStacktrace(false && "Function::tableGet is not implemented yet");
#endif
	}
	int tableSize() 
	{
#ifdef WITH_TABLE 
		return table_.size();
#else
		return tableSize_;
#endif
	}
	double MSE(Function& function);
	void project(Function& function);
	void product(Function& function);
	void divide(Function& function);
	void marginalize(vector<Variable*>&marg_variables,Function& function);
	void normalize();
	void print(ostream& out=cout);
	//void readCNF(istream& in, vector<Variable*>& all_variables,vector<Lit>& clause, double damp=0.0, int c_bound=15);
	//void readVIB(istream& in,vector<Variable*>& all_variables);
	void readCF(istream& in,vector<Variable*>& all_variables);
	virtual void removeEvidence();
	virtual void reduceDomains();
	Double getWeight()
	{
#ifdef WITH_TABLE
		return table_[Variable::getAddress(variables_)];
#else
		return tableEntry(Variable::getAddress(variables_));
#endif
	}

	static void dummy_product(Function& f1, Function& f2, Function& f3);
	static void dummy_divide(Function& f1, Function& f2, Function& f3);
	static void dummy_multiplyMarginalize( vector<Variable*>& marg_variables, vector<Function*>& functions, Function& function);
	static double MSE(Function* f1, Function* f2);
	//static void product(vector<Function*>& functions, Function& function);
	static void marginalize(vector<Variable*>& marg_variables, vector<Function*>& functions, Function& function);
	static void multiplyAndMarginalize( vector<Variable*>& marg_variables, vector<Function*>& functions, Function& function);
	//static void divide(Function& f1, Function& f2, Function& f3);
	
};

}
#endif

