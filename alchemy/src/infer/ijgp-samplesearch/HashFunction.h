#ifndef HASH_FUNCTION_H_
#define HASH_FUNCTION_H_
#include "Function.h"
#include <google/sparse_hash_map>
#include <google/sparsetable>
using google::sparse_hash_map; 
using google::sparsetable; 

class HashFunction: public Function
{
public:
	vector<vector<int> > relations_;
	sparsetable<Double> sparse_table_;
public:
	HashFunction(){ }
	~HashFunction() {}
	HashFunction(Function& function);
	vector<vector<int> >& relations() { return relations_;}
	sparsetable<Double>& stable() { return sparse_table_;}
	void print(ostream& out=cout);
	void product(HashFunction& function, HashFunction& out_function);
	static void product(vector<HashFunction*>& functions, HashFunction& out_function);
	static void multiplyAndMarginalize(vector<Variable*>& marg_variables,vector<HashFunction*>& functions, HashFunction* out_function);
	
};
#endif