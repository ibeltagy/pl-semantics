#ifndef GRAY_CODE_H_
#define GRAY_CODE_H

#include "Variable.h"
#include "Function.h"
#include <iostream>
#include <vector>
using namespace std;
// Gray Code for storing product of functions
struct GrayCode
{
protected:
	// Variables required by Knuth's algorithm
	int j;
	vector<int> F;
	vector<int> O;
	vector<int> A;
	vector<int> M;

	// Gray code in our implementation requires a vector of variables and functions
	// another requirement is that the union of the variables in the functions is equal to the variables
	vector<Variable*> variables;
	vector<Function*> functions;
	
	// Internal variables to maintain state
	vector<vector<int> > vars_to_functions;
	vector<vector<int> > vars_to_functions_multipliers;
	vector<int> address_of_functions;
	bool first;
	void incrementAddress();
public:
	GrayCode(const vector<Variable*>& variables_,const vector<Function*>& functions_);
	virtual void moveForward()=0;
};

// Gray code for product
struct GrayCodeP: public GrayCode
{
protected:
	// Function where the product is to be stored
	Function* f;
	// The current address of the function
	int address_of_f;
	// The appropriate multipliers for the function
	vector<int> vars_to_f_multipliers;
	

public:
	GrayCodeP(const vector<Variable*>& variables_,const vector<Function*>& functions_, Function* function);
	void moveForward();
};

// Gray code for product and then marginalize
// you can simulate pure marginalization using this gray code
struct GrayCodePM: public GrayCode
{
private:
	Function* f;
	// The appropriate multipliers for the function
	vector<int> vars_to_f_multipliers;
	int address_of_f;
public:
	GrayCodePM(const vector<Variable*>& variables_,const vector<Function*>& functions_, Function* function_, const vector<Variable*>& marg_variables);
	void moveForward();
};


// Gray code for division...this is a pure hack...should improve in future
// We assume that the vector of functions in gray code has only two functions, say F1 and F2.
// We will store F1/F2 in F
struct GrayCodeD: public GrayCode
{
private:
	Function* f;
	// The appropriate multipliers for the function
	vector<int> vars_to_f_multipliers;
	int address_of_f;
public:
	GrayCodeD(const vector<Variable*>& variables_,const vector<Function*>& functions_, Function* function_);
	void moveForward();
};

#endif