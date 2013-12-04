#ifndef BE_H_
#define BE_H_

#include <vector>
#include "GM.h"
#include "Double.h"
#include "LogFunction.h"
#include "Vec.h"
#include "Heap.h"
#include "Alg.h"
#include "SolverTypes.h"
#include "myRandom.h"
struct BE
{
	LogDouble log_pe;
	BE(vector<Variable*>& variables, vector<Function*>& functions, vector<int>& order);
	
};

struct BESample
{
	LogDouble log_pe;
	BESample(vector<Variable*>& variables, vector<Function*>& functions, vector<int>& order, myRandom& random);
};
struct BESAT
{
	double log_pe;
	BESAT(vector<Variable*>& all_variables,vector<vector<Lit> >& clauses, vector<int>& order);
	
};

struct BESampleSAT
{
	BESampleSAT(vector<Variable*>& all_variables, vector<vector<Lit> >& clauses, vector<int>& order, myRandom& random);
};

struct BucketProp
{
	LogDouble log_pe;
	vector<Function> marginals;
	BucketProp(vector<Variable*>& variables, vector<Function*>& functions, vector<int>& order);
};
#endif
