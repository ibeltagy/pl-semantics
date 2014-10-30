#ifndef SS_AOIJGPSamplingHelper_H_
#define SS_AOIJGPSamplingHelper_H_

#include "GM.h"
#include "JG.h"
#include "SF.h"
#include "CPT.h"
#include "DRC.h"
#include "AOTree.h"
#include "IJGPSamplingHelper.h"

namespace ss{

//typedef enum { POSITIVE_SAMPLER, ZERO_SAMPLER} SAMPLER_TYPE;
// Abstract class Ordered Sampler
class AO_OS
{
public:
	AO_OS() { }
	virtual ~AO_OS(){ }
	virtual void getSample(const int& variable, int& value, Double& weight,myRandom& random){ }
	virtual Function& getFunction(int variable){ }
};
// Positive Ordered sampler
class AO_POS: public AO_OS
{
private:
	vector<SF> sampling_functions;
public:
	AO_POS(GM* gm_, vector<int>& order,JG* jg_,vector<vector<Variable*> >& ancestors);
	~AO_POS(){ }
	void getSample(const int& variable, int& value, Double& weight,myRandom& random);
	Function& getFunction(int variable);
};
// Positive Ordered Sampler with parameter p
class AO_POSP: public AO_OS
{
private:
	int p;
	vector<JGNode*> var_nodes;
public:
	AO_POSP(GM* gm_, vector<int>& order,JG* jg_,vector<vector<Variable*> >& ancestors,int p_);
	~AO_POSP(){ }
	void getSample(const int& variable, int& value, Double& weight,myRandom& random);
	Function& getFunction(int variable);

};
// Constraint propagation based ordered Sampler
class AO_COS: public AO_OS
{
private:
	vector<CPT> sampling_functions;
	DRC* cp_algo;
	GM* csp;
public:
	AO_COS(GM* gm_, vector<int>& order,JG* jg_,vector<vector<Variable*> >& ancestors);
	~AO_COS(){ delete(csp);delete(cp_algo);}
	void getSample(const int& variable, int& value, Double& weight,myRandom& random);
	Function& getFunction(int variable);
};
// Constraint propagation based ordered Sampler with paramter p
class AO_COSP: public AO_OS
{
private:
	// The sampling functions are different here. Once a sample is taken the sampling functions will just contain
	// the marginal from which the sample was taken.
	vector<CPT> sampling_functions;
	// The other required arguments
	GM* gm;
	vector<int> order;
	DRC *cp_algo;
	GM* csp;
	JG* jg;
	int p;
	// run_ijgp[i]=True if for variable at position i, you have to run IJGP else it is false
	vector<bool> run_ijgp;
	vector<int> mapped_order;
	vector<JGNode*> cpt_jg_nodes;
	//Build sampling functions
	void buildSF(int curr_variable);
public:
	AO_COSP(GM* gm_, vector<int>& order, JG* jg_,vector<vector<Variable*> >& ancestors,int p_);
	~AO_COSP(){ delete(csp);delete(cp_algo);}
	void getSample(const int& variable, int& value, Double& weight,myRandom& random);
	Function& getFunction(int variable);
};

class AO_IJGPSamplingHelper
{
protected:
	GM* gm;
	JG* jg;
	int p;
	AO_OS* sampler;
	vector<vector<Variable*> > ancestors;
	// Pointer to the join graph nodes for each variable
public:
	AO_IJGPSamplingHelper(){}
	AO_IJGPSamplingHelper(GM* gm_, JG* jg_, int p_, vector<int>& order, PseudoTree& pseudo_tree,SAMPLER_TYPE type=POSITIVE_SAMPLER);
	Function& getFunction(int variable){ return sampler->getFunction(variable);}
	void getSample(const int& variable, int& value, Double& weight,myRandom& random)
	{
		sampler->getSample(variable,value,weight,random);
	}
};
}
#endif
