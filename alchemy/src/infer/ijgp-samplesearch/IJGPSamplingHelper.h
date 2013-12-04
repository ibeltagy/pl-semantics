#ifndef IJGPSamplingHelper_H_
#define IJGPSamplingHelper_H_

#include "GM.h"
#include "JG.h"
#include "SF.h"
#include "CPT.h"
#include "DRC.h"
typedef enum { POSITIVE_SAMPLER, ZERO_SAMPLER} SAMPLER_TYPE;
// Abstract class Ordered Sampler
class OS
{
public:
	OS() { }
	virtual ~OS(){ }
	virtual void getSample(const int& variable, int& value, Double& weight,myRandom& random){ }
	virtual Function& getFunction(int variable){ }
};
// Positive Ordered sampler
class POS: public OS
{
private:
	vector<SF> sampling_functions;
public:
	POS(GM* gm_, vector<int>& order,JG* jg_);
	~POS(){ }
	void getSample(const int& variable, int& value, Double& weight,myRandom& random);
	Function& getFunction(int variable);
};
// Positive Ordered Sampler with parameter p
class POSP: public OS
{
private:
	int p;
	vector<JGNode*> var_nodes;
public:
	POSP(GM* gm_, vector<int>& order,JG* jg_,int p_);
	~POSP(){ }
	void getSample(const int& variable, int& value, Double& weight,myRandom& random);
	Function& getFunction(int variable);

};
// Constraint propagation based ordered Sampler
class COS: public OS
{
private:
	vector<CPT> sampling_functions;
	DRC* cp_algo;
	GM* csp;
public:
	COS(GM* gm_, vector<int>& order,JG* jg_);
	~COS(){ delete(csp);delete(cp_algo);}
	void getSample(const int& variable, int& value, Double& weight,myRandom& random);
	Function& getFunction(int variable);
};
// Constraint propagation based ordered Sampler with paramter p
class COSP: public OS
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
	COSP(GM* gm_, vector<int>& order, JG* jg_,int p_);
	~COSP(){ delete(csp);delete(cp_algo);}
	void getSample(const int& variable, int& value, Double& weight,myRandom& random);
	Function& getFunction(int variable);
};

class IJGPSamplingHelper
{
protected:
	GM* gm;
	JG* jg;
	int p;
	OS* sampler;
	// Pointer to the join graph nodes for each variable
public:
	IJGPSamplingHelper(){}
	IJGPSamplingHelper(GM* gm_, JG* jg_, int p_, vector<int>& order, SAMPLER_TYPE type=ZERO_SAMPLER);
	~IJGPSamplingHelper() {delete(sampler);}
	Function& getFunction(int variable){ return sampler->getFunction(variable);}
	void getSample(const int& variable, int& value, Double& weight,myRandom& random)
	{
		sampler->getSample(variable,value,weight,random);
	}
};
#endif
