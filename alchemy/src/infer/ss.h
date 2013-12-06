
#ifndef SS_H_
#define SS_H_

#include "MAIN.h"

const int ssdebug = true;

struct SampleSearchParams
{
	int maxSeconds;
	int iBound;
	int rbBound;
	int numItr;
};

/**
 * Calling SampleSearch
 */

class SampleSearchProxy: public Inference
{
 public:

 SampleSearchProxy(VariableState* state, long int seed, const bool& trackClauseTrueCnts,
        SampleSearchParams* sampleSearchParams,
        Array<Array<Predicate* >* >* queryFormulas = NULL)
    : Inference(state, seed, trackClauseTrueCnts, queryFormulas)
  {
	this->params = (*sampleSearchParams);
	if(this->params.maxSeconds <=0 )
		this->params.maxSeconds = 100;
  }

  ~SampleSearchProxy()
  {
  }
  
  void init()
  {

  }


  void infer()
  {
    ////////////////////////////////
    ofstream outFile;
    outFile.open ("mln.uai");

    outFile <<"MARKOV"<<endl;
    outFile << state_->getNumAtoms() << endl;

    for (int i = 0; i < state_->getNumAtoms(); i++)
    {
	outFile <<"2 ";
    }
    outFile << endl;

    outFile << state_->getNumClauses() << endl;

    for (int i = 0; i < state_->getNumClauses(); i++)
    {
	GroundClause *gndClause = state_->getGndClause(i);
	outFile <<gndClause->getNumGroundPredicates()<<" " ;
	for (int j = 0; j < gndClause->getNumGroundPredicates(); j++)
	{
		outFile <<abs(gndClause->getGroundPredicateIndex(j))-1 << " ";
	}
	outFile << endl;
    }

    for (int i = 0; i < state_->getNumClauses(); i++)
    {
	GroundClause *gndClause = state_->getGndClause(i);
	unsigned long long int numGndPred = gndClause->getNumGroundPredicates();
	assert(numGndPred < 64);
	unsigned long long int tableSize = (1LL<<numGndPred) ;
	double weight = gndClause->getWt();
	double expNegWeight = exp(-weight);
	if(gndClause->isHardClause())
		expNegWeight = 0;
	outFile <<tableSize<<endl;
	for (int j = 0; j < tableSize; j++)
	{
		bool satisfied = false;
		unsigned long long int mask = 1;
		for (int k = 0; k<numGndPred; k++)
		{
			bool isTrue = mask&j;
			int predIndex = gndClause->getGroundPredicateIndex(numGndPred-1-k);
			if( predIndex > 0 && isTrue || predIndex < 0 && !isTrue )
			{
				satisfied = true;
				break;
			}
			mask = mask << 1;
		}
		if(satisfied)
			outFile << "1 ";
		else outFile << expNegWeight <<" ";
	}
	outFile << endl;
    }
    outFile.close(); 

    std::system("cat mln.uai");
    //std::ostringstream command;
    //command << "./ijgp-samplesearch mln.uai empty.evd " << this->params.maxSeconds <<" PR";
    //std::system(command.str().c_str());
    std::ostringstream t;
    t << this->params.maxSeconds;
    const char * argv[] = {"./ijgp-samplesearch", "mln.uai", "empty.evd", t.str().c_str(), "PR"};
    ss::MAIN (5, argv);

    std::system("cat mln.uai.PR");
  }

  void printNetwork(std::ostream& out) 
  {}
  void printProbabilities(std::ostream& out)
  {}
  void getChangedPreds(std::vector<std::basic_string<char> >& a , std::vector<float>& b, std::vector<float>& c, const float& d)
  {}
  void printTruePreds(std::ostream& out)
  {}
  void printTruePredsH(std::ostream& out)
  {}
  double getProbabilityH(GroundPredicate* const& p)
  {return 0;}
  double getProbability(GroundPredicate* const& gndPred)
  {return 0;}

private:
	SampleSearchParams params;
};

#endif /*SS_H_*/
