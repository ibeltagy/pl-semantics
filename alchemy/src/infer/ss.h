
#ifndef SS_H_
#define SS_H_

#include "MAIN.h"
#include <stdlib.h>
#include <stdio.h>
#include "infer.h"

const int ssdebug = false;

extern char* aresultsFile;

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
/*
    ofstream outFile;
    outFile.open ("mln.uai");

    //outFile <<"MARKOV"<<endl;
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
	double weight = gndClause->getWt();
	double expNegWeight = exp(-weight);
	if(gndClause->isHardClause())
		expNegWeight = 0;
	outFile <<gndClause->getNumGroundPredicates()<<" " ;
	for (int j = 0; j < gndClause->getNumGroundPredicates(); j++)
	{
		//outFile <<abs(gndClause->getGroundPredicateIndex(j))-1 << " ";
		outFile <<gndClause->getGroundPredicateIndex(j)<< " ";
	}
	outFile <<expNegWeight<<endl;
	//cout<<endl;
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
*/
    //std::system("cat mln.uai");
    //std::ostringstream command;
    //command << "./ijgp-samplesearch mln.uai empty.evd " << this->params.maxSeconds <<" PR";
    //std::system(command.str().c_str());
    std::ostringstream t;
    t << this->params.maxSeconds;
    cout << "calling SS"<<endl;
    char ** argv  = new char*[5];
    for(int i = 0;i<5;i++)
	argv[i] = new char[50];

    strcpy (argv[0],"./ijgp-samplesearch");
    strcpy (argv[1],aresultsFile);
    strcpy (argv[2],"empty.evd");
    strcpy (argv[3],t.str().c_str());
    strcpy (argv[4],"PR");

    //cout <<argv[0]<<endl;
    //cout <<argv[1]<<endl;
    //cout <<argv[2]<<endl;
    //cout <<argv[3]<<endl;
    //cout <<argv[4]<<endl;

    ss::MAIN (state_, 5, argv);
    //cout << aresultsFile << endl;

    //std::system("cat mln.uai.PR");
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
