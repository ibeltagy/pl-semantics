
#ifndef SampleSearch_H_
#define SampleSearch_H_

#include "mcmc.h"
#include "mcsatparams.h"

const int ssdebug = true;

/**
 * Calling SampleSearch
 */
class SampleSearch : public MCMC
{
 public:

  SampleSearch(VariableState* state, long int seed, const bool& trackClauseTrueCnts,
        MCSatParams* mcsatParams,
        Array<Array<Predicate* >* >* queryFormulas = NULL)
    : MCMC(state, seed, trackClauseTrueCnts, mcsatParams, queryFormulas)
  {
  }

  ~SampleSearch()
  {
  }
  
  void init()
  {

  }

  void infer()
  {
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
	outFile <<(gndClause->getNumGroundPredicates()<<1)<<endl;
	for (int j = 0; j < (gndClause->getNumGroundPredicates()<<1); j++)
	{
		outFile <<"1 ";
	}
	outFile << endl;
    }
    outFile.close(); 

    std::system("cat mln.uai");
    std::system("./ijgp-samplesearch mln.uai empty.evd  1000000 PR");
    std::system("cat mln.uai.PR");
  }

  double getProbability(GroundPredicate* const& gndPred)
  {
	return 0;
  }

};

#endif /*SampleSearch_H_*/
