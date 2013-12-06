#ifndef SS_DRC_H_
#define SS_DRC_H_

#include "GM.h"

namespace ss{

class DRC
{
protected:
	GM& gm;
	vector<vector<Function> > buckets;
public:
	DRC(GM& gm_, int i_bound_,vector<int>& order);
	bool isConsistent(int var, int value);
	void domainConsistency(int var,vector<bool>& new_domain);
	void reduceFunction(Function* function);

};
}
#endif
