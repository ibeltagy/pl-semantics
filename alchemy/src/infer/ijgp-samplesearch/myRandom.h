#ifndef SS_MY_RANDOM_H_
#define SS_MY_RANDOM_H_

#include "randomc.h"
using namespace std;

namespace ss{

struct myRandom {
	int seed;
	CRandomMersenne RanGen;
	// choose one of the random number generators:

	myRandom() {
		//seed=time(NULL);
		seed=1000;
		RanGen = CRandomMersenne(seed);
	}
	void setSeed(int seed_)
	{
		//seed=time(NULL);
		seed=1000;
		RanGen = CRandomMersenne(seed);
	}
	double getDouble() {
		return RanGen.Random();
	}
	int getInt()
	{
		return RanGen.BRandom();
	}
	int getInt(int max_value) {
		RanGen.IRandomX(0,max_value-1);
	}
};
}
#endif
