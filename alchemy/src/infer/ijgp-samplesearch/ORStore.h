#ifndef ORSTORE_H_
#define ORSTORE_H_

#include <vector>
#include "GM.h"
using namespace std;
typedef enum {CONSISTENT, INCONSISTENT, DONTKNOW} ORSTORENODE_VAL;
struct ORStoreNode
{
	ORSTORENODE_VAL v;
	int child_loc;
	ORStoreNode(){
		v=DONTKNOW;
		child_loc=INVALID_VALUE;
	}
};

struct ORStore
{
	vector<ORStoreNode> storage;
	int next_pointer;
	int next_var;
	vector<int>& sampling_order;
	GM& gm;
	ORStore(GM& gm_, vector<int>& sampling_order_): gm(gm_),sampling_order(sampling_order_){
		next_pointer=0;
		assert(!sampling_order.empty());
		storage=vector<ORStoreNode>(gm.variables[sampling_order[0]]->domain_size());
		next_var=0;
	}
	// The following will add a nogood to the problem without modifying the position of next_pointer
	void AddNoGood(int var, int val);
	// The following will add a good to the problem without modifying the position of next_pointer
	void AddGood(int var, int val);
	// The following will add an assignment and update the next_pointer
	void AddAssignment(int var, int val);
	// Check the value of the node and increment the next_pointer if increment is true
	ORSTORENODE_VAL check(int var, int val,bool to_increment=false);
  	void reset(){
    	next_pointer=0;
    	storage=vector<ORStoreNode>(gm.variables[sampling_order[0]]->domain_size());
    	next_var=0;
	}
    
  
private:
	void resetPointer();
};
#endif
