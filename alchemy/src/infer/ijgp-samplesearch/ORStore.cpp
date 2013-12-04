#include "ORStore.h"

// Effects :	
//	(a) Next pointer is incremented
//	(b) the assignment is added to the store				
void ORStore::AddAssignment(int var, int val)
{
	assert(sampling_order[next_var]==var);
	next_var++;
	if (next_var == (int) sampling_order.size()){
		next_var=0;
	}
	assert(storage[next_pointer+val].v == DONTKNOW || storage[next_pointer+val].v == CONSISTENT);
	storage[next_pointer+val].v=CONSISTENT;
	if (var == sampling_order[sampling_order.size()-1]){
		assert(next_var ==0);
		storage[next_pointer+val].child_loc=0;
		next_pointer=0;
	}
	else {
	  // Update the child pointer if it is not there
		if (storage[next_pointer+val].child_loc == INVALID_VALUE){
		  storage[next_pointer+val].child_loc=(int)storage.size();
		  int new_size=(int)storage.size()+gm.variables[sampling_order[next_var]]->domain_size();
		  storage.resize(new_size);
		}
		next_pointer=storage[next_pointer+val].child_loc;
	}
}

void ORStore::AddNoGood(int var, int val)
{
	assert(sampling_order[next_var]==var);
	assert(storage[next_pointer+val].v == DONTKNOW || storage[next_pointer+val].v==INCONSISTENT);
	storage[next_pointer+val].v=INCONSISTENT;
}

void ORStore::AddGood(int var, int val)
{
	assert(sampling_order[next_var]==var);
	assert(storage[next_pointer+val].v == DONTKNOW || storage[next_pointer+val].v==CONSISTENT);
	storage[next_pointer+val].v=CONSISTENT;
}
ORSTORENODE_VAL ORStore::check(int var, int val,bool to_increment)
{
	assert(sampling_order[next_var]==var);
	ORSTORENODE_VAL ret_value=storage[next_pointer+val].v;
	if (to_increment){
		next_var++;
		if (next_var == (int) sampling_order.size()){
			next_var=0;
		}
		assert(storage[next_pointer+val].v==CONSISTENT);
		assert(storage[next_pointer+val].child_loc != INVALID_VALUE);
		next_pointer=storage[next_pointer+val].child_loc;
	}
	return ret_value;
}
