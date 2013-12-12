#include "Variable.h"
namespace ss{
bool less_than_comparator_variable(const Variable* a, const Variable* b)
{
	return (a->id()<b->id());
}
}
