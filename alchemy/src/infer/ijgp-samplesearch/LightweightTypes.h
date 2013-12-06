#ifndef SS_LightweightTypes_h
#define SS_LightweightTypes_h

///////////////////////////////////////////////////////////////////////////////////////////////////
// This file contains typedefs, enums, and definitions of their special values.

#include "Debug.h"

namespace ss{

typedef signed char DomainValue;
// The NON_VALUE is a special domain value. Domain values are usually positive integers.
#define NON_VALUE   -1             

typedef long int VariableID;    
typedef long int LiteralID;
typedef char boolean;
}
#endif // LightweightTypes_h
