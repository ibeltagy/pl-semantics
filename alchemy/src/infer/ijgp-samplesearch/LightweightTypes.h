#ifndef LightweightTypes_h
#define LightweightTypes_h

///////////////////////////////////////////////////////////////////////////////////////////////////
// This file contains typedefs, enums, and definitions of their special values.

#include "Debug.h"
typedef signed char DomainValue;
// The NON_VALUE is a special domain value. Domain values are usually positive integers.
#define NON_VALUE   -1             

typedef long int VariableID;    
typedef long int LiteralID;
typedef char boolean;

#endif // LightweightTypes_h
