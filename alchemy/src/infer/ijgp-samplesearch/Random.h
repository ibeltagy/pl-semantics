#ifndef Random_h
#define Random_h

///////////////////////////////////////////////////////////////////////////////
// Initializes random number generation

/////////////////
// OS Includes
#include <stdlib.h>

//////////////
// Includes
#include "LightweightTypes.h"

/////////////
// Defines
void sgenrand(unsigned long seed);
unsigned long genrand();

////////////////////////
// Class Declarations
class Random {
public:
  static void vInitRandom(int);
  static double dRandom1() {return ((double)(genrand())/((double)2147483648L));} // [0-1.0)
  static double dRandom2() {return ((double)(genrand()+1)/((double)2147483648L));}  // (0-1.0]
  static double dRandom3() {return ((double)(genrand())/((double)2147483647L));}  // [0-1.0]
  static double dRandom4() {return ((double)(genrand()+1)/((double)2147483649L));}  // (0-1.0)
  static unsigned int iRandom(unsigned int iMax_) {return ((unsigned int)genrand())%iMax_;}
  static unsigned long lRandom(unsigned long lMax_) {return ((unsigned long)genrand())%lMax_;}
  static boolean bRandom() {return (boolean)(genrand()%2);}
};

///////////////////////////////////////////////////////////////////////////////
// Class Definitions

///////////////////////////////////////////////////////////////////////////////
// Inlines

#endif // Random_h

