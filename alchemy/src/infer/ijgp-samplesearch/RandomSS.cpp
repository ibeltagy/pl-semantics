/////////////////
// OS Includes

//////////////
// Includes
#include "RandomSS.h"

/////////////
// Defines
namespace ss{
///////////////////////////////////////////////////////////////////////////////
// Public Methods
void Random::vInitRandom(int iSeed_)
{
  assert(iSeed_ > 0);
  sgenrand(iSeed_);
}

///////////////////////////////////////////////////////////////////////////////
// Protected Methods

///////////////////////////////////////////////////////////////////////////////
// Private Methods
}
