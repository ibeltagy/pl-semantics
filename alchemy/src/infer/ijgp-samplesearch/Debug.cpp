/////////////////
// OS Includes
#include <cstdlib>
#include <iostream>

using namespace std;

//////////////
// Includes
#include "Debug.h"

/////////////
// Defines
namespace ss{		    
///////////////////////////
// Static Initialization
Debug Debug::xDebugger;

//////////////////////////////////////////////////////////////////////////////////////////////////
// Public Methods

Debug::Debug()
{

  /*
#ifdef MEM_DEBUG
  cout << "c Memory debugging is ON (output appears in memtrace.log after termination)." << endl;
  Memtrace::init();
#endif
*/
}

Debug::~Debug()
{
#ifdef MEM_DEBUG
  Memtrace::term();
#endif
}

/*static*/ void Debug::vErrorReport(char* aError_) 
{
  // Report an error and halt program execution

  cout << "Fatal error: " << aError_ << endl;
  exit(1);
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// Protected Methods

//////////////////////////////////////////////////////////////////////////////////////////////////
// Private Methods


}
