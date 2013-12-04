#ifndef BigNum_fake_h
#define BigNum_fake_h

//////////////////////////////////////////////////////////////////////////////////////////////////
// BigNum_fake.h: Implements BigNum interface using a long int. Obviously, this has a high
// potential to overflow. I didn't put in any checks for this either. Use it only if you
// can't get the Gnu GMP package (or some other bignum package) to work and you just want to
// get it up and running.

/////////////////
// OS Includes
#include <stdio.h>

//////////////
// Includes
#include "LightweightTypes.h"

/////////////
// Defines

////////////////////////
// Class Declarations

//////////////////////////////////////////////////////////////////////////////////////////////////
// Class Definitions

class BigNum {
public:
  BigNum() { }
  BigNum(long int iInitWithMe_) { _iCount = iInitWithMe_; }
  BigNum(const BigNum& xInit_) { _iCount = xInit_._iCount; }
  
  void operator +=(const BigNum& xMe_) { _iCount += xMe_._iCount; }
  void operator *=(const BigNum& xMe_) { _iCount *= xMe_._iCount; }
  void operator *=(unsigned long iCount_) { _iCount *= iCount_; }

  boolean operator >(const BigNum& xMe_) { return (_iCount > xMe_._iCount); }

  void vSet(long int iTo_) { _iCount = iTo_; }
  void vSet(const BigNum& xMe_) { _iCount = xMe_._iCount; }

  char* aToString() {
    // Caller responsible for deleting the returned string.
    char* buffer = new char[20];
    sprintf(buffer, "%ld", _iCount);
    return buffer;
  }

private:
  long int _iCount;
};

//////////////////////////////////////////////////////////////////////////////////////////////////
// Inlines

#endif //BigNum_fake_h
