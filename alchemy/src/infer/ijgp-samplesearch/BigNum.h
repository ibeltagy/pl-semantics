#ifndef SS_BigNum_h
#define SS_BigNum_h

//////////////////////////////////////////////////////////////////////////////////////////////////
// BigNum.h: Encapsulates bignum functionality. This implementation uses the Gnu "gmp" package.

/////////////////
// OS Includes
#include "LightweightTypes.h"
#include <gmp.h>

//////////////
// Includes

/////////////
// Defines

////////////////////////
// Class Declarations

//////////////////////////////////////////////////////////////////////////////////////////////////
// Class Definitions
using namespace std;

namespace ss{

struct BigNum {
public:
  BigNum() { mpz_init(_xCount); }
  BigNum(long int iInitWithMe_) { mpz_init_set_si(_xCount, iInitWithMe_); }
  BigNum(const BigNum& xBigNum_) { mpz_init_set(_xCount, xBigNum_._xCount); }
  ~BigNum() { mpz_clear(_xCount); }
  
  void operator +=(const BigNum& xMe_) { mpz_add(_xCount,_xCount,xMe_._xCount); }
  void operator *=(const BigNum& xMe_) { mpz_mul(_xCount,_xCount,xMe_._xCount); }
  void operator *=(unsigned long iCount_) { mpz_mul_ui(_xCount,_xCount,iCount_); }

  boolean operator >(const BigNum& xMe_) { return (mpz_cmp(_xCount, xMe_._xCount)) == 1 ? 1 : 0; }

  void vSet(long int iTo_) { mpz_set_si(_xCount, iTo_); }
  void vSet(const BigNum& xMe_) { mpz_set(_xCount, xMe_._xCount); }

  char* aToString() {
    // Caller responsible for deleting the returned string.
    int buf_size = mpz_sizeinbase (_xCount, 10) + 2;
    char* buffer = new char[buf_size];
    mpz_get_str(buffer, 10, _xCount);
    return buffer;
  }

private:
  mpz_t _xCount;
};

//////////////////////////////////////////////////////////////////////////////////////////////////
// Inlines
}
#endif //BigNum_h
