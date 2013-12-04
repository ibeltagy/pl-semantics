#ifndef VariableList_h
#define VariableList_h

//////////////////////////////////////////////////////////////////////////////////////////////////
// A simple array of VariableID's

/////////////////
// OS Includes

//////////////
// Includes
#include "LightweightTypes.h"

/////////////
// Defines

////////////////////////
// Class Declarations

//////////////////////////////////////////////////////////////////////////////////////////////////
// Class Definitions
class VariableList {
public:
  inline VariableList(int iMaxVars_);
  ~VariableList() { delete [] _aVariableList; }

  inline void vClear() {_iVariableCount = 0;}
  inline void vAdd(VariableID eID_) {_aVariableList[_iVariableCount++] = eID_;}
  inline void vAppend(const VariableList& xMe_);
  int iCount() const {return _iVariableCount;}
  void vOutput() const;
  inline void vRemoveAtIndex(int iWhich_);
  VariableID eTop() { if (_iVariableCount) { return _aVariableList[_iVariableCount-1]; } else return -1; }
  VariableID ePop() { assert(_iVariableCount); return _aVariableList[--_iVariableCount]; }
  VariableID iVariable(int iWhich_) const 
  {assert(iWhich_ < _iVariableCount); return _aVariableList[iWhich_];}

protected:
  VariableID* _aVariableList;
  int _iVariableCount;
};

//////////////////////////////////////////////////////////////////////////////////////////////////
// Inlines

inline VariableList::VariableList(int iMaxVars_) 
: _iVariableCount(0), 
  _aVariableList(new VariableID[iMaxVars_]) 
{}

inline void VariableList::vRemoveAtIndex(int iWhich_) 
{ 
  _aVariableList[iWhich_] = _aVariableList[--_iVariableCount];
}

inline void VariableList::vAppend(const VariableList& xMe_) 
{
  for (int i=0; i<xMe_.iCount(); i++) {
    vAdd(xMe_.iVariable(i));
  }
}
#endif // VariableList_h

