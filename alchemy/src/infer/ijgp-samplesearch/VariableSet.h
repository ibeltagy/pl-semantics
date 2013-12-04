#ifndef VariableSet_h
#define VariableSet_h

//////////////////////////////////////////////////////////////////////////////////////////////////
// This class allows us to manipulate a set of VariableID's. 

/////////////////
// OS Includes

//////////////
// Includes
#include "LightweightTypes.h"
#include "VariableList.h"

/////////////
// Defines

////////////////////////
// Class Declarations

//////////////////////////////////////////////////////////////////////////////////////////////////
// Class Definitions
class VariableSet : public VariableList {
public:
  VariableSet(int iMaxVars_) : VariableList(iMaxVars_), 
    _aHasVariable(new VariableID[iMaxVars_]) 
  { for (int i=0; i<iMaxVars_; i++) _aHasVariable[i]=-1;}
  ~VariableSet() {delete [] _aHasVariable;}

  inline void vClear();
  inline void vRemoveVariable(VariableID eID_);
  inline void vRemoveVariableCheck(VariableID eID_);
  inline void vAddVariableNoCheck(VariableID eID_);
  inline void vAddVariable(VariableID eID_);
  inline void vAppendVariables(const VariableList&);
  inline void vAppendNoCheck(const VariableList&);
  inline void vRemove(const VariableList& xThese_);
  inline boolean bAddVariable(VariableID eID_);
  boolean bHasVariable(VariableID eID_) const {return (_aHasVariable[eID_]!=-1);}

private:
  VariableID* _aHasVariable;
};

//////////////////////////////////////////////////////////////////////////////////////////////////
// Inlines

inline void VariableSet::vRemove(const VariableList& xThese_) 
{
  for (int i=0; i<xThese_.iCount(); i++) {
    vRemoveVariableCheck(xThese_.iVariable(i));
  }
}

inline void VariableSet::vAppendVariables(const VariableList& xMe_) 
{
  for (int i=0; i<xMe_.iCount(); i++) {
    vAddVariable(xMe_.iVariable(i));
  }
}

inline void VariableSet::vAppendNoCheck(const VariableList& xMe_) 
{
  for (int i=0; i<xMe_.iCount(); i++) {
    vAddVariableNoCheck(xMe_.iVariable(i));
  }
}

inline void VariableSet::vClear()
{
  for (int i=0; i < _iVariableCount; i++) {
    assert(_aHasVariable[_aVariableList[i]] != -1);
    _aHasVariable[_aVariableList[i]] = -1;
  }
  _iVariableCount = 0;
}

inline void VariableSet::vRemoveVariable(VariableID eID_)
{
  assert(eID_ >= 0);
  assert(bHasVariable(eID_));
  _aVariableList[_aHasVariable[eID_]] = _aVariableList[--_iVariableCount];
  _aHasVariable[_aVariableList[_iVariableCount]] = _aHasVariable[eID_];
  _aHasVariable[eID_] = -1;
}

inline void VariableSet::vRemoveVariableCheck(VariableID eID_)
{
  assert(eID_ >= 0);
  if (_aHasVariable[eID_] != -1) {
    _aVariableList[_aHasVariable[eID_]] = _aVariableList[--_iVariableCount];
    _aHasVariable[_aVariableList[_iVariableCount]] = _aHasVariable[eID_];
    _aHasVariable[eID_] = -1;
  }
}

inline void VariableSet::vAddVariableNoCheck(VariableID eID_)
{
  // Add a variable without checking for duplicates.
  assert(eID_ >= 0);
  assert(_aHasVariable[eID_] == -1);
  _aHasVariable[eID_] = _iVariableCount;
  _aVariableList[_iVariableCount++] = eID_;
}

inline void VariableSet::vAddVariable(VariableID eID_)
{
  if (_aHasVariable[eID_] == -1) {
    vAddVariableNoCheck(eID_);
  }
}

inline boolean VariableSet::bAddVariable(VariableID eID_)
{
  if (_aHasVariable[eID_] == -1) {
    vAddVariableNoCheck(eID_);
    return 1;
  }
  return 0;
}

#endif // VariableSet_h

