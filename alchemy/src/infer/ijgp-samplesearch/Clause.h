#ifndef Clause_h
#define Clause_h

///////////////////////////////////////////////////////////////////////////////
// Clause: Represents an individual SAT clause.

/////////////////
// OS Includes
#include <assert.h>

//////////////
// Includes
#include "LightweightTypes.h"
#include "VariableSet.h"

/////////////
// Defines

////////////////////////
// Class Declarations

//////////////////////
// Class Definitions

class RClause {
public:
  RClause() : _aConstrainedLiteral(0), _iVariableCount(0), _iPermaCount(0) {}
  inline RClause(VariableList& rConstrainedVariables_, 
		VariableList& rNegatedVariables_, 
		int iUseless_);
  inline RClause(VariableList& rConstrainedVariables_, VariableList& rNegatedVariables_);
  inline RClause(VariableList& rConstrainedVariables_, 
		VariableSet& rNegatedVariables_, 
		VariableSet& rPermaVariables_);
  ~RClause() {delete [] _aConstrainedLiteral;}

  inline VariableID eConstrainedVariable(int iWhich_) const
    {return iGetVariable(_aConstrainedLiteral[iWhich_]);}

  inline LiteralID eConstrainedLiteral(int iWhich_) const
    {return _aConstrainedLiteral[iWhich_];}

  inline LiteralID iIsNegated(int iWhich_) const
    {return RClause::iIsLiteralNegated(_aConstrainedLiteral[iWhich_]);}

  boolean bLearned() const { return _bLearned;}

  void vFudge() { _iSatisfyingCount = 1; _iUnlabeledCount = 1; }

  inline boolean bHasVariable(VariableID eVar_) const;
  short int bIsSatisfied() const {return _iSatisfyingCount;}
  int iVariableCount() const {return _iVariableCount;}
  int iPermaCount() const {return _iPermaCount;}
  int iWorkingLength() const { return _iUnlabeledCount; }
  void vMakeLearned() {_bLearned = 1;}  
  // ^^ Implies clause is redundant (can be deleted from instance 
  // without affecting its solubility status)
  void vMakeRequired() { _bLearned = 0; } 

  // For fast unit propagations:
  inline short int iReduce(); 
  inline short int iExpand();

  void vFlagAsDeleted() {_bLearned = 2; _iSatisfyingCount = 9999; } // hack
  boolean bIsDeleted() const {return (_bLearned == 2);} // hack
  boolean bIsTemporary() const { return (_iPermaCount!=_iVariableCount); }

  inline short int iFilter();
  inline void vUnfilter();
  inline void vMakeSatisfied();
  inline void vMakeUnsatisfied();

  void vReset() { _iSatisfyingCount = 0; _iUnlabeledCount = _iVariableCount; }
  void vOutput() const;
  void vSortVariableList();
  int iCompare(const RClause& xWithMe_) const;
  boolean bIsEqual(const RClause& xWithMe_) const;

  static LiteralID iMakeNegatedLiteral(VariableID eFrom_) {return eFrom_ | 0x80000000;}
  static LiteralID iIsLiteralNegated(LiteralID eLit_) {return eLit_ & 0x80000000;}
  static VariableID iGetVariable(LiteralID eLit_) {return eLit_ & 0x7FFFFFFF;}

protected:
  inline void _vInitLiterals(VariableList& rVars, VariableList& rNeg_);

  short int _iSatisfyingCount;
  short int _iUnlabeledCount;
  short int _iPermaCount;
  LiteralID* _aConstrainedLiteral;
  const short int _iVariableCount;
  boolean _bLearned;
};

//////////////////////////////////////////////////////////////////////////////////////////////////
// Inlines

inline void RClause::_vInitLiterals(VariableList& rPositiveVariables_, 
				   VariableList& rNegativeVariables_) 
{
  int iStart = 0;
  int i;
  for (i=0; i < rPositiveVariables_.iCount(); i++) {
    _aConstrainedLiteral[iStart++] = rPositiveVariables_.iVariable(i);
  }
  for (i=0; i < rNegativeVariables_.iCount(); i++) {
    _aConstrainedLiteral[iStart++] = 
      RClause::iMakeNegatedLiteral(rNegativeVariables_.iVariable(i));
  }
}

RClause::RClause(VariableList& rPositiveVariables_, VariableList& rNegativeVariables_, int)
: _iVariableCount(rPositiveVariables_.iCount() + rNegativeVariables_.iCount()),
  _iSatisfyingCount(0),
  _bLearned(0)
{
  _aConstrainedLiteral = new LiteralID[_iVariableCount];
  _iPermaCount = _iVariableCount;
  _iUnlabeledCount = _iVariableCount;
  _vInitLiterals(rPositiveVariables_, rNegativeVariables_);
  vSortVariableList();
}

inline RClause::RClause(VariableList& rPositiveVariables_, VariableList& rNegativeVariables_)
: _iVariableCount(rPositiveVariables_.iCount() + rNegativeVariables_.iCount()),
  _iUnlabeledCount(0),
  _iSatisfyingCount(0),
  _bLearned(1)
{
  // This constructor is used by the learning mechanism
  _aConstrainedLiteral = new LiteralID[_iVariableCount];
  _iPermaCount = _iVariableCount;
  _vInitLiterals(rPositiveVariables_, rNegativeVariables_);
  vSortVariableList();
}

inline RClause::RClause(VariableList& rPositiveVariables_, 
		      VariableSet& rNegativeVariables_, 
		      VariableSet& rPermaVariables_)
: _iVariableCount(rPositiveVariables_.iCount() + rNegativeVariables_.iCount()),
  _iPermaCount(rPermaVariables_.iCount()),
  _iSatisfyingCount(0),
  _bLearned(1),
  _iUnlabeledCount(0)
{
  // This constructor is used by the relevance-bounded learning mechanism
  _aConstrainedLiteral = new LiteralID[_iVariableCount];
  int i;
  for (i=0; i< _iPermaCount; i++) {
    VariableID eWorkID = rPermaVariables_.iVariable(i);
    if (rNegativeVariables_.bHasVariable(eWorkID)) {
      _aConstrainedLiteral[i] = iMakeNegatedLiteral(eWorkID);
    }
    else {
      _aConstrainedLiteral[i] = eWorkID;
    }
  }
  int iStart = _iPermaCount;
  for (i=0; i < rPositiveVariables_.iCount(); i++) {
    VariableID eWorkID = rPositiveVariables_.iVariable(i);
    if (!rPermaVariables_.bHasVariable(eWorkID)) {
      _aConstrainedLiteral[iStart++] = eWorkID;
    }
  }
  for (i=0; i < rNegativeVariables_.iCount(); i++) {
    VariableID eWorkID = rNegativeVariables_.iVariable(i);
    if (!rPermaVariables_.bHasVariable(eWorkID)) {
      _aConstrainedLiteral[iStart++] = RClause::iMakeNegatedLiteral(eWorkID);
    }
  }
  assert(iStart == _iVariableCount);
}

inline boolean RClause::bHasVariable(VariableID eVar_) const
{
  for (int i=0; i<_iVariableCount; i++) {
    if (eConstrainedVariable(i) == eVar_)
      return 1;
  }
  return 0;
}

inline short int RClause::iReduce()
{
  assert(_iSatisfyingCount == 0);
  --_iUnlabeledCount;
  assert(_iUnlabeledCount >= 0);
  return _iUnlabeledCount;
}

inline short int RClause::iExpand()
{
  if (_iSatisfyingCount == 0) {
    _iUnlabeledCount++;
    assert(_iUnlabeledCount <= _iVariableCount);
    return _iUnlabeledCount;
  }
  return 0;
}

inline void RClause::vMakeSatisfied()
{
  assert(_iUnlabeledCount);
  _iSatisfyingCount++;
}

inline void RClause::vMakeUnsatisfied()
{
  assert(_iSatisfyingCount);
  _iSatisfyingCount--;
}

#endif // Clause_h
