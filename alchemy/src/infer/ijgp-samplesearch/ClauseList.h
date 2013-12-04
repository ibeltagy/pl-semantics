#ifndef ClauseList_h
#define ClauseList_h

//////////////////////////////////////////////////////////////////////////////////////////////////
// ClauseList

/////////////////
// OS Includes
#include <assert.h>

//////////////
// Includes
#include "LightweightTypes.h"

/////////////
// Defines
#define INITIAL_SIZE 4

////////////////////////
// Class Declarations
class RClause;
class SATSolver;

class ClauseList {
public:
  ClauseList() 
    : _iClauseCount(0), _iMaxSize(INITIAL_SIZE) { _aClause = new RClause*[_iMaxSize];}
  ClauseList(int iInitialSize_) 
    : _iClauseCount(0), _iMaxSize(iInitialSize_) {_aClause = new RClause*[_iMaxSize];}
  ~ClauseList() {delete [] _aClause;}
  inline void vAddClause(RClause*);
  inline void vDeleteClause(RClause*);
  void vRemoveDeletedClauses();
  void vRemoveRequiredClauses();
  void vDestroyDeletedClauses();
  inline RClause* pClause(const int iWhich_) const;
  int iClauseCount() const {return _iClauseCount;}
  void vSortClausesByLength();

  // For fast iteration...
  RClause** pEntry(const int iWhich_) const { return _aClause + iWhich_; }
  RClause** pLastEntry() const { return _aClause + _iClauseCount; }

  void vClear() {_iClauseCount = 0;}
  void vDestroy();

protected:
  void _vExpand();

  int _iClauseCount;
  RClause** _aClause;
  int _iMaxSize;
};


//////////////////////////////////////////////////////////////////////////////////////////////////
// Class Definitions

//////////////////////////////////////////////////////////////////////////////////////////////////
// Inlines

inline RClause* ClauseList::pClause(const int iWhich_) const 
{
  assert(iWhich_>=0);
  assert(iWhich_<_iClauseCount);
  return _aClause[iWhich_];
}

inline void ClauseList::vAddClause(RClause* pAddMe_)
{
  // We assume there is no need to update iAlmostReady and iNotQuiteReady
  // because instantiated counts of the variables will either be zero or ALL
  if (_iMaxSize == _iClauseCount) {
    _vExpand();
  }
  _aClause[_iClauseCount++] = pAddMe_;
}

inline void ClauseList::vDeleteClause(RClause* pDeleteMe_) 
{
  // Assumes the clause is in the list, and probably resides somewhere near the end.
  for (RClause** pStart = &_aClause[_iClauseCount-1]; ; pStart--) {
    if (*pStart == pDeleteMe_) {
      *pStart = _aClause[--_iClauseCount];
      return;
    }
  }
}

#endif // ClauseList_h


