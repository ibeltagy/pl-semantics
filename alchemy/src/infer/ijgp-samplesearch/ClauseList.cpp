/////////////////
// OS Includes
#include <memory.h>
#include <stdlib.h>

//////////////
// Includes
#include "ClauseList.h"
#include "Clause.h"

/////////////
// Defines

//////////////////////////////////////////////////////////////////////////////////////////////////
// Public Methods

void ClauseList::vDestroy()
{
  for (int i=0; i<_iClauseCount; i++)
    delete _aClause[i]; 
  _iClauseCount = 0;
}

void ClauseList::vRemoveDeletedClauses()
{
  int i=0; 
  while(i<_iClauseCount) {
    if (_aClause[i]->bIsDeleted()) {
      _aClause[i] = _aClause[--_iClauseCount];
    }
    else {
      i++;
    }
  }
}

void ClauseList::vRemoveRequiredClauses()
{
  int i=0; 
  while(i<_iClauseCount) {
    if (_aClause[i]->bLearned()) {
      _aClause[i] = _aClause[--_iClauseCount];
    }
    else {
      i++;
    }
  }
}

void ClauseList::vDestroyDeletedClauses()
{
  int i=0; 
  while(i<_iClauseCount) {
    if (_aClause[i]->bIsDeleted()) {
      delete _aClause[i];
      _aClause[i] = _aClause[--_iClauseCount];
    }
    else {
      i++;
    }
  }
}

int compare_clauselength(const void* pFirst_, const void* pSecond_) 
{
  if ((*(RClause**)pFirst_)->iVariableCount() > (*(RClause**)pSecond_)->iVariableCount()) {
    return 1; // swap!
  }
  else if ((*(RClause**)pFirst_)->iVariableCount() < (*(RClause**)pSecond_)->iVariableCount()) {
    return -1;
  }
  else {
    for (int i=0; i<(*(RClause**)pFirst_)->iVariableCount(); i++) {
      if ((*(RClause**)pFirst_)->eConstrainedVariable(i) >
	  (*(RClause**)pSecond_)->eConstrainedVariable(i)) {
	return 1;
      }
      else if ((*(RClause**)pFirst_)->eConstrainedVariable(i) <
	       (*(RClause**)pSecond_)->eConstrainedVariable(i)) {
	return -1;
      }
    }
    return 0;
  }
}

void ClauseList::vSortClausesByLength()
{
  // Warning: With Microsoft Visual C++ compiler, this qsort sometimes performs abysmally.
  // (Note: This problem seems to have disappeared now that I have lexical tie-breaking
  //  in the compare function -- MS VC++ sort routine seemed to only have
  //  problems when most/many elements are "equal" according to the compare function).
  qsort((void*)_aClause, iClauseCount(), sizeof(RClause*), compare_clauselength);
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// Private/Protected Methods

void ClauseList::_vExpand()
{
  _iMaxSize *= 2;
  RClause** _aNewList = new RClause*[_iMaxSize];
  memcpy(_aNewList, _aClause, _iClauseCount*sizeof(RClause*));
  delete [] _aClause;
  _aClause = _aNewList;
}
