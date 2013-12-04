/////////////////
// OS Includes
#include <memory.h>
#include <stdlib.h>

//////////////
// Includes
#include "Clause.h"
#include "Debug.h"
#include "SATSolver.h"
#include "SATInstance.h"
#include "SATSolver.h"
#include "VariableList.h"
#include "VariableSet.h"

/////////////
// Defines
#define MAX_CLAUSE_LENGTH 3

///////////////////////////
// Static Initialization

//////////////////////////////////////////////////////////////////////////////////////////////////
// Public Methods

void SATSolver::vIncorporateLearnedClauses()
{
  // Incorporate any clauses that were learned from the previous run into the SAT instance.
  for (int i=0; i<_xLearnedClauses.iClauseCount(); i++) {
    _pInstance->vAddClause(_xLearnedClauses.pClause(i));
  }
  _xLearnedClauses.vClear();
}

boolean SATSolver::bPreprocess(int iLevel_, int iIterationBound_)
{
  if (iLevel_ == 0)
    return 0;
  // Preprocesses the instance. Returns 1 if instance is unsatisfiable.
  if (!_bInitialize()) {
    _vCleanup();
    return 1;
  }
  if (iIterationBound_ == -1) {
    iIterationBound_ = 99999; // -1 implies no bound
  }
  time_t iStart;
  time(&iStart);
  _iLastCheckTime = iStart;
  int iInitialClauseCount = _pInstance->iClauseCount();
  _pSet0 = new VariableSet(_iVariableCount);
  _pSet1 = new VariableSet(_iVariableCount);
  int iInitialCount = _pInstance->iClauseCount();
  int iIteration = 0;
  cout << "c Processing instance: sorting clauses..." << flush;
  _pInstance->vSortClausesByLength();
  cout << "\nc   removing redundancies..." << flush;
  int i;
  for (i=1; i<_pInstance->iClauseCount(); i++) {
    RClause* pCheck = _pInstance->pClause(i);
    if (pCheck->bIsEqual(*(_pInstance->pClause(i-1)))) {
      pCheck->vFlagAsDeleted(); // duplicate clause
    }
  }
  _vRemoveRedundancies(0);
  _vCleanClauseLists(); 

  boolean bContinue;
  do {
    bContinue = 0;
    cout << "\nc   unit reducing..." << flush; 
    int iNewUnaryClauses;
    if (_bUnitReduce(iNewUnaryClauses)) {
      _vCleanup(iStart, iInitialClauseCount);
      _vCleanup();
      return 1;
    }
    cout << iNewUnaryClauses << " unary clauses identified." << flush;
    if (iNewUnaryClauses) {
      //bContinue = 1; Not needed since unary reduction goes until completion
      _vCleanClauseLists();
    }
    int iBinaryClauses = 0;
    if (iLevel_ > 1) {
      cout << "\nc   binary inferring..." << flush;
      if (_bBinaryInfer(iBinaryClauses)) {
	_vCleanup(iStart, iInitialClauseCount);
	_vCleanup();
	return 1;
      }
      cout << iBinaryClauses << " unary+binary clauses identified." << flush;
      if (iBinaryClauses) {
	bContinue = 1;
	_vCleanClauseLists();
      }
    }
    // END TEMP
    cout << "\nc   resolving..." << flush;
    iBinaryClauses = 0;
    if (_bResolve(iBinaryClauses)) {
      _vCleanup(iStart, iInitialClauseCount);
      _vCleanup();
      return 1;
    }
    cout << iBinaryClauses << " resolutions performed." << flush;
    if (iBinaryClauses) {
      bContinue = 1;
      _vCleanClauseLists();
    }
    if (iLevel_ > 2) {
      cout << "\nc   reducing clauses..." << flush;
      iBinaryClauses = 0;
      if (_bReduceClauses(iBinaryClauses)) {
	_vCleanup(iStart, iInitialClauseCount);
	_vCleanup();
	return 1;
      }
      cout << iBinaryClauses << " clauses reduced." << flush;
      if (iBinaryClauses) {
	bContinue = 1;
	_vCleanClauseLists();
      }
    }
  } while (bContinue && ++iIteration < iIterationBound_);

  _vCleanup(iStart, iInitialClauseCount);

  _vCleanup();
  return 0;
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// Private/Protected Methods

void SATSolver::_vCleanClauseLists()
{
  for (int i=0; i<_iVariableCount; i++) {
    _aVariableStruct[i].xPositiveClauses.vRemoveDeletedClauses();
    _aVariableStruct[i].xNegativeClauses.vRemoveDeletedClauses();    
  }
}

void SATSolver::_vCleanup(time_t iStart_, int iInitialClauseCount)
{
  cout << "\nc Processing phase stats: " << endl;
  cout << "c   Initial clause count       : " << iInitialClauseCount << endl;
  _pInstance->vDestroyDeletedClauses();
  cout << "c   New clause count           : " 
       << _pInstance->iClauseCount()
       << endl;
  time_t iEnd;
  time(&iEnd);
  cout << "c   Seconds elapsed (real time): " << iEnd - iStart_ << " seconds." << endl;
  delete _pSet0;
  delete _pSet1;
}

boolean SATSolver::_bRedundancyCheck(RClause* pNewClause_)
{
  RClause** pStart;
  RClause** pEnd;
  for (int i=0; i<pNewClause_->iVariableCount(); i++) {
    VariableID iVar = pNewClause_->eConstrainedVariable(i);
    ClauseList* pCheckList;
    if (pNewClause_->iIsNegated(i)) {
      pStart = _aVariableStruct[iVar].xNegativeClauses.pEntry(0);
      pEnd = _aVariableStruct[iVar].xNegativeClauses.pLastEntry();
    }
    else {
      pStart = _aVariableStruct[iVar].xPositiveClauses.pEntry(0);
      pEnd = _aVariableStruct[iVar].xPositiveClauses.pLastEntry();
    }
    for (; pStart < pEnd; pStart++) {
      if (*pStart != pNewClause_ && !(*pStart)->bIsDeleted()) {
	switch(_iRedundancyCheck(pNewClause_, *pStart)) {
	case 1:
	  delete pNewClause_; // clause is redundant.
	  //cout << "Redundant because of: "; (*pStart)->vOutput(); cout << endl;
	  return 1;
	case 2:
	  //cout << "Redundant clause found: "; (*pStart)->vOutput(); cout << endl;
	  if (!(*pStart)->bLearned()) {
	    pNewClause_->vMakeRequired();
	  }
	  (*pStart)->vFlagAsDeleted();
	}
      }
    }
  }
  return 0;
}

void SATSolver::_vOneSidedRedundancyCheck(RClause* pClause_)
{
  // Check only if pClause_ makes other clauses redundant.
  RClause** pStart;
  RClause** pEnd;
  for (int i=0; i<pClause_->iVariableCount(); i++) {
    VariableID iVar = pClause_->eConstrainedVariable(i);
    ClauseList* pCheckList;
    if (pClause_->iIsNegated(i)) {
      pStart = _aVariableStruct[iVar].xNegativeClauses.pEntry(0);
      pEnd = _aVariableStruct[iVar].xNegativeClauses.pLastEntry();
    }
    else {
      pStart = _aVariableStruct[iVar].xPositiveClauses.pEntry(0);
      pEnd = _aVariableStruct[iVar].xPositiveClauses.pLastEntry();
    }
    for (; pStart < pEnd; pStart++) {
      if (*pStart != pClause_ && !(*pStart)->bIsDeleted()) {
	if(_bMakesRedundant(pClause_, *pStart)) {
	  (*pStart)->vFlagAsDeleted();
	}
      }
    }
  }
}

int SATSolver::_iRedundancyCheck(RClause* pClause1_, RClause* pClause2_)
{
  // Returns 1 if pClause1_ is redundant, and 2 if pClause2_ is redundant.
  // Returns 0 otherwise
  
  _pSet0->vClear();
  _pSet1->vClear();
  int i;
  boolean bSwap;
  if (pClause1_->iVariableCount() < pClause2_->iVariableCount()) {
    bSwap = 1;
    RClause* pTemp = pClause1_;
    pClause1_ = pClause2_;
    pClause2_ = pTemp;
  }
  else {
    bSwap = 0;
  }

  for (i=0; i<pClause1_->iVariableCount(); i++) {
    if (pClause1_->iIsNegated(i)) {
      _pSet0->vAddVariableNoCheck(pClause1_->eConstrainedVariable(i));
    }
    else {
      _pSet1->vAddVariableNoCheck(pClause1_->eConstrainedVariable(i));
    }
  }
 
  for (i=0; i<pClause2_->iVariableCount(); i++) {
    if (pClause2_->iIsNegated(i)) {
      if (!_pSet0->bHasVariable(pClause2_->eConstrainedVariable(i))) {
	return 0; 
      }
    }
    else {
      if (!_pSet1->bHasVariable(pClause2_->eConstrainedVariable(i))) {
	return 0;
      }
    }
  }

  // pClause1 is redundant!
  if (bSwap) {
    return 2;
  }
  return 1;
}


boolean SATSolver::_bMakesRedundant(RClause* pClause1_, RClause* pClause2_)
{
  // Returns 1 if pClause2_ is redundant because of pClause1_
  // Returns 0 otherwise.
  // Does not detect duplicate clauses.
  // Assumes variables of each clause are sorted.
  
  assert(pClause1_->iVariableCount()); // assumes no null clause
  if (pClause2_->iVariableCount() <= pClause1_->iVariableCount()) {
    return 0;
  }
  int iDiff = 0;
  int i = 0;
  while (1) {
    if (pClause1_->eConstrainedLiteral(i) == pClause2_->eConstrainedLiteral(iDiff)) {
      i++; 
      if (i == pClause1_->iVariableCount()) {
	return 1;
      }
    }
    iDiff++;
    if (iDiff == pClause2_->iVariableCount()) {
      return 0;
    }
  }
}

boolean SATSolver::_bResolve(RClause* pClause_, int& iCount_)
{
  // returns 1 if contradiction derived
  int i;
  RClause** pStart;
  RClause** pEnd;
  for (i=0; i<pClause_->iVariableCount(); i++) {
    VariableID iVar = pClause_->eConstrainedVariable(i);
    ClauseList* pCheckList;
    if (pClause_->iIsNegated(i)) {
      pStart = _aVariableStruct[iVar].xPositiveClauses.pEntry(0);
      pEnd = _aVariableStruct[iVar].xPositiveClauses.pLastEntry();
    }
    else {
      pStart = _aVariableStruct[iVar].xNegativeClauses.pEntry(0);
      pEnd = _aVariableStruct[iVar].xNegativeClauses.pLastEntry();
    }
    for (; pStart < pEnd; pStart++) {
      RClause* pCompare = *pStart;
      if (pCompare != pClause_ && !pCompare->bIsDeleted()) {
	RClause* pNewClause = _pResolve(pClause_, pCompare, iVar);
	if (pNewClause) {
	  if (pNewClause->iVariableCount() == 0) {
	    // Instance is UNSAT
	    delete pNewClause;
	    return 1;
	  }
	  else if (!_bRedundancyCheck(pNewClause)) {
	    if (_bInitializeClause(pNewClause)) {
	      delete pNewClause;
	      return 1;
	    }
	    else {
	      iCount_++;
	      _pInstance->vAddClause(pNewClause);
	    }
	  }
	  if (pClause_->bIsDeleted()) {
	    return 0;
	  }
	}
      }
    }
  }
  return 0;
}

RClause* SATSolver::_pResolve(RClause* pClause1_, 
			     RClause* pClause2_, 
			     VariableID iResolveVariable_)
{
  int iMaximumLength;
  if (pClause1_->iVariableCount() < pClause2_->iVariableCount()) {
    iMaximumLength = pClause2_->iVariableCount() - 1;
  }
  else { 
    iMaximumLength = pClause1_->iVariableCount() - 1;
  }
  if (iMaximumLength < MAX_CLAUSE_LENGTH &&
      pClause1_->iVariableCount() > 2 && pClause2_->iVariableCount() > 2) {
    iMaximumLength = MAX_CLAUSE_LENGTH;
  }
  if (pClause1_->iVariableCount() == 2 && pClause2_->iVariableCount() == 2) {
    return 0;
  }

  int i1 = 0;
  int i2 = 0;
  int iLength = 0;
  while (iLength <= iMaximumLength+1) {
    while (pClause1_->eConstrainedVariable(i1) < pClause2_->eConstrainedVariable(i2)) {
      iLength++;
      i1++;
      if (i1 == pClause1_->iVariableCount()) {
	iLength += pClause2_->iVariableCount() - i2;
	goto done;
      }
    }
    while (pClause1_->eConstrainedVariable(i1) > pClause2_->eConstrainedVariable(i2)) {
      iLength++;
      i2++;
      if (i2 == pClause2_->iVariableCount()) {
	iLength += pClause1_->iVariableCount() - i1;
	goto done;
      }
    }
    if (pClause1_->eConstrainedVariable(i1) == pClause2_->eConstrainedVariable(i2)) {
      iLength++;
      if (pClause1_->eConstrainedVariable(i1) != iResolveVariable_ &&
	  pClause1_->iIsNegated(i1) != pClause2_->iIsNegated(i2)) {
	return 0; // tautology
      }
      i1++;
      i2++;
      if (i1 == pClause1_->iVariableCount()) {
	iLength += pClause2_->iVariableCount() - i2;
	goto done;
      }
      if (i2 == pClause2_->iVariableCount()) {
	iLength += pClause1_->iVariableCount() - i1;
	goto done;
      }
    }
  }
done:
  if (iLength-1 > iMaximumLength) {
    return 0;
  }
  _pSet0->vClear();
  _pSet1->vClear();
  int i;
  for (i=0; i<pClause1_->iVariableCount(); i++) {
    VariableID iAddMe = pClause1_->eConstrainedVariable(i);
    if (iAddMe != iResolveVariable_) {
      if (pClause1_->iIsNegated(i)) {
	_pSet0->vAddVariableNoCheck(iAddMe);
      }
      else {
	_pSet1->vAddVariableNoCheck(iAddMe);
      }
    }
  }
  for (i=0; i<pClause2_->iVariableCount(); i++) {
    VariableID iAddMe = pClause2_->eConstrainedVariable(i);
    if (iAddMe != iResolveVariable_) {
      if (pClause2_->iIsNegated(i)) {
	_pSet0->vAddVariable(iAddMe);
      }
      else {
	_pSet1->vAddVariable(iAddMe);
      }
    }
  }
  assert(_pSet1->iCount() + _pSet0->iCount() == iLength-1);
  // up-casting to VariableList to keep xlC compiler happy:
  RClause* pNew = new RClause(*(VariableList*)_pSet1, *(VariableList*)_pSet0, 1);
  pNew->vMakeLearned();
  return pNew;
}

boolean SATSolver::_bResolve(int& iClausesResolved_)
{
  int i;
  for (i=0; i<_pInstance->iClauseCount(); i++) {
    RClause* pCheck = _pInstance->pClause(i);
    if (!pCheck->bIsDeleted()) {
      if (_bResolve(pCheck, iClausesResolved_)) {
	return 1;
      }
    }
  }
  return 0;
}

boolean SATSolver::_bReduceClauses(int& iNewClauses_)
{
  int i;
  time(&_iLastCheckTime);
  for (i=0; i<_pInstance->iClauseCount(); i++) {
    if (_bPrintStack) {
      time_t iCheckTime;
      time(&iCheckTime);
      if (iCheckTime - _iLastCheckTime >= _iPrintStackPeriod) {
	double fI = i;
	double fT = _pInstance->iClauseCount();
	double fPercent = (fI / fT) * 100.0;
	cout << (int) fPercent << "%.." << flush;
	_iLastCheckTime = iCheckTime;
      }
    }
    RClause* pCheck = _pInstance->pClause(i);
    if (!pCheck->bIsDeleted() && pCheck->iVariableCount() > 2) {
      RClause* pNew = _pReduceClause(pCheck);
      if (pNew) {
	//cout << "Reduced clause: "; pNew->vOutput(); cout << endl;
	//cout << "From: "; pCheck->vOutput(); cout << endl;
	if (pCheck->bLearned()) {
	  pNew->vMakeLearned();
	}
	pCheck->vFlagAsDeleted(); // redundant
	if (pNew->iVariableCount() == 0) {
	  // Instance is UNSAT
	  delete pNew;
	  return 1;
	}
	else {
	  boolean bResult = _bRedundancyCheck(pNew);
	  assert(bResult == 0);
	  if (_bInitializeClause(pNew)) {
	    delete pNew;
	    return 1;
	  }
	  iNewClauses_++;
	  _pInstance->vAddClause(pNew);
	  /*if (_bResolve(pNew)) {
	    return 1;
	  }*/
	}
      }
    }
  }
  return 0;
}

static long int* aBinaryCount0;
static long int* aBinaryCount1;

int compare_binary_counts(const void* pFirst_, const void* pSecond_)
{
  LiteralID eFirst = *(long*)pFirst_;
  LiteralID eSecond = *(long*)pSecond_;
  int iCount1, iCount2;
  if (RClause::iIsLiteralNegated(eFirst)) {
    iCount1 = aBinaryCount0[RClause::iGetVariable(eFirst)];
  }
  else {
    iCount1 = aBinaryCount1[eFirst];
  }
  if (RClause::iIsLiteralNegated(eSecond)) {
    iCount2 = aBinaryCount0[RClause::iGetVariable(eSecond)];
  }
  else {
    iCount2 = aBinaryCount1[eSecond];
  }
  if (iCount1 < iCount2) {
    return -1;
  }
  else if (iCount1 > iCount2) {
    return 1;
  }
  else return 0;
}

int compare_binary_count_sums(const void* pFirst_, const void* pSecond_)
{
  LiteralID eFirst = *(long*)pFirst_;
  LiteralID eSecond = *(long*)pSecond_;
  int iCount1 = aBinaryCount0[RClause::iGetVariable(eFirst)] +
    aBinaryCount1[RClause::iGetVariable(eFirst)];
  int iCount2 = aBinaryCount0[RClause::iGetVariable(eSecond)] +
    aBinaryCount1[RClause::iGetVariable(eSecond)];
  if (iCount1 < iCount2) {
    return 1;
  }
  else if (iCount1 > iCount2) {
    return 0;
  }
  else return 0;
}

boolean SATSolver::_bBinaryInfer(int& iNewClauses_)
{
  if (_bUnitPropagate()) {
    return 1;
  }
  int i;
  double fI = 0.0;
  double fT = ((double)_iVariableCount * (double)(_iVariableCount+1)) / 2.0;
  time(&_iLastCheckTime);
  for (i=0; i<_iVariableCount; i++) {
    if (_bPrintStack) {
      time_t iCheckTime;
      time(&iCheckTime);
      if (iCheckTime - _iLastCheckTime >= _iPrintStackPeriod) {
	double fPercent = (fI / fT) * 100.0;
	cout << (int) fPercent << "%.." << flush;
	_iLastCheckTime = iCheckTime;
      }
    }
    if (!_aVariableStruct[i].pReason) {
      //cout << "c Trying " << i << endl;
      if (_aBinaryCount0[i]) {
	if (_bBinaryReduce(i, 1, iNewClauses_))
	  return 1;
      }
    }
    if (!_aVariableStruct[i].pReason) {
      if (_aBinaryCount1[i]) {
	if (_bBinaryReduce(i, 0, iNewClauses_))
	  return 1;      
      }
    }
    fI += _iVariableCount-i;
  }
  return 0;
}

RClause* SATSolver::_pReduceClause(RClause* pReduceMe_)
{
  // See if unit propagation can derive a clause that makes
  // this one redundant.
  if (pReduceMe_->iVariableCount() < 3) {
    return 0;
  }
  //cout << "Reducing: " ; pReduceMe_->vOutput(); cout << endl;
  for (int i=0; i<pReduceMe_->iVariableCount(); i++) {
    assert(_pUnitList->iCount() == 0);
    for (int j=0; j<pReduceMe_->iVariableCount(); j++) {
      if (j!=i) {
	VariableID iVar = pReduceMe_->eConstrainedVariable(j);
	_pUnitList->vAdd(iVar);
	if (pReduceMe_->iIsNegated(j)) {
	  _aAssignment[iVar] = 1;
	}
	else {
	  _aAssignment[iVar] = 0;
	}
      }
    }
    if (_bFastUnitPropagate()) {
      _pSet0->vClear();
      _pSet1->vClear();
      for (int k=0; k<pReduceMe_->iVariableCount(); k++) {
	if (k!=i) {
	  VariableID iVar = pReduceMe_->eConstrainedVariable(k);
	  if (pReduceMe_->iIsNegated(k)) {
	    _pSet0->vAddVariableNoCheck(iVar);
	  }
	  else {
	    _pSet1->vAddVariableNoCheck(iVar);
	  }
	}
      } // for (int k
      RClause* pReducedClause =  new RClause(*(VariableList*)_pSet1, *(VariableList*)_pSet0, (int)1);
      //pReducedClause->vOutput(); cout << endl;
      //cout << '.' << flush;
      return pReducedClause;
    } // if (bFastUnitPropagate
  }
  return 0;
}

int SATSolver::_iFastUnitPropagate(VariableID& eUnitVar_)
{
  int i;
  VariableID eID;
  VariableStruct* pWorkStruct;
  RClause** pStart;
  RClause** pEnd;
  RClause** pBegin;
  RClause* pReduceMe;
  int j;

  for (i=0; i<_pUnitList->iCount(); i++) {
    eID = _pUnitList->iVariable(i);
    pWorkStruct = &_aVariableStruct[eID];
    if (_aAssignment[eID] == 0) {
      pStart = pWorkStruct->xPositiveClauses.pEntry(0);
      pEnd = pWorkStruct->xPositiveClauses.pLastEntry();
    }
    else {
      assert(_aAssignment[eID] == 1);
      pStart = pWorkStruct->xNegativeClauses.pEntry(0);
      pEnd = pWorkStruct->xNegativeClauses.pLastEntry();
    }

    pBegin = pStart;
    for (;pStart < pEnd; pStart++) {
      pReduceMe = *pStart;
      if (!pReduceMe->bIsSatisfied()) {
	switch(pReduceMe->iReduce()) {
	case 0:
	  // Contradiction!
	  for (; pStart >= pBegin; pStart--) {
	    (*pStart)->iExpand();
	  }
	  _vFastBackup(i);
	  return 1;
	case 1:
	  for (j = 0; j < pReduceMe->iPermaCount(); j++) {
	    eID = pReduceMe->eConstrainedVariable(j);
	    if (_aAssignment[eID] == NON_VALUE) {
	      _pUnitList->vAdd(eID);
	      if (pReduceMe->iIsNegated(j)) {	    
		_aAssignment[eID] = 0;
		if (!_pNegativeBackup->bAddVariable(eID)) {
		  // found a unit var.
		  for (; pStart >= pBegin; pStart--) {
		    (*pStart)->iExpand();
		  }
		  _vFastBackup(i);
		  eUnitVar_ = eID;
		  return 3; // var is negated
		}
	      }
	      else {
		_aAssignment[eID] = 1;
		if (!_pPositiveBackup->bAddVariable(eID)) {
		  // found a unit var.
		  for (; pStart >= pBegin; pStart--) {
		    (*pStart)->iExpand();
		  }
		  _vFastBackup(i);
		  eUnitVar_ = eID;
		  return 2; // var is positive
		}
	      }
	      break;
	    }
	  }
	}
      }
    } // for (;pStart...
  } // for (i=..
  _vFastBackupScore();
  return 0;
}

boolean SATSolver::_bFastUnitPropagate()
{
  int i;
  VariableID eID;
  VariableStruct* pWorkStruct;
  RClause** pStart;
  RClause** pEnd;
  RClause** pBegin;
  RClause* pReduceMe;
  int j;

  for (i=0; i<_pUnitList->iCount(); i++) {
    eID = _pUnitList->iVariable(i);
    pWorkStruct = &_aVariableStruct[eID];
    if (_aAssignment[eID] == 0) {
      pStart = pWorkStruct->xPositiveClauses.pEntry(0);
      pEnd = pWorkStruct->xPositiveClauses.pLastEntry();
    }
    else {
      assert(_aAssignment[eID] == 1);
      pStart = pWorkStruct->xNegativeClauses.pEntry(0);
      pEnd = pWorkStruct->xNegativeClauses.pLastEntry();
    }

    pBegin = pStart;
    for (;pStart < pEnd; pStart++) {
      pReduceMe = *pStart;
      if (!pReduceMe->bIsSatisfied()) {
	switch(pReduceMe->iReduce()) {
	case 0:
	  // Contradiction!
	  for (; pStart >= pBegin; pStart--) {
	    (*pStart)->iExpand();
	  }
	  _vFastBackup(i);
	  return 1;
	case 1:
	  for (j = 0; j < pReduceMe->iPermaCount(); j++) {
	    eID = pReduceMe->eConstrainedVariable(j);
	    if (_aAssignment[eID] == NON_VALUE) {
	      _pUnitList->vAdd(eID);
	      if (pReduceMe->iIsNegated(j)) {	    
		_aAssignment[eID] = 0;
	      }
	      else {
		_aAssignment[eID] = 1;
	      }
	      break;
	    }
	  }
	}
      }
    } // for (;pStart...
  } // for (i=..
  _vFastBackupScore();
  return 0;
}

void SATSolver::_vRemoveRedundancies(int iStartIndex_)
{
  int i;
  for (i=iStartIndex_; i<_pInstance->iClauseCount(); i++) {
    RClause* pCheck = _pInstance->pClause(i);
    if (!pCheck->bIsDeleted()) {
      _vOneSidedRedundancyCheck(pCheck);
    }
  }
}

boolean SATSolver::_bUnitReduce(int& iNewClauses_)
{
  if (_bUnitPropagate()) {
    return 1;
  }
  iNewClauses_ = 0;
  boolean bAgain;
  VariableID eOtherUnitVar;
  do {
    bAgain = 0;
    for (int i=0; i<_iVariableCount; i++) {
      VariableID iVar = i;
      _pPositiveBackup->vClear();
      _pNegativeBackup->vClear();
      if (!_aVariableStruct[iVar].pReason) {
	if (_aScore1[iVar] != -1 && _aBinaryCount1[iVar]) {
	  _pUnitList->vAdd(iVar);
	  _aAssignment[iVar] = 0;
	  int iResult = _iFastUnitPropagate(eOtherUnitVar);
	  assert(iResult < 2);
	  if (iResult) {
	    memset(_aScore0, 0, sizeof(int)*_iVariableCount);
	    memset(_aScore1, 0, sizeof(int)*_iVariableCount);
	    bAgain = 1;
	    _pSet0->vClear();
	    _pSet1->vClear();
	    _pSet1->vAddVariableNoCheck(iVar);
	    RClause* pNew =  new RClause(*(VariableList*)_pSet1, *(VariableList*)_pSet0, (int)1);
	    _pInstance->vAddClause(pNew);
	    //cout << "New unary clause: "; pNew->vOutput(); cout << endl;
	    iNewClauses_++;
	    boolean bResult = _bRedundancyCheck(pNew);
	    assert(bResult == 0);
	    if (_bInitializeClause(pNew)) {
	      return 1;
	    }
	    /*if (_bResolve(pNew)) {
	      return 1;
	    }*/
	    if (_bUnitPropagate()) {
	      return 1;
	    }
	  }
	}
      }
      if (!_aVariableStruct[iVar].pReason) {
	if (_aScore0[iVar] != -1 && _aBinaryCount0[iVar]) { 
	  _pUnitList->vAdd(iVar);
	  _aAssignment[iVar] = 1;
	  int iResult = _iFastUnitPropagate(eOtherUnitVar);
	  if (iResult) {
	    memset(_aScore0, 0, sizeof(*_aScore0)*_iVariableCount);
	    memset(_aScore1, 0, sizeof(*_aScore1)*_iVariableCount);
	    bAgain = 1;
	    _pSet0->vClear();
	    _pSet1->vClear();
	    switch(iResult) {
	    case 1:
	      _pSet0->vAddVariableNoCheck(iVar);
	      break;
	    case 2:
	      //cout << "Wheee!: " <<eOtherUnitVar << endl;
	      _pSet1->vAddVariableNoCheck(eOtherUnitVar);
	      break;
	    case 3:
	      //cout << "Wheee!: " <<eOtherUnitVar << endl;
	      _pSet0->vAddVariableNoCheck(eOtherUnitVar);
	      break;
	    }
	    RClause* pNew =  new RClause(*(VariableList*)_pSet1, *(VariableList*)_pSet0, (int)1);
	    _pInstance->vAddClause(pNew);
	    //cout << "New unary clause: "; pNew->vOutput(); cout << endl;
	    iNewClauses_++;
	    boolean bResult = _bRedundancyCheck(pNew);
	    assert(bResult == 0);
	    if (_bInitializeClause(pNew)) {
	      return 1;
	    }
	    /*if (_bResolve(pNew)) {
	      return 1;
	    }*/
	    if (_bUnitPropagate()) {
	      return 1;
	    }
	  }
	}
      }
    } // for
  } while (bAgain);
  memset(_aScore0, 0, sizeof(*_aScore0)*_iVariableCount);
  memset(_aScore1, 0, sizeof(*_aScore1)*_iVariableCount);
  return 0;
}

boolean SATSolver::_bBinaryReduce(VariableID eWith_, DomainValue lWhich_, int& iNewClauses_)
{
  memcpy(_aScore0, _aBinaryCount0, sizeof(int)*_iVariableCount);
  memcpy(_aScore1, _aBinaryCount1, sizeof(int)*_iVariableCount);

  _aVariableStruct[eWith_].bBranch = 1;
  assert(_aAssignment[eWith_] == NON_VALUE);
  assert(!_aVariableStruct[eWith_].pReason);
  if (lWhich_) {
    _pUnitVariables1->vAddVariable(eWith_);
  }
  else {
    _pUnitVariables0->vAddVariable(eWith_);
  }
  if (_bUnitPropagate()) {
    _pBackupToFirstBranch();
    assert(_aAssignment[eWith_] == NON_VALUE);
    assert(_aVariableStruct[eWith_].bBranch  == 0);
    _pSet0->vClear();
    _pSet1->vClear();
    if (lWhich_) {
      _pSet0->vAddVariableNoCheck(eWith_);
    }
    else {
      _pSet1->vAddVariableNoCheck(eWith_);
    }
    RClause* pNew =  new RClause(*(VariableList*)_pSet1, *(VariableList*)_pSet0, (int)1);
    _pInstance->vAddClause(pNew);
    //cout << "New unary clause: "; pNew->vOutput(); cout << endl;
    iNewClauses_++;
    boolean bResult = _bRedundancyCheck(pNew);
    assert(bResult == 0);
    if (_bInitializeClause(pNew)) {
      return 1;
    }
    /*if (_bResolve(pNew)) {
      return 1;
    }*/
    if (_bUnitPropagate()) {
      return 1;
    }
    return 0;
  }

  VariableID eOtherUnitVar;
  int i;
  for (i=eWith_+1; i<_iVariableCount; i++) {
    VariableID iVar = i;
    //cout << _aBinaryCount0[iVar] + _aBinaryCount1[iVar] << endl;
    _pPositiveBackup->vClear();
    _pNegativeBackup->vClear();
    if (!_aVariableStruct[iVar].pReason) {
      if (_aScore1[iVar] != -1 
	  //&& _aBinaryCount1[iVar]
	  && _aBinaryCount1[iVar] > _aScore1[iVar]
	  ) {
	_pUnitList->vAdd(iVar);
	_aAssignment[iVar] = 0;
	if (_iFastUnitPropagate(eOtherUnitVar)) {
	  assert(_pUnitVariables0->iCount() == 0);
	  assert(_pUnitVariables0->iCount() == 0);
	  _pUnitVariables1->vAddVariableNoCheck(iVar);
	  if (_bUnitPropagate()) {
	    _pBackupToFirstBranch();
	    assert(_aAssignment[eWith_] == NON_VALUE);
	    assert(_aVariableStruct[eWith_].bBranch  == 0);
	    _pSet0->vClear();
	    _pSet1->vClear();
	    if (lWhich_) {
	      _pSet0->vAddVariableNoCheck(eWith_);
	    }
	    else {
	      _pSet1->vAddVariableNoCheck(eWith_);
	    }
	    RClause* pNew =  new RClause(*(VariableList*)_pSet1, *(VariableList*)_pSet0, (int)1);
	    _pInstance->vAddClause(pNew);
	    //cout << "New unary clause?!?!!?: "; pNew->vOutput(); cout << endl;
	    iNewClauses_++;
	    boolean bResult = _bRedundancyCheck(pNew);
	    assert(bResult == 0);
	    if (_bInitializeClause(pNew)) {
	      return 1;
	    }
	    if (_bUnitPropagate()) {
	      return 1;
	    }
	    return 0;	
	  } // _bUnitPropagate
	  assert(_aAssignment[iVar] != NON_VALUE);
	  _pSet0->vClear();
	  _pSet1->vClear();
	  _pSet1->vAddVariableNoCheck(iVar);
	  if (lWhich_) {
	    _pSet0->vAddVariableNoCheck(eWith_);
	  }
	  else {
	    _pSet1->vAddVariableNoCheck(eWith_);
	  }
	  RClause* pNew = new RClause(*(VariableList*)_pSet1, *(VariableList*)_pSet0, (int)1);
	  _aVariableStruct[iVar].pReason = pNew;
	  //cout << "New binary clause: "; pNew->vOutput(); cout << endl;
	  _pInstance->vAddClause(pNew);
	  iNewClauses_++;
	  boolean bResult = _bRedundancyCheck(pNew);
	  assert(bResult == 0);
	  if (_bInitializeClause(pNew)) {
	    return 1;
	  }
	  pNew->vFudge();
	  memset(_aScore0, 0, sizeof(*_aScore0)*_iVariableCount);
	  memset(_aScore1, 0, sizeof(*_aScore1)*_iVariableCount);
	} // _iFastUnitPropagate
      }
      if (!_aVariableStruct[iVar].pReason) {
	if (_aScore0[iVar] != -1 
	    //&& _aBinaryCount0[iVar]
	    && _aBinaryCount0[iVar] > _aScore0[iVar]
	    ) { 
	  _pUnitList->vAdd(iVar);
	  _aAssignment[iVar] = 1;

	  int iResult = _iFastUnitPropagate(eOtherUnitVar);
	  if (iResult) {
	    assert(_pUnitVariables0->iCount() == 0);
	    assert(_pUnitVariables0->iCount() == 0);
	    _pSet0->vClear();
	    _pSet1->vClear();
	    switch(iResult) {
	    case 1:
	      _pSet0->vAddVariableNoCheck(iVar);
	      _pUnitVariables0->vAddVariableNoCheck(iVar);
	      eOtherUnitVar = iVar;
	      break;
	    case 2:
	      _pSet1->vAddVariableNoCheck(eOtherUnitVar);
	      _pUnitVariables1->vAddVariableNoCheck(eOtherUnitVar);
	      break;
	    case 3:
	      _pSet0->vAddVariableNoCheck(eOtherUnitVar);
	      _pUnitVariables0->vAddVariableNoCheck(eOtherUnitVar);
	      break;
	    }
	    if (_bUnitPropagate()) {
	      _pBackupToFirstBranch();
	      assert(_aAssignment[eWith_] == NON_VALUE);
	      assert(_aVariableStruct[eWith_].bBranch  == 0);
	      _pSet0->vClear();
	      _pSet1->vClear();
	      if (lWhich_) {
		_pSet0->vAddVariableNoCheck(eWith_);
	      }
	      else {
		_pSet1->vAddVariableNoCheck(eWith_);
	      }
	      RClause* pNew = new RClause(*(VariableList*)_pSet1, *(VariableList*)_pSet0, (int)1);
	      _pInstance->vAddClause(pNew);
	      //cout << "New unary clause!!!: "; pNew->vOutput(); cout << endl;
	      iNewClauses_++;
	      boolean bResult = _bRedundancyCheck(pNew);
	      assert(bResult == 0);
	      if (_bInitializeClause(pNew)) {
		return 1;
	      }
	      if (_bUnitPropagate()) {
		return 1;
	      }
	      return 0;	
	    }
	    if (lWhich_) {
	      _pSet0->vAddVariableNoCheck(eWith_);
	    }
	    else {
	      _pSet1->vAddVariableNoCheck(eWith_);
	    }
	    RClause* pNew = new RClause(*(VariableList*)_pSet1, *(VariableList*)_pSet0, (int)1);
	    _aVariableStruct[eOtherUnitVar].pReason = pNew;
	    assert(_aAssignment[eOtherUnitVar] != NON_VALUE);
	    //cout << "New binary clause: "; pNew->vOutput(); cout << endl;
	    _pInstance->vAddClause(pNew);
	    iNewClauses_++;
	    boolean bResult = _bRedundancyCheck(pNew);
	    assert(bResult == 0);
	    if (_bInitializeClause(pNew)) {
	      return 1;
	    }
	    pNew->vFudge();
	    memset(_aScore0, 0, sizeof(*_aScore0)*_iVariableCount);
	    memset(_aScore1, 0, sizeof(*_aScore1)*_iVariableCount);
	  }
	}
      }
    }
  }
  memset(_aScore0, 0, sizeof(*_aScore0)*_iVariableCount);
  memset(_aScore1, 0, sizeof(*_aScore1)*_iVariableCount);
  _pBackupToFirstBranch();
  assert(_aAssignment[eWith_] == NON_VALUE);
  assert(_aVariableStruct[eWith_].bBranch  == 0);
  return 0;
}
