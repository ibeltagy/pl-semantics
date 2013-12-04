/////////////////
// OS Includes
#include <memory.h>
#include <stdlib.h>

//////////////
// Includes
#include "Clause.h"
#include "Random.h"
#include "SATInstance.h"
#include "SATSolver.h"
#include "VariableList.h"

/////////////////////////////
// Static Data Initialization

/////////////
// Defines

//////////////////////////////////////////////////////////////////////////////////////////////////
// Public Methods

SATSolver::SATSolver(SATInstance* pSATInstance_)
{
  _pInstance = pSATInstance_;
  _aAssignment = 0;

  // Set intelligent defaults for runtime parameters:
  _bFindAll = 1; // default to finding one solution
  _iMaxSolutions = 1;
  _fFudgeFactor = .9;
  _iLearnOrder = 3;
  _bNoTimeLimit = 0;
  _iMaxTime = 43200; // 12 hours
  _bFavorSmallClauses = 1;
  _bRelevanceBounding = 1;
  _bPrintStack = 1;
  _iPrintStackPeriod = 10;
  _bRestarts = 0; 
}

SATSolver::~SATSolver()
{
  _xLearnedClauses.vDestroy();
}

relsat_enum SATSolver::eSolve()
{
  time(&_iElapsedTime);
  _iLastCheckTime = _iElapsedTime;
  relsat_enum eReturn;
  _iBranchSelections = _iVariablesLabeled = _iContradictions = 0;
  if (!_bInitialize()) {
    eReturn = UNSAT;
  }
  else {
    // Here we do an initial unit propagation to handle base unit clauses.
    if (_bUnitPropagate()) {
      eReturn = UNSAT;
    }
    else {
      boolean bFailed_;
      boolean bReturn;
      if (_bRestarts) {
	bReturn = _bRestartLoop(bFailed_);
      }
      else {
	bReturn = _bLoop(bFailed_);
      }
      if (bFailed_) {
	eReturn = TIMEOUT;
	cout << "c   Timeout." << endl;
      }
      else if (bReturn == 1) {
	eReturn = SAT;
      }
      else { 
	eReturn = UNSAT;
      }
    }
  }
  _vCleanup();
  time_t iEnd;
  time(&iEnd);
  _iElapsedTime = iEnd - _iElapsedTime;
  return eReturn;
}

void SATSolver::vSetPrintStackPeriod(long int iSeconds_) 
{ 
  _iPrintStackPeriod = iSeconds_; 
  _bPrintStack = 1;
}

void SATSolver::vSetRestartInterval(int iSeconds_) 
{
  _iRestartInterval = (clock_t) (iSeconds_ * CLOCKS_PER_SEC);
  _bRestarts = 1;
}

void SATSolver::vOutputStatusUpdateInterval()
{
  if (_bPrintStack) {
    cout << "c Status update interval: " << _iPrintStackPeriod << " seconds." << endl;
  }
}

void SATSolver::vOutputWarnings() 
{
  cout << "c Learn order: " << _iLearnOrder << endl;
  cout << "c Fudge factor: " << _fFudgeFactor << endl;
  if (!_bNoTimeLimit) {
    cout << "c Solution phase timeout after: " << _iMaxTime << " seconds." << endl;
  }
  if (_bRestarts) {
    cout << "c Restart interval: " << _iRestartInterval/CLOCKS_PER_SEC << " seconds." << endl;
    if (!_bFindAll) {
      cout << "c WARNING: Restarts override model counting. Searching for first solution only." 
	   << endl;
      _bFindAll = 1;
      _iMaxSolutions = 1;
    }
    else {
      if (_iMaxSolutions == 0) {
	cout << "c WARNING: Find all solutions not a valid option when using restarts.\n" 
	     << "c          Searching for first solution only." << endl;
	_iMaxSolutions = 1;
      }
    }
    cout << "c Finding up to " << _iMaxSolutions << " solutions with restarts." << endl;
    if (_iMaxSolutions > 1) {
      cout <<"c WARNING: With restarts, some solutions may be duplicates." << endl;
    }
  }
  else {
    if (!_bFindAll) {
      cout << "c Counting solutions (will output first solution if one exists)..." << endl;
#ifdef NO_GMP
      cout << "c WARNING: Not using a bignum package. Solution counts may overflow." << endl;
#endif
    }
    else {
      if (_iMaxSolutions == 0) {
	cout << "c Finding all solutions..." << endl;
      }
      else {
	cout << "c Finding up to " << _iMaxSolutions << " solutions..." << endl;
      }
    }
  }
}

//////////////////////////////////////////////////////////////////////////////////////////////////
// Protected Methods

//////////////////////////////////////////////////////////////////////////////////////////////////
// Private Methods

boolean SATSolver::_bLoop(boolean& bFailed_)
{
  bFailed_ = 0;
  boolean bReturnValue = 0;
  while (1) {
    if (_bTimeLimitExpired()) {
      bFailed_ = 1;
      return bReturnValue;
    }
    VariableID eBestID;
    boolean bZeroFirst;
    eBestID = _eGiveMeTheBest(bZeroFirst);
    if (eBestID == -1) {
      bReturnValue = 1;
      if (_iCurrentVariable == _iVariableCount) {
	if (_bOutputSolution()) {
	  cout << "c   Solution limit reached. " << endl;
	  return bReturnValue;
	}
      }
      if (_bSpecialBackup()) {
	cout << "c   All solutions found." << endl;
	return bReturnValue;
      }      
    }
    else {

      _aVariableStruct[eBestID].bBranch = 1;
      if (bZeroFirst) {
	_pUnitVariables0->vAddVariableNoCheck(eBestID);
      }
      else {
	_pUnitVariables1->vAddVariableNoCheck(eBestID);
      }

      if (_bUnitPropagate()) {
	if (_bBackup()) {
	  return bReturnValue;
	}
      } 
    }
  }
}

boolean SATSolver::_bOutputSolution()
{
  _iSolutionCount++;
  if (_bFindAll || _iSolutionCount == 1) { // output only first solution found if counting
    cout << "Solution " << _iSolutionCount << ": ";
    for (int i=0; i<_iVariableCount; i++) {
      assert(_aAssignment[i] != NON_VALUE);
      if (_aAssignment[i]) {
	cout << i+1 << " ";
      }
    }
    cout << endl;
  } 
#ifndef NDEBUG
  if (_bVerifySolution()) {
    // cout << "c   Solution verified" << endl;
  }
  else {
    Debug::vErrorReport("Found an invalid solution.");
  }
#endif
  if (_bFindAll && _iSolutionCount >= _iMaxSolutions && _iMaxSolutions) {
    return 1;
  }
  return 0;
}

boolean SATSolver::_bVerifySolution()
{
  // returns 1 if solution checks out OK 
  for (int i=0; i<_pInstance->iClauseCount(); i++) {
    RClause* pTestMe = _pInstance->pClause(i);
    boolean bSatisfied = 0;
    for (int j=0; j<pTestMe->iVariableCount(); j++) {
      VariableID eCheckMe = pTestMe->eConstrainedVariable(j);
      if (_aAssignment[eCheckMe] && !pTestMe->iIsNegated(j) ||
	  !_aAssignment[eCheckMe] && pTestMe->iIsNegated(j)) {
	bSatisfied = 1;
	break;
      }
    }
    if (!bSatisfied) {
      return 0;
    }
  }
  return 1;
}

VariableStruct* SATSolver::_pBackupToFirstBranch() 
{
  //returns 1 if the search is complete.
  int i=0; 
  while (i< _iCurrentVariable) {
    if(_aVariableStruct[_aPositionToID[i]].bBranch) {
      break;
    }
    i++;
  }
  VariableStruct* pWork = &(_aVariableStruct[_eCurrentID]);
  while (_iCurrentVariable != i) {
    pWork = &(_aVariableStruct[_eCurrentID]);
    _vUndoClauseModifications();    
    pWork->bBranch = 0;
    _aAssignment[_eCurrentID] = NON_VALUE;
    pWork->pReason = 0;
    _iCurrentVariable--;
    if (_iCurrentVariable != i) {
      pWork->xUnitClause.vClear();
    }
    if (_iCurrentVariable) {
      _eCurrentID = _aPositionToID[_iCurrentVariable-1];
    }
    else {
      _eCurrentID = -1;
    }
  }
  return pWork;
}

boolean SATSolver::_bRestartLoop(boolean& bFailed_)
{
  bFailed_ = 0;
  boolean bReturnValue = 0;
  clock_t iLastRestart = clock();
  while (1) {
    if (_bTimeLimitExpired()) {
      bFailed_ = 1;
      return bReturnValue;
    }
    if (clock() - iLastRestart > _iRestartInterval) {
      cout << "c   Restarting." << endl;
      VariableStruct* pWork = _pBackupToFirstBranch();
      if (_bNonTrivialUnitPropagate(pWork->xUnitClause) || _bUnitPropagate()) {
	return 0;
      }
      iLastRestart = clock();
    }
    VariableID eBestID;
    boolean bZeroFirst;
    eBestID = _eGiveMeTheBest(bZeroFirst);
    if (eBestID == -1) {
      bReturnValue = 1;
      if (_iCurrentVariable == _iVariableCount) {
	if (_bOutputSolution()) {
	  cout << "c   Solution limit reached. " << endl;
	  return bReturnValue;
	}
      }
      if (_bSpecialBackup()) {
	cout << "c   All solutions found." << endl;
	return bReturnValue;
      }      
    }
    else {
      _aVariableStruct[eBestID].bBranch = 1;
	  //added by V
	  bZeroFirst = Random::iRandom(2);
	  //end added by V
      if (bZeroFirst) {
	_pUnitVariables0->vAddVariableNoCheck(eBestID);
      }
      else {
	_pUnitVariables1->vAddVariableNoCheck(eBestID);
      }
      if (_bUnitPropagate()) {
	if (_bBackup()) {
	  return bReturnValue;
	}
      } 
    }
  }
}

VariableID SATSolver::_eGiveMeTheBest(boolean& bZeroFirst)
{
  assert(_pUnitVariables0->iCount() == 0);
  assert(_pUnitVariables1->iCount() == 0);
  double fBest = -1.0;
  VariableStruct* pWorkStruct;
  VariableID eID, eID2;
  int i, j;
  int iScore0, iScore1;
  boolean bNoBinary = 1;
  for (i=0; i<_pGoodList->iCount(); i++) {
    eID = _pGoodList->iVariable(i);
    if (_aAssignment[eID] == NON_VALUE) {
      if (_aBinaryCount0[eID] || _aBinaryCount1[eID]) {
	bNoBinary = 0;
      }
      // Compute iScore0
      if (_aBinaryCount0[eID] > 0 
	  && _aScore0[eID] != -1 
	  && _aBinaryCount0[eID] > _aScore0[eID]) {
	if (_bFastUnitPropagate(eID, 1, iScore0)) {
	  for (j=0; j<i; j++) {
	    eID2 = _pGoodList->iVariable(j);
	    if (_aAssignment[eID2] == NON_VALUE) {
	      _aScore0[eID2] = _aBinaryCount0[eID2];
	      _aScore1[eID2] = _aBinaryCount1[eID2];
	    }
	  }
	  for (j=i; j<_pGoodList->iCount(); j++) {
	    eID2 = _pGoodList->iVariable(j);
	    if (_aAssignment[eID2] == NON_VALUE) {
	      if (_aScore0[eID2] == -1) {
		_aScore0[eID2] = _aBinaryCount0[eID2];
	      }
	      if (_aScore1[eID2] == -1) {
		_aScore1[eID2] = _aBinaryCount1[eID2];
	      }
	    }		
	  }
	  bZeroFirst = 0;
	  //cout<<"eID="<<eID<<endl;
	  return eID;  // leads to contradiction
	} //if (_bFastUnit...
      }
      else {
	iScore0 = _aBinaryCount0[eID];
      }
      // Compute iScore1
      if (_aBinaryCount1[eID] > 0 
	  && _aScore1[eID] != -1
	  && _aBinaryCount1[eID] > _aScore1[eID]) {
	if (_bFastUnitPropagate(eID, 0, iScore1)) {
	  for (j=0; j<i; j++) {
	    eID2 = _pGoodList->iVariable(j);
	    if (_aAssignment[eID2] == NON_VALUE) {
	      _aScore0[eID2] = _aBinaryCount0[eID2];
	      _aScore1[eID2] = _aBinaryCount1[eID2];
	    }
	  }
	  for (j=i; j<_pGoodList->iCount(); j++) {
	    eID2 = _pGoodList->iVariable(j);
	    if (_aAssignment[eID2] == NON_VALUE) {
	      if (_aScore0[eID2] == -1) {
		_aScore0[eID2] = _aBinaryCount0[eID2];
	      }
	      if (_aScore1[eID2] == -1) {
		_aScore1[eID2] = _aBinaryCount1[eID2];
	      }
	    }		
	  }
	  bZeroFirst = 1;
	  //cout<<"eID="<<eID<<endl;
	  return eID;  // leads to contradiction
	} // if (_bFastUnit...
      }
      else {
	iScore1 = _aBinaryCount1[eID];
      }
      _aScore[eID] = _iCombineScores(iScore1, iScore0);
      if (_aScore[eID] > fBest) {
	fBest = _aScore[eID];
      }
    } // if (...NON_VALUE...
  } // for (int i=0;...

  if (bNoBinary) {
    _vComputeNoBinaryScores(fBest);

    if (fBest == -2.0) {
      return -1;
    }
  }

  _iBranchSelections++;
  // Danger: reusing _pUnitList
  for (i=0; i<_pGoodList->iCount(); i++) {
    eID = _pGoodList->iVariable(i);
    if (_aAssignment[eID] == NON_VALUE) {
      if (_aScore[eID]  >= fBest * _fFudgeFactor) {
	_pUnitList->vAdd(eID);
      }
    }
  }
  VariableID eReturn = _pUnitList->iVariable(Random::iRandom(_pUnitList->iCount()));
  _pUnitList->vClear();
  bZeroFirst = Random::iRandom(2);
  /*bZeroFirst = 
    _aBinaryCount1[eReturn] > 
    _aBinaryCount0[eReturn] ? 
    0 : 1;*/
  memcpy(_aScore0, _aBinaryCount0, _iVariableCount * sizeof(*_aScore1));
  memcpy(_aScore1, _aBinaryCount1, _iVariableCount * sizeof(*_aScore0));
  //cout<<"eReturn = "<<eReturn<<endl;
  return eReturn;
}

void SATSolver::_vComputeNoBinaryScores(double& fBest)
{
  // Scoring function for when there are no binary clauses.
  fBest = -2.0;
  int i;
  VariableID eID;
  int iScore0, iScore1;
  for (i=0; i<_pGoodList->iCount(); i++) {
    eID = _pGoodList->iVariable(i);
    if (_aAssignment[eID] == NON_VALUE) {
      iScore0 = _iScoreClauseList(_aVariableStruct[eID].xPositiveClauses.pEntry(0),
				  _aVariableStruct[eID].xPositiveClauses.pLastEntry());
      iScore1 = _iScoreClauseList(_aVariableStruct[eID].xNegativeClauses.pEntry(0),
				  _aVariableStruct[eID].xNegativeClauses.pLastEntry()); 
      _aScore[eID] = _iCombineScores(iScore0, iScore1);
      if (_aScore[eID] >= fBest) {
	fBest = _aScore[eID];
      }
    }
  }
}

boolean SATSolver::_bInitialize()
{
  // temp stuff
  _bReverse = 0;
  // Return 0 if problem is UNSAT due to unary clauses (node-consistency)
  _xLearnedClauses.vDestroy();
  _iSolutionCount = 0;
  _eCurrentID = -1;
  _iCurrentVariable = 0;
  _xSolutionCount.vSet(1);
  _xKnownSolutions.vSet(0);
  _iVariableCount = _pInstance->iVariableCount;
  _aBinaryCount0 = new long int[_iVariableCount];
  _aBinaryCount1 = new long int[_iVariableCount];
  _aAssignment = new DomainValue[_pInstance->iVariableCount];
  _aVariableStruct = new VariableStruct[_pInstance->iVariableCount];
  _aPositionToID = new VariableID[_pInstance->iVariableCount]; 
  _aIDToPosition = new long int[_pInstance->iVariableCount]; 
  _aScore0 = new int[_pInstance->iVariableCount]; 
  _aScore = new double[_pInstance->iVariableCount]; 
  _aScore1 = new int[_pInstance->iVariableCount]; 
  _pUnitVariables0 = new VariableSet(_pInstance->iVariableCount); 
  _pUnitVariables1 = new VariableSet(_pInstance->iVariableCount); 
  _pUnitList = new VariableList(_pInstance->iVariableCount);
  _pPositiveBackup = new VariableSet(_pInstance->iVariableCount); 
  _pNegativeBackup = new VariableSet(_pInstance->iVariableCount); 
  _pGoodList = new VariableSet(_pInstance->iVariableCount); 
  _pGoodReason = new VariableList(_pInstance->iVariableCount); 
  int i;
  for (i=0; i< _iVariableCount; i++) {
    _aAssignment[i] = NON_VALUE;
    _pGoodList->vAddVariableNoCheck(i);
    _aBinaryCount0[i] = _aBinaryCount1[i] = _aScore0[i] = _aScore1[i] = 0;
  }

  for (i=0; i<_pInstance->iClauseCount(); i++) {
    RClause* pClause = _pInstance->pClause(i);
    pClause->vReset(); 
    if (_bInitializeClause(pClause)) {
      return 0;
    }
  }
  for (i=0; i<_iVariableCount; i++) {
    _aVariableStruct[i].xPositiveClauses.vSortClausesByLength();
    _aVariableStruct[i].xNegativeClauses.vSortClausesByLength();
  }
  return 1;
}

boolean SATSolver::_bInitializeClause(RClause* pClause_)
{
  // returns 1 if instance is UNSAT
  if (pClause_->iVariableCount() == 1) {
    if (_bInitializeUnaryClause(pClause_)) {
      return 1;
    }
  }
  boolean bIsBinary = (pClause_->iVariableCount() == 2);
  for (int j=0; j<pClause_->iVariableCount(); j++) {
    if (pClause_->iIsNegated(j)) {
      _aVariableStruct[pClause_->eConstrainedVariable(j)].xNegativeClauses.vAddClause(pClause_);
      if (bIsBinary) {
	_aBinaryCount0[pClause_->eConstrainedVariable(j)]++;
      }
    }
    else {
      _aVariableStruct[pClause_->eConstrainedVariable(j)].xPositiveClauses.vAddClause(pClause_);
      if (bIsBinary) {
	_aBinaryCount1[pClause_->eConstrainedVariable(j)]++;
      }
    }
  }
  return 0;
}

boolean SATSolver::_bInitializeLearnedClause(RClause* pClause_)
{
  // returns 1 if instance is UNSAT
  // Call to initialize a clause learned during backtracking.
  for (int j=0; j<pClause_->iPermaCount(); j++) {
    if (pClause_->iIsNegated(j)) {
      _aVariableStruct[pClause_->eConstrainedVariable(j)].xNegativeClauses.vAddClause(pClause_);
      _aBinaryCount0[pClause_->eConstrainedVariable(j)]++;
    }
    else {
      _aVariableStruct[pClause_->eConstrainedVariable(j)].xPositiveClauses.vAddClause(pClause_);
      _aBinaryCount1[pClause_->eConstrainedVariable(j)]++;
    }
  }
  return 0;
}

boolean SATSolver::_bInitializeUnaryClause(RClause* pClause_)
{
  // returns 0 if problem is determined to be UNSAT 
  assert(pClause_->iVariableCount() == 1);
  VariableID eConstrainedVariable = pClause_->eConstrainedVariable(0);
  if (pClause_->iIsNegated(0)) {
    if (_pUnitVariables1->bHasVariable(eConstrainedVariable)) {
      return 1; // problem is UNSAT
    }
    if (_aVariableStruct[eConstrainedVariable].pReason == 0) {
      _pUnitVariables0->vAddVariable(eConstrainedVariable);
      _aVariableStruct[eConstrainedVariable].pReason = pClause_;
    }
  }
  else {
    if (_pUnitVariables0->bHasVariable(eConstrainedVariable)) {
      return 1; // problem is UNSAT
    }
    if (_aVariableStruct[eConstrainedVariable].pReason == 0) {    
      _pUnitVariables1->vAddVariable(eConstrainedVariable);
      _aVariableStruct[eConstrainedVariable].pReason = pClause_;
    }
  }
  return 0;
}

int SATSolver::_iScoreClauseList(register RClause** pStart_, RClause** const pEnd_) 
{
  // Score the clause list based on clause lengths. assumes no clauses are binary.
  int iCount = 0;
  for (; pStart_ < pEnd_; pStart_++) {
    if (!(*pStart_)->bIsSatisfied()) {
      switch ((*pStart_)->iWorkingLength()) {
      case 3:
	iCount += 256;
	break;
      case 4:
	iCount += 16;
	break;
      case 5:
	iCount += 4;
	break;
      default:
	iCount += 1;
      }
    }
  }
  return iCount;
}

void SATSolver::_vFastBackupScore()
{
  register RClause** pStart;
  RClause** pEnd;
  int i;
  const int iCount = _pUnitList->iCount();
  for (i=0; i<iCount; i++) {
    VariableID eUndo = _pUnitList->iVariable(i);
    if (_aAssignment[eUndo]) {
      pStart = _aVariableStruct[eUndo].xNegativeClauses.pEntry(0);
      pEnd = _aVariableStruct[eUndo].xNegativeClauses.pLastEntry();
      _aScore0[eUndo] = -1; // indicate no dead end
    }
    else {
      pStart = _aVariableStruct[eUndo].xPositiveClauses.pEntry(0);
      pEnd = _aVariableStruct[eUndo].xPositiveClauses.pLastEntry();
      _aScore1[eUndo] = -1; // indicate no dead end
    }
    for (; pStart < pEnd; pStart++) {
      (*pStart)->iExpand();
    }
    _aAssignment[eUndo] = NON_VALUE;
  } 
  _pUnitList->vClear();
}

void SATSolver::_vFastBackup(const int iToIndex_)
{
  register RClause** pStart;
  RClause** pEnd;
  int i;
  for (i=0; i<iToIndex_; i++) {
    VariableID eUndo = _pUnitList->iVariable(i);
    if (_aAssignment[eUndo]) {
      pStart = _aVariableStruct[eUndo].xNegativeClauses.pEntry(0);
      pEnd = _aVariableStruct[eUndo].xNegativeClauses.pLastEntry();
    }
    else {
      pStart = _aVariableStruct[eUndo].xPositiveClauses.pEntry(0);
      pEnd = _aVariableStruct[eUndo].xPositiveClauses.pLastEntry();
    }
    for (; pStart < pEnd; pStart++) {
      (*pStart)->iExpand();
    }
    _aAssignment[eUndo] = NON_VALUE;
  } 
  for (; i<_pUnitList->iCount(); i++) {
    _aAssignment[_pUnitList->iVariable(i)] = NON_VALUE;
  }
  _pUnitList->vClear();
}

boolean SATSolver::_bFastUnitPropagate(VariableID eWhich_, DomainValue iAssignment_, int& iScore_)
{
  // Return 1 if unit propagation leads to a contradiction.
  int i,j;
  RClause **pStart, **pEnd;
  RClause *pReduceMe;
  VariableID eID;

  assert(_pUnitList->iCount() == 0);
  _pUnitList->vAdd(eWhich_);
  _aAssignment[eWhich_] = iAssignment_;
  
  for (i=0; i<_pUnitList->iCount(); i++) {
    eID = _pUnitList->iVariable(i);
    if (_aAssignment[eID] == 0) {
      pStart = _aVariableStruct[eID].xPositiveClauses.pEntry(0);
      pEnd = _aVariableStruct[eID].xPositiveClauses.pLastEntry();
    }
    else {
      pStart = _aVariableStruct[eID].xNegativeClauses.pEntry(0);
      pEnd = _aVariableStruct[eID].xNegativeClauses.pLastEntry();
    }
    RClause **const pBegin = pStart;
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
  } // for (i=.n.
  iScore_ = _pUnitList->iCount();
  _vFastBackupScore();
  return 0;
}

boolean SATSolver::_bUnitPropagate()
{
  // Return 1 if unit propagation leads to a contradiction.
  // Does not back up, does not learn.
  // Simply leaves the list of contradicting variables intact.

  boolean bContradiction = 0;
  VariableStruct* pWork;
  int distance = 0;
  while(1) {
    if (_pUnitVariables1->iCount()) {
      int iWhich = Random::iRandom(_pUnitVariables1->iCount()); 
      _vLabelVariable(_pUnitVariables1->iVariable(iWhich), 1);
      _pUnitVariables1->vRemoveVariable(_eCurrentID);
      assert(_pGoodList->bHasVariable(_eCurrentID));
      pWork = &_aVariableStruct[_eCurrentID];
      _vSatisfyWithClauseList(pWork->xPositiveClauses.pEntry(0), 
			      pWork->xPositiveClauses.pLastEntry());
      if (_bFilterWithClauseList(pWork->xNegativeClauses.pEntry(0),
				 pWork->xNegativeClauses.pLastEntry())) {
	return 1;
      }
    }
    else if (_pUnitVariables0->iCount()) {
      int iWhich = Random::iRandom(_pUnitVariables0->iCount()); 
      _vLabelVariable(_pUnitVariables0->iVariable(iWhich), 0);
      _pUnitVariables0->vRemoveVariable(_eCurrentID);
      assert(_pGoodList->bHasVariable(_eCurrentID));
      pWork = &_aVariableStruct[_eCurrentID];
      _vSatisfyWithClauseList(pWork->xNegativeClauses.pEntry(0),
			      pWork->xNegativeClauses.pLastEntry());      
      if (_bFilterWithClauseList(pWork->xPositiveClauses.pEntry(0),
				 pWork->xPositiveClauses.pLastEntry())) {
	return 1;
      }
    }
    else {
      return 0;
    }
  } // while(1);
}

boolean SATSolver::_bFilterWithClauseList(RClause** pStart_, RClause** pEnd_)
{
  // return 1 if contradiction encountered.

  boolean bReturn = 0;
  RClause* pWorkClause;
  VariableID eContradictionID;
  RClause* pContradictionReason;
  int j;
  for (; pStart_ < pEnd_; pStart_++) {
    pWorkClause = *pStart_;
    if (!pWorkClause->bIsSatisfied()) {
      switch (pWorkClause->iReduce()) {
      case 2:
	for (j=0; j<pWorkClause->iPermaCount(); j++) {
	  if (pWorkClause->iIsNegated(j)) {
	    _aBinaryCount0[pWorkClause->eConstrainedVariable(j)]++;
	  }
	  else {
	    _aBinaryCount1[pWorkClause->eConstrainedVariable(j)]++;
	  }
	}
	break;
      case 1:
	// Find the variable it filters.
	for (int j = 0; ; j++) {
	  assert(j < pWorkClause->iPermaCount());
	  VariableID eID = pWorkClause->eConstrainedVariable(j);
	  if (_aAssignment[eID] == NON_VALUE) {
	    if (!_pGoodList->bHasVariable(eID)) {
	      // it's outside the goodlist
	      break;
	    }
	    // we've found the filtering variable
	    if (pWorkClause->iIsNegated(j)) {
	      if (_aVariableStruct[eID].pReason) {
		if (_pUnitVariables1->bHasVariable(eID)) {
		  // Contradiction!
		  // Can't return immediately because it will confuse the state undoing code
		  eContradictionID = eID;
		  pContradictionReason = pWorkClause;
		  bReturn = 1;
		}
		else {
		  assert(_pUnitVariables0->bHasVariable(eID));
		  _vDecideFilterClause(eID, pWorkClause);
		}
	      } // if (_...pReason)
	      else {
		assert(!_pUnitVariables1->bHasVariable(eID));
		assert(_pGoodList->bHasVariable(eID));
		_pUnitVariables0->vAddVariableNoCheck(eID);
		_aVariableStruct[eID].pReason = pWorkClause;
	      }
	    } // if (pWorkClause->iIsNegated(j)...
	    else  {
	      if (_aVariableStruct[eID].pReason) {
		if (_pUnitVariables0->bHasVariable(eID)) {
		  // Contradiction!
		  // Can't return immediately because it will confuse the state undoing code
		  eContradictionID = eID;
		  pContradictionReason = pWorkClause;
		  bReturn = 1;
		}		
		else {
		  assert(_pUnitVariables1->bHasVariable(eID));
		  _vDecideFilterClause(eID, pWorkClause);
		}
	      } // if (...pReason...
	      else {
		assert(!_pUnitVariables0->bHasVariable(eID));
		assert(_pGoodList->bHasVariable(eID));
		_pUnitVariables1->vAddVariableNoCheck(eID);
		_aVariableStruct[eID].pReason = pWorkClause;
	      }
	    }
	    break;
	  }
	} // for (int j=
      }
    }
  }
  if (bReturn) {
    _vSetContradiction(eContradictionID, pContradictionReason);
  }
  return bReturn;
}

inline void SATSolver::_vSetContradiction(VariableID eContradictionID_, RClause* pReason_)
{
  _eContradictionID = eContradictionID_;
  _pContradictionClause1 = pReason_;
  _pContradictionClause2 = _aVariableStruct[eContradictionID_].pReason;
  int i;
  for (i=0; i<_pUnitVariables0->iCount(); i++) {
    _aVariableStruct[_pUnitVariables0->iVariable(i)].pReason = 0;
  }
  _pUnitVariables0->vClear();
  for (i=0; i<_pUnitVariables1->iCount(); i++) {
    _aVariableStruct[_pUnitVariables1->iVariable(i)].pReason = 0;
  }
  _pUnitVariables1->vClear();
}

void SATSolver::_vSatisfyWithClauseList(register RClause** pStart_, RClause** pEnd_)
{
  RClause* pWorkClause;
  for (; pStart_ < pEnd_; pStart_++) { 
    pWorkClause = *pStart_;
    if (!pWorkClause->bIsSatisfied()) {
      if (pWorkClause->iWorkingLength() == 2 
	  ) {
	for (int j=0; j<pWorkClause->iPermaCount(); j++) {
	  if (pWorkClause->iIsNegated(j)) {
	    _aBinaryCount0[pWorkClause->eConstrainedVariable(j)]--;
	    assert(_aBinaryCount0[pWorkClause->eConstrainedVariable(j)] >= 0);
	  }
	  else {
	    _aBinaryCount1[pWorkClause->eConstrainedVariable(j)]--;
	    assert(_aBinaryCount1[pWorkClause->eConstrainedVariable(j)] >= 0);
	  }
	}
      }
    }
    pWorkClause->vMakeSatisfied();
  }
}

void SATSolver::_vCreateBackupClauseFromContradiction()
{
  // Create a new clause representing the nogood derived from the
  // current contradiction on _eContradictionID.
  _iContradictions++;
  _pPositiveBackup->vClear();
  _pNegativeBackup->vClear();
  int i;
  for (i=0; i<_pContradictionClause1->iVariableCount(); i++) {
    if (_pContradictionClause1->iIsNegated(i)) {
      _pNegativeBackup->vAddVariableNoCheck(_pContradictionClause1->eConstrainedVariable(i));
    }
    else {
      _pPositiveBackup->vAddVariableNoCheck(_pContradictionClause1->eConstrainedVariable(i));
    }
  }
  for (i=0; i<_pContradictionClause2->iVariableCount(); i++) {
    if (_pContradictionClause2->iIsNegated(i)) {
      _pNegativeBackup->vAddVariable(_pContradictionClause2->eConstrainedVariable(i));
    }
    else {
      _pPositiveBackup->vAddVariable(_pContradictionClause2->eConstrainedVariable(i));
    }
  }
  _pPositiveBackup->vRemoveVariable(_eContradictionID);
  _pNegativeBackup->vRemoveVariable(_eContradictionID);
}

inline void SATSolver::_vCreateNewBackupClause()
{
  RClause* pFailClause2;
  pFailClause2 = _aVariableStruct[_eCurrentID].pReason;
  for (int i=0; i<pFailClause2->iVariableCount(); i++) {
    VariableID eWorkID = pFailClause2->eConstrainedVariable(i);
    if (pFailClause2->iIsNegated(i)) {
      _pNegativeBackup->vAddVariable(eWorkID);
    }
    else {
      _pPositiveBackup->vAddVariable(eWorkID);
    }
  }  
  _pPositiveBackup->vRemoveVariableCheck(_eCurrentID);
  _pNegativeBackup->vRemoveVariableCheck(_eCurrentID);
}

inline RClause* SATSolver::_pLearn()
{
  if (_pPositiveBackup->iCount() + _pNegativeBackup->iCount() <= _iLearnOrder) {
    RClause* pLearn = new RClause(*_pPositiveBackup, *_pNegativeBackup);
    _xLearnedClauses.vAddClause(pLearn);
    int i = _iCurrentVariable-1;
    boolean bFoundFirst = 0;
    while(i >= 0) {
      VariableID eID = _aPositionToID[i];
      if (_pPositiveBackup->bHasVariable(eID) ||
	  _pNegativeBackup->bHasVariable(eID)) {
	if (!bFoundFirst) {
	  bFoundFirst = 1;
	}
	else break;
      }
      else if (bFoundFirst && _aVariableStruct[eID].bBranch) {
	_aVariableStruct[eID].xUnitClause.vAddClause(pLearn);
      }
      i--;
    }
    _bInitializeLearnedClause(pLearn);
    return pLearn;
  }
  else if (_bRelevanceBounding && _iLearnOrder) {
    // Learn a temporary clause
    int i = _iCurrentVariable-1;
    int iBranches = 0;
    int iPermaCount = 0;
    VariableID eResetID;
    assert(_pUnitVariables0->iCount() == 0);  // reusing this VariableSet
    assert(_pUnitVariables1->iCount() == 0);  // reusing this VariableSet for PermaVars
    while (1) {
      VariableID eID = _aPositionToID[i];
      assert(_aAssignment[eID] !=  NON_VALUE);
      if (_pPositiveBackup->bHasVariable(eID) || _pNegativeBackup->bHasVariable(eID)) {
	if (iPermaCount == _iLearnOrder) {
	  eResetID = eID;
	  break;
	}
	if (_aVariableStruct[eID].bBranch) {
	  iBranches++;
	}
	_pUnitVariables1->vAddVariableNoCheck(eID);
	iPermaCount++;
      } 
      else if (_aVariableStruct[eID].bBranch) {
	if (iPermaCount > 0) {
	  iBranches++;
	}
	if (iPermaCount == 1) {
	  _pUnitVariables0->vAddVariableNoCheck(eID);
	}	
      }
      i--;
      assert(i>=0);
    } // while
    //assert(_pUnitVariables1->iCount() == _iLearnOrder);
    if (iBranches > 0) {
      RClause* pLearn = new RClause(*_pPositiveBackup, *_pNegativeBackup, *_pUnitVariables1);
      _bInitializeLearnedClause(pLearn);
      _aVariableStruct[eResetID].xDeleteList.vAddClause(pLearn);
      for (int k=0; k<_pUnitVariables0->iCount(); k++) {
	_aVariableStruct[_pUnitVariables0->iVariable(k)].xUnitClause.vAddClause(pLearn);
      }
      _pUnitVariables0->vClear();
      _pUnitVariables1->vClear();
      return pLearn;
    }
    _pUnitVariables0->vClear();
    _pUnitVariables1->vClear();
  } // else 
  return 0;
}

inline void SATSolver::_vDeleteClauses(ClauseList& rClauseList_)
{
  // Delete these learned clauses since they are no longer relevant
  VariableStruct* pWork;
  RClause** pStart = rClauseList_.pEntry(0);
  RClause** pEnd = rClauseList_.pLastEntry();
  for (; pStart < pEnd; pStart++) {
    RClause* pDeleteMe = *pStart;

    for (int j=0; j<pDeleteMe->iPermaCount(); j++) {
      pWork = &(_aVariableStruct[pDeleteMe->eConstrainedVariable(j)]);
      if (pDeleteMe->iPermaCount() < 3) {
	if (pDeleteMe->iIsNegated(j)) {
	  pWork->xNegativeClauses.vDeleteClause(pDeleteMe);
	  _aBinaryCount0[pDeleteMe->eConstrainedVariable(j)]--;
	}
	else {
	  pWork->xPositiveClauses.vDeleteClause(pDeleteMe);
	  _aBinaryCount1[pDeleteMe->eConstrainedVariable(j)]--;
	}
      }
      else {
	if (pDeleteMe->iIsNegated(j)) {
	  pWork->xNegativeClauses.vDeleteClause(pDeleteMe);
	}
	else {
	  pWork->xPositiveClauses.vDeleteClause(pDeleteMe);
	}
      }
      assert(_aAssignment[pDeleteMe->eConstrainedVariable(j)] == NON_VALUE);
    }
    delete pDeleteMe;
  }
  rClauseList_.vClear();
}

inline void SATSolver::_vLabelVariable(VariableID eID_, DomainValue lWhich_)
{
  // Label a variable and make it current.
  //cout << "Labeling: " << eID_ << " to " << (int)lWhich_ << endl;
  assert(_aAssignment[eID_] == NON_VALUE);
  assert(_iCurrentVariable < _iVariableCount);
  _aAssignment[eID_] = lWhich_;
  _aIDToPosition[eID_] = _iCurrentVariable;
  _aPositionToID[_iCurrentVariable++] = eID_;
  _eCurrentID = eID_;
  _iVariablesLabeled++;
}

boolean SATSolver::_bBackup()
{
  // Returns 1 if the search is complete.
start:
  //^^ We goto start instead of call bBackup() recursively since some compilers don't
  // properly support tail recursion optimization.
  //_vLearnBranchClauseFromContradiction();
  _vCreateBackupClauseFromContradiction();
  if (_pPositiveBackup->iCount() == 0 && _pNegativeBackup->iCount() == 0) {
    return 1;
  }
  RClause* pJustLearned = _pLearn();

  VariableStruct* pWork;
  do { // while (1)
    pWork = &(_aVariableStruct[_eCurrentID]);
    if (pWork->pSolutionInfo) {
      pWork->pSolutionInfo->xSolutionCount.vSet(0);
      pWork->pReason = new RClause(*_pPositiveBackup, *_pNegativeBackup);
      delete pWork->pDeleteReason;
      pWork->pDeleteReason = pWork->pReason;
      // NASTY UGLY HACK
      // This hack is needed so that _vCreateGoodReason properly computes the reason.
      _bReverse = 1;
      // END NASTY UGLY HACK
      return _bSpecialBackup();
    }
    _vUndoClauseModifications();
    if (_pPositiveBackup->bHasVariable(_eCurrentID) ||
	_pNegativeBackup->bHasVariable(_eCurrentID)) {
      if (pWork->bBranch) {
	pWork->bBranch = 0;
	if (pJustLearned) {
	  pWork->pReason = pJustLearned;
	}
	else {
	  // No reason was created by the learning mechanism, so we need to create one.
	  pWork->pReason = new RClause(*_pPositiveBackup, *_pNegativeBackup);
	  delete pWork->pDeleteReason; // we lazily delete any old one if it exists
	  pWork->pDeleteReason = pWork->pReason;
	}
	if (_aAssignment[_eCurrentID] == 0) {  // try the opposite assignment
	  _pUnitVariables1->vAddVariable(_eCurrentID);
	}
	else {
	  _pUnitVariables0->vAddVariable(_eCurrentID);
	}
	_aAssignment[_eCurrentID] = NON_VALUE;
	_iCurrentVariable--;	
	if (_bNonTrivialUnitPropagate(pWork->xUnitClause)) {
	  _eCurrentID = _aPositionToID[_iCurrentVariable-1];
	  goto start; 
	}
	VariableID eRemember = _eCurrentID;
	if (_bUnitPropagate()) {
	  goto start; 
	}
	return 0;
      }
      else {
	_vCreateNewBackupClause();
	if (_pPositiveBackup->iCount() == 0 && _pNegativeBackup->iCount() == 0) {
	  return 1;
	}
	pJustLearned = _pLearn();
      }
    } // if (_pBackupSet->bHasVariable
    pWork->bBranch = 0;
    _aAssignment[_eCurrentID] = NON_VALUE;
    pWork->pReason = 0;
    pWork->xUnitClause.vClear();
    _iCurrentVariable--;
    assert(_iCurrentVariable != 0);
    _eCurrentID = _aPositionToID[_iCurrentVariable-1];
  } while(1);
}

void SATSolver::_vCreateGoodReason()
{
  // Create a reason that explains the solution count of the current subproblem.

  VariableStruct* pWork = &(_aVariableStruct[_eCurrentID]);

  int i;
  assert(_aAssignment[_eCurrentID] != NON_VALUE);

  RClause** pStart;
  RClause** pEnd;
  if (_bReverse) {
    if (_aAssignment[_eCurrentID] == 0 || !_aVariableStruct[_eCurrentID].pReason) {
      pStart = pWork->xNegativeClauses.pEntry(0);
      pEnd = pWork->xNegativeClauses.pLastEntry();
      _vUpdateGoodReason(pStart, pEnd, pWork->pSolutionInfo->xGoodReason);
    }
    if (_aAssignment[_eCurrentID] || !_aVariableStruct[_eCurrentID].pReason) {
      pStart = pWork->xPositiveClauses.pEntry(0);
      pEnd = pWork->xPositiveClauses.pLastEntry();
      _vUpdateGoodReason(pStart, pEnd, pWork->pSolutionInfo->xGoodReason);
    }
    _bReverse = 0;
  }
  else {
    if (_aAssignment[_eCurrentID] || !_aVariableStruct[_eCurrentID].pReason) {
      pStart = pWork->xNegativeClauses.pEntry(0);
      pEnd = pWork->xNegativeClauses.pLastEntry();
      _vUpdateGoodReason(pStart, pEnd, pWork->pSolutionInfo->xGoodReason);
    }
    if (_aAssignment[_eCurrentID] == 0 || !_aVariableStruct[_eCurrentID].pReason) {
      pStart = pWork->xPositiveClauses.pEntry(0);
      pEnd = pWork->xPositiveClauses.pLastEntry();
      _vUpdateGoodReason(pStart, pEnd, pWork->pSolutionInfo->xGoodReason);
    }
  }
  if (_aVariableStruct[_eCurrentID].pReason) {
    RClause* pReason = _aVariableStruct[_eCurrentID].pReason;
    for (int i=0; i<pReason->iVariableCount(); i++) {
      VariableID eID = pReason->eConstrainedVariable(i);
      if (eID != _eCurrentID && _aAssignment[eID] != NON_VALUE) {
	pWork->pSolutionInfo->xGoodReason.vAddVariable(pReason->eConstrainedVariable(i));
      }
    }
  }
  if (_pGoodReason->eTop() != -1) {
    assert(_aAssignment[_pGoodReason->eTop()] != NON_VALUE);
    pWork->pSolutionInfo->xGoodReason.vAddVariable(_pGoodReason->eTop());
  }
  pWork->pSolutionInfo->xGoodReason.vRemoveVariableCheck(_eCurrentID);
}

void SATSolver::_vUpdateGoodReason(RClause** pStart, 
				   RClause** pEnd, 
				   VariableSet& xGoodReason_)
{
  VariableStruct* pWork = &(_aVariableStruct[_eCurrentID]);
  int i;
  VariableID eID, eBest;
  RClause* pBestUnsatisfied = 0;
  for (; pStart < pEnd; pStart++) {
    if ((*pStart)->bLearned()) {  
      // Learned clauses are redundant, so they need not contribute to the reason.
      continue;
    }
    if ((*pStart)->bIsSatisfied()) {
      // Unlike reasons for contradiction, reasons for a positive solution count must consider
      // satisfied clauses, since if they were not satisfied, this might reduce the solution
      // count.
      int iEarliest = 9999999;
      for (i=0; i<(*pStart)->iPermaCount(); i++) {
	eID = (*pStart)->eConstrainedVariable(i);
	if (eID != _eCurrentID && _aAssignment[eID] != NON_VALUE) {
	  if ((_aAssignment[eID] && !(*pStart)->iIsNegated(i))
	      || (!_aAssignment[eID] && (*pStart)->iIsNegated(i))) {
	    // found a satisfying var.
	    if (_aIDToPosition[eID] < iEarliest) {
	      iEarliest = _aIDToPosition[eID];
	      eBest = eID;
	    }
	  }
	}
      } // for
      assert(iEarliest != 9999999);
      xGoodReason_.vAddVariable(eBest);
    }
  }
}

boolean SATSolver::_bSpecialBackup()
{
  // Back up from a state where solutions exist. Returns 1 if there are no more solutions.

  // WARNING: Convoluted code. There's a bunch of wierd stuff going on here which I just
  // haven't had time to clean up or properly document.

  VariableStruct* pWork;
  VariableStruct* pParentWork;
  VariableID eParentID;
  do { // while (1)
    pWork = &(_aVariableStruct[_eCurrentID]);
    _vUndoClauseModifications();
    if (!pWork->pSolutionInfo) {
      pWork->pSolutionInfo = new SolutionInfo(_iVariableCount);
    }
    SolutionInfo* pSolutionInfo = pWork->pSolutionInfo;
    if (pWork->bBranch) {
      // we have backed up to a branch
      //cout << "Branch: " << _eCurrentID << endl;
      assert(!pWork->pReason);
      pWork->bBranch = 0;
      _iCurrentVariable--;
      if (!_bFindAll) {
	_pGoodReason->vAdd(_eCurrentID);
	pSolutionInfo->pOldSolutionCount = new BigNum(pSolutionInfo->xSolutionCount);
	pSolutionInfo->xSolutionCount.vSet(1);
	_pGoodList->vClear();
	_pGoodList->vAppendNoCheck(pSolutionInfo->xGoodList);
	_pGoodList->vAddVariableNoCheck(_eCurrentID);
      }
      assert(_pUnitVariables0->iCount() == 0);
      assert(_pUnitVariables1->iCount() == 0);
      if (_aAssignment[_eCurrentID] == 0) {  // try the opposite assignment
	_aAssignment[_eCurrentID] = NON_VALUE;
	_vLabelVariable(_eCurrentID,1);
	_vSatisfyWithClauseList(pWork->xPositiveClauses.pEntry(0), 
				pWork->xPositiveClauses.pLastEntry());
	if (_bFilterWithClauseList(pWork->xNegativeClauses.pEntry(0),
				   pWork->xNegativeClauses.pLastEntry())) {
	  return _bBackup();
	}
      }
      else {
	_aAssignment[_eCurrentID] = NON_VALUE;
	_vLabelVariable(_eCurrentID,0);
	_vSatisfyWithClauseList(pWork->xNegativeClauses.pEntry(0), 
				pWork->xNegativeClauses.pLastEntry());
	if (_bFilterWithClauseList(pWork->xPositiveClauses.pEntry(0),
				   pWork->xPositiveClauses.pLastEntry())) {
	  return _bBackup();
	}
      }
      if (_bNonTrivialUnitPropagate(pWork->xUnitClause)) { 
	return _bBackup();
      }
      if (_bUnitPropagate()) {
	return _bBackup();
      }
      return 0;
    }
    if (_pGoodReason->eTop() == _eCurrentID) {
      _pGoodReason->ePop();
    }
    if (!_bFindAll) {
      _vCreateGoodReason();
      eParentID = _eFindDeepestID(pWork->pSolutionInfo->xGoodReason);
      assert(eParentID != _eCurrentID);
      // Add branch counts together
      if (pSolutionInfo->pOldSolutionCount) {
	pSolutionInfo->xSolutionCount += *(pSolutionInfo->pOldSolutionCount);
	if (pSolutionInfo->xSolutionCount > _xKnownSolutions) {
	  _xKnownSolutions.vSet(pSolutionInfo->xSolutionCount);
	}
	delete pSolutionInfo->pOldSolutionCount;
	pSolutionInfo->pOldSolutionCount = 0;
      }
      if (eParentID != -1) {
	pParentWork = &(_aVariableStruct[eParentID]);
	if (!pParentWork->pSolutionInfo) {
	  pParentWork->pSolutionInfo = pWork->pSolutionInfo;
	  pParentWork->pSolutionInfo->xGoodList.vAdd(_eCurrentID);
	}
	else {
	  pParentWork->pSolutionInfo->xGoodReason.vAppendVariables(pSolutionInfo->xGoodReason);
	  if (!pParentWork->pSolutionInfo->pOldSolutionCount) {
	    pParentWork->pSolutionInfo->xGoodList.vAppend(pSolutionInfo->xGoodList);
	    pParentWork->pSolutionInfo->xGoodList.vAdd(_eCurrentID);
	  }
	  pParentWork->pSolutionInfo->xSolutionCount *= pSolutionInfo->xSolutionCount;
	  if (pParentWork->pSolutionInfo->xSolutionCount > _xKnownSolutions) {
	    _xKnownSolutions.vSet(pParentWork->pSolutionInfo->xSolutionCount);
	  }
	  delete pSolutionInfo;
	}
      }
      else {
	_xSolutionCount *= pSolutionInfo->xSolutionCount;
	if (_xSolutionCount > _xKnownSolutions) {
	  _xKnownSolutions.vSet(_xSolutionCount);
	}
	delete pSolutionInfo;
	_eCurrentID = _aPositionToID[_iCurrentVariable-1];
      }
    } // if (!_bFindAll)
    else {
      // Finding all solutions instead of counting
      if (_iCurrentVariable) {
	eParentID = _aPositionToID[_iCurrentVariable-1];
	pParentWork = &(_aVariableStruct[eParentID]);
	if (!pParentWork->pSolutionInfo) {
	  pParentWork->pSolutionInfo = pWork->pSolutionInfo;
	}
	else {
	  delete pSolutionInfo;
	}
      }
      else {
	eParentID = -1;
	delete pSolutionInfo;
      }
    }
    pWork->pSolutionInfo = 0;
    _aAssignment[_eCurrentID] = NON_VALUE;
    pWork->pReason = 0;
    pWork->xUnitClause.vClear();
    _iCurrentVariable--;
    if (_iCurrentVariable == 0) {
      // the search is complete.
      assert(_pGoodReason->iCount() == 0);
      return 1;
    } 
    _eCurrentID = _aPositionToID[_iCurrentVariable-1];
  } while(1);
}

VariableID SATSolver::_eFindDeepestID(VariableList& xList_)
{
  int iDeepestIndex = -1;
  VariableID eBest = -1;
  for (int i=0; i<xList_.iCount(); i++) {
    VariableID eID = xList_.iVariable(i);
    if (_aIDToPosition[eID] > iDeepestIndex) {
      iDeepestIndex = _aIDToPosition[eID];
      eBest = eID;
    }
  }
  return eBest;
}

void SATSolver::_vPrintStack()
{
  int k;
  int iBranchDepth = 0;
  for (k=0; k<_iCurrentVariable; k++) {
    VariableID eWorkID = _aPositionToID[k];
    if (_aVariableStruct[eWorkID].bBranch) {
      iBranchDepth++;
    }
  }
  cout << "c   Stats: BD=" << iBranchDepth 
       << ", BP=" << _iBranchSelections 
       << ", CD=" << _iContradictions << endl;
  if (_iSolutionCount && !_bFindAll) {
    cout << "c     Solutions: ";
    char* aBuffer = _xKnownSolutions.aToString();
    cout << aBuffer << endl;
    delete [] aBuffer;
  }
  cout << "c     Stack: "; 
  int iNoBranch = 0;
  for (k=0; k<_iCurrentVariable; k++) {
    VariableID eWorkID = _aPositionToID[k];
    if (_aVariableStruct[eWorkID].bBranch) {
      cout << iNoBranch << ' ';
      iNoBranch = 0;
    }
    else {
      iNoBranch++;
    }
  }
  if (iNoBranch) 
    cout << iNoBranch;
  cout << endl;
}

void SATSolver::_vCleanup()
{
  delete [] _aAssignment;
  delete [] _aVariableStruct;
  delete [] _aPositionToID;
  delete [] _aIDToPosition;
  delete [] _aScore0;
  delete [] _aScore1;
  delete [] _aScore;
  delete [] _aBinaryCount0;
  delete [] _aBinaryCount1;
  delete _pUnitVariables0;
  delete _pUnitVariables1; 
  delete _pUnitList;
  delete _pPositiveBackup;
  delete _pNegativeBackup;
  delete _pGoodList;
  delete _pGoodReason;
}

inline void SATSolver::_vDecideFilterClause(VariableID eID, RClause* pWorkClause)
{
  if (_bFavorSmallClauses && 
      _aVariableStruct[eID].pReason->iVariableCount() > pWorkClause->iVariableCount()) {
    _aVariableStruct[eID].pReason = pWorkClause;
  }
  else {
    if (Random::bRandom()) {
      _aVariableStruct[eID].pReason = pWorkClause;
    }
  }
}

boolean SATSolver::_bNonTrivialUnitPropagate(ClauseList& xUnitClauses_)
{
  // Returns 1 if contradiction is reached
  RClause** pStart = xUnitClauses_.pEntry(0);
  RClause** pEnd = xUnitClauses_.pLastEntry();
  for (; pStart < pEnd; pStart++) {
    RClause* pClause = *pStart;
    VariableID eUnitVar;
    int x;
    for (x=0; ; x++) {
      assert(x<pClause->iPermaCount());
      if (_aAssignment[pClause->eConstrainedVariable(x)] == NON_VALUE) {
	eUnitVar = pClause->eConstrainedVariable(x);
	break;
      }
    }
    if (!_pGoodList->bHasVariable(eUnitVar)) {
      continue;
    }
    if (pClause->iIsNegated(x)) {
      if (!_aVariableStruct[eUnitVar].pReason) {
	assert(!_pUnitVariables1->bHasVariable(eUnitVar));
	_pUnitVariables0->vAddVariableNoCheck(eUnitVar);
	_aVariableStruct[eUnitVar].pReason = pClause;
      }
      else if (_pUnitVariables1->bHasVariable(eUnitVar)) {
	_vSetContradiction(eUnitVar, pClause);
	xUnitClauses_.vClear();
	return 1;
      }
      else {
	assert(_pUnitVariables0->bHasVariable(eUnitVar));
	_vDecideFilterClause(eUnitVar, pClause);
      }
    }
    else {
      if (!_aVariableStruct[eUnitVar].pReason) {
	assert(!_pUnitVariables0->bHasVariable(eUnitVar));
	_pUnitVariables1->vAddVariableNoCheck(eUnitVar);
	_aVariableStruct[eUnitVar].pReason = pClause;
      }
      else if (_pUnitVariables0->bHasVariable(eUnitVar)) {
	_vSetContradiction(eUnitVar, pClause);
	xUnitClauses_.vClear();
	return 1;
      }
      else {
	assert(_pUnitVariables1->bHasVariable(eUnitVar));
	_vDecideFilterClause(eUnitVar, pClause);
      }
    }
  }
  xUnitClauses_.vClear();
  return 0;
}

void SATSolver::_vUndoClauseModifications()
{
  RClause** pStart1;
  RClause** pEnd1;
  RClause** pStart2;
  RClause** pEnd2;
  VariableStruct* pWork = &(_aVariableStruct[_eCurrentID]);  

  if (_aAssignment[_eCurrentID] == 1) {
    pStart1 = pWork->xNegativeClauses.pEntry(0);
    pEnd1 = pWork->xNegativeClauses.pLastEntry();
    pStart2 = pWork->xPositiveClauses.pEntry(0);
    pEnd2 = pWork->xPositiveClauses.pLastEntry();
  }
  else {
    pStart2 = pWork->xNegativeClauses.pEntry(0);
    pEnd2 = pWork->xNegativeClauses.pLastEntry();
    pStart1 = pWork->xPositiveClauses.pEntry(0);
    pEnd1 = pWork->xPositiveClauses.pLastEntry();
  }
  RClause* pWorkClause;
  for (;pStart1 < pEnd1; pStart1++) {
    pWorkClause = *pStart1;
    if (pWorkClause->iExpand() == 3) {
      for (int j=0; j<pWorkClause->iPermaCount(); j++) {
	if (pWorkClause->iIsNegated(j)) {
	  _aBinaryCount0[pWorkClause->eConstrainedVariable(j)]--;
	  assert(_aBinaryCount0[pWorkClause->eConstrainedVariable(j)] >= 0);
	}
	else {
	  _aBinaryCount1[pWorkClause->eConstrainedVariable(j)]--;
	  assert(_aBinaryCount1[pWorkClause->eConstrainedVariable(j)] >= 0);
	}
      }   
    }
  }
  for (;pStart2 < pEnd2; pStart2++) {
    pWorkClause = *pStart2;
    pWorkClause->vMakeUnsatisfied();
    if (!pWorkClause->bIsSatisfied()) {
      if (pWorkClause->iWorkingLength() == 2
	  ) {
	for (int j=0; j<pWorkClause->iPermaCount(); j++) {
	  if (pWorkClause->iIsNegated(j)) {
	    _aBinaryCount0[pWorkClause->eConstrainedVariable(j)]++;
	  }
	  else {
	    _aBinaryCount1[pWorkClause->eConstrainedVariable(j)]++;
	  }
	}   
      }
    }
  }
  _vDeleteClauses(pWork->xDeleteList);
}

boolean SATSolver::_bTimeLimitExpired()
{
  time_t iCheckTime;
  if (!_bNoTimeLimit) {
    time(&iCheckTime);
    if (iCheckTime - _iElapsedTime >= _iMaxTime) {
      return 1;
    }
  }
  if (_bPrintStack) {
    time(&iCheckTime);
    if (iCheckTime - _iLastCheckTime >= _iPrintStackPeriod) {
      _vPrintStack();
      _iLastCheckTime = iCheckTime;
    }
  }
  return 0;
}
