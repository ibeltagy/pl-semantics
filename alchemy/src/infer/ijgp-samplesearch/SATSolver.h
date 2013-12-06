#ifndef SS_SATSolver_h
#define SS_SATSolver_h

//////////////////////////////////////////////////////////////////////////////////////////////////
// SATSolver

/////////////////
// OS Includes
#include <time.h>

//////////////
// Includes
//#ifdef NO_GMP
//#include "BigNum_fake.h"
//#else

//#endif

#include "ClauseList.h"
#include "BigNum.h"

namespace ss{

/////////////
// Defines

////////////////////////
// Class Declarations
class SATInstance;
class SATPreprocessor;
class VariableList;
class VariableSet;

using namespace std;

//////////////////////////////////////////////////////////////////////////////////////////////////
// Class Definitions

enum relsat_enum { UNSAT, SAT, TIMEOUT };

class SolutionInfo {
public:
  SolutionInfo(int iVariableCount_) : xSolutionCount(1), xGoodList(iVariableCount_),
  xGoodReason(iVariableCount_), pOldSolutionCount(0) {}
  ~SolutionInfo() {
    delete pOldSolutionCount;
  }

  VariableList xGoodList;
  VariableSet xGoodReason;
  BigNum xSolutionCount;
  BigNum* pOldSolutionCount;
};

class VariableStruct {
 public:
  VariableStruct() : bBranch(0), pSolutionInfo(0), pReason(0), pDeleteReason(0) {}
  ~VariableStruct() {
    delete pSolutionInfo;
    delete pDeleteReason;
    xDeleteList.vDestroy();
  }
  // stores list of clauses containing positive occurence of this variable.
  ClauseList xPositiveClauses; 
  // stores list of clauses containing negative occurence of this variable.
  ClauseList xNegativeClauses; 
  RClause* pReason; // stores the clause that excludes some assignment
  RClause* pDeleteReason; // set if the reason needs to be deleted once assignment is changed
  ClauseList xUnitClause; // stores a list of non-trivial unit clauses
  // stores a list of clauses that must be deleted once assignment is changed.
  ClauseList xDeleteList; 
  boolean bBranch;
  SolutionInfo* pSolutionInfo;
};

class SATSolver {
  friend class SATPreprocessor;
public:
  SATSolver(SATInstance*);
  ~SATSolver();

  // Use to modify runtime parameters.
  void vSetLearnOrder(int iLearnOrder_) { _iLearnOrder = iLearnOrder_; }
  void vSetFindAll(boolean bFindAll_) { _bFindAll = bFindAll_; }
  void vSetSolutionLimit(int iMaxSolutions_) { _iMaxSolutions = iMaxSolutions_; }
  // ^^ set iMaxSolutions_ to 0 to find all solutions.
  void vSetTimeout(long int iSeconds_) { _iMaxTime = iSeconds_; _bNoTimeLimit = 0; }
  void vSetNoTimeLimit(boolean bWhich_) { _bNoTimeLimit = bWhich_; }
  void vSetFavorSmallClauses(boolean bWhich_) { _bFavorSmallClauses = bWhich_; }
  void vSetRelevanceBounding(boolean bWhich_) { _bRelevanceBounding = bWhich_; }
  void vSetPrintStack(boolean bWhich_) { _bPrintStack = bWhich_; }
  void vSetPrintStackPeriod(long int iSeconds_);
  void vSetFudgeFactor(float fTo_) { _fFudgeFactor = fTo_; }
  void vSetRestarts(boolean bRestart_) { _bRestarts = bRestart_; }
  void vSetRestartInterval(int iSeconds_);
  
  boolean bPreprocess(int iLevel_, int iIterationBound_);
  void vIncorporateLearnedClauses();
  void vOutputWarnings();
  void vOutputStatusUpdateInterval();
  relsat_enum eSolve();

  unsigned long iBranchSelections() {return _iBranchSelections;}
  unsigned long iVariablesLabeled() {return _iVariablesLabeled;}
  unsigned long iContradictionsDiscovered() {return _iContradictions;}
  time_t iElapsedSeconds() { return _iElapsedTime;}

  boolean bIsCounting() { return !_bFindAll; }
  BigNum* pSolutionCount() { return &_xSolutionCount; }  
  // ^^ Use only if bIsCounting(), after solving the instance.

private:
  boolean _bVerifySolution();
  boolean _bOutputSolution();
  boolean _bInitialize();
  boolean _bInitializeClause(RClause*);
  boolean _bInitializeLearnedClause(RClause*);
  boolean _bInitializeUnaryClause(RClause*);
  void _vCleanup();

  VariableID _eGiveMeTheBest(boolean& bZeroFirst_);
  void _vComputeNoBinaryScores(double& fBest_);
  VariableID _eMostDistantVariable();
  VariableID _eFindContradiction(boolean& bZeroFirst_, VariableID eID_);
  int _iScoreClauseList(RClause** pStart_, RClause** const pEnd_);
  inline double _iCombineScores(double i1_, double i2_);
  boolean _bLoop(boolean&);
  boolean _bRestartLoop(boolean&);

  boolean _bFastUnitPropagate(VariableID eWhich_, DomainValue iAssignment_, int& iScore_);

  void _vFastBackup(const int iIndex_);
  void _vFastBackupScore();

  boolean _bUnitPropagate();
  boolean _bNonTrivialUnitPropagate(ClauseList& xUnitClauses_);
  boolean _bBackup();
  boolean _bSpecialBackup();
  VariableStruct* _pBackupToFirstBranch();

  boolean _bFilterWithClauseList(RClause** pStart_, RClause** pEnd_);
  void _vSatisfyWithClauseList(RClause** pStart_, RClause** pEnd_);

  inline void _vDecideFilterClause(VariableID eID_, RClause* pWorkClause_);
  void _vCreateBackupClauseFromContradiction();
  void _vCreateNewBackupClause();
  RClause* _pLearn();

  void _vCreateGoodReason();
  void _vUpdateGoodReason(RClause** pStart, RClause** pEnd, VariableSet&);
  VariableID _eFindDeepestID(VariableList& xList_);

  inline void _vLabelVariable(VariableID, DomainValue);
  inline void _vDeleteClauses(ClauseList& rClauseList_);
  void _vUndoClauseModifications();
  inline void _vSetContradiction(VariableID, RClause*);

  boolean _bTimeLimitExpired();
  void _vPrintStack();

  // Preprocessor methods.
  void _vCleanClauseLists();
  boolean _bFastUnitPropagate();
  int _iFastUnitPropagate(VariableID& eUnit_);
  boolean _bMakesRedundant(RClause* pClause1_, RClause* pClause2_);
  boolean _bRedundancyCheck(RClause* pClause_);
  void _vOneSidedRedundancyCheck(RClause* pClause_);
  int _iRedundancyCheck(RClause* pClause1_, RClause* pClause2_);
  boolean _bResolve(RClause* pClause_, int& iCount_);
  boolean _bResolve(int& iCount_);
  RClause* _pResolve(RClause* pClause1_, 
		    RClause* pClause2_, 
		    VariableID iResolveVariable_);
  boolean _bUnitReduce(int& iNewClauses_);
  boolean _bBinaryReduce(VariableID eWith_, DomainValue lWhich_, int& iNewClauses_);
  void _vRemoveRedundancies(int iStartIndex_);
  RClause* _pReduceClause(RClause* pReduceMe_);
  boolean _bReduceClauses(int& iNewClauses_);
  boolean _bBinaryInfer(int& iNewClauses_);
  void _vCleanup(time_t iStart_, int iInitialClauseCount_);

  // Private data
  VariableID _eLastWasBranch;
  clock_t _iLastRestart;

  BigNum _xSolutionCount;
  BigNum _xKnownSolutions;
  long int _iSolutionCount; // when not counting all solutions, use this long

  unsigned long _iVariablesLabeled, _iBranchSelections, _iContradictions;
  time_t _iElapsedTime;
  time_t _iLastCheckTime;

  long int _iCurrentVariable;
  VariableID _eCurrentID;
  int _iVariableCount;
  VariableID _eContradictionID;
  RClause* _pContradictionClause1;
  RClause* _pContradictionClause2;

  VariableStruct* _aVariableStruct;
  VariableID* _aPositionToID; // Maps variable stack position to variable ID
  long int* _aIDToPosition; // Maps variable ID to stack position.
  int* _aScore0;
  int* _aScore1;
  double* _aScore;
  long int* _aBinaryCount0;
  long int* _aBinaryCount1;

  VariableSet* _pUnitVariables0;  // Variables with unit sized domains containing 0
  VariableSet* _pUnitVariables1;  // Variables with unit sized domains containing 1
  VariableSet* _pPositiveBackup;
  VariableSet* _pNegativeBackup;
  VariableList* _pUnitList;

  VariableSet* _pGoodList;
  VariableList* _pGoodReason;

  SATInstance* _pInstance;
  DomainValue* _aAssignment;
  ClauseList _xLearnedClauses;

  // Ugly hack vars.
  boolean _bReverse;

  // Preprocessor vars.
  VariableSet* _pSet0;
  VariableSet* _pSet1;

  // Runtime parameters:
  boolean _bNoTimeLimit;
  long int _iMaxTime;
  int _iLearnOrder;
  boolean _bFavorSmallClauses;
  boolean _bPrintStack;
  int _iPrintStackPeriod;
  boolean _bRelevanceBounding;
  float _fFudgeFactor;
  boolean _bFindAll;
  long int _iMaxSolutions;
  clock_t _iRestartInterval;
  boolean _bRestarts;
};

//////////////////////////////////////////////////////////////////////////////////////////////////
// Inlines

inline double SATSolver::_iCombineScores(double i1_, double i2_)
{
  return (2 * i1_ * i2_) + i1_ + i2_ + 1;
}
}
#endif // SATSolver_h

