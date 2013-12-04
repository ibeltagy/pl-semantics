/////////////////
// OS Includes
#include <fstream>
using namespace std;

//////////////
// Includes
#include "Clause.h"
#include "Random.h"
#include "SATInstance.h"
#include "SATSolver.h"


/////////////
// Defines

/////////////////////////////
// Static data initialization 

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Public Methods

SATInstance::~SATInstance() 
{
  vDestroy(); 
}

void SATInstance::vOutputDimacs(ostream& xUseMe_)
{
  xUseMe_ << "p cnf " << iVariableCount << ' ' << _iClauseCount << '\n';
  int i;
  for (i=0; i<_iClauseCount; i++) {
    RClause* pPrintMe = _aClause[i];
    if (!pPrintMe->bLearned()) {
      for (int j=0; j < pPrintMe->iVariableCount(); j++) {
	if (pPrintMe->iIsNegated(j))
	  xUseMe_ << "-";
	xUseMe_ << (int) (pPrintMe->eConstrainedVariable(j)+1) << " "; 
      }
      xUseMe_ << " 0\n";
    }
    xUseMe_ << flush;
  }

  xUseMe_ << "c NOTE: The following clauses are derived (non-essential):\n";
  for (i=0; i<_iClauseCount; i++) {
    RClause* pPrintMe = _aClause[i];
    if (pPrintMe->bLearned()) {
      for (int j=0; j < pPrintMe->iVariableCount(); j++) {
	if (pPrintMe->iIsNegated(j))
	  xUseMe_ << "-";
	xUseMe_ << (int) (pPrintMe->eConstrainedVariable(j)+1) << " "; 
      }
      xUseMe_ << " 0\n";
    }
    xUseMe_ << flush;
  }
}

boolean SATInstance::bReadDimacs(char* aFileName_)
{
  ifstream xInputFile(aFileName_);
  return bReadDimacs(xInputFile);
}

void SATInstance::vMakeRandom_3SAT(int iNumVars_, int iNumClauses_)
{
  iVariableCount = iNumVars_;
  VariableList xPositiveVars(iVariableCount);
  VariableList xNegativeVars(iVariableCount);
  for (int i=0; i<iNumClauses_; i++) {
    // Select three distinct variables at random, and negate at random
    // We do not worry about generating a duplicate clause.
    xPositiveVars.vClear();
    xNegativeVars.vClear();
    VariableID eVar1 = Random::iRandom(iNumVars_);
    VariableID eVar2;
    do {
      eVar2 = Random::iRandom(iNumVars_);
    } while (eVar2 == eVar1);
    VariableID eVar3;
    do {
      eVar3 = Random::iRandom(iNumVars_);
    } while (eVar3 == eVar2 || eVar3 == eVar1);
    
    if (Random::bRandom()) {
      xNegativeVars.vAdd(eVar1);
    }
    else {
      xPositiveVars.vAdd(eVar1);
    }
    if (Random::bRandom()) {
      xNegativeVars.vAdd(eVar2);
    }
    else {
      xPositiveVars.vAdd(eVar2);
    }

    if (Random::bRandom()) {
      xNegativeVars.vAdd(eVar3);
    }
    else {
      xPositiveVars.vAdd(eVar3);
    }
    RClause* pNewConstraint = new RClause(xPositiveVars, xNegativeVars, 1);
    pNewConstraint->vSortVariableList();
    vAddClause(pNewConstraint);
  }
}

boolean SATInstance::bReadDimacs(istream& xInputFile)
{
  cout << "c Reading instance..." << flush;
  char buffer[5000];
  VariableID eMaxVar = 0;
  int iClauseCount;
  char cCheck;

  while (1) {
    xInputFile >> cCheck;
    if (cCheck == 'c') {
      xInputFile.getline(buffer, 5000);
      continue;
    }
    else if (cCheck == 'p') {
      xInputFile >> buffer;
      xInputFile >> eMaxVar;
      xInputFile >> iClauseCount;
      break;
    }
    else {
      cout << "\nError: File not in DIMACS format?\n";
      return 0;
    }
  }
  VariableSet xPositiveVariables(eMaxVar);
  VariableSet xNegativeVariables(eMaxVar);

  iVariableCount = eMaxVar;
  int iWorkCount = 0;
  while (1) {
    if (xInputFile.eof()) {
      cout << "\nError: Unexpected end of file.\n";
      return 0;
    }
    xInputFile >> cCheck;
    if (cCheck == 'c') {
      xInputFile.getline(buffer, 5000);
      continue;
    }
    else xInputFile.putback(cCheck);

    xPositiveVariables.vClear();
    xNegativeVariables.vClear();
    boolean bIgnoreMe = 0;
    do {
      if (xInputFile.eof()) {
	cout << "\nError: Unexpected end of file.\n";
	return 0;
      }
      VariableID eVar;
      xInputFile >> eVar;
      if (eVar == 0)
	break;
      if (eVar > eMaxVar){
	cout << "\nError: some variable is numbered larger than the maximum.\n";
	return 0;
      }
      if (eVar < 0) {
	if (xPositiveVariables.bHasVariable(0-(eVar+1))) {
	  bIgnoreMe = 1;// Tautology
	}
	else {
	  xNegativeVariables.vAddVariable(0-(eVar+1));
	}
      }
      else {
	if (xNegativeVariables.bHasVariable(eVar-1)) {
	  bIgnoreMe = 1;// Tautology
	}
	else {
	  xPositiveVariables.vAddVariable(eVar-1);
	}
      }
    } while (1);

    if (!bIgnoreMe) {
      if (xNegativeVariables.iCount() + xPositiveVariables.iCount() == 0) {
	cout << "\nError: encountered a 0 length clause \n" << endl ;
	return 0;
      }
      // Silly up-casting needed to keep xlC compiler happy
      RClause* pNewConstraint = new RClause((VariableList&)xPositiveVariables, 
					  (VariableList&)xNegativeVariables,
					  1);
      pNewConstraint->vSortVariableList();
      vAddClause(pNewConstraint);
      iWorkCount++;
    }
    else iClauseCount--;
    if (iWorkCount == iClauseCount)
      break;
  }
  cout << "..done." << endl; 
  return 1;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Protected Methods

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Private Methods
