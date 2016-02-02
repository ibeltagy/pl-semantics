/*
 * All of the documentation and software included in the
 * Alchemy Software is copyrighted by Stanley Kok, Parag
 * Singla, Matthew Richardson, Pedro Domingos, Marc
 * Sumner, Hoifung Poon, and Daniel Lowd.
 * 
 * Copyright [2004-07] Stanley Kok, Parag Singla, Matthew
 * Richardson, Pedro Domingos, Marc Sumner, Hoifung
 * Poon, and Daniel Lowd. All rights reserved.
 * 
 * Contact: Pedro Domingos, University of Washington
 * (pedrod@cs.washington.edu).
 * 
 * Redistribution and use in source and binary forms, with
 * or without modification, are permitted provided that
 * the following conditions are met:
 * 
 * 1. Redistributions of source code must retain the above
 * copyright notice, this list of conditions and the
 * following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the
 * above copyright notice, this list of conditions and the
 * following disclaimer in the documentation and/or other
 * materials provided with the distribution.
 * 
 * 3. All advertising materials mentioning features or use
 * of this software must display the following
 * acknowledgment: "This product includes software
 * developed by Stanley Kok, Parag Singla, Matthew
 * Richardson, Pedro Domingos, Marc Sumner, Hoifung
 * Poon, and Daniel Lowd in the Department of Computer Science and
 * Engineering at the University of Washington".
 * 
 * 4. Your publications acknowledge the use or
 * contribution made by the Software to your research
 * using the following citation(s): 
 * Stanley Kok, Parag Singla, Matthew Richardson and
 * Pedro Domingos (2005). "The Alchemy System for
 * Statistical Relational AI", Technical Report,
 * Department of Computer Science and Engineering,
 * University of Washington, Seattle, WA.
 * http://www.cs.washington.edu/ai/alchemy.
 * 
 * 5. Neither the name of the University of Washington nor
 * the names of its contributors may be used to endorse or
 * promote products derived from this software without
 * specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY OF WASHINGTON
 * AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE UNIVERSITY
 * OF WASHINGTON OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
 * IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 */
#ifndef MRF_H_SEP_23_2005
#define MRF_H_SEP_23_2005

#include <sys/times.h>
#include <sys/time.h>
#include <cstdlib>
#include <cfloat>
#include <fstream>
#include "timer.h"
#include "mln.h"
#include "groundpredicate.h"
#include "arguments.h"
#include <tr1/unordered_set>
#include <tr1/unordered_map>

extern bool afocusedGrounding;
extern char *  asamplesearchQueryFile;
extern int amcmcMaxSeconds;
const bool mrfdebug = false;

///////////////////////////////////////////////////////////////////////////

  // used as parameter of addGndClause()
struct AddGroundClauseStruct
{
  AddGroundClauseStruct(const GroundPredicateSet* const & sseenPreds,
                        GroundPredicateSet* const & uunseenPreds,
                        GroundPredicateHashArray* const & ggndPreds,
                        const Array<int>* const & aallPredGndingsAreQueries,
                        GroundClauseSet* const & ggndClausesSet,
                        Array<GroundClause*>* const & ggndClauses,
                        const bool& mmarkHardGndClauses,
                        const double* const & pparentWtPtr,
                        const int & cclauseId)
    : seenPreds(sseenPreds), unseenPreds(uunseenPreds), gndPreds(ggndPreds),
      allPredGndingsAreQueries(aallPredGndingsAreQueries),
      gndClausesSet(ggndClausesSet),
      gndClauses(ggndClauses), markHardGndClauses(mmarkHardGndClauses),
      parentWtPtr(pparentWtPtr), clauseId(cclauseId) {}
  
  ~AddGroundClauseStruct() {}
  
  const GroundPredicateSet* seenPreds;
  GroundPredicateSet* unseenPreds;
  GroundPredicateHashArray* gndPreds;
  const Array<int>* allPredGndingsAreQueries;
  GroundClauseSet* gndClausesSet;
  Array<GroundClause*>* gndClauses;
  const bool markHardGndClauses;
  const double* parentWtPtr;
  const int clauseId;
};

///////////////////////////////////////////////////////////////////////////


class MRF
{
 public:
    //allPredGndingsAreQueries[p] is 1 (true) if all groundings of predicate p 
    //are in queries, otherwise it is 0 (false). 
    //allPredGndingsAreQueries can be
    //NULL if none of the predicates have all their groundings as queries.
  MRF( GroundPredicateHashArray* const& queries, 
      const Array<int>* const & allPredGndingsAreQueries,
      const Domain* const & domain,  const Database * const & db, 
      const MLN* const & mln, const bool& markHardGndClauses,
      const bool& trackParentClauseWts, const int& memLimit)
  {

    cout << "creating mrf..." << endl; 



    Timer timer;
    GroundPredicateSet unseenPreds, seenPreds;
    GroundPredicateToIntMap gndPredsMap;
    GroundClauseSet gndClausesSet;
    gndPreds_ = new GroundPredicateHashArray;
    gndClauses_ = new Array<GroundClause*>;
    long double memNeeded = 0;


      // If too much memory to build MRF then destroy it
    if (memLimit > 0)
    {
      memNeeded = sizeKB();
      if (memNeeded > memLimit)
      {
        for (int i = 0; i < gndClauses_->size(); i++)
          delete (*gndClauses_)[i];
        delete gndClauses_;    

        for (int i = 0; i < gndPreds_->size(); i++)
          if ((*gndPreds_)[i]) delete (*gndPreds_)[i];
        delete gndPreds_;
                    
        throw 1;
      }
    }

    if (afocusedGrounding) //use theorem prover to check for inferrable and non-inferrable ground atoms 
      doFocusedGrounding(domain, mln);


    //add "unknown" GroundPredicates in queries to unseenPreds
    for (int i = 0; i < queries->size(); i++)
    {
      GroundPredicate* gp = (*queries)[i];
      if ( domain->getDB()->getValue(gp) != UNKNOWN)
        continue; // this is a knowned ground atom. No need to ground it
      unseenPreds.insert(gp);
      int gndPredIdx = gndPreds_->append(gp);
      assert(gndPredsMap.find(gp) == gndPredsMap.end());
      gndPredsMap[gp] = gndPredIdx;
    }
    //grounding backward starting from the query ground atoms
    cout << "number of ground query predicates = " << gndPreds_->size() << endl;

    //if query formula 
    if (asamplesearchQueryFile)
        //generate all possible ground clauses (of course taking the negative evidence -if any- into account )
    {
        for (int i = 0; i < mln->getNumClauses() ; i++)
        {
          Clause* c = const_cast<Clause*>( mln->getClause(i));
          const int clauseId = mln->findClauseIdx(c);  
          assert(clauseId >= 0);

          if (mrfdebug)
          {
            cout << "\tIn clause c: ";  c->printWithWtAndStrVar(cout, domain); cout << endl;
            cout << "number of grounded predicates = " << gndPreds_->size() << endl;
            cout << "number of grounded clauses = " << gndClauses_->size() << endl;
          }
        
          //ignore clause with zero weight
            if (c->getWt() == 0) continue;

            //add gnd clauses with unknown truth values to gndClauses_
            const double* parentWtPtr =
              (trackParentClauseWts) ? c->getWtPtr() : NULL;
            AddGroundClauseStruct agc(&seenPreds, &unseenPreds, gndPreds_,
                                      NULL,
                                      &gndClausesSet, gndClauses_,
                                      markHardGndClauses, parentWtPtr,
                                      clauseId);
          try
            {
              c->addUnknownClauses(domain, db, -1, NULL, &agc);
            }
            catch (bad_alloc&)
            {
              cout << "Bad alloc when adding unknown ground clauses to MRF!\n";
              cerr << "Bad alloc when adding unknown ground clauses to MRF!\n";
              throw 1;
            }

              // If too much memory to build MRF then destroy it
            if (memLimit > 0)
            {
              memNeeded = sizeKB();
              if (memNeeded > memLimit)
              {
                for (int i = 0; i < gndClauses_->size(); i++)
                  delete (*gndClauses_)[i];
                delete gndClauses_;    

                for (int i = 0; i < gndPreds_->size(); i++)
                  if ((*gndPreds_)[i]) delete (*gndPreds_)[i];
                delete gndPreds_;
        
                throw 1;
              }
            }
        }
    } //end of asamplesearchQueryFile
    else 
    {  
      //while there are still unknown preds we have not looked at
      while (!unseenPreds.empty())   
      {
        GroundPredicateSet::iterator predIt = unseenPreds.begin();
        GroundPredicate* pred = *predIt;
        unsigned int predId = pred->getId();
        //cout << "\tlooking at pred: ";  pred->print(cout, domain); cout << endl;

        bool genClausesForAllPredGndings = false;
          // if all groundings of predicate with predId are queries
        if (allPredGndingsAreQueries && (*allPredGndingsAreQueries)[predId] >= 1)
        {
            // if we have not generated gnd clauses containing the queries before
          if ((*allPredGndingsAreQueries)[predId] == 1) 
            genClausesForAllPredGndings = true;
          else
          {   //we have dealt with predicate predId already          
            //cout << "\terasing at pred: ";  pred->print(cout, domain); 
            //cout<< endl;
            unseenPreds.erase(predIt);
            seenPreds.insert(pred);
            continue;
          }
        }
          //get all clauses that contains pred with predId
        const Array<IndexClause*>* clauses
          = mln->getClausesContainingPred(predId);

          //for each clause, ground it and find those with unknown truth values,
          //dropping ground preds which do not matter to the clause's truth value
        cout << "\tFrom Pred: " <<  pred->getPredString(domain) << endl;
        
        for (int i = 0; clauses && i < clauses->size(); i++)
        {
          Clause* c = (*clauses)[i]->clause;
          cout << "\tIn clause c: ";  c->printWithWtAndStrVar(cout, domain); cout << endl;
          const int clauseId = mln->findClauseIdx(c);  
          assert(clauseId >= 0);
      
          //ignore clause with zero weight
          if (c->getWt() == 0) continue;

            //add gnd clauses with unknown truth values to gndClauses_
          const double* parentWtPtr =
            (trackParentClauseWts) ? c->getWtPtr() : NULL;
          AddGroundClauseStruct agc(&seenPreds, &unseenPreds, gndPreds_,
                                    allPredGndingsAreQueries,
                                    &gndClausesSet, gndClauses_,
                                    markHardGndClauses, parentWtPtr,
                                    clauseId);

        try
          {
            addUnknownGndClauses(pred, c, domain, db, genClausesForAllPredGndings,
                                 &agc);
          }
          catch (bad_alloc&)
          {
            cout << "Bad alloc when adding unknown ground clauses to MRF!\n";
            cerr << "Bad alloc when adding unknown ground clauses to MRF!\n";
            throw 1;
          }

            // If too much memory to build MRF then destroy it
          if (memLimit > 0)
          {
            memNeeded = sizeKB();
              //cout << "preds " << gndPreds_->size() << endl;
              //cout << "clauses " << gndClauses_->size() << endl;
              //cout << "memory " << memNeeded << endl;
              //cout << "limit " << memLimit << endl;
            if (memNeeded > memLimit)
            {
              for (int i = 0; i < gndClauses_->size(); i++)
                delete (*gndClauses_)[i];
              delete gndClauses_;    

              for (int i = 0; i < gndPreds_->size(); i++)
                if ((*gndPreds_)[i]) delete (*gndPreds_)[i];
              delete gndPreds_;
      
              throw 1;
            }
          }
        }

        //clauses with negative wts are handled by the inference algorithms

        //if all the gnd clauses that pred appears in have known truth value,
        //it is not added to gndPreds_ and excluded from the MCMC network

        //cout << "\terasing pred: ";  pred->print(cout, domain); cout << endl;
        unseenPreds.erase(predIt);
        seenPreds.insert(pred);
        if (genClausesForAllPredGndings)
        {
          assert(allPredGndingsAreQueries && 
                 (*allPredGndingsAreQueries)[predId]==1);
            //indicate we have seen all groundings of pred predId
          (*allPredGndingsAreQueries)[predId]++;
        }
      }//while (!unseenPreds.empty())

    } //end of grounding backwork from the queries 

    cout << "number of grounded predicates = " << gndPreds_->size() << endl;
    cout << "number of grounded clauses = " << gndClauses_->size() << endl;

    if (gndClauses_->size() == 0)
      cout<< "Markov blankets of query ground predicates are empty" << endl;

    if (mrfdebug)
    {
      cout << "Ground predicates in MRF: " << endl;
      for (int i = 0; i < gndPreds_->size(); i++)
      {
        (*gndPreds_)[i]->print(cout, domain);
        cout << endl;
      }

      cout << "Clauses in MRF: " << endl;
      for (int i = 0; i < gndClauses_->size(); i++)
      {
        (*gndClauses_)[i]->print(cout, domain, gndPreds_);
        cout << endl;
      }
    }
      // Compress preds
    for (int i = 0; i < gndPreds_->size(); i++)
      (*gndPreds_)[i]->compress();

    gndPreds_->compress();
    gndClauses_->compress();

    cout <<"Time taken to construct MRF = ";
    Timer::printTime(cout,timer.time());
    cout << endl;
  }

  /**
   * Computes and returns size of the mrf in kilobytes
   */
  long double sizeKB()
  {
      // # of ground clauses times memory for a ground clause +
      // # of ground predicates times memory for a ground predicate
    long double size = 0;
    for (int i = 0; i < gndClauses_->size(); i++)
      size += (*gndClauses_)[i]->sizeKB();
    for (int i = 0; i < gndPreds_->size(); i++)
      size += (*gndPreds_)[i]->sizeKB();

    return size;    
  }

    // Do not delete the clause and truncClause argument.
    // This function is tightly bound to Clause::createAndAddUnknownClause().
  static void addUnknownGndClause(const AddGroundClauseStruct* const & agcs, 
                                  const Clause* const & clause,
                                  const Clause* const & truncClause,
                                  const bool& isHardClause)
  {
    const GroundPredicateSet* seenPreds     = agcs->seenPreds;
    GroundPredicateSet*       unseenPreds   = agcs->unseenPreds;
    GroundPredicateHashArray* gndPreds      = agcs->gndPreds;
    const Array<int>* allGndingsAreQueries  = agcs->allPredGndingsAreQueries;
    GroundClauseSet*          gndClausesSet = agcs->gndClausesSet;
    Array<GroundClause*>*     gndClauses    = agcs->gndClauses;
    const bool markHardGndClauses           = agcs->markHardGndClauses;
    const double* parentWtPtr               = agcs->parentWtPtr;
    const int clauseId                      = agcs->clauseId;

    // Check none of the grounded clause's predicates have been seen before.
    // If any of them have been seen before, this clause has been created 
    // before (for that seen predicate), and can be ignored

      // Check the untruncated ground clause whether any of its predicates
      // have been seen before
    bool seenBefore = false;
    for (int j = 0; j < clause->getNumPredicates(); j++)
    {
      Predicate* p = clause->getPredicate(j);
      GroundPredicate* gp = new GroundPredicate(p);
      if (seenPreds->find(gp) !=  seenPreds->end() ||
          (allGndingsAreQueries && (*allGndingsAreQueries)[gp->getId()] > 1) )
      { 
        seenBefore = true;
        delete gp;
        break;
      }
      delete gp;
    }

    if (seenBefore) return;

    GroundClause* gndClause = new GroundClause(truncClause, gndPreds);
    if (markHardGndClauses && isHardClause) gndClause->setWtToHardWt();
    assert(gndClause->getWt() != 0);

    bool invertWt = false;
      // We want to normalize soft unit clauses to all be positives
//NO we do not. Keep them as they are. I want to avoid all negative weights
//    if (!isHardClause && gndClause->getNumGroundPredicates() == 1 &&
//        !gndClause->getGroundPredicateSense(0))
//    {
//      gndClause->setGroundPredicateSense(0, true);
//      gndClause->setWt(-gndClause->getWt());
//      invertWt = true;
//    }

    GroundClauseSet::iterator iter = gndClausesSet->find(gndClause);
      // If the unknown clause is not in gndClauses
    if (iter == gndClausesSet->end())
    {
      gndClausesSet->insert(gndClause);
      gndClauses->append(gndClause);
      gndClause->appendToGndPreds(gndPreds);
        // gndClause's wt is set when it was constructed
      if (parentWtPtr)
        gndClause->incrementClauseFrequency(clauseId, 1, invertWt);

        // Add the unknown predicates of the clause to unseenPreds if 
        // the predicates are already not in it
      for (int j = 0; j < gndClause->getNumGroundPredicates(); j++)
      {
        GroundPredicate* gp =
          (GroundPredicate*)gndClause->getGroundPredicate(j, gndPreds);
        assert(seenPreds->find(gp) == seenPreds->end());
          // if the ground predicate is not in unseenPreds
        GroundPredicateSet::iterator it = unseenPreds->find(gp);
        if (it == unseenPreds->end())
        {
          //cout << "\tinserting into unseen pred: ";  
          //pred->print(cout, domain); cout << endl;
          unseenPreds->insert(gp);
        }
      }
    }
    else
    {  // gndClause has appeared before, so just accumulate its weight
      (*iter)->addWt(gndClause->getWt());

      if (parentWtPtr)
        (*iter)->incrementClauseFrequency(clauseId, 1, invertWt);

      delete gndClause;
    }
  } //addUnknownGndClause()



  ~MRF()
  {
    for (int i = 0; i < gndClauses_->size(); i++)
      if ((*gndClauses_)[i]) delete (*gndClauses_)[i];
    delete gndClauses_;    

    for (int i = 0; i < gndPreds_->size(); i++)
      if ((*gndPreds_)[i]) delete (*gndPreds_)[i];
    delete gndPreds_;
  }

  void deleteGndPredsGndClauseSets()
  {
    for (int i = 0; i < gndPreds_->size(); i++)
      (*gndPreds_)[i]->deleteGndClauseSet();
  }  

  const GroundPredicateHashArray* getGndPreds() const { return gndPreds_; }

  const Array<GroundClause*>* getGndClauses() const { return gndClauses_; }

 private:

  void addUnknownGndClauses(const GroundPredicate* const& queryGndPred,
                            Clause* const & c, const Domain* const & domain, 
                            const Database* const & db, 
                            const bool& genClauseForAllPredGndings,
                            const AddGroundClauseStruct* const & agcs)
  {
    
    if (genClauseForAllPredGndings)
      c->addUnknownClauses(domain, db, -1, NULL, agcs);
    else
    {
      for (int i = 0; i < c->getNumPredicates(); i++)
      {
        if (c->getPredicate(i)->canBeGroundedAs(queryGndPred))
          c->addUnknownClauses(domain, db, i, queryGndPred, agcs);
      }
    }
  } 


  //>>>>>>>>>>>>>>>>>> begin focused-grounding functions

  char negationSymbol = '~';

  string & toProver9String (string & s)
  {
    bool isVarFirstChar = false;
    for(std::string::size_type i = 0; i < s.size(); ++i) 
    {
      if (s[i] == '!')
        s[i] = negationSymbol;
      else if (s[i] == 'v' && i > 0 && s[i-1] == ' ' && s[i+1] == ' ')
        s[i] = '|';
      else if (isVarFirstChar ) //order is important
      {
        isVarFirstChar = false;
        if (isupper(s[i]))
          s[i]=tolower(s[i]);
        else if (islower(s[i]))
          s[i]=toupper(s[i]);
        else if (s[i] == '"')
          isVarFirstChar = true;  // still at the first char, pred("var")
        else
        {
          cerr << "string can not be changed to Prover9 format: "  << s<< endl;
          throw 1;
        }
      }
      else if (s[i] == ',' || s[i] == '(' )
        isVarFirstChar = true;
    }
    return s;
  }

  void readEProverOutput (char * tmpOutputFile, std::tr1::unordered_set<std::string> * inferrable, std::tr1::unordered_map<int, int> & eskMap)
  {
    //Open tmpOutputFile and read lines of the format: 
    //# SZS answers ?- ~$answer(esk4_2(john,mary)). Tuple [[john, mary]|_]

    std::ifstream infile(tmpOutputFile);

    std::string line;
    while (std::getline(infile, line))
    {
        string linePrefix =  "# SZS answers ?- ~$answer(esk";
        string groundAtomSuffix = ")). Tuple [";
        
        switch (line.find(linePrefix))
        {
          case -1: continue; //ignore the line
          case 0: break; //process the line after the switch case
          default: cerr << "unexpected output: " << line << endl;
                    throw 1;
        }
        if (mrfdebug)
          cout << line << endl;

        int groundAtomSuffixIndx = line.find(groundAtomSuffix);
        int underscoreIndx = line.find ("_");
        int openBracketIndx = line.find("(", underscoreIndx); //search for the first open bracket after the "_"

        string questionIdStr = line.substr(linePrefix.length(), underscoreIndx - linePrefix.length());

        int questionId =  0;
        istringstream ( questionIdStr ) >> questionId;

        if (mrfdebug)
          cout << "questionIdStr: " << questionIdStr << ", questionId Int" << questionId;

        string tuple = line.substr(openBracketIndx + 1, groundAtomSuffixIndx - openBracketIndx -1 );
        if (mrfdebug)
          cout << ", tuple: " << tuple << endl;

        if (tuple.find("~$answer(esk") != std::string::npos)
        {
          if (mrfdebug)
            cout << "invalid ground atom" << endl;
          continue;
        }
        inferrable[eskMap[questionId]].insert (tuple);
    }
    infile.close();
  }

  void doFocusedGrounding (const Domain* const & domain, const MLN* const & mln)
  {
      //prepare the tmp file
      char tmpFileName [L_tmpnam];
      ofstream prover9file;
      tmpnam (tmpFileName);
      cout << "tmpFileName #1: " << tmpFileName << endl;
      
      char tmpOutputFile [L_tmpnam];
      tmpnam (tmpOutputFile);
      cout << "tmpOutputFile #1: " << tmpOutputFile << endl;

      prover9file.open (tmpFileName);
      prover9file << "hi" << endl;
      prover9file.close();

      //get the command line to run prover9
      string progName(ARGS::progName);
      string inferBin = "infer";
      if ( ! (progName.size() >= inferBin.size() &&
               progName.compare(progName.size() - inferBin.size(), inferBin.size(), inferBin) == 0 ) )
      {
        cerr << "invalid command line: " <<progName << endl;
        throw 1;
      }

      //""Assuming the right search policy in the theorem proof"", 5 seconds is actually a lot. 
      //What the theorem proof can not find in 5 seconds, Alchemy can not process in any reasonable time
      int timelimit = 5; 

      stringstream prover9path ;
      prover9path << progName.substr(0, progName.size() - inferBin.size()) << "/../E/bin/eprover --tptp3-in --answers " ; 
      prover9path << " --output-file=" << tmpOutputFile << " "; 
      prover9path << " --output-level=0 "; 
      prover9path << " --auto ";  //automatically choose the search policy and search parameters 
      prover9path << " --soft-cpu-limit=" << timelimit << " " ;
      prover9path << tmpFileName;
      char proverBin [1024];
      strcpy(proverBin, prover9path.str().c_str());  
      
      cout << "E: " << proverBin << endl;

      int ruleIdx = 0;
      std::stringstream prover9str;

      std::stringstream ss;

      //print clauses in the tmp file
      for (int i = 0; i < mln->getNumClauses() ; i++)
      {
        ss.str(std::string());
        Clause* c = const_cast<Clause*>( mln->getClause(i));
        c->printWithoutWtWithStrVar(ss, domain);
        std::string s = ss.str();
        //fof(cool2,definition,(  ( human(X) => philosopher(X)  ))).
        prover9str << "fof(r_"  << ruleIdx++ << ",axiom,(" <<toProver9String(s)  << "))." << endl;
      }

      //print evidence ground atoms
      int predCnt = domain->getNumPredicates();
      for (int i = 0; i < predCnt; i++)
      {  
        Array<Predicate*>* indexedGndings = new Array<Predicate*>();
        
        //Print True evidence
        domain->getDB()->getTrueGndings(i, indexedGndings);
        for(int k=0;k<indexedGndings->size();k++)
        {
          ss.str(std::string());
          GroundPredicate* gpTmp = new GroundPredicate((*indexedGndings)[k]);
          gpTmp->print(ss, domain);
          std::string s = ss.str();
          prover9str << "fof(r_"  << ruleIdx++ << ",axiom,(" <<toProver9String(s)  << "))." << endl;
        }
        indexedGndings->deleteItemsAndClear();

        //Print False evidence
        bool isClosedWorld = domain->getDB()->isClosedWorld(i);
        if (isClosedWorld)
        {
          Array<Predicate*> predArr;
          Predicate::createAllGroundings(i, domain, predArr);
          int numPreds = predArr.size();
          for (int j = 0; j < numPreds; j++)
          {
            Predicate* newPred = predArr[j];
            TruthValue tv = domain->getDB()->getValue(newPred);
            if ( tv == FALSE )
            { 
              ss.str(std::string());
              newPred->printWithStrVar(ss, domain);
              std::string s = ss.str();
              prover9str << "fof(r_"  << ruleIdx++ << ",axiom,(" << "~" << toProver9String(s)  << "))." << endl;
            }
          }
          predArr.deleteItemsAndClear();
        }
        else
        {
          domain->getDB()->getFalseGndings(i, indexedGndings);
          for(int k=0;k<indexedGndings->size();k++)
          {
            ss.str(std::string());
            GroundPredicate* gpTmp = new GroundPredicate((*indexedGndings)[k]);
            gpTmp->print(ss, domain);
            std::string s = ss.str();
            prover9str << "fof(r_"  << ruleIdx++ << ",axiom,(" << "~" << toProver9String(s)  << "))." << endl;
          }
          indexedGndings->deleteItemsAndClear();
        }
      }

      string prover9string = prover9str.str();

      prover9file.open (tmpFileName);
      prover9file << prover9string;
      prover9file.close();

      //array of set of tuples. Each tuple is a string
      //leave index 0 empty because X in eskX_Y starts from 1
      std::tr1::unordered_set<std::string> *  inferrable = new std::tr1::unordered_set<std::string> [predCnt + 1] ;

      std::tr1::unordered_map<int, int>  eskMap ; //map question index to predicate index 

      //iterate twice, one time with for positive ground atoms and another for negatives.
      char isNegated [] = {' ', negationSymbol }; 
      for (int ch = 0; ch < 2 ; ch ++ )
      {
        prover9file.open (tmpFileName);
        prover9file << prover9string;

        ruleIdx = 0;
        for (int i = 0; i < predCnt; i++)
        {
          bool isClosedWorld = domain->getDB()->isClosedWorld(i);
          if (isClosedWorld)
            continue;

          const PredicateTemplate*  predTemp = domain->getPredicateTemplate(i);

          ss.str(std::string());
          predTemp->printWithStrVar(ss);
          std::string s = ss.str();
          
          toProver9String(s);  

          int openBracketIdx = s.find("(");
          string terms = s.substr(openBracketIdx + 1, s.length() - openBracketIdx - 2);
          prover9file << "fof(q_"  << ++ruleIdx << ",question,( ? [" << terms << "] : (" << isNegated[ch] << s << ")))." << endl;
          eskMap[ruleIdx] = i;
        }
        prover9file.close();
        
        cout << "Running theorem prover iteration " << ch + 1 << " ... " << endl;
        int status = system(proverBin);
        int exitCode = WEXITSTATUS(status);
        if (exitCode == 8 )
              cerr << "E timesout with error code: " << exitCode << endl;
        else if (exitCode != 0 && exitCode != 1)
        {
          cerr << "E exit with error: " << exitCode << endl;
          throw 1;
        }
        //parse the output file and fill in the set of inferrable 
        readEProverOutput (tmpOutputFile, inferrable, eskMap);
      }

      /*cout << "print inferrables" << endl;
      for (int i = 0; i < predCnt; i++)
      {
        bool isClosedWorld = domain->getDB()->isClosedWorld(i);
        if (isClosedWorld)
          continue;

        const PredicateTemplate*  predTemp = domain->getPredicateTemplate(i);
        ss.str(std::string());
        predTemp->printWithStrVar(ss);
        std::string s = ss.str();
        if (mrfdebug)
        {
          cout << "checking pred: " << s <<  ", inferables: " << inferrable[i].size() << endl;
          //for (int j = 0; j< inferrable[i].size(); j++)
          for (std::tr1::unordered_set<string>::iterator itr = inferrable[i].begin(); itr != inferrable[i].end(); ++itr)
          {
            cout << (*itr) << endl;
          }
        }
      }
      cout << "done print inferrables" << endl;
      */

      cout << "Setting False/Unknown flags ... " << endl;

      // keep just the inferrables 
      int inferrablesCnt = 0;
      for (int i = 0; i < predCnt; i++)
      {
        bool isClosedWorld = domain->getDB()->isClosedWorld(i);
        if (isClosedWorld)
          continue;

        const PredicateTemplate*  predTemp = domain->getPredicateTemplate(i);
        ss.str(std::string());
        predTemp->printWithStrVar(ss);
        std::string s = ss.str();
        Array<Predicate*> predArr;
        Predicate::createAllGroundings(i, domain, predArr);
        int numPreds = predArr.size();
        for (int j = 0; j < numPreds; j++)
        {
          Predicate* newPred = predArr[j];
          TruthValue tv = domain->getDB()->getValue(newPred);
          if (tv == UNKNOWN)
          {
            ss.str(std::string());
            newPred->printWithStrVar(ss, domain);
            std::string s = ss.str();
            size_t openBracketIdx = s.find("(");
            toProver9String(s);
            string tuple = s.substr(openBracketIdx + 1, s.length() - openBracketIdx - 2) ; 

            std::tr1::unordered_set<std::string>::const_iterator got = inferrable[i].find (tuple);

            if ( got == inferrable[i].end() )
            {
              newPred->setTruthValue(FALSE);
              domain->getDB()->setEvidenceStatus(newPred, true);
              domain->getDB()->setValue(newPred, FALSE);
            }
            else 
            {
              inferrablesCnt ++;
              if (mrfdebug)
                std::cout << "inferrable: " <<  s << " with tuple: " << tuple <<  endl;
            }
          }
        }

      }

      cout << "Done setting flags with Unknowns count =  " << inferrablesCnt << endl ;
     
  }//>>>>>>>>>>>>>>>>>>>end of focused-grounding

 public:

  const int getNumGndPreds()
  {
    return gndPreds_->size();
  }

  const int getNumGndClauses()
  {
    return gndClauses_->size();
  }

 private:
  GroundPredicateHashArray* gndPreds_;
  Array<GroundClause*>* gndClauses_;
};


#endif
