// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include "parser/_printer.h"
#include "parser/print_deps.h"
#include "parser/print_bankdeps.h"

using namespace std;

namespace NLP { namespace CCG {

void
BankDepsPrinter::header(const std::string &PREFACE){
  out.stream << PREFACE << endl;
  log.stream << PREFACE << endl;
}

void
BankDepsPrinter::unary(Sentence &sent){
  out.stream << "(<L " << sent.msuper[0][0].raw << ' ';
  out.stream << sent.pos[0] << ' ' << sent.pos[0];
  out.stream << sent.msuper[0][0].raw << ">)" << endl;
}

void
BankDepsPrinter::recurse_ccgbank(const KBest &kb, Sentence &sent){
  const SuperCat *sc = kb.sc;
  if(sc->left){
    assert(sc->left->nk);
    out.stream << "(<T ";
    sc->cat->out_novar_noX(out.stream, false, sc->conj());

    //JC+DNG: fixing dodgy head hack
    int head = 0;
    int nchildren = 1;
    if(sc->right){
      nchildren = 2;
      if(!(sc->right->kbest[kb.right].sc->vars[Vars::X] != sc->vars[Vars::X]))
        head = 1;
    }

    out.stream << ' ' << head << ' ' << nchildren << "> ";
    recurse_ccgbank(sc->left->kbest[kb.left], sent);

    if(sc->right){
      out.stream << ' ';
      assert(sc->right->nk);
      recurse_ccgbank(sc->right->kbest[kb.right], sent);
    }
    out.stream << ')';
  }else{
    // leaf case (<L NP[nb]/N DT DT the NP[nb]_131/N_131>)
    Position pos = (sc->vars[sc->cat->var]).pos();
    out.stream << "(<L ";
    sc->cat->out_novar_noX(out.stream, false) << ' ';
    out.stream << sent.pos[pos - 1] << ' ' << sent.pos[pos - 1] << ' ';
    out.stream << sent.words[pos - 1] << ' ';
    sc->cat->out_novar_noX(out.stream, false);
    out.stream << ">)";
  }
}

void BankDepsPrinter::recurse_deps(const KBest &kb, Sentence &sent) {
  //taken from DepsPrinter::derivation
  const SuperCat *sc = kb.sc;
  if(sc->left){
    assert(sc->left->nk);
    recurse_deps(sc->left->kbest[kb.left], sent);
    if(sc->right){
      assert(sc->right->nk);
      recurse_deps(sc->right->kbest[kb.right], sent);
    }
  }
  sc->print_filled(out.stream, cats.markedup, cats.relations, sent.words, sent.words,
                   FORMAT & FMT_JULIA_SLOTS);

  //store the lexical categories for printing
  if(!sc->left)
    sent.cats.push_back(sc->cat);
}

void
BankDepsPrinter::derivation(const KBest &kb, Sentence &sent, const ushort rank){
  out.stream << "ID=" << nsentences << " PARSER=GOLD NUMPARSE=" << rank << endl;
  recurse_deps(kb, sent);
  recurse_ccgbank(kb, sent);
  out.stream << '\n';
  grs.clear();
  filled.clear();
  get_grs(kb, sent);

  for(GRs::const_iterator i = grs.begin(); i != grs.end(); ++i){
    out.stream << '(' << i->label;
    for(Arguments::const_iterator j = i->args.begin(); j != i->args.end(); ++j){
      out.stream << ' ' << j->raw;
      if(j->pos >= 0)
        out.stream << '_' << j->pos;
    }
    out.stream << ')' << endl;
  }

}

void
BankDepsPrinter::get_grs(const KBest &kb, Sentence &sent){
  const SuperCat *sc = kb.sc;

  if(sc->left){
    assert(sc->left->nk);
    get_grs(sc->left->kbest[kb.left], sent);
    if(sc->right){
      assert(sc->right->nk);
      get_grs(sc->right->kbest[kb.right], sent);
    }
  }

  sc->get_grs(grs, cats.relations, filled, sent);
}


void
BankDepsPrinter::lexical(Sentence &sent){
  const ulong NWORDS = sent.words.size();

  if((FORMAT & FMT_LEMMA) && sent.lemmas.size() != NWORDS)
    throw NLP::Exception("not enough lemmas for the sentence");

  // don't need to check POS tags since they must already exist

  if((FORMAT & FMT_CHUNK) && sent.chunks.size() != NWORDS)
    throw NLP::Exception("not enough chunks for the sentence");

  if((FORMAT & FMT_NER) && sent.entities.size() != NWORDS)
    throw NLP::Exception("not enough named entities for the sentence");

  const bool HAS_CORRECT_STAG = sent.cats.size();

  if(FORMAT & FMT_WORDS){
    out.stream << "<c>";
    for(ulong i = 0; i < NWORDS; ++i){
      out.stream << ' ' << sent.words[i];
      if(FORMAT & FMT_LEMMA)
        out.stream << '|' << sent.lemmas[i];
      if(FORMAT & FMT_POS)
        out.stream << '|' << sent.pos[i];
      if(FORMAT & FMT_CHUNK)
        out.stream << '|' << sent.chunks[i];
      if(FORMAT & FMT_NER)
        out.stream << '|' << sent.entities[i];
      if(FORMAT & FMT_SUPER){
        out.stream << '|';
        if(HAS_CORRECT_STAG)
          sent.cats[i]->out_novar_noX(out.stream, false);
        else
          out.stream << sent.msuper[i][0].raw;
      }
      if(FORMAT & FMT_CAT){
        out.stream << '|';
        if(HAS_CORRECT_STAG)
          sent.cats[i]->out(out.stream);
        else
          out.stream << "none";
      }
    }
    out.stream << "\n\n";
  }

}

} }
