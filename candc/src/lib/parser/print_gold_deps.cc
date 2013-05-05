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
#include "parser/print_gold_deps.h"

using namespace std;

namespace NLP { namespace CCG {

void
GoldDepsPrinter::header(const std::string &PREFACE){
  out.stream << PREFACE << endl;
  log.stream << PREFACE << endl;
}

void
GoldDepsPrinter::footer(void){
  out.stream << precision() << ' ' << recall() << ' ' << fscore() << endl;
}

static std::ostream &
output_cat(const SuperCat *sc, std::ostream &out, unsigned int depth) {
  for(unsigned int i = 0; i != depth; ++i){
    out << '|';
    if (i == depth - 1)
      out << "-- ";
    else
      out << "   ";
  }
  out << Port::BOLD;
  sc->cat->out_novar(out, false);
  out << Port::OFF;
  return out;
}

void
GoldDepsPrinter::recurse(const KBest &kb, Sentence &sent, Deps &good, Deps &bad, Deps &ignored, Deps &missing, unsigned int depth){
  const SuperCat *sc = kb.sc;
  sc->count_deps(cats.markedup, cats.relations, sent, good, bad, ignored);
  sc->count_missing_deps(cats.relations, missing);
  if(sc->left){ 
    assert(sc->left->nk);
    output_cat(sc, out.stream, depth);
    out.stream << ' ' << sc->ngood_fillers << ' ' << sc->nbad_fillers << ' ' << sc->nmissing_fillers;
    out.stream << /*" (" << (sc->right ? 2 : 1) << ")"*/ '\n';
    recurse(sc->left->kbest[kb.left], sent, good, bad, ignored, missing, depth + 1);
    if(sc->right){
      assert(sc->right->nk);
      recurse(sc->right->kbest[kb.right], sent, good, bad, ignored, missing, depth + 1);
    }
  }
  else{ /* leaf */
    Position pos = (sc->vars[sc->cat->var]).pos();
    output_cat(sc, out.stream, depth);
    out.stream << ' ' << Port::RED << sent.words[pos - 1] << Port::OFF << '\n';
  }
}

void
GoldDepsPrinter::_print_dep(Sentence &sent, const Dep &dep){
  out.stream << sent.words[dep.head - 1] << '_' << dep.head << ' ' << dep.plain_str << ' ';
  out.stream << dep.jslot << ' ';
  out.stream << sent.words[dep.filler - 1] << '_' << dep.filler << ' ' << dep.ruleid;
}

void
GoldDepsPrinter::_print_deps(Sentence &sent, const char *suffix, const Deps &deps){
  for(Deps::const_iterator it = deps.begin(); it != deps.end(); ++it){
    _print_dep(sent, *it);
    out.stream << ' ' << suffix << '\n';
  }
}

void
GoldDepsPrinter::print_deps(Sentence &sent, const Deps &good, const Deps &bad, const Deps &ignored, const Deps &missing){
  _print_deps(sent, "good", good);
  _print_deps(sent, "bad", bad);
  _print_deps(sent, "missing", missing);
  _print_deps(sent, "ignored", ignored);
}

void
GoldDepsPrinter::derivation(const KBest &kb, Sentence &sent, const ushort rank){
  Deps good, bad, ignored, missing(sent.deps);
  recurse(kb, sent, good, bad, ignored, missing, 0);

  ngood += good.size();
  nbad += bad.size();
  nignored += ignored.size();
  nmissing += missing.size();

  print_deps(sent, good, bad, ignored, missing);
  out.stream << "gold: " << good.size() << "\tbad: " << bad.size() << "\tmissing: " << missing.size() << "\tignored: " << ignored.size() << "\texpected: " << sent.deps.size() << "\n\n";
}

} }
