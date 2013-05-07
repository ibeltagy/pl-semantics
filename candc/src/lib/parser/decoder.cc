// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include "parser/_decoder.h"

#include <limits>

using namespace std;

namespace NLP { namespace CCG {

ulong
Decoder::_calc_dhash(const Filled *filled) const{
  Hash h(filled->head);
  h += filled->rel;
  h += filled->filler;
  h += filled->rule;
  return h.value();;
}

ulong
Decoder::calc_dhash(const SuperCat *sc) const{
  ulong hash = 0;
  for(const Filled *filled = sc->filled; filled; filled = filled->next)
    hash ^= _calc_dhash(filled);
  return hash;
}

bool
Decoder::has_same_deps(const KBest &kb1, const KBest &kb2) const{
  FilledDeps f1, f2;
  get_filled_deps(kb1, f1);
  get_filled_deps(kb2, f2);
  if(f1.size() != f2.size())
    return false;

  for(FilledDeps::iterator it_out = f1.begin(); it_out != f1.end(); ++it_out){
    bool found = false;
    for(FilledDeps::iterator it_in = f2.begin(); it_in != f2.end(); ){
      const Filled *d1 = *it_out;
      const Filled *d2 = *it_in;
      if(d1->rel == d2->rel && d1->head == d2->head && 
          d1->filler == d2->filler && d1->rule == d2->rule){
        f2.erase(it_in);
        found = true;
        break;
      }
      ++it_in;
    }
    if(!found)
      return false;
  }

  return f2.size() == 0;
}

void
Decoder::get_filled_deps(const KBest &kb, FilledDeps &deps) const{
  const SuperCat *sc = kb.sc;
  for(const Filled *dep = sc->filled; dep; dep = dep->next)
    deps.push_back(dep);

  if(sc->left){
    get_filled_deps(sc->left->kbest[kb.left], deps);
    if(sc->right)
      get_filled_deps(sc->right->kbest[kb.right], deps);
  }
}

bool
Decoder::equal_deps(const KBest &kb1, const KBest &kb2) const {
  if(kb1.dhash == kb2.dhash){
    ncomps += 1;
    if(!has_same_deps(kb1, kb2))
      ncollisions += 1;
    return true;
  }
  return false;
  //return kb1.dhash == kb2.dhash;
}

const SuperCat *
Decoder::best_equiv(const SuperCat *sc){
  if(sc->max)
    return sc->max;

  const SuperCat *max_sc = 0;
  volatile double max_score = -numeric_limits<double>::max();

  for(const SuperCat *equiv = sc; equiv; equiv = equiv->next){
    double current = best_score(equiv);
    if(current > max_score){
      max_score = current;
      max_sc = equiv;
    }
  }

  //assert(max_sc->score == max_score);
  sc->kbest[0] = KBest(max_sc, 0, 0, max_score, max_sc->kbest[0].dhash);
  sc->nk = 1;
  return sc->max = max_sc;
}

ushort
Decoder::best(KBest ret[], Chart &chart){
  Cell &root = chart.root();

  const SuperCat *max_sc = 0;
  double max_score = -numeric_limits<double>::max();

  if(root.size()){
    for(Cell::iterator i = root.begin(); i != root.end(); ++i){
      const SuperCat *current = *i;
      best_equiv(current);
      if(current->kbest[0].score > max_score){
        max_score = current->kbest[0].score;
        max_sc = current;
      }
    }

    memcpy(ret, max_sc->kbest, sizeof(KBest));
    return 1;
  }
  else
    return 0;
}

bool
Decoder::deps_seen(KBest ret_start[], KBest ret_end[], const KBest &kb) const {
  while(ret_start != ret_end){
    if(equal_deps(*ret_start, kb))
      return true;
    ++ret_start;
  }
  return false;
}

bool
Decoder::better_deps_seen(KBest ret_start[], KBest ret_end[], const KBest &kb) const {
  while(ret_start != ret_end && (*ret_start) > kb){
    if(equal_deps(*ret_start, kb))
      return true;
    ++ret_start;
  }
  return false;
}

bool
Decoder::kbest_seen(KBest ret_start[], KBest ret_end[], const KBest &kb, const bool last_changed) const {
  if(last_changed)
    --ret_end;
  while(--ret_end >= ret_start && (*ret_end).score <= kb.score)
    if(kb.eq(*ret_end))
      return true;
  
  return false;
}

void
Decoder::copy_candidates_4(const SuperCat *sc, std::vector<KBest> &candidates){
  ushort i = 0;

  if(candidates.size() > 1){
    while(i <= sc->ncand){
      if(sc->kbest[0].eq(candidates[i])){
        memcpy(sc->kbest + 1, &candidates[0], sizeof(KBest) * i);
        memcpy(sc->kbest + i + 1, &candidates[i+1], sizeof(KBest) * (sc->ncand - i));
        break;
      }
      ++i;
    }
  }
}


ushort
Decoder::merge_0(KBest ret[], KBest a_begin[], KBest a_end[], KBest b_begin[], KBest b_end[]) {
  ushort i = 0;
  
  while (i < k && (a_begin != a_end || b_begin != b_end)) {
    if(a_begin == a_end)
      ret[i++] = *b_begin++;
    else if(b_begin == b_end || *a_begin > *b_begin)
      ret[i++] = *a_begin++;
    else
      ret[i++] = *b_begin++;
  }
  return i;
}

ushort
Decoder::equiv_0(KBest ret[], const SuperCat *sc) {
  ushort curr_size = 0, prev_size = 0;

  if(sc->nk){
    memcpy(ret, sc->kbest, sizeof(KBest) * sc->nk);
    return sc->nk;
  }

  KBest *current = new KBest[tmp_size]; // k^2 temporary space
  KBest *previous = new KBest[tmp_size]; // k^2 temporary space
  for(const SuperCat *equiv = sc; equiv; equiv = equiv->next){
    curr_size = score_0(current, equiv);
    memcpy(previous, ret, sizeof(KBest) * prev_size);
    prev_size = merge_0(ret, current, current + curr_size, previous, previous + prev_size);
  }

  memcpy(sc->kbest, ret, sizeof(KBest) * prev_size);
  sc->nk = prev_size;
  delete [] current;
  delete [] previous;

  return prev_size;
}

ushort
Decoder::best_0(KBest ret[], Chart &chart){
  Cell &root = chart.root();
  ushort curr_size = 0, prev_size = 0;

 // k^2 temporary space
  KBest *current = new KBest[tmp_size];
  KBest *previous = new KBest[tmp_size];

  for(Cell::iterator i = root.begin(); i != root.end(); ++i){
    curr_size = equiv_0(current, *i);
    memcpy(previous, ret, sizeof(KBest) * prev_size);
    prev_size = merge_0(ret, current, current + curr_size, previous, previous + prev_size);
  }

  delete [] current;
  delete [] previous;

  return prev_size;
}

ushort
Decoder::best_1(KBest ret[], Chart &chart){
  Cell &root = chart.root();
  ushort curr_size = 0, prev_size = 0;

  KBest *current = new KBest[k+1];
  KBest *previous = new KBest[k+1];

  for(Cell::iterator i = root.begin(); i != root.end(); ++i){
    curr_size = equiv_1(current, *i);
    memcpy(previous, ret, sizeof(KBest) * prev_size);
    prev_size = merge_0(ret, current, current + curr_size, previous, previous + prev_size);
  }

  delete [] current;
  delete [] previous;

  return prev_size;
}

ushort
Decoder::equiv_1(KBest ret[], const SuperCat *sc) {
  ushort curr_size = 0, prev_size = 0;

  if(sc->nk){
    memcpy(ret, sc->kbest, sizeof(KBest) * sc->nk);
    return sc->nk;
  }

  KBest *current = new KBest[k+1];
  KBest *previous = new KBest[k+1];
  for(const SuperCat *equiv = sc; equiv; equiv = equiv->next){
    curr_size = score_1(current, equiv);
    memcpy(previous, ret, sizeof(KBest) * prev_size);
    prev_size = merge_0(ret, current, current + curr_size, previous, previous + prev_size);
  }

  memcpy(sc->kbest, ret, sizeof(KBest) * prev_size);
  sc->nk = prev_size;
  delete [] current;
  delete [] previous;

  return prev_size;
}

ushort
Decoder::equiv_2(KBest ret[], const SuperCat *sc) {
  equiv_2(sc);
  memcpy(ret, sc->kbest, sizeof(KBest) * sc->nk);
  return sc->nk;
}

ushort
Decoder::equiv_2(const SuperCat *sc) {
  if(sc->nk)
    return sc->nk;

  uint x = 0;
  const SuperCat *l;
  const SuperCat *r;

  std::set<KBest, std::greater<KBest> > candidates;
  for(const SuperCat *equiv = sc; equiv; equiv = equiv->next)
    candidates.insert(KBest(equiv, 0, 0, score_2(equiv)));

  ushort tmp;
  while(!candidates.empty() && x < k){
    KBest current = *candidates.begin();
    sc->kbest[x++] = current;
    candidates.erase(candidates.begin());

    const SuperCat *equiv = current.sc;
    if((l = equiv->left)){
      if((r = equiv->right)){
        tmp = current.left + 1;
        if(tmp < l->nk)
          candidates.insert(KBest(equiv, tmp, current.right, calc_score(equiv) + l->kbest[tmp].score + r->kbest[current.right].score));

        tmp = current.right + 1;
        if(tmp < r->nk)
          candidates.insert(KBest(equiv, current.left, tmp, calc_score(equiv) + l->kbest[current.left].score + r->kbest[tmp].score));
      }
      else{
        tmp = current.left + 1;
        if(tmp < l->nk)
          candidates.insert(KBest(equiv, tmp, 0, calc_score(equiv) + l->kbest[tmp].score));
      }
    }
  }

  sc->nk = x;
  return ushort(x);
}

ushort
Decoder::best_2(KBest ret[], Chart &chart){
  Cell &root = chart.root();
  ushort curr_size = 0, prev_size = 0;

  KBest *current = new KBest[k+1];
  KBest *previous = new KBest[k+1];

  for(Cell::iterator i = root.begin(); i != root.end(); ++i){
    curr_size = equiv_2(current, *i);
    memcpy(previous, ret, sizeof(KBest) * prev_size);
    prev_size = merge_0(ret, current, current + curr_size, previous, previous + prev_size);
  }

  delete [] current;
  delete [] previous;

  return prev_size;
}

ushort
Decoder::best_3(KBest ret[], Chart &chart){
  Cell &root = chart.root();
  sc_candidates.clear();
  std::set<KBest, std::greater<KBest> > candidates;
  uint x = 0;

  for(Cell::iterator i = root.begin(); i != root.end(); ++i){
    for(const SuperCat *equiv = *i; equiv; equiv = equiv->next){
      KBest current(equiv, 0, 0, calc_score(equiv));
      if(equiv->left){
        current.score += equiv->left->kbest[0].score;
        if(equiv->right)
          current.score += equiv->right->kbest[0].score;
      }
      candidates.insert(current);
    }
  }

  while(x < k){
    if(x > 0)
      lazy_next_3(candidates, ret[x-1]);
    if(!candidates.empty()){
      ret[x++] = *candidates.begin();
      candidates.erase(candidates.begin());
    }
    else
      break;
  }

  return x;
}

void
Decoder::lazy_kbest_3(const SuperCat *sc, const ushort rank){
  if(!sc->has_candidates())
    get_candidates_3(sc);

  std::set<KBest, std::greater<KBest> > &candidates = sc_candidates[sc];
  while(sc->nk <= rank){
    if(sc->nk > 0)
      lazy_next_3(candidates, sc->kbest[sc->nk-1]);

    if(!candidates.empty()){
      sc->kbest[sc->nk++] = *candidates.begin();
      candidates.erase(candidates.begin());
    }
    else
      break;
  }
}

ushort
Decoder::best_4(KBest ret[], Chart &chart){
  Cell &root = chart.root();
  std::vector<KBest> candidates;

  for(Cell::iterator i = root.begin(); i != root.end(); ++i){
    for(const SuperCat *equiv = *i; equiv; equiv = equiv->next){
      KBest current(equiv, 0, 0, calc_score(equiv));
      if(equiv->left){
        current.score += equiv->left->kbest[0].score;
        if(equiv->right)
          current.score += equiv->right->kbest[0].score;
      }
      candidates.push_back(current);
    }
  }
  if(candidates.size()){
    std::sort(candidates.begin(), candidates.end(), std::greater<KBest>());
    ushort nk = 0;
    ushort ncand = (candidates.size() > k) ? k : candidates.size();
    memcpy(ret, &candidates[0], sizeof(KBest) * ncand);

    bool same_last = false;
    while(nk < k){
      if(nk > 0 && !same_last)
        lazy_next_4(ret, nk, ncand, ret[nk-1]);

      if(ncand > 0){
        --ncand;
        if(!(same_last = kbest_seen(ret, ret + nk, ret[nk])))
          ++nk;
        else if(ncand)
          memmove(ret + nk, ret + nk + 1, sizeof(KBest) * ncand);
      }
      else
        break;
    }

    return nk;
  }
  return 0;
}

void
Decoder::lazy_kbest_4(const SuperCat *sc, const ushort rank){
  if(!sc->has_candidates())
    get_candidates_4(sc);

  //ignore candidates that have been seen before - avoids generating duplicates
  bool same_last = false;
  while(sc->nk <= rank){
    if(sc->nk > 0 && !same_last)
      lazy_next_4(sc->kbest, sc->nk, sc->ncand, sc->kbest[sc->nk-1]);

    if(sc->ncand){
      --(sc->ncand);
      if(!(same_last = kbest_seen(sc->kbest, sc->kbest + sc->nk, sc->kbest[sc->nk])))
        ++(sc->nk);
      else
        memmove(sc->kbest + sc->nk, sc->kbest + sc->nk + 1, sizeof(KBest) * sc->ncand);
    }
    else
      break;
  }
}

} }
