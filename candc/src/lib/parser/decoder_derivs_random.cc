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
#include "parser/decoder_derivs_random.h"

namespace NLP { namespace CCG {

double
DerivsRandomDecoder::calc_score(const SuperCat *sc){
  return rand();
}

double
DerivsRandomDecoder::best_score(const SuperCat *sc){
  if(sc->left){
    best_equiv(sc->left);

    if(sc->right)
      best_equiv(sc->right);
  }

  return sc->score = rand();
}

ushort
DerivsRandomDecoder::score_0(KBest ret[], const SuperCat *sc) {
  KBest *tmp = new KBest[tmp_size];

  if(sc->left){
    equiv_0(tmp, sc->left);

    if(sc->right)
      equiv_0(tmp, sc->right);
  }
  ret[0] = KBest(sc, rand());
  delete [] tmp;

  return 1;
}

ushort
DerivsRandomDecoder::score_1(KBest ret[], const SuperCat *sc) {
  KBest *tmp = new KBest[k+1];

  if(sc->left){
    equiv_1(tmp, sc->left);

    if(sc->right)
      equiv_1(tmp, sc->right);
  }
  ret[0] = KBest(sc, rand());
  delete [] tmp;

  return 1;
}

double
DerivsRandomDecoder::score_2(const SuperCat *sc) {
  if(sc->left){
    equiv_2(sc->left);

    if(sc->right)
      equiv_2(sc->right);
  }

  return rand();
}

void
DerivsRandomDecoder::get_candidates_3(const SuperCat *sc){
  std::set<KBest, std::greater<KBest> > &candidates = sc_candidates[sc];
  candidates.clear();

  for(const SuperCat *equiv = sc; equiv; equiv = equiv->next){
    candidates.insert(KBest(equiv, 0, 0, rand()));
  }
  // first item is already in sc->kbest from 1-best pass. doesn't really
  // make much sense for random decoder but here for consistency
  candidates.erase(candidates.begin());
  sc->flags |= SuperCat::HAS_CANDIDATES;
}

void
DerivsRandomDecoder::lazy_next_3(std::set<KBest, std::greater<KBest> > &candidates, const KBest &kb){
  const SuperCat *sc = kb.sc;
  uint index;

  if(sc->left){
    lazy_kbest_3(sc->left, kb.left + 1);

    if(sc->right){
      lazy_kbest_3(sc->right, kb.right + 1);

      index = kb.left + 1;
      if(index < sc->left->nk)
        candidates.insert(KBest(sc, index, kb.right, rand()));

      index = kb.right + 1;
      if(index < sc->right->nk)
        candidates.insert(KBest(sc, kb.left, index, rand()));

    }
    else{
      index = kb.left + 1;
      if(index < sc->left->nk)
        candidates.insert(KBest(sc, index, 0, rand()));
    }
  }
}

void
DerivsRandomDecoder::get_candidates_4(const SuperCat *sc){
  std::vector<KBest> candidates;

  for(const SuperCat *equiv = sc; equiv; equiv = equiv->next)
    candidates.push_back(KBest(equiv, 0, 0, rand()));

  std::sort(candidates.begin(), candidates.end(), std::greater<KBest>());
  sc->ncand = candidates.size() >= k ? (k-1) : candidates.size() - 1;

  // first item is already in sc->kbest from 1-best pass. doesn't really
  // make much sense for random decoder but here for consistency
  memcpy(sc->kbest + 1, &candidates[1], sizeof(KBest) * sc->ncand);
  sc->flags |= SuperCat::HAS_CANDIDATES;
}

void
DerivsRandomDecoder::lazy_next_4(const SuperCat *sc, const KBest &kb){
  const SuperCat *l, *r, *kbsc = kb.sc;
  uint index;

  if((l = kbsc->left)){
    KBest *candidates = sc->kbest + sc->nk;
    ushort last = sc->nk + sc->ncand - 1;
    lazy_kbest_4(l, kb.left + 1);

    if((r = kbsc->right)){
      lazy_kbest_4(r, kb.right + 1);

      index = kb.left + 1;
      bool last_changed = false;
      KBest tmp, frontier;
      if(index < l->nk){
        tmp = KBest(kbsc, index, kb.right, rand());
        if(!kbest_seen(sc->kbest, candidates + sc->ncand, tmp)){
          if(last < k-1){
            last_changed = true;
            frontier = sc->kbest[last];
            sc->kbest[++last] = tmp;
            ++(sc->ncand);
          }
          else if(sc->ncand > 0 && tmp > sc->kbest[last]){
            last_changed = true;
            frontier = sc->kbest[last-1];
            sc->kbest[last] = tmp;
          }
        }
      }

      index = kb.right + 1;
      if(index < r->nk){
        tmp = KBest(kbsc, kb.left, index, rand());
        if(!kbest_seen(sc->kbest, candidates + sc->ncand, tmp)){
          if(last < k-1){
            sc->kbest[++last] = tmp;
            ++(sc->ncand);
          }
          else if(last_changed){ // don't overwrite new one from left if possible
            if(sc->ncand > 1 && tmp > sc->kbest[last-1]){
              if(frontier > sc->kbest[last])
                sc->kbest[last] = frontier;
              sc->kbest[last-1] = tmp;
            }
            else if(sc->ncand > 0 && tmp > sc->kbest[last])
              sc->kbest[last] = tmp;
          }
          else if(sc->ncand > 0 && tmp > sc->kbest[last])
            sc->kbest[last] = tmp;
        }
      }
    }
    else{
      index = kb.left + 1;
      if(index < l->nk){
        KBest tmp(kbsc, index, 0, rand());
        if(!kbest_seen(sc->kbest, candidates + sc->ncand, tmp)){
          if(last < k-1){
            sc->kbest[++last] = tmp;
            ++(sc->ncand);
          }
          else if(sc->ncand > 0 && tmp > sc->kbest[last])
            sc->kbest[last] = tmp;
        }
      }
    }
    std::sort(candidates, candidates + sc->ncand, std::greater<KBest>());
  }
}

void
DerivsRandomDecoder::lazy_next_4(KBest ret[], ushort &nk, ushort &ncand, const KBest &kb){
  const SuperCat *l, *r, *sc = kb.sc;
  uint index;

  if((l = sc->left)){
    KBest *candidates = ret + nk;
    ushort last = nk + ncand - 1;
    lazy_kbest_4(l, kb.left + 1);

    if((r = sc->right)){
      lazy_kbest_4(r, kb.right + 1);

      index = kb.left + 1;
      bool last_changed = false;
      KBest tmp, frontier;
      if(index < l->nk){
        tmp = KBest(sc, index, kb.right, rand());
        if(!kbest_seen(ret, candidates + ncand, tmp)){
          if(last < k-1){
            last_changed = true;
            frontier = ret[last];
            ret[++last] = tmp;
            ++ncand;
          }
          else if(ncand > 0 && tmp > ret[last]){
            last_changed = true;
            frontier = ret[last];
            ret[last] = tmp;
          }
        }
      }

      index = kb.right + 1;
      if(index < r->nk){
        tmp = KBest(sc, kb.left, index, rand());
        if(!kbest_seen(ret, candidates + ncand, tmp)){
          if(last < k-1){
            ret[++last] = tmp;
            ++ncand;
          }
          else if(last_changed){ // don't overwrite one from left if possible
            if(ncand > 1 && tmp > ret[last-1]){
              if(frontier > ret[last])
                ret[last] = frontier;
              ret[last-1] = tmp;
            }
            else if(ncand > 0 && tmp > ret[last])
              ret[last] = tmp;
          }
          else if(ncand > 0 && tmp > ret[last])
            ret[last] = tmp;
        }
      }
    }
    else{
      index = kb.left + 1;
      if(index < l->nk){
        KBest tmp(sc, index, 0, rand());
        if(!kbest_seen(ret, candidates + ncand, tmp)){
          if(last < k-1){
            ret[++last] = tmp;
            ++ncand;
          }
          else if(ncand > 0 && tmp > ret[last])
            ret[last] = tmp;
        }
      }
    }
    std::sort(candidates, candidates + ncand, std::greater<KBest>());
  }
}

} }
