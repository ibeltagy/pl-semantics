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
#include "parser/decoder_derivs.h"
#include "parser/decoder_gold.h"

#include <limits>

using namespace std;

namespace NLP { namespace CCG {

const SuperCat *
GoldDecoder::best_equiv(const SuperCat *sc){
  if(sc->max)
    return sc->max;

  const SuperCat *max_sc = 0;
  volatile double max_score = -numeric_limits<double>::max();
  volatile ulong least_bad = numeric_limits<long>::max();

  for(const SuperCat *equiv = sc; equiv; equiv = equiv->next){
    double current_score = best_score(equiv);
    ulong current_bad = count_bad(equiv);
    if(current_score > max_score || (current_score == max_score && current_bad < least_bad)){
      max_score = current_score;
      least_bad = current_bad;
      max_sc = equiv;
    }
  }

  //assert(max_sc->score == max_score);
  sc->kbest[0] = KBest(max_sc, 0, 0, max_score, max_sc->kbest[0].dhash, least_bad);
  sc->nk = 1;
  return sc->max = max_sc;
}

ushort
GoldDecoder::best(KBest ret[], Chart &chart){
  Cell &root = chart.root();

  const SuperCat *max_sc = 0;
  double max_score = -numeric_limits<double>::max();
  ulong least_bad = numeric_limits<long>::max();

  if(root.size()){
    for(Cell::iterator i = root.begin(); i != root.end(); ++i){
      const SuperCat *current = *i;
      best_equiv(current);
      if(current->kbest[0].score > max_score || (current->kbest[0].score == max_score && current->kbest[0].nbad < least_bad)){
        max_score = current->kbest[0].score;
        least_bad = current->kbest[0].nbad;
        max_sc = current;
      }
    }

    memcpy(ret, max_sc->kbest, sizeof(KBest));
    return 1;
  }
  else
    return 0;
}


double
GoldDecoder::calc_score(const SuperCat *sc){
  return sc->ngood_fillers;
}

ulong
GoldDecoder::count_bad(const SuperCat *sc){
  ulong nbad = sc->nbad_fillers;
  
  if(sc->left){
    nbad += sc->left->kbest[0].nbad;

    if(sc->right){
      nbad += sc->right->kbest[0].nbad;
    }
  }
  return nbad;
}

ushort
GoldDecoder::score_0(KBest ret[], const SuperCat *sc) {
  double score = calc_score(sc);
  ulong nbad = sc->nbad_fillers;
  uint x = 0;

  if(sc->left){
    KBest *l = new KBest[tmp_size];
    ushort l_size = equiv_0(l, sc->left);

    if(sc->right){
      KBest *r = new KBest[tmp_size];
      ushort r_size = equiv_0(r, sc->right);
  
      for(ushort i = 0; i < l_size; ++i)
        for(ushort j = 0; j < r_size; ++j)
          ret[x++] = KBest(sc, i, j, score + l[i].score + r[j].score, 0,
                           nbad + l[i].nbad + r[j].nbad);

      std::sort(ret, ret + x, std::greater<KBest>()); //sort desc by score
      if(x > k)
        x = k; //cap to k
      delete [] r;
    }
    else{
      for(ushort i = 0; i < l_size; ++i)
        ret[x++] = KBest(sc, i, 0, score + l[i].score, 0, nbad + l[i].nbad);
      if(x > k)
        x = k; //cap to k
    }
    delete [] l;
  }
  else
    ret[x++] = KBest(sc, score, 0, 0, 0, nbad);

  return ushort(x); // guaranteed to be short again after truncating array
}

ushort
GoldDecoder::score_1(KBest ret[], const SuperCat *sc) {
  double score = calc_score(sc);
  ulong nbad = sc->nbad_fillers;
  uint x = 0;

  if(sc->left){
    KBest *l = new KBest[k+1];
    ushort l_size = equiv_1(l, sc->left);

    if(sc->right){
      KBest *r = new KBest[k+1];
      ushort r_size = equiv_1(r, sc->right);

      std::set<KBest, std::greater<KBest> > candidates;
      //bogus type aliasing warning here in gcc 4.4.3, -O3
      candidates.insert(KBest(sc, 0, 0, score + l[0].score + r[0].score, 0, nbad + l[0].nbad + r[0].nbad));

      uint tmp;
      while(!candidates.empty() && x < k) {
        KBest current = *candidates.begin();
        ret[x++] = current;
        candidates.erase(candidates.begin());
  
        tmp = current.left + 1;
        if(tmp < l_size)
          candidates.insert(KBest(sc, tmp, current.right, 
                                  score + l[tmp].score + r[current.right].score,
                                  0, nbad + l[tmp].nbad + r[current.right].nbad));
        tmp = current.right + 1;
        if(tmp < r_size)
          candidates.insert(KBest(sc, current.left, tmp,
                                  score + l[current.left].score + r[tmp].score,
                                  0, nbad + l[current.left].nbad + r[tmp].nbad));
      }
  
      delete [] r;
    }
    else{
      for(ushort i = 0; i < l_size && x < k; ++i)
        ret[x++] = KBest(sc, i, 0, score + l[i].score, 0, nbad + l[i].nbad);
    }
    delete [] l;
  }
  else
    ret[x++] = KBest(sc, score, 0, 0, 0, nbad);

  return ushort(x); // guaranteed to be short again after truncating array
}

ushort
GoldDecoder::equiv_2(const SuperCat *sc) {
  if(sc->nk)
    return sc->nk;

  uint x = 0;
  const SuperCat *l;
  const SuperCat *r;

  std::set<KBest, std::greater<KBest> > candidates;
  for(const SuperCat *equiv = sc; equiv; equiv = equiv->next)
    candidates.insert(KBest(equiv, 0, 0, score_2(equiv), 0, count_bad(equiv)));

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
          candidates.insert(KBest(equiv, tmp, current.right,
                                  calc_score(equiv) + l->kbest[tmp].score + r->kbest[current.right].score,
                                  0, equiv->nbad_fillers + l->kbest[tmp].nbad + r->kbest[current.right].nbad));

        tmp = current.right + 1;
        if(tmp < r->nk)
          candidates.insert(KBest(equiv, current.left, tmp,
                                  calc_score(equiv) + l->kbest[current.left].score + r->kbest[tmp].score,
                                  0, equiv->nbad_fillers + l->kbest[current.left].nbad + r->kbest[tmp].nbad));
      }
      else{
        tmp = current.left + 1;
        if(tmp < l->nk)
          candidates.insert(KBest(equiv, tmp, 0, 
                                  calc_score(equiv) + l->kbest[tmp].score,
                                  0, equiv->nbad_fillers + l->kbest[tmp].nbad));
      }
    }
  }

  sc->nk = x;
  return ushort(x);
}

ushort
GoldDecoder::best_3(KBest ret[], Chart &chart){
  Cell &root = chart.root();
  sc_candidates.clear();
  std::set<KBest, std::greater<KBest> > candidates;
  uint x = 0;

  for(Cell::iterator i = root.begin(); i != root.end(); ++i){
    for(const SuperCat *equiv = *i; equiv; equiv = equiv->next){
      KBest current(equiv, 0, 0, calc_score(equiv), 0, equiv->nbad_fillers);
      if(equiv->left){
        current.score += equiv->left->kbest[0].score;
        current.nbad += equiv->left->kbest[0].nbad;
        if(equiv->right){
          current.score += equiv->right->kbest[0].score;
          current.nbad += equiv->right->kbest[0].nbad;
        }
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
GoldDecoder::get_candidates_3(const SuperCat *sc){
  std::set<KBest, std::greater<KBest> > &candidates = sc_candidates[sc];
  candidates.clear();

  for(const SuperCat *equiv = sc; equiv; equiv = equiv->next){
    KBest current(equiv, 0, 0, calc_score(equiv), 0, equiv->nbad_fillers);
    if(equiv->left){
      current.score += equiv->left->kbest[0].score;
      current.nbad += equiv->left->kbest[0].nbad;
      if(equiv->right){
        current.score += equiv->right->kbest[0].score;
        current.nbad += equiv->right->kbest[0].nbad;
      }
    }
    candidates.insert(current);
  }
  // first item is already in sc->kbest from 1-best pass
  //candidates.erase(candidates.begin());
  candidates.erase(sc->kbest[0]);
  sc->flags |= SuperCat::HAS_CANDIDATES;
}

void
GoldDecoder::lazy_next_3(std::set<KBest, std::greater<KBest> > &candidates, const KBest &kb){
  const SuperCat *l, *r, *sc = kb.sc;
  double score = calc_score(sc);
  ulong nbad = sc->nbad_fillers;
  uint index;

  if((l = sc->left)){
    lazy_kbest_3(l, kb.left + 1);

    if((r = sc->right)){
      lazy_kbest_3(r, kb.right + 1);

      index = kb.left + 1;
      if(index < l->nk)
        candidates.insert(KBest(sc, index, kb.right, 
                                score + l->kbest[index].score + r->kbest[kb.right].score,
                                0, nbad + l->kbest[index].nbad + r->kbest[kb.right].nbad));

      index = kb.right + 1;
      if(index < r->nk)
        candidates.insert(KBest(sc, kb.left, index,
                                score + l->kbest[kb.left].score + r->kbest[index].score,
                                0, nbad + l->kbest[kb.left].nbad + r->kbest[index].nbad));
    }
    else{
      index = kb.left + 1;
      if(index < l->nk)
        candidates.insert(KBest(sc, index, 0,
                                score + l->kbest[index].score, 0,
                                nbad + l->kbest[index].nbad));
    }
  }
}

ushort
GoldDecoder::best_4(KBest ret[], Chart &chart){
  Cell &root = chart.root();
  std::vector<KBest> candidates;

  for(Cell::iterator i = root.begin(); i != root.end(); ++i){
    for(const SuperCat *equiv = *i; equiv; equiv = equiv->next){
      KBest current(equiv, 0, 0, calc_score(equiv), 0, equiv->nbad_fillers);
      if(equiv->left){
        current.score += equiv->left->kbest[0].score;
        current.nbad += equiv->left->kbest[0].nbad;
        if(equiv->right)
          current.score += equiv->right->kbest[0].score;
          current.nbad += equiv->right->kbest[0].nbad;
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
GoldDecoder::get_candidates_4(const SuperCat *sc){
  std::vector<KBest> candidates;

  for(const SuperCat *equiv = sc; equiv; equiv = equiv->next){
    KBest current(equiv, 0, 0, calc_score(equiv), 0, equiv->nbad_fillers);
    if(equiv->left){
      current.score += equiv->left->kbest[0].score;
      current.nbad += equiv->left->kbest[0].nbad;
      if(equiv->right){
        current.score += equiv->right->kbest[0].score;
        current.nbad += equiv->right->kbest[0].nbad;
      }
    }
    candidates.push_back(current);
  }
  std::sort(candidates.begin(), candidates.end(), std::greater<KBest>());
  sc->ncand = (candidates.size() >= k) ? (k-1) : candidates.size() - 1;

  // first item is already in sc->kbest from 1-best pass
  copy_candidates_4(sc, candidates);
  sc->flags |= SuperCat::HAS_CANDIDATES;
}

void
GoldDecoder::lazy_next_4(KBest ret[], ushort &nk, ushort &ncand, const KBest &kb){
  const SuperCat *l, *r, *sc = kb.sc;
  double score = calc_score(sc);
  ulong nbad = sc->nbad_fillers;
  uint index;

  if((l = sc->left)){
    KBest *candidates = ret + nk;
    ushort last = nk + ncand - 1;
    lazy_kbest_4(l, kb.left + 1);

    if((r = sc->right)){
      lazy_kbest_4(r, kb.right + 1);

      index = kb.left + 1;
      bool last_changed = false;
      KBest tmp, frontier; //one from last
      if(index < l->nk){
        tmp = KBest(sc, index, kb.right,
                    score + l->kbest[index].score + r->kbest[kb.right].score,
                    0, nbad + l->kbest[index].nbad + r->kbest[kb.right].nbad);
        if(!kbest_seen(ret, candidates + ncand, tmp)){
          if(last < k-1){
            last_changed = true;
            frontier = ret[last];
            ret[++last] = tmp;
            ++ncand;
          }
          else if(ncand > 0 && tmp > ret[last]){
            last_changed = true;
            frontier = ret[last-1];
            ret[last] = tmp;
          }
        }
      }

      index = kb.right + 1;
      if(index < r->nk){
        tmp = KBest(sc, kb.left, index,
                    score + l->kbest[kb.left].score + r->kbest[index].score,
                    0, nbad + l->kbest[kb.left].nbad + r->kbest[index].nbad);
        if(!kbest_seen(ret, candidates + ncand, tmp, last_changed)){
          if(last < k-1){
            ret[++last] = tmp;
            ++ncand;
          }
          else if(last_changed){
            // don't overwrite one from left if possible, so check last - 1
            // first before checking last
            if(ncand > 1 && tmp > ret[last-1]){
              //we're replacing the frontier (one fom last) so check the edge 
              //case where the frontier should replace the current last
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
        KBest tmp(sc, index, 0, score + l->kbest[index].score, 0, nbad + l->kbest[index].nbad);
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
