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
#include "parser/inside_outside.h"
#include "parser/decoder_deps_recall.h"

namespace NLP { namespace CCG {

DepsRecallDecoder::DepsRecallDecoder(const ushort k, const uchar k_alg) 
    : Decoder(k, k_alg), inside_outside(new InsideOutside()) {}

DepsRecallDecoder::~DepsRecallDecoder(void){
  delete inside_outside;
}

double 
DepsRecallDecoder::calc_score(const SuperCat *sc){
  double score = 0.0;
  for(const Filled *filled = sc->filled; filled; filled = filled->next)
    score += inside_outside->depscores[filled] / filled->conj;
  return score;
}

ushort
DepsRecallDecoder::best(KBest ret[], Chart &chart){
  inside_outside->calc(chart);
  return Decoder::best(ret, chart);
}

double
DepsRecallDecoder::best_score(const SuperCat *sc){
  double score = calc_score(sc);
  ulong hash = calc_dhash(sc);

  sc->score = score; //set to score of deps on this supercat only

  if(sc->left){
    best_equiv(sc->left);
    score += sc->left->kbest[0].score;
    hash ^= sc->left->kbest[0].dhash;

    if(sc->right){
      best_equiv(sc->right);
      score += sc->right->kbest[0].score;
      hash ^= sc->right->kbest[0].dhash;
    }
  }

  assert(score >= 0.0);
  sc->kbest[0].dhash = hash; // hack in the hash
  return score;
}

ushort
DepsRecallDecoder::merge_0(KBest ret[], KBest a_begin[], KBest a_end[], KBest b_begin[], KBest b_end[]) {
  ushort i = 0;
  
  while (i < k && (a_begin != a_end || b_begin != b_end)) {
    if(a_begin == a_end){
      if(!deps_seen(ret, ret + i, *b_begin))
        ret[i++] = *b_begin;
      ++b_begin;
    }
    else if((b_begin == b_end || *a_begin > *b_begin)){
      if(!deps_seen(ret, ret + i, *a_begin))
        ret[i++] = *a_begin;
      ++a_begin;
    }
    else {
      if(!deps_seen(ret, ret + i, *b_begin))
        ret[i++] = *b_begin;
      ++b_begin;
    }
  }
  return i;
}

ushort
DepsRecallDecoder::best_0(KBest ret[], Chart &chart){
  inside_outside->calc(chart);
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
DepsRecallDecoder::score_0(KBest ret[], const SuperCat *sc){
  uint x = 0;
  ulong hash = calc_dhash(sc);
  sc->score = calc_score(sc);

  if(sc->left){
    KBest *l = new KBest[tmp_size];
    ushort l_size = equiv_0(l, sc->left);
    std::set<KBest, std::greater<KBest> > candidates;

    //generate the candidates and sort them desc by score in a set
    //this is necessary so that the dhash checking doesn't eliminate
    //worse options before better ones
    if(sc->right){
      KBest *r = new KBest[tmp_size];
      ushort r_size = equiv_0(r, sc->right);
  
      for(ushort i = 0; i < l_size; ++i)
        for(ushort j = 0; j < r_size; ++j)
          candidates.insert(KBest(sc, i, j, sc->score + l[i].score + r[j].score, hash ^ l[i].dhash ^ r[j].dhash));

      delete [] r;
    }
    else{
      for(ushort i = 0; i < l_size; ++i)
        candidates.insert(KBest(sc, i, 0, sc->score + l[i].score, hash ^ l[i].dhash));
    }

    //add to ret, checking dhash values in descending score order
    while(!candidates.empty()){
      if(!deps_seen(ret, ret + x, *candidates.begin()))
        ret[x++] = *candidates.begin();
      candidates.erase(candidates.begin());
    }

    if(x > k)
      x = k; //cap to k

    delete [] l;
  }
  else
    ret[x++] = KBest(sc, 0, 0, sc->score, hash);

  return ushort(x); // guaranteed to be short again after truncating array
}

ushort
DepsRecallDecoder::best_1(KBest ret[], Chart &chart){
  inside_outside->calc(chart);
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
DepsRecallDecoder::score_1(KBest ret[], const SuperCat *sc){
  uint x = 0;
  ulong tmp, hash = calc_dhash(sc);
  sc->score = calc_score(sc);

  if(sc->left){
    KBest *l = new KBest[k+1];
    ushort l_size = equiv_1(l, sc->left);

    if(sc->right){
      KBest *r = new KBest[k+1];
      ushort r_size = equiv_1(r, sc->right);

      std::set<KBest, std::greater<KBest> > candidates;
      candidates.insert(KBest(sc, 0, 0, sc->score + l[0].score + r[0].score, hash ^ l[0].dhash ^ r[0].dhash));

      uint index;
      while(!candidates.empty() && x < k) {
        KBest current = *candidates.begin();
        if(!deps_seen(ret, ret + x, current))
          ret[x++] = current;
        candidates.erase(candidates.begin());
  
        index = current.left + 1;
        if(index < l_size){
          tmp = hash ^ l[index].dhash ^ r[current.right].dhash;
          candidates.insert(KBest(sc, index, current.right, sc->score + l[index].score + r[current.right].score, tmp));
        }

        index = current.right + 1;
        if(index < r_size){
          tmp = hash ^ l[current.left].dhash ^ r[index].dhash;
          candidates.insert(KBest(sc, current.left, index, sc->score + l[current.left].score + r[index].score, tmp));
        }
      }
  
      delete [] r;
    }
    else{
      for(ushort i = 0; i < l_size && x < k; ++i){
        KBest kb = KBest(sc, i, 0, sc->score + l[i].score, hash ^ l[i].dhash);
        if(!deps_seen(ret, ret + x, kb))
          ret[x++] = kb;
      }
    }
    delete [] l;
  }
  else
    ret[x++] = KBest(sc, 0, 0, sc->score, hash);

  return ushort(x); // guaranteed to be short again after truncating array
}

ushort
DepsRecallDecoder::best_2(KBest ret[], Chart &chart){
  inside_outside->calc(chart);
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
DepsRecallDecoder::equiv_2(KBest ret[], const SuperCat *sc){
  ushort x = equiv_2(sc);
  memcpy(ret, sc->kbest, sizeof(KBest) * sc->nk);
  return x;
}

ushort
DepsRecallDecoder::equiv_2(const SuperCat *sc) {
  if(sc->nk)
    return sc->nk;

  uint x = 0;
  ulong hash;
  const SuperCat *l, *r;
  double score;

  std::set<KBest, std::greater<KBest> > candidates;
  for(const SuperCat *equiv = sc; equiv; equiv = equiv->next){
    equiv->score = calc_score(equiv); // needs to be accessed later
    hash = calc_dhash(equiv);
    score = equiv->score + score_2(equiv);

    if(equiv->left){
      hash ^= equiv->left->kbest[0].dhash;
      if(equiv->right)
        hash ^= equiv->right->kbest[0].dhash;
    }
    candidates.insert(KBest(equiv, 0, 0, score, hash));
  }

  ushort index;
  while(!candidates.empty() && x < k){
    KBest current = *candidates.begin();
    if(!deps_seen(sc->kbest, sc->kbest + x, current))
      sc->kbest[x++] = current;
    candidates.erase(candidates.begin());

    const SuperCat *equiv = current.sc;
    if((l = equiv->left)){
      if((r = equiv->right)){
        index = current.left + 1;
        if(index < l->nk){
          hash = current.dhash ^ l->kbest[current.left].dhash ^ l->kbest[index].dhash;
          candidates.insert(KBest(equiv, index, current.right, equiv->score + l->kbest[index].score + r->kbest[current.right].score, hash));
        }

        index = current.right + 1;
        if(index < r->nk){
          hash = current.dhash ^ r->kbest[current.right].dhash ^ r->kbest[index].dhash;
          candidates.insert(KBest(equiv, current.left, index, equiv->score + l->kbest[current.left].score + r->kbest[index].score, hash));
        }
      }
      else{
        index = current.left + 1;
        if(index < l->nk){
          hash = current.dhash ^ l->kbest[current.left].dhash ^ l->kbest[index].dhash;
          candidates.insert(KBest(equiv, index, 0, equiv->score + l->kbest[index].score, hash));
        }
      }
    }
  }

  sc->nk = x;
  return ushort(x);
}

double
DepsRecallDecoder::score_2(const SuperCat *sc){
  double score = 0.0;

  if(sc->left){
    equiv_2(sc->left);
    score += sc->left->kbest[0].score;
    if(sc->right){
      equiv_2(sc->right);
      score += sc->right->kbest[0].score;
    }
  }
  return score;
}

ushort
DepsRecallDecoder::best_3(KBest ret[], Chart &chart){
  Cell &root = chart.root();
  sc_candidates.clear();
  std::set<KBest, std::greater<KBest> > candidates;
  uint x = 0;
  double score;

  //initial candidates
  for(Cell::iterator i = root.begin(); i != root.end(); ++i){
    for(const SuperCat *equiv = *i; equiv; equiv = equiv->next){
      score = calc_score(equiv);
      ulong hash = calc_dhash(equiv);

      KBest current(equiv, 0, 0, score, hash);
      if(equiv->left){
        current.score += equiv->left->kbest[0].score;
        current.dhash ^= equiv->left->kbest[0].dhash;
        if(equiv->right){
          current.score += equiv->right->kbest[0].score;
          current.dhash ^= equiv->right->kbest[0].dhash;
        }
      }
      candidates.insert(current);
    }
  }

  KBest current;
  while(x < k){
    if(x > 0)
      lazy_next_3(candidates, current);

    if(!candidates.empty()){
      current = *candidates.begin();
      if(!deps_seen(ret, ret + x, current))
        ret[x++] = current;
      candidates.erase(candidates.begin());
    }
    else
      break;
  }

  return x;
}

void
DepsRecallDecoder::get_candidates_3(const SuperCat *sc){
  std::set<KBest, std::greater<KBest> > &candidates = sc_candidates[sc];
  candidates.clear();
  double score;

  for(const SuperCat *equiv = sc; equiv; equiv = equiv->next){
    score = calc_score(equiv);
    ulong hash = calc_dhash(equiv);

    KBest current(equiv, 0, 0, score, hash);
    if(equiv->left){
      current.score += equiv->left->kbest[0].score;
      current.dhash ^= equiv->left->kbest[0].dhash;
      if(equiv->right){
        current.score += equiv->right->kbest[0].score;
        current.dhash ^= equiv->right->kbest[0].dhash;
      }
    }
    candidates.insert(current);
  }
  // first item is already in sc->kbest from 1-best pass
  candidates.erase(sc->kbest[0]);
  sc->flags |= SuperCat::HAS_CANDIDATES;
}

void
DepsRecallDecoder::lazy_next_3(std::set<KBest, std::greater<KBest> > &candidates, const KBest &kb){
  const SuperCat *l, *r, *sc = kb.sc;
  double score = sc->score;
  uint index;
  ulong hash;

  if((l = sc->left)){
    lazy_kbest_3(l, kb.left + 1);

    if((r = sc->right)){
      lazy_kbest_3(r, kb.right + 1);

      index = kb.left + 1;
      if(index < l->nk){
        hash = kb.dhash ^ l->kbest[kb.left].dhash ^ l->kbest[index].dhash;
        candidates.insert(KBest(sc, index, kb.right, score + l->kbest[index].score + r->kbest[kb.right].score, hash));
      }

      index = kb.right + 1;
      if(index < r->nk){
        hash = kb.dhash ^ r->kbest[kb.right].dhash ^ r->kbest[index].dhash;
        candidates.insert(KBest(sc, kb.left, index, score + l->kbest[kb.left].score + r->kbest[index].score, hash));
      }
    }
    else{
      index = kb.left + 1;
      if(index < l->nk){
        hash = kb.dhash ^ l->kbest[kb.left].dhash ^ l->kbest[index].dhash;
        candidates.insert(KBest(sc, index, 0, score + l->kbest[index].score, hash));
      }
    }
  }
}

void
DepsRecallDecoder::lazy_kbest_3(const SuperCat *sc, const ushort rank){
  if(!sc->has_candidates())
    get_candidates_3(sc);

  KBest current = sc->kbest[sc->nk-1];
  std::set<KBest, std::greater<KBest> > &candidates = sc_candidates[sc];
  while(sc->nk <= rank){
    if(sc->nk > 0)
      lazy_next_3(candidates, current);

    if(!candidates.empty()){
      current = *candidates.begin();
      if(!deps_seen(sc->kbest, sc->kbest + sc->nk, current))
        sc->kbest[sc->nk++] = current;
      candidates.erase(candidates.begin());
    }
    else
      break;
  }
}

ushort
DepsRecallDecoder::process_candidates_4(KBest ret[], std::set<KBest, std::greater<KBest> > &candidates) {
  ushort ncand = 1;
  ushort nk = 0;
  ret[0] = *candidates.begin();
  candidates.erase(candidates.begin());
  if(candidates.size() > 1){
    std::vector<KBest> redundant;
    while(ncand < k && candidates.size()){
      KBest tmp = *candidates.begin();
      ret[ncand] = tmp;
      candidates.erase(candidates.begin());
      if(deps_seen(ret, ret + ncand, tmp))
        redundant.push_back(tmp);
      else
        ++ncand;
    }
    for(std::vector<KBest>::iterator it = redundant.begin(); it != redundant.end(); ++it)
      lazy_next_4(ret, nk, ncand, *it);
  }
  else
    ncand = 0;

  return ncand;
}

void
DepsRecallDecoder::process_candidates_4(const SuperCat *sc, std::set<KBest, std::greater<KBest> > &candidates) {
  ushort ncand = 1;
  if(candidates.size() > 1){
    std::vector<KBest> redundant;
    while(ncand < (k-1) && candidates.size()){
      if(*candidates.begin() != sc->kbest[0]){
        KBest tmp = *candidates.begin();
        candidates.erase(candidates.begin());
        if(deps_seen(sc->kbest, sc->kbest + ncand, tmp))
          redundant.push_back(tmp);
        else
          sc->kbest[ncand++] = tmp;
      }
      else
        candidates.erase(candidates.begin());
    }
    sc->ncand = ncand - 1;

    for(std::vector<KBest>::iterator it = redundant.begin(); it != redundant.end(); ++it)
      lazy_next_4(sc->kbest, sc->nk, sc->ncand, *it);
  }
  else
    sc->ncand = 0;
}

ushort
DepsRecallDecoder::best_4(KBest ret[], Chart &chart){
  return best_3(ret, chart);
  // this method won't work inplace due to the deps filtering and the need
  // to expand out even filtered cases
  //Cell &root = chart.root();
  //std::set<KBest, std::greater<KBest> > candidates;
  //double score;

  //for(Cell::iterator i = root.begin(); i != root.end(); ++i){
    //for(const SuperCat *equiv = *i; equiv; equiv = equiv->next){
      //score = calc_score(equiv);
      //ulong hash = calc_dhash(equiv);

      //KBest current(equiv, 0, 0, score, hash);
      //if(equiv->left){
        //current.score += equiv->left->kbest[0].score;
        //current.dhash ^= equiv->left->kbest[0].dhash;
        //if(equiv->right){
          //current.score += equiv->right->kbest[0].score;
          //current.dhash ^= equiv->right->kbest[0].dhash;
        //}
      //}
      //candidates.insert(current);
    //}
  //}
  //if(candidates.size()){
    //ushort ncand = process_candidates_4(ret, candidates);
    //ushort nk = 0;

    //while(nk < k){
      //if(nk > 0)
        //lazy_next_4(ret, nk, ncand, ret[nk-1]);

      //if(ncand){
        //--ncand;
        //++nk;
      //}
      //else
        //break;
    //}

    //return nk;
  //}
  //return 0;
}

void
DepsRecallDecoder::get_candidates_4(const SuperCat *sc){
  std::set<KBest, std::greater<KBest> > candidates;
  double score;

  for(const SuperCat *equiv = sc; equiv; equiv = equiv->next){
    score = calc_score(equiv);
    ulong hash = calc_dhash(equiv);

    KBest current(equiv, 0, 0, score, hash);
    if(equiv->left){
      current.score += equiv->left->kbest[0].score;
      current.dhash ^= equiv->left->kbest[0].dhash;
      if(equiv->right){
        current.score += equiv->right->kbest[0].score;
        current.dhash ^= equiv->right->kbest[0].dhash;
      }
    }
    candidates.insert(current);
  }
  process_candidates_4(sc, candidates);
  sc->flags |= SuperCat::HAS_CANDIDATES;
}

void
DepsRecallDecoder::lazy_next_4(KBest ret[], ushort &nk, ushort &ncand, const KBest &kb){
  const SuperCat *l, *r, *sc = kb.sc;
  double score = sc->score;
  uint index;
  ulong hash;

  if((l = sc->left)){
    KBest *candidates = ret + nk;
    ushort last = nk + ncand - 1;
    lazy_kbest_4(l, kb.left + 1);

    if((r = sc->right)){
      lazy_kbest_4(r, kb.right + 1);

      index = kb.left + 1;
      bool last_changed = false;
      KBest tmp, frontier; //one from end
      if(index < l->nk){
        hash = kb.dhash ^ l->kbest[kb.left].dhash ^ l->kbest[index].dhash;
        tmp = KBest(sc, index, kb.right, score + l->kbest[index].score + r->kbest[kb.right].score, hash);
        if(!kbest_seen(ret, candidates + ncand, tmp)){
          if(deps_seen(ret, candidates + ncand, tmp))
            lazy_next_4(ret, nk, ncand, tmp);
          else {
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
      }

      index = kb.right + 1;
      if(index < r->nk){
        hash = kb.dhash ^ r->kbest[kb.right].dhash ^ r->kbest[index].dhash;
        tmp = KBest(sc, kb.left, index, score + l->kbest[kb.left].score + r->kbest[index].score, hash);
        if(!kbest_seen(ret, candidates + ncand, tmp)){
          if(deps_seen(ret, candidates + ncand, tmp))
            lazy_next_4(ret, nk, ncand, tmp);
          else {
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
    }
    else{
      index = kb.left + 1;
      if(index < l->nk){
        hash = kb.dhash ^ l->kbest[kb.left].dhash ^ l->kbest[index].dhash;
        KBest tmp(sc, index, 0, score + l->kbest[index].score, hash);
        if(!kbest_seen(ret, candidates + ncand, tmp)){
          if(deps_seen(ret, candidates + ncand, tmp))
            lazy_next_4(ret, nk, ncand, tmp);
          else {
            if(last < k-1){
              ret[++last] = tmp;
              ++ncand;
            }
            else if(ncand > 0 && tmp > ret[last])
              ret[last] = tmp;
          }
        }
      }
    }
    std::sort(candidates, candidates + ncand, std::greater<KBest>());
  }
}

void
DepsRecallDecoder::lazy_kbest_4(const SuperCat *sc, const ushort rank){
  if(!sc->has_candidates())
    get_candidates_4(sc);

  //we still need to generate the candidates from supercats we don't choose
  //this is because here they could potentially generate new possibilities
  while(sc->nk <= rank){
    if(sc->nk > 0){
      lazy_next_4(sc->kbest, sc->nk, sc->ncand, sc->kbest[sc->nk-1]);
    }

    if(sc->ncand){
      --(sc->ncand);
      ++(sc->nk);
    }
    else
      break;
  }
}

} }
