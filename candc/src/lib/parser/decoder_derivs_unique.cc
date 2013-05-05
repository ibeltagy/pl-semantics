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
#include "parser/decoder_deps_recall.h"
#include "parser/decoder_derivs_unique.h"

namespace NLP { namespace CCG {

double
DerivsUniqueDecoder::calc_score(const SuperCat *sc){
  return sc->score;
}

double
DerivsUniqueDecoder::best_score(const SuperCat *sc){
  double score = calc_score(sc);
  ulong hash = calc_dhash(sc);

  sc->score = score; //set to score of deps on this supercat only

  if(sc->left){
    best_equiv(sc->left);
    score += sc->left->kbest[0].score;
    if(sc->left->kbest[0].dhash)
      hash ^= sc->left->kbest[0].dhash;

    if(sc->right){
      best_equiv(sc->right);
      score += sc->right->kbest[0].score;
      if(sc->right->kbest[0].dhash)
        hash ^= sc->right->kbest[0].dhash;
    }
  }

  sc->kbest[0].dhash = hash; // hack in the hash
  return score;
}

//ushort
//DerivsUniqueDecoder::score_0(KBest ret[], const SuperCat *sc) {
  //double score = calc_score(sc);
  //uint x = 0;

  //if(sc->left){
    //KBest *l = new KBest[tmp_size];
    //ushort l_size = equiv_0(l, sc->left);

    //if(sc->right){
      //KBest *r = new KBest[tmp_size];
      //ushort r_size = equiv_0(r, sc->right);
  
      //for(ushort i = 0; i < l_size; ++i)
        //for(ushort j = 0; j < r_size; ++j)
          //ret[x++] = KBest(sc, i, j, score + l[i].score + r[j].score);

      //std::sort(ret, ret + x, std::greater<KBest>()); //sort desc by score
      //if(x > k)
        //x = k; //cap to k
      //delete [] r;
    //}
    //else{
      //for(ushort i = 0; i < l_size; ++i)
        //ret[x++] = KBest(sc, i, 0, score + l[i].score);
      //if(x > k)
        //x = k; //cap to k
    //}
    //delete [] l;
  //}
  //else
    //ret[x++] = KBest(sc, score);

  //return ushort(x); // guaranteed to be short again after truncating array
//}

//ushort
//DerivsUniqueDecoder::score_1(KBest ret[], const SuperCat *sc) {
  //double score = calc_score(sc);
  //uint x = 0;

  //if(sc->left){
    //KBest *l = new KBest[k+1];
    //ushort l_size = equiv_1(l, sc->left);

    //if(sc->right){
      //KBest *r = new KBest[k+1];
      //ushort r_size = equiv_1(r, sc->right);

      //std::set<KBest, std::greater<KBest> > candidates;
      ////bogus type aliasing warning here in gcc 4.4.3, -O3
      //candidates.insert(KBest(sc, 0, 0, score + l[0].score + r[0].score));

      //uint tmp;
      //while(!candidates.empty() && x < k) {
        //KBest current = *candidates.begin();
        //ret[x++] = current;
        //candidates.erase(candidates.begin());
  
        //tmp = current.left + 1;
        //if(tmp < l_size)
          //candidates.insert(KBest(sc, tmp, current.right, score + l[tmp].score + r[current.right].score));
        //tmp = current.right + 1;
        //if(tmp < r_size)
          //candidates.insert(KBest(sc, current.left, tmp, score + l[current.left].score + r[tmp].score));
      //}
  
      //delete [] r;
    //}
    //else{
      //for(ushort i = 0; i < l_size && x < k; ++i)
        //ret[x++] = KBest(sc, i, 0, score + l[i].score);
    //}
    //delete [] l;
  //}
  //else
    //ret[x++] = KBest(sc, score);

  //return ushort(x); // guaranteed to be short again after truncating array
//}

//double
//DerivsUniqueDecoder::score_2(const SuperCat *sc){
  //double score = calc_score(sc);

  //if(sc->left){
    //equiv_2(sc->left);
    //score += sc->left->kbest[0].score;
    //if(sc->right){
      //equiv_2(sc->right);
      //score += sc->right->kbest[0].score;
    //}
  //}
  //return score;
//}

//void
//DerivsUniqueDecoder::get_candidates_3(const SuperCat *sc){
  //std::set<KBest, std::greater<KBest> > &candidates = sc_candidates[sc];
  //candidates.clear();

  //for(const SuperCat *equiv = sc; equiv; equiv = equiv->next){
    //KBest current(equiv, 0, 0, calc_score(equiv));
    //if(equiv->left){
      //current.score += equiv->left->kbest[0].score;
      //if(equiv->right)
        //current.score += equiv->right->kbest[0].score;
    //}
    //candidates.insert(current);
  //}
  //// first item is already in sc->kbest from 1-best pass
  ////candidates.erase(candidates.begin());
  //candidates.erase(sc->kbest[0]);
  //sc->flags |= SuperCat::HAS_CANDIDATES;
//}

//void
//DerivsUniqueDecoder::lazy_next_3(std::set<KBest, std::greater<KBest> > &candidates, const KBest &kb){
  //const SuperCat *l, *r, *sc = kb.sc;
  //double score = calc_score(sc);
  //uint index;

  //if((l = sc->left)){
    //lazy_kbest_3(l, kb.left + 1);

    //if((r = sc->right)){
      //lazy_kbest_3(r, kb.right + 1);

      //index = kb.left + 1;
      //if(index < l->nk)
        //candidates.insert(KBest(sc, index, kb.right, score + l->kbest[index].score + r->kbest[kb.right].score));

      //index = kb.right + 1;
      //if(index < r->nk)
        //candidates.insert(KBest(sc, kb.left, index, score + l->kbest[kb.left].score + r->kbest[index].score));
    //}
    //else{
      //index = kb.left + 1;
      //if(index < l->nk)
        //candidates.insert(KBest(sc, index, 0, score + l->kbest[index].score));
    //}
  //}
//}

//void
//DerivsUniqueDecoder::get_candidates_4(const SuperCat *sc){
  //std::vector<KBest> candidates;

  //for(const SuperCat *equiv = sc; equiv; equiv = equiv->next){
    //KBest current(equiv, 0, 0, calc_score(equiv));
    //if(equiv->left){
      //current.score += equiv->left->kbest[0].score;
      //if(equiv->right)
        //current.score += equiv->right->kbest[0].score;
    //}
    //candidates.push_back(current);
  //}
  //std::sort(candidates.begin(), candidates.end(), std::greater<KBest>());
  //sc->ncand = (candidates.size() >= k) ? (k-1) : candidates.size() - 1;

  //// first item is already in sc->kbest from 1-best pass
  //copy_candidates_4(sc, candidates);
  //sc->flags |= SuperCat::HAS_CANDIDATES;
//}

//void
//DerivsUniqueDecoder::lazy_next_4(KBest ret[], ushort &nk, ushort &ncand, const KBest &kb){
  //const SuperCat *l, *r, *sc = kb.sc;
  //double score = calc_score(sc);
  //uint index;

  //if((l = sc->left)){
    //KBest *candidates = ret + nk;
    //ushort last = nk + ncand - 1;
    //lazy_kbest_4(l, kb.left + 1);

    //if((r = sc->right)){
      //lazy_kbest_4(r, kb.right + 1);

      //index = kb.left + 1;
      //bool last_changed = false;
      //KBest tmp, frontier; //one from last
      //if(index < l->nk){
        //tmp = KBest(sc, index, kb.right, score + l->kbest[index].score + r->kbest[kb.right].score);
        //if(!kbest_seen(ret, candidates + ncand, tmp)){
          //if(last < k-1){
            //last_changed = true;
            //frontier = ret[last];
            //ret[++last] = tmp;
            //++ncand;
          //}
          //else if(ncand > 0 && tmp > ret[last]){
            //last_changed = true;
            //frontier = ret[last-1];
            //ret[last] = tmp;
          //}
        //}
      //}

      //index = kb.right + 1;
      //if(index < r->nk){
        //tmp = KBest(sc, kb.left, index, score + l->kbest[kb.left].score + r->kbest[index].score);
        //if(!kbest_seen(ret, candidates + ncand, tmp, last_changed)){
          //if(last < k-1){
            //ret[++last] = tmp;
            //++ncand;
          //}
          //else if(last_changed){
            //// don't overwrite one from left if possible, so check last - 1
            //// first before checking last
            //if(ncand > 1 && tmp > ret[last-1]){
              ////we're replacing the frontier (one fom last) so check the edge 
              ////case where the frontier should replace the current last
              //if(frontier > ret[last])
                //ret[last] = frontier;
              //ret[last-1] = tmp;
            //}
            //else if(ncand > 0 && tmp > ret[last])
              //ret[last] = tmp;
          //}
          //else if(ncand > 0 && tmp > ret[last])
            //ret[last] = tmp;
        //}
      //}
    //}
    //else{
      //index = kb.left + 1;
      //if(index < l->nk){
        //KBest tmp(sc, index, 0, score + l->kbest[index].score);
        //if(!kbest_seen(ret, candidates + ncand, tmp)){
          //if(last < k-1){
            //ret[++last] = tmp;
            //++ncand;
          //}
          //else if(ncand > 0 && tmp > ret[last])
            //ret[last] = tmp;
        //}
      //}
    //}
    //std::sort(candidates, candidates + ncand, std::greater<KBest>());
  //}
//}

} }
