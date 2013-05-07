/* -*- Mode: C++; -*- */
// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

namespace NLP {
  namespace CCG {

    class InsideOutside;

    class DepsRecallDecoder: public Decoder {
    public:
      InsideOutside *inside_outside;

      DepsRecallDecoder(const ushort k, const uchar k_alg);
      virtual ~DepsRecallDecoder(void);

      virtual double calc_score(const SuperCat *sc);
      virtual double best_score(const SuperCat *sc);
      virtual ushort best(KBest ret[], Chart &chart);

      virtual ushort merge_0(KBest ret[], KBest a_begin[], KBest a_end[], KBest b_begin[], KBest b_end[]);
      virtual ushort best_0(KBest ret[], Chart &chart);
      virtual ushort score_0(KBest ret[], const SuperCat *sc);

      virtual ushort best_1(KBest ret[], Chart &chart);
      virtual ushort score_1(KBest ret[], const SuperCat *sc);

      virtual ushort best_2(KBest ret[], Chart &chart);
      virtual ushort equiv_2(const SuperCat *sc);
      virtual ushort equiv_2(KBest ret[], const SuperCat *sc);
      virtual double score_2(const SuperCat *sc);

      virtual ushort best_3(KBest ret[], Chart &chart);
      virtual void get_candidates_3(const SuperCat *sc);
      virtual void lazy_kbest_3(const SuperCat *sc, const ushort rank);
      virtual void lazy_next_3(std::set<KBest, std::greater<KBest> > &candidates, const KBest &kb);

      ushort process_candidates_4(KBest ret[], std::set<KBest, std::greater<KBest> > &candidates);
      void process_candidates_4(const SuperCat *sc, std::set<KBest, std::greater<KBest> > &candidates);
      virtual ushort best_4(KBest ret[], Chart &chart);
      virtual void get_candidates_4(const SuperCat *sc);
      virtual void lazy_kbest_4(const SuperCat *sc, const ushort rank);
      virtual void lazy_next_4(KBest ret[], ushort &nk, ushort &ncand, const KBest &kb);
    };
  }
}
