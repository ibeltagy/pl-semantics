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

    class GoldDecoder: public DerivsDecoder {
    public:
      GoldDecoder(const ushort k, const uchar k_alg)
          : DerivsDecoder(k, k_alg) {}
      virtual ~GoldDecoder(void){ /* do nothing */ }

      virtual double calc_score(const SuperCat *sc);
      virtual ulong count_bad(const SuperCat *sc);
      virtual const SuperCat *best_equiv(const SuperCat *sc);
      virtual ushort best(KBest ret[], Chart &chart);

      virtual ushort score_0(KBest ret[], const SuperCat *sc);

      virtual ushort score_1(KBest ret[], const SuperCat *sc);

      virtual ushort equiv_2(const SuperCat *sc);

      virtual ushort best_3(KBest ret[], Chart &chart);
      virtual void get_candidates_3(const SuperCat *sc);
      virtual void lazy_next_3(std::set<KBest, std::greater<KBest> > &candidates, const KBest &kb);

      virtual ushort best_4(KBest ret[], Chart &chart);
      virtual void get_candidates_4(const SuperCat *sc);
      virtual void lazy_next_4(KBest ret[], ushort &nk, ushort &ncand, const KBest &kb);
    };
  }
}
