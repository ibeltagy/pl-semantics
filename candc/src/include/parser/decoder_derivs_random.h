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

    class DerivsRandomDecoder: public Decoder {
    public:
      DerivsRandomDecoder(const ushort k, const uchar k_alg)
          : Decoder(k, k_alg) {}
      virtual ~DerivsRandomDecoder(void){ /* do nothing */ }

      virtual double calc_score(const SuperCat *sc);
      virtual double best_score(const SuperCat *sc);

      virtual ushort score_0(KBest ret[], const SuperCat *sc);

      virtual ushort score_1(KBest ret[], const SuperCat *sc);

      virtual double score_2(const SuperCat *sc);

      virtual void get_candidates_3(const SuperCat *sc);
      virtual void lazy_next_3(std::set<KBest, std::greater<KBest> > &candidates, const KBest &kb);

      virtual void get_candidates_4(const SuperCat *sc);
      virtual void lazy_next_4(const SuperCat *sc, const KBest &kb);
      virtual void lazy_next_4(KBest ret[], ushort &nk, ushort &ncand, const KBest &kb);
    };
  }
}
