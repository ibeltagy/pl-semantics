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

    class SuperCat;
    class Chart;
    class KBest;

    class Decoder {
    public:
      const uint tmp_size;
      const ushort k;
      const ushort k_alg;
      mutable ulong ncomps;
      mutable ulong ncollisions;

      std::map<const SuperCat *, std::set<KBest, std::greater<KBest> > > sc_candidates;

      explicit Decoder(const ushort k, const uchar k_alg)
        : tmp_size(k*k + 1), k(k), k_alg(k_alg), ncomps(0), ncollisions(0), sc_candidates() {}
      virtual ~Decoder(void){ /* do nothing */ }

      virtual double calc_score(const SuperCat *sc) = 0;

      virtual ulong _calc_dhash(const Filled *filled) const;
      virtual ulong calc_dhash(const SuperCat *sc) const;
      virtual bool equal_deps(const KBest &kb1, const KBest &kb2) const;

      virtual bool has_same_deps(const KBest &kb1, const KBest &kb2) const;
      virtual void get_filled_deps(const KBest &kb, FilledDeps &deps) const;

      virtual double best_score(const SuperCat *sc) = 0;
      virtual const SuperCat *best_equiv(const SuperCat *sc);
      virtual ushort best(KBest ret[], Chart &chart);

      bool deps_seen(KBest ret_start[], KBest ret_end[], const KBest &kb) const;
      bool better_deps_seen(KBest ret_start[], KBest ret_end[], const KBest &kb) const;
      bool kbest_seen(KBest ret_start[], KBest ret_end[], const KBest &kb, const bool last_changed = false) const;
      void copy_candidates_4(const SuperCat *sc, std::vector<KBest> &candidates);

      virtual ushort best_0(KBest ret[], Chart &chart);
      virtual ushort merge_0(KBest ret[], KBest a_begin[], KBest a_end[], KBest b_begin[], KBest b_end[]);
      ushort equiv_0(KBest ret[], const SuperCat *sc);
      virtual ushort score_0(KBest ret[], const SuperCat *sc) = 0;

      virtual ushort best_1(KBest ret[], Chart &chart);
      ushort equiv_1(KBest ret[], const SuperCat *sc);
      virtual ushort score_1(KBest ret[], const SuperCat *sc) = 0;

      virtual ushort best_2(KBest ret[], Chart &chart);
      virtual ushort equiv_2(KBest ret[], const SuperCat *sc);
      virtual ushort equiv_2(const SuperCat *sc);
      virtual double score_2(const SuperCat *sc) = 0;

      virtual ushort best_3(KBest ret[], Chart &chart);
      virtual void lazy_next_3(std::set<KBest, std::greater<KBest> > &candidates, const KBest &kb) = 0;
      virtual void lazy_kbest_3(const SuperCat *sc, const ushort rank);
      virtual void get_candidates_3(const SuperCat *sc) = 0;

      virtual ushort best_4(KBest ret[], Chart &chart);
      virtual void lazy_next_4(KBest ret[], ushort &nk, ushort &ncand, const KBest &kb) = 0;
      virtual void lazy_kbest_4(const SuperCat *sc, const ushort rank);
      virtual void get_candidates_4(const SuperCat *sc) = 0;
    };
  }
}
