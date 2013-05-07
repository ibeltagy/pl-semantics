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

    class DecoderFactory: public Decoder {
    public:
      static void check(const std::string &name);
    private:
      Decoder *decoder;
    public:
      DecoderFactory(const std::string &name, const ushort k = 1, const uchar k_alg = 0);
      virtual ~DecoderFactory(void){ delete decoder; }

      virtual double calc_score(const SuperCat *sc){
        return decoder->calc_score(sc);
      }
      virtual double best_score(const SuperCat *sc){
        return decoder->best_score(sc);
      }
      const SuperCat *best_equiv(const SuperCat *sc){
        return decoder->best_equiv(sc);
      }
      virtual ushort best(KBest ret[], Chart &chart){
        if(k == 1)
          return decoder->best(ret, chart);
        else{
          switch(k_alg){
            case 0:
              return decoder->best_0(ret, chart);
            case 1:
              return decoder->best_1(ret, chart);
            case 2:
              return decoder->best_2(ret, chart);
            case 3:
              decoder->best(ret, chart);
              return decoder->best_3(ret, chart);
            case 4:
              decoder->best(ret, chart);
              return decoder->best_4(ret, chart);
            default:
              return decoder->best(ret, chart);
          }
        }
      }

      virtual ushort score_0(KBest ret[], const SuperCat *sc){
        return decoder->score_0(ret, sc);
      }

      virtual ushort score_1(KBest ret[], const SuperCat *sc){
        return decoder->score_1(ret, sc);
      }

      virtual double score_2(const SuperCat *sc){
        return decoder->score_2(sc);
      }

      virtual void get_candidates_3(const SuperCat *sc){
        decoder->get_candidates_3(sc);
      }

      virtual void lazy_next_3(std::set<KBest, std::greater<KBest> > &candidates, const KBest &kb){
        decoder->lazy_next_3(candidates, kb);
      }

      virtual void get_candidates_4(const SuperCat *sc){
        decoder->get_candidates_4(sc);
      }

      virtual void lazy_next_4(KBest ret[], ushort &nk, ushort &ncand, const KBest &kb){
        decoder->lazy_next_4(ret, nk, ncand, kb);
      }
    };

  }
}
