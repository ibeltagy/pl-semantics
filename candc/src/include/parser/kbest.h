// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) Dominick Ng
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

namespace NLP {
  namespace CCG {

    class SuperCat;

    // Used to create a k-best ranking. stored in an array within a supercat
    // left and right designate which of the kbest parses from the left/right
    // children of the supercat are used
    // score is the aggregate score: score(sc) + score(left) + score(right)
    class KBest {
      public:
        double score;
        const SuperCat *sc;
        ulong dhash; //dependency hash
        ulong nbad; //for gold decoding
        ushort left;
        ushort right;

        KBest(void) :
          score(0.0), sc(0), dhash(0), nbad(0), left(0), right(0) {}

        KBest(const SuperCat *sc, const ushort left, const ushort right) : 
          score(0.0), sc(sc), dhash(0), nbad(0), left(left), right(right) {}

        KBest(const SuperCat *sc, const ushort left, const ushort right, double score) : 
          score(score), sc(sc), dhash(0), nbad(0), left(left), right(right) {}

        KBest(const SuperCat *sc, const ushort left, const ushort right, double score, const ulong dhash) : 
          score(score), sc(sc), dhash(dhash), nbad(0), left(left), right(right) {}

        KBest(const SuperCat *sc, const ushort left, const ushort right, double score, const ulong dhash, const ulong nbad) : 
          score(score), sc(sc), dhash(dhash), nbad(nbad), left(left), right(right) {}

        KBest(const SuperCat *sc, const double score) : 
          score(score), sc(sc), dhash(0), nbad(0), left(0), right(0){}

        ~KBest(void) { /* do nothing */}

        bool operator<(const KBest &k2) const { return score < k2.score; }

        /* DNG - compiler bug in gcc 4.4.x causes a bogus type-aliasing 
         * warning here in the DerivsDecoder and DepsRecallDecoder. the warning
         * can safely be ignored. 
         *
         * This operation requires more than just the score comparison as STL
         * set will use !(a > b) && !(b > a) to test for equality 
         *
         * Degenerate cases include same scores, supercats, and sums of left 
         * and right (e.g. [1,3], [2,2])
         *
         * So if the scores are equal we order by the sum of left and right
         * indices. If these are equal, we then order by the value of the
         * sc pointer */
        inline bool operator>(const KBest &k2) const {
          if(score == k2.score){
            if(nbad != k2.nbad)
              return nbad < k2.nbad; //want less bad to be better
            else if(sc == k2.sc){
              if((left + right) == (k2.left + k2.right))
                return left < k2.left; //cannot be equal with same sc
              return (left + right) < (k2.left + k2.right);
            }
            else{
              if((left + right) == (k2.left + k2.right))
                return sc < k2.sc; //lefts could be equal but sc's won't be
              return (left + right) < (k2.left + k2.right);
            }
          }
          return score > k2.score;
        }

        inline bool operator==(const KBest &k2) const {
          return sc == k2.sc && left == k2.left && right == k2.right && score == k2.score && dhash == k2.dhash; 
        }

        inline bool operator!=(const KBest &k2) const {
          return !(sc == k2.sc);
        }

        inline bool eq(const KBest &k2) const {
          return sc == k2.sc && left == k2.left && right == k2.right && score == k2.score;
        }
    };
  }
}
