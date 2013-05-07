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

    class GoldDepsPrinter : public StreamPrinter {
    protected:
      ulong ngood;
      ulong nbad;
      ulong nignored;
      ulong nmissing;

      double precision(void) {
        double x = ngood + nbad;
        if(x){
          return ngood / x;
        }
        return 0;
      }

      double recall(void) {
        double x = ngood + nmissing;
        if(x){
          return ngood / x;
        }
        return 0;
      }

      double fscore(void) {
        double p = precision();
        double r = recall();

        if(p + r)
          return 2 * p * r / (p + r);
        return 0;
      }

      void recurse(const KBest &kb, Sentence &sent, Deps &good, Deps &bad, Deps &ignored, Deps &missing, unsigned int depth);
      void _print_dep(Sentence &sent, const Dep &dep);
      void _print_deps(Sentence &sent, const char *suffix, const Deps &deps);
      void print_deps(Sentence &sent, const Deps &good, const Deps &bad, const Deps &ignored, const Deps &missing);

      virtual void unary(Sentence &sent) { }
      virtual void derivation(const KBest &kb, Sentence &sent, const ushort rank);
    public:
      GoldDepsPrinter(Categories &cats, const Format FORMAT, IO::Output &out, IO::Log &log) : StreamPrinter(cats, FORMAT, out, log), ngood(0), nbad(0), nignored(0), nmissing(0) { }
      virtual ~GoldDepsPrinter(void){ /* do nothing */ }

      virtual void header(const std::string &PREFACE);
      virtual void footer(void);
    };

  }
}
