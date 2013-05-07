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

    class BankDepsPrinter: public StreamPrinter {
    protected:
      void recurse_ccgbank(const KBest &kb, Sentence &sent);
      void recurse_deps(const KBest &kb, Sentence &sent);

      virtual void unary(Sentence &sent);
      virtual void derivation(const KBest &kb, Sentence &sent, const ushort rank);
      virtual void get_grs(const KBest &kb, Sentence &sent);
      virtual void lexical(Sentence &sent);
    public:
      FilledDeps filled;
      GRs grs;

      BankDepsPrinter(Categories &cats, const Format FORMAT,
                     IO::Output &out, IO::Log &log)
        : StreamPrinter(cats, FORMAT, out, log){}

      virtual ~BankDepsPrinter(void){ /* do nothing */ }

      virtual void header(const std::string &PREFACE);
      virtual void footer(void){}

      virtual void parsed(const KBest ret[], ushort nparses, Sentence &sent, double BETA, ulong DICT_CUTOFF, std::string meta){
        filled.clear();
        grs.clear();
        StreamPrinter::parsed(ret, nparses, sent, BETA, DICT_CUTOFF, meta);
      }

    };

  }
}
