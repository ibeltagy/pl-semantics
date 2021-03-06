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

    class CCGbankPrinter: public StreamPrinter {
    protected:
      void recurse(const KBest &kb, Sentence &sent);

      virtual void unary(Sentence &sent);
      virtual void derivation(const KBest &root, Sentence &sent, const ushort rank);
      virtual void lexical(Sentence &sent);
    public:
      CCGbankPrinter(Categories &cats, const Format FORMAT,
                     IO::Output &out, IO::Log &log)
        : StreamPrinter(cats, FORMAT, out, log){}

      virtual ~CCGbankPrinter(void){ /* do nothing */ }

      virtual void header(const std::string &PREFACE);
      virtual void footer(void){}
    };

  }
}
