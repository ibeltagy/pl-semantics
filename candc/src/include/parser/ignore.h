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

    struct Ignore {
    public:
      const static uchar nrules = 15;
      const static Dep rules[nrules];

      const static uchar nconj = 1;
      const static Dep conj[nconj];

      const static uchar ncats = 17;
      const static Dep cats[ncats];

      const static uchar nother = 16;
      const static Dep other[nother];

      const static uchar nignores = 49;
    };

  }
}
