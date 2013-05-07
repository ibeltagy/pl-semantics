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
    class Cat;
  }

  typedef std::string Raw;
  typedef Raw RawWord;
  typedef Raw RawTag;
  typedef Raw RawCat;
  typedef Raw RawMarkedup;

  struct Dep {
    const CCG::Cat *cat;
    ulong head;
    ulong filler;
    ulong jslot;
    ulong ruleid;
    RawCat plain_str;
    RawMarkedup markedup_str;
    RawWord head_word;
    RawWord filler_word;

    explicit Dep(void) : cat(0), head(0), filler(0), jslot(0), ruleid(0),
      plain_str(""), markedup_str(""), head_word(""), filler_word("") {}

    explicit Dep(const CCG::Cat *cat, ulong head, ulong filler, ulong jslot,
                 ulong ruleid, const char *plain_str, const char *markedup_str,
                 const char *head_word, const char *filler_word) : cat(cat),
      head(head), filler(filler), jslot(jslot), ruleid(ruleid), plain_str(plain_str),
      markedup_str(markedup_str), head_word(head_word), filler_word(filler_word) {}
  };

}
