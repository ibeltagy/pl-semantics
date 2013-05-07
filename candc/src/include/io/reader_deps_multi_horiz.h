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
  namespace IO {

class DepsMultiHReader: public MultiHReader {
protected:
  char deps_buffer[SBUFFER];
  ulong deps_len;
  ulong ndepslines;

  std::string deps_uri;
  std::string DEPS_PREFACE;

  std::istream &deps;

  bool next_deps_line(void);

public:
  DepsMultiHReader(std::istream &in, std::istream &deps, const std::string &uri,
                   const std::string &deps_uri, 
                   const Sentence::FieldNames &fieldnames, char SEP = '|',
	                 const std::string &name = "");
  virtual ~DepsMultiHReader(void){ /* do nothing */ }

  virtual void reset(void);
  virtual bool next(NLP::Sentence &sent, bool add = false, bool expect = false);
};

  }
}
