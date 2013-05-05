// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include "base.h"

#include "io/reader.h"
#include "io/reader_horiz.h"
#include "io/reader_multi_horiz.h"
#include "io/reader_deps_multi_horiz.h"

namespace NLP { namespace IO {

typedef Sentence::FieldNames FN;

DepsMultiHReader::DepsMultiHReader(std::istream &in, std::istream &deps, 
                           const std::string &uri, const std::string &deps_uri, 
                           const FN &fieldnames, char SEP, 
                           const std::string &name):
    MultiHReader(in, uri, fieldnames, SEP, name), deps_len(0), ndepslines(0), deps(deps){
  DEPS_PREFACE = read_preface(deps_uri, deps, ndepslines, PREFACE_OPTIONAL);
}

bool
DepsMultiHReader::next_deps_line(void){
  deps.getline(deps_buffer, sizeof(deps_buffer), '\x0a');
  deps_len = deps.gcount();

  if(deps.eof() && deps_len == 0)
    return false;

  // remove the Windows carriage return if it exists
  if(deps_buffer[deps_len - 1] == '\x0d')
    deps_buffer[--deps_len] = '\0';

  ++ndepslines;

  if(!deps)
    die(msg << "unexpected stream error (probably the dependency string is too long)");

  return true;
}
void
DepsMultiHReader::reset(void){
  nlines = 0;
  ndepslines = 0;
  in.clear();
  in.seekg(0, std::ios::beg);
  deps.clear();
  deps.seekg(0, std::ios::beg);
  if(!in)
    die(msg << "the input stream could not be seeked to the beginning");
  if(!deps)
    die(msg << "the deps stream could not be seeked to the beginning");
  PREFACE = read_preface(uri, in, nlines, PREFACE_OPTIONAL);
  DEPS_PREFACE = read_preface(deps_uri, deps, ndepslines, PREFACE_OPTIONAL);
}

bool
DepsMultiHReader::next(NLP::Sentence &sent, bool add, bool expect){
  MultiHReader::next(sent, add, expect);

  if(!next_deps_line())
    return check(add, expect, false);

  struct Dep dep;

  for(; deps_len != 1; next_deps_line()){
    char *begin = deps_buffer;
    char *end = begin;
    if(*begin == '\0')
      return check(add, expect, true);

    if(*begin == ' ')
      die(msg << "whitespace at the beginning of a dependency is illegal");

    while(*end && *end != ' ')
      ++end;
    begin = end;
    while(*begin && *begin != '_')
      --begin;
    if(!isdigit(*(++begin)))
      die(msg << "head index not found");
    dep.head = atol(begin);
    begin = ++end;

    for(; *end; ++end){
      if(*end == ' '){
        if(begin == end){
          while(*end && *end == ' ')
            ++end;
          if(*end)
            die(msg << "multiple spaces between dep components is illegal (after '" << sent.last() << "')");
          else
            die(msg << "unexpected end of dependency string");
        }
        *end = '\0';
        dep.plain_str = begin;
        break;
      }
    }

    begin = ++end;
    if(!isdigit(*end))
      die(msg << "jslot not found");
    while(*end != ' ')
      ++end;
    dep.jslot = atol(begin);
    ++end;

    while(*end && *end != '\0')
      ++end;
    begin = end - 1;
    while(*begin && *begin != '_')
      --begin;
    if(!isdigit(*(++begin)))
      die(msg << "filler index not found");
    dep.filler = atol(begin);

    sent.deps.push_back(dep);
  }

  return check(add, expect, true);

}


} }
