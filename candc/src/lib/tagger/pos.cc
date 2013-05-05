// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

// NLP::Tagger::POS
// takes tokenized text input and adds part of speech tags to the output
// there are two interfaces:
//   a input/output file interface
//   a single input sentence interface

#include "tagger/_baseimpl.h"
#include "tagger/pos.h"

namespace NLP { namespace Taggers {

POS::Config::Config(const OpPath *base, Mode mode,
		    const std::string &name, const std::string &desc)
  : Tagger::Config(name, desc, base, mode, 1.414, 200),
    number_unknowns(*this, "number_unknowns", "the set of tags for unknown number tokens", "//number_unknowns", &path),
    assign_gold_unknown(*this, "assign_unknown_gold", "assign gold-standard pos tag to unknown words (requires ifmt = %w|%g)", false) {}

typedef Tagger::Impl Base;

class POS::Impl: public Base {
public:
  NLP::Tags number_klasses;	// tags which can be assigned to previously unseen numbers

  Impl(POS::Config &cfg);
  virtual ~Impl(void);
protected:
  BinAttributes uc_attribs;
  BinAttributes hyph_attribs;
  BinAttributes dig_attribs;
  AffixAttributes a_attribs;

  const bool assign_unknown_gold;

  mutable ulong nunknowns;
  mutable ulong nunknowns_nnp;
  mutable ulong nunknowns_not_nnp;
  mutable ulong nunknowns_correct;
  mutable ulong nunknowns_nnp_correct;
  mutable ulong nunknowns_not_nnp_correct;
  mutable ulong ntotal;

  void add_prefixes(const RawWord &raw, PDF &dist) const;
  void add_suffixes(const RawWord &raw, PDF &dist) const;
  void add_special(const RawWord &raw, PDF &dist) const;

  void reg_attributes(void);
  void create_unknowns(const Tagger::Config &cfg);
  void can_sentence(const Sentence &sent, State &state) const;
  const Tags &get_permitted(const State &state, ulong i, ulong DICT_CUTOFF) const;
  void add_features(const State &state, ulong i, PDF &pdf) const;
  void unpack_tags(State &state, Sentence &sent) const;
  void unpack_mtags(State &state, Sentence &sent, double BETA) const;

  virtual void tag(NLP::IO::Reader &reader, NLP::IO::Writer &writer,
           Algorithm alg, ulong DICT_CUTOFF) const;
  virtual void mtag(NLP::IO::Reader &reader, NLP::IO::Writer &writer, Algorithm alg,
            const ulong DICT_CUTOFF, double BETA) const;

  void stats(void) const;
};

POS::Impl::Impl(POS::Config &cfg)
  : Base("POS", cfg),
	assign_unknown_gold(cfg.assign_gold_unknown()),
  nunknowns(0), nunknowns_nnp(0), nunknowns_not_nnp(0), nunknowns_correct(0),
  nunknowns_nnp_correct(0), nunknowns_not_nnp_correct(0), ntotal(0){
  create_model(cfg);
}

POS::Impl::~Impl(void){}

// add the prefix and suffix features by calling the
// Impl::_add_attribute method with the attributes
// retrieved from the Attributes classes

void
POS::Impl::add_prefixes(const RawWord &raw, PDF &dist) const {
  RawWord::const_iterator i = raw.begin();

  Affix affix(*i);
  if(_add_attribute(a_attribs(Types::pref, affix), dist) && ++i != raw.end())
    if(_add_attribute(a_attribs(Types::pref, affix += *i), dist) && ++i != raw.end())
      if(_add_attribute(a_attribs(Types::pref, affix += *i), dist) && ++i != raw.end())
        _add_attribute(a_attribs(Types::pref, affix += *i), dist);
}

void
POS::Impl::add_suffixes(const RawWord &raw, PDF &dist) const {
  RawWord::const_reverse_iterator i = raw.rbegin();

  Affix affix(*i);
  if(_add_attribute(a_attribs(Types::suff, affix), dist) && ++i != raw.rend())
    if(_add_attribute(a_attribs(Types::suff, affix += *i), dist) && ++i != raw.rend())
      if(_add_attribute(a_attribs(Types::suff, affix += *i), dist) && ++i != raw.rend())
        _add_attribute(a_attribs(Types::suff, affix += *i), dist);
}

void
POS::Impl::add_special(const RawWord &raw, PDF &dist) const {
  bool upper = false;
  bool digit = false;
  bool hyphen = false;

  for(RawWord::const_iterator i = raw.begin(); i != raw.end(); i++){
    upper = upper || isupper(*i);
    digit = digit || isdigit(*i);
    hyphen = hyphen || *i == '-';
  }

  if(upper)
    _add_attribute(uc_attribs(), dist);

  if(hyphen)
    _add_attribute(hyph_attribs(), dist);

  if(digit)
    _add_attribute(dig_attribs(), dist);
}

void
POS::Impl::reg_attributes(void){
  Base::reg_attributes();

  // tag types
  registry.reg(Types::pt, pk_attribs);
  registry.reg(Types::ppt, ppkpk_attribs);

  // prefix and suffix types
  registry.reg(Types::suff, a_attribs);
  registry.reg(Types::pref, a_attribs);

  // orthographic types
  registry.reg(Types::has_digit, dig_attribs);
  registry.reg(Types::has_hyphen, hyph_attribs);
  registry.reg(Types::has_uppercase, uc_attribs);
}

void
POS::Impl::create_unknowns(const Tagger::Config &tagger_cfg){
  const POS::Config &cfg = dynamic_cast<const POS::Config &>(tagger_cfg);

  klasses.load_tags(cfg.unknowns(), unknown_klasses);
  klasses.load_tags(cfg.number_unknowns(), number_klasses);
}

void
POS::Impl::can_sentence(const Sentence &sent, State &state) const {
  if(sent.words.size() > maxwords)
    throw NLP::Exception("sentence length exceeds maximum number of words for POS tagger");

  state.raws = sent.words;
  lexicon.can(state.raws, state.words);
  klasses.tag(sent.gold_pos, state.gold_pos);
}

const Tags &
POS::Impl::get_permitted(const State &state, ulong i, ulong DICT_CUTOFF) const {
  if(state.words[i].freq() >= DICT_CUTOFF){
    Tags &tags = tagdict[state.words[i]];
    if(tags.size())
      return tags;
  }

  if(is_number(state.raws[i]))
    return number_klasses;
  else
    return unknown_klasses;
}

void
POS::Impl::add_features(const State &state, ulong i, PDF &pdf) const {
  const RawWord &raw = state.raws[i];
  const Word word = state.words[i];

  _add_attribute(w_attribs(Types::w, word), pdf);

  if(word.freq() < rare_cutoff){
    add_prefixes(raw, pdf);
    add_suffixes(raw, pdf);
    add_special(raw, pdf);
  }

  add_surrounding_words(state.words, i, pdf);
}

void
POS::Impl::unpack_tags(State &state, Sentence &sent) const {
  _unpack_tags(state, sent.pos, false);
  const int LEN = state.raws.size();
  const bool GOLD_POS = (state.gold_pos.size() > 0);
  for(int i = 0; i < LEN; ++i){
    bool correct = false;
    if(state.words[i].freq() == 0){
      ++nunknowns;
      if(GOLD_POS){
        if(sent.gold_pos[i] == sent.pos[i]){
          ++nunknowns_correct;
          correct = true;
        }
        if(sent.gold_pos[i] == "NNP" || sent.gold_pos[i] == "NNPS"){
          ++nunknowns_nnp;
          if(correct)
            ++nunknowns_nnp_correct;
        }
        else{
          ++nunknowns_not_nnp;
          if(correct)
            ++nunknowns_not_nnp_correct;
        }
      }
    }
    ++ntotal;
  }
}

void
POS::Impl::unpack_mtags(State &state, Sentence &sent, const double BETA) const {
  _unpack_mtags(state, sent.mpos, BETA, false);
  const int LEN = state.raws.size();
  const bool GOLD_POS = (state.gold_pos.size() > 0);
  for(int i = 0; i < LEN; ++i){
    bool correct = false;
    if(state.words[i].freq() == 0){
      ++nunknowns;
      if(GOLD_POS){
        if(sent.gold_pos[i] == sent.pos[i]){
          ++nunknowns_correct;
          correct = true;
        }
        if(sent.gold_pos[i] == "NNP" || sent.gold_pos[i] == "NNPS"){
          ++nunknowns_nnp;
          if(correct)
            ++nunknowns_nnp_correct;
        }
        else{
          ++nunknowns_not_nnp;
          if(correct)
            ++nunknowns_not_nnp_correct;
        }
      }
    }
    ++ntotal;
  }
}

POS::POS(POS::Config &cfg): Tagger(cfg, new Impl(cfg)){}
POS::POS(POS &other): Tagger(other){}
POS::~POS(void){}

const Tags &POS::number_tags(void) const {
  return dynamic_cast<const POS::Impl *>(impl_)->number_klasses;
}

inline void
POS::Impl::tag(NLP::IO::Reader &reader, NLP::IO::Writer &writer,
                  const Algorithm alg, const ulong DICT_CUTOFF) const {
  Tagger::Impl::tag(reader, writer, alg, DICT_CUTOFF);
  stats();
}

inline void
POS::Impl::mtag(NLP::IO::Reader &reader, NLP::IO::Writer &writer,
                   const Algorithm alg, const ulong DICT_CUTOFF,
                   const double BETA) const { 
  Tagger::Impl::mtag(reader, writer, alg, DICT_CUTOFF, BETA);
  stats();
}

inline void
POS::Impl::stats(void) const{
  std::cerr << "nunknowns: " << nunknowns << '\n';
  std::cerr << "nunknowns (proper noun): " << nunknowns_nnp << '\n';
  std::cerr << "nunknowns (not proper noun): " << nunknowns_not_nnp << '\n';
  std::cerr << "nunknowns (correct): " << nunknowns_correct << '\n';
  std::cerr << "nunknowns (correct, proper noun): " << nunknowns_nnp_correct << '\n';
  std::cerr << "nunknowns (correct, not proper noun): " << nunknowns_not_nnp_correct << '\n';
  std::cerr << "total tokens: " << ntotal << "\n\n";
  std::cerr << "pct unknown: " << (nunknowns * 1.0) / ntotal * 100 << "%\n";
  std::cerr << "pct unknown (proper noun): " << (nunknowns_nnp * 1.0) / nunknowns * 100 << "%\n";
  std::cerr << "pct unknown (not proper noun): " << (nunknowns_not_nnp * 1.0) / nunknowns * 100 << "%\n";
  std::cerr << "pct unknown (correct): " << (nunknowns_correct * 1.0) / nunknowns * 100 << "%\n";
  std::cerr << "pct unknown (correct, proper noun): " << (nunknowns_nnp_correct * 1.0) / nunknowns_nnp * 100 << "%\n";
  std::cerr << "pct unknown (correct, not proper noun): " << (nunknowns_not_nnp_correct * 1.0) / nunknowns_not_nnp * 100 << "%\n";
}

} }
