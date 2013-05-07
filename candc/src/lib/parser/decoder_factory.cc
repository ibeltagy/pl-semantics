// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include "parser/_decoder.h"
#include "parser/decoder_factory.h"
#include "parser/decoder_deps_recall.h"
#include "parser/decoder_derivs.h"
#include "parser/decoder_derivs_random.h"
#include "parser/decoder_gold.h"
#include "parser/decoder_derivs_unique.h"

using namespace std;

namespace NLP { namespace CCG {

void
DecoderFactory::check(const std::string &name){
  if(name != "deps" && name != "derivs" && name != "random"
     && name != "gold_deps" && name != "derivs_unique")
    throw NLP::Exception("unrecognised decoder name '" + name + "' [deps, derivs, random, gold_deps, derivs_unique]");
}

DecoderFactory::DecoderFactory(const std::string &name, const ushort k, const uchar k_alg) 
    : Decoder(k, k_alg) {
  if(name == "deps")
    decoder = new DepsRecallDecoder(k, k_alg);
  else if(name == "derivs")
    decoder = new DerivsDecoder(k, k_alg);
  else if(name == "random")
    decoder = new DerivsRandomDecoder(k, k_alg);
  else if(name == "gold_deps")
    decoder = new GoldDecoder(k, k_alg);
  else if(name == "derivs_unique")
    decoder = new DerivsUniqueDecoder(k, k_alg);
  else
    throw NLP::Exception("unrecognised decoder name '" + name + "'");
}

} }
