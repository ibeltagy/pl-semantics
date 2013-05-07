// C&C NLP tools
// Copyright (c) Universities of Edinburgh, Oxford and Sydney
// Copyright (c) James R. Curran
//
// This software is covered by a non-commercial use licence.
// See LICENCE.txt for the full text of the licence.
//
// If LICENCE.txt is not included in this distribution
// please email candc@it.usyd.edu.au to obtain a copy.

#include "parser/_printer.h"
#include "parser/print_factory.h"
#include "parser/print_deps.h"
#include "parser/print_grs.h"
#include "parser/print_prolog.h"
#include "parser/print_boxer.h"
#include "parser/print_ccgbank.h"
#include "parser/print_xml.h"
#include "parser/print_debug.h"
#include "parser/print_js.h"
#include "parser/print_bankdeps.h"
#include "parser/print_gold_deps.h"

using namespace std;

namespace NLP { namespace CCG {

void
PrinterFactory::check(const std::string &name){
  if(name != "deps" && name != "prolog" && name != "boxer" &&
     name != "ccgbank" && name != "grs" && name != "xml" &&
     name != "debug" && name != "js" && name != "bankdeps" && 
     name != "gold_deps")
    throw NLP::Exception("unrecognised printer name '" + name +
                         "' [deps, prolog, boxer, ccgbank, grs, xml, debug, js, bankdeps, gold_deps]");
}

StreamPrinter *
PrinterFactory::create_printer(const std::string &name) const {
  if(name == "deps")
    return new DepsPrinter(cats, FORMAT, out, log);
  else if(name == "prolog")
    return new PrologPrinter(cats, FORMAT, out, log);
  else if(name == "boxer")
    return new BoxerPrinter(cats, FORMAT, out, log);
  else if(name == "ccgbank")
    return new CCGbankPrinter(cats, FORMAT, out, log);
  else if(name == "grs")
    return new GRsPrinter(cats, FORMAT, out, log);
  else if(name == "xml")
    return new XMLPrinter(cats, FORMAT, out, log);
  else if(name == "debug")
    return new DebugPrinter(cats, FORMAT, out, log);
  else if(name == "js")
    return new JSPrinter(cats, FORMAT, out, log);
  else if(name == "bankdeps")
    return new BankDepsPrinter(cats, FORMAT, out, log);
  else if(name == "gold_deps")
    return new GoldDepsPrinter(cats, FORMAT, out, log);
  else
    throw NLP::Exception("unrecognised printer name '" + name + "'");
}

PrinterFactory::PrinterFactory(const std::string &name, IO::Output &out,
                               IO::Log &log, Categories &cats,
                               const StreamPrinter::Format FORMAT)
  : StreamPrinter(cats, FORMAT, out, log),
    printer(create_printer(name)){}

} }
