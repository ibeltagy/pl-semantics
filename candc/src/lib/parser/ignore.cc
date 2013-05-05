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

#include "parser/ignore.h"

namespace NLP { namespace CCG {

const Dep Ignore::rules[Ignore::nrules] = {
    Dep(0, 0, 0, 0, 7, "", "", "", ""),
    Dep(0, 0, 0, 0, 11, "", "", "", ""),
    Dep(0, 0, 0, 0, 12, "", "", "", ""),
    Dep(0, 0, 0, 0, 14, "", "", "", ""),
    Dep(0, 0, 0, 0, 15, "", "", "", ""),
    Dep(0, 0, 0, 0, 16, "", "", "", ""),
    Dep(0, 0, 0, 0, 17, "", "", "", ""),
    Dep(0, 0, 0, 0, 51, "", "", "", ""),
    Dep(0, 0, 0, 0, 52, "", "", "", ""),
    Dep(0, 0, 0, 0, 56, "", "", "", ""),
    Dep(0, 0, 0, 0, 91, "", "", "", ""),
    Dep(0, 0, 0, 0, 92, "", "", "", ""),
    Dep(0, 0, 0, 0, 95, "", "", "", ""),
    Dep(0, 0, 0, 0, 96, "", "", "", ""),
    Dep(0, 0, 0, 0, 98, "", "", "", ""),
};

const Dep Ignore::conj[Ignore::nconj] = {
    Dep(0, 0, 0, 1, 0, "conj", "conj", "", ""),
};

const Dep Ignore::cats[Ignore::ncats] = {
    Dep(0, 0, 0, 1, 0, "", "((S[to]{_}\\NP{Z}<1>){_}/(S[b]{Y}<2>\\NP{Z*}){Y}){_}", "", ""),
    Dep(0, 0, 0, 1, 2, "", "((S[to]{_}\\NP{Z}<1>){_}/(S[b]{Y}<2>\\NP{Z*}){Y}){_}", "", ""),
    Dep(0, 0, 0, 1, 3, "", "((S[to]{_}\\NP{Z}<1>){_}/(S[b]{Y}<2>\\NP{Z*}){Y}){_}", "", ""),
    Dep(0, 0, 0, 1, 6, "", "((S[to]{_}\\NP{Z}<1>){_}/(S[b]{Y}<2>\\NP{Z*}){Y}){_}", "", ""),
    Dep(0, 0, 0, 1, 9, "", "((S[to]{_}\\NP{Z}<1>){_}/(S[b]{Y}<2>\\NP{Z*}){Y}){_}", "", ""),

    Dep(0, 0, 0, 1, 6, "", "((S[b]{_}\\NP{Y}<1>){_}/NP{Z}<2>){_}", "", ""),

    Dep(0, 0, 0, 1, 6, "", "((S[b]{_}\\NP{Y}<1>){_}/PP{Z}<2>){_}", "", ""),

    Dep(0, 0, 0, 1, 6, "", "(((S[b]{_}\\NP{Y}<1>){_}/PP{Z}<2>){_}/NP{W}<3>){_}", "", ""),

    Dep(0, 0, 0, 1, 13, "", "(S[X]{Y}/S[X]{Y}<1>){_}", "", ""),
    Dep(0, 0, 0, 1, 5, "", "(S[X]{Y}/S[X]{Y}<1>){_}", "", ""),
    Dep(0, 0, 0, 1, 55, "", "(S[X]{Y}/S[X]{Y}<1>){_}", "", ""),

    Dep(0, 0, 0, 2, 97, "", "((S[X]{Y}/S[X]{Y}){Z}\\(S[X]{Y}/S[X]{Y}){Z}<1>){_}", "", ""),

    Dep(0, 0, 0, 2, 4, "", "((S[X]{Y}\\NP{Z}){Y}\\(S[X]{Y}<1>\\NP{Z}){Y}){_}", "", ""),
    Dep(0, 0, 0, 2, 93, "", "((S[X]{Y}\\NP{Z}){Y}\\(S[X]{Y}<1>\\NP{Z}){Y}){_}", "", ""),
    Dep(0, 0, 0, 2, 8, "", "((S[X]{Y}\\NP{Z}){Y}\\(S[X]{Y}<1>\\NP{Z}){Y}){_}", "", ""),

    Dep(0, 0, 0, 2, 94, "", "((S[X]{Y}\\NP{Z}){Y}/(S[X]{Y}<1>\\NP{Z}){Y}){_}", "", ""),
    Dep(0, 0, 0, 2, 18, "", "((S[X]{Y}\\NP{Z}){Y}/(S[X]{Y}<1>\\NP{Z}){Y}){_}", "", ""),
};

const Dep Ignore::other[Ignore::nother] = {
    Dep(0, 0, 0, 1, 0, "", "((S[pt]{_}\\NP{Y}<1>){_}/(S[ng]{Z}<2>\\NP{Y*}){Z}){_}", "been", ""),
    Dep(0, 0, 0, 1, 0, "", "((S[pt]{_}\\NP{Y}<1>){_}/NP{Z}<2>){_}", "been", "there"),
    Dep(0, 0, 0, 1, 0, "", "((S[pt]{_}\\NP{Y}<1>){_}/NP{Z}<2>){_}", "been", "There"),

    Dep(0, 0, 0, 1, 0, "", "((S[b]{_}\\NP{Y}<1>){_}/NP{Z}<2>){_}", "be", "there"),
    Dep(0, 0, 0, 1, 0, "", "((S[b]{_}\\NP{Y}<1>){_}/NP{Z}<2>){_}", "be", "There"),

    Dep(0, 0, 0, 1, 0, "", "((S[pt]{_}\\NP{Y}<1>){_}/(S[pss]{Z}<2>\\NP{Y*}){Z}){_}", "been", ""),
    Dep(0, 0, 0, 1, 0, "", "((S[pt]{_}\\NP{Y}<1>){_}/(S[adj]{Z}<2>\\NP{Y*}){Z}){_}", "been", ""),

    Dep(0, 0, 0, 1, 0, "", "((S[b]{_}\\NP{Y}<1>){_}/(S[pss]{Z}<2>\\NP{Y*}){Z}){_}", "be", ""),

    Dep(0, 0, 0, 1, 0, "", "((S[b]{_}\\NP{Y}<1>){_}/(S[adj]{Z}<2>\\NP{Y*}){Z}){_}", "be", ""),
    Dep(0, 0, 0, 1, 0, "", "((S[b]{_}\\NP{Y}<1>){_}/(S[ng]{Z}<2>\\NP{Y*}){Z}){_}", "be", ""),
    Dep(0, 0, 0, 1, 0, "", "((S[b]{_}\\NP{Y}<1>){_}/(S[pss]{Z}<2>\\NP{Y*}){Z}){_}", "be", ""),

    Dep(0, 0, 0, 1, 0, "", "((S[b]{_}\\NP{Y}<1>){_}/(S[pt]{Z}<2>\\NP{Y*}){Z}){_}", "have", ""),
    Dep(0, 0, 0, 1, 0, "", "((S[ng]{_}\\NP{Y}<1>){_}/(S[to]{Z}<2>\\NP{Y*}){Z}){_}", "going", ""),
    Dep(0, 0, 0, 1, 0, "", "((S[b]{_}\\NP{Y}<1>){_}/(S[to]{Z}<2>\\NP{Y*}){Z}){_}", "have", ""),
    Dep(0, 0, 0, 1, 0, "", "(S[adj]{_}\\NP{Y}<1>){_}", "Here", ""),

    Dep(0, 0, 0, 1, 0, "", "(((NP{Y}\\NP{Y}<1>){_}/(NP{Z}\\NP{Z}){W}<3>){_}/NP{V}<2>){_}", "from", ""),
};

} }
