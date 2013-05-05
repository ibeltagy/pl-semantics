
:- module(ne,[netype/2]).

netype('I-LOC',loc).
netype('B-LOC',loc).
netype('E-LOC',loc).

netype('I-ORG',org).
netype('B-ORG',org).
netype('E-ORG',org).

netype('I-PER',per).
netype('B-PER',per).
netype('E-PER',per).

netype('I-QUO',quo).
netype('B-QUO',quo).
netype('E-QUO',quo).

netype(_,nam).
