
:- module(betaConversionDRT,[betaConvert/2]).

:- use_module(boxer(alphaConversionDRT),[alphaConvertDRS/2]).
:- use_module(library(lists),[append/3]).
:- use_module(semlib(errors),[warning/2]).


/*========================================================================
   Beta-Conversion (introducing stack)
========================================================================*/

betaConvert(X,Y):-
   betaConvert(X,Y,[]), !.

betaConvert(X,X):-
   warning('beta-conversion failed for ~p',[X]).


/*========================================================================
   Beta-Conversion (core stuff)
========================================================================*/

betaConvert(X,Y,[]):- var(X), !, Y=X.

betaConvert(app(Functor,Argument),Result,Stack):- 
   nonvar(Functor), !,
   alphaConvertDRS(Functor,Converted),
   betaConvert(Argument,ConArg),               %%% much faster this way!
   betaConvert(Converted,Result,[ConArg|Stack]).

betaConvert(lam(X,Formula),Result,[X|Stack]):- !,
   betaConvert(Formula,Result,Stack).

betaConvert(lam(X,Formula),lam(X,Result),[]):- !,
   betaConvert(Formula,Result).

betaConvert(merge(B1,B2),Result,[]):- !,
   betaConvert(B1,K1),
   betaConvert(B2,K2),
   reduce(K1,K2,Result).

betaConvert(sdrs([],C),sdrs([],C),[]):- !.

betaConvert(sdrs([B1|L1],C1),sdrs([B2|L2],C2),[]):- !,
   betaConvert(B1,B2),
   betaConvert(sdrs(L1,C1),sdrs(L2,C2)).

betaConvert(lab(X,B1),lab(X,B2),[]):- !,
   betaConvert(B1,B2).

betaConvert(sub(B1,B3),sub(B2,B4),[]):- !,
   betaConvert(B1,B2),
   betaConvert(B3,B4).

betaConvert(drs(D,C1),drs(D,C2),[]):- !, betaConvertCond(C1,C2).

betaConvert(alfa(T,B1,B2),alfa(T,K1,K2),[]):- !, betaConvert(B1,K1), betaConvert(B2,K2).

betaConvert(smerge(B1,B2),smerge(K1,K2),[]):- !, betaConvert(B1,K1), betaConvert(B2,K2).

betaConvert(app(B1,B2),app(K1,K2),[]):- !, betaConvert(B1,K1), betaConvert(B2,K2).

betaConvert(X,Result,[A]):-  !, Result=app(X,A).

betaConvert(X,Result,[A,B]):- !, Result=app(app(X,A),B).

betaConvert(X,Result,[A,B,C]):- !, Result=app(app(app(X,A),B),C).

betaConvert(X,Result,[A,B,C,D]):- !, Result=app(app(app(app(X,A),B),C),D).


/*========================================================================
   Beta-Convert DRS Conditions
========================================================================*/

betaConvertCond([],[]).

betaConvertCond([I:not(A1)|L1],[I:not(B1)|L2]):- !,
   betaConvert(A1,B1),
   betaConvertCond(L1,L2).

betaConvertCond([I:pos(A1)|L1],[I:pos(B1)|L2]):- !,
   betaConvert(A1,B1),
   betaConvertCond(L1,L2).

betaConvertCond([I:nec(A1)|L1],[I:nec(B1)|L2]):- !,
   betaConvert(A1,B1),
   betaConvertCond(L1,L2).

betaConvertCond([I:prop(X,A1)|L1],[I:prop(X,B1)|L2]):- !,
   betaConvert(A1,B1),
   betaConvertCond(L1,L2).

betaConvertCond([I:imp(A1,A2)|L1],[I:imp(B1,B2)|L2]):- !,
   betaConvert(A1,B1), betaConvert(A2,B2),
   betaConvertCond(L1,L2).

betaConvertCond([I:or(A1,A2)|L1],[I:or(B1,B2)|L2]):- !,
   betaConvert(A1,B1), betaConvert(A2,B2),
   betaConvertCond(L1,L2).

betaConvertCond([I:whq(A1,A2)|L1],[I:whq(B1,B2)|L2]):- !,
   betaConvert(A1,B1), betaConvert(A2,B2),
   betaConvertCond(L1,L2).

betaConvertCond([I:whq(T,A1,X,A2)|L1],[I:whq(T,B1,X,B2)|L2]):- !,
   betaConvert(A1,B1), betaConvert(A2,B2),
   betaConvertCond(L1,L2).

betaConvertCond([I:C|L1],[I:C|L2]):-
   betaConvertCond(L1,L2).


/*========================================================================
   Merge reduction
========================================================================*/

reduce(Var,B,merge(Var,B)):- var(Var), !.

reduce(B,Var,merge(B,Var)):- var(Var), !.

reduce(drs(D1,C1),drs(D2,C2),drs(D3,C3)):- !,
   append(D1,D2,D3),
   append(C1,C2,C3).

reduce(drs(D1,C1),merge(drs(D2,C2),B),merge(drs(D3,C3),B)):- !,
   append(D1,D2,D3),
   append(C1,C2,C3).

reduce(B1,B2,merge(B1,B2)).
