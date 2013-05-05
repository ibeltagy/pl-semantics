
:- module(betaConversionDRT,[betaConvert/2]).

:- use_module(boxer(alphaConversionDRT),[alphaConvertDRS/2]).
:- use_module(boxer(noncomp),[noncomp/3]).
:- use_module(library(lists),[append/3]).
:- use_module(semlib(errors),[warning/2]).


/* ========================================================================
   Beta-Conversion (introducing stack)
======================================================================== */

betaConvert(X,Y):- betaConvert(X,Y,[]), !.

betaConvert(X,X):- warning('beta-conversion failed for ~p',[X]).


/* ========================================================================
   Beta-Conversion (core stuff)
======================================================================== */

betaConvert(X,Y,[]):- var(X), !, Y=X.

betaConvert(X,Y,L):-  var(X), !, wrapArguments(L,X,Y).

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
   reduceMerge(merge(K1,K2),Result).

betaConvert(sdrs([],C),sdrs([],C),[]):- !.

betaConvert(sdrs([B1|L1],C1),sdrs([B2|L2],C2),[]):- !,
   betaConvert(B1,B2),
   betaConvert(sdrs(L1,C1),sdrs(L2,C2)).

betaConvert(lab(X,B1),lab(X,B2),[]):- !,
   betaConvert(B1,B2).

betaConvert(sub(B1,B3),sub(B2,B4),[]):- !,
   betaConvert(B1,B2),
   betaConvert(B3,B4).

betaConvert(B:drs(D,C1),B:drs(D,C2),[]):- !, 
   betaConvertConditions(C1,C2).

betaConvert(alfa(T,B1,B2),Result,[]):- !, 
   betaConvert(B1,K1), 
   betaConvert(B2,K2),
   reduceMerge(alfa(T,K1,K2),Result).

betaConvert(smerge(B1,B2),smerge(K1,K2),[]):- !, 
   betaConvert(B1,K1), 
   betaConvert(B2,K2).

betaConvert(app(B1,B2),app(K1,K2),[]):- !, 
   betaConvert(B1,K1), 
   betaConvert(B2,K2).

betaConvert(X,Y,L):- !,  
   wrapArguments(L,X,Y).


/* ========================================================================
   Beta-Convert DRS Conditions
======================================================================== */

betaConvertConditions([],[]).

betaConvertConditions([B:I:C1|L1],[B:I:C2|L2]):- !,
   betaConvertCond(C1,C2),
   betaConvertConditions(L1,L2).


/* ========================================================================
   Beta-Convert DRS Condition
======================================================================== */

betaConvertCond(not(A1),not(B1)):- !, betaConvert(A1,B1).
betaConvertCond(pos(A1),pos(B1)):- !, betaConvert(A1,B1).
betaConvertCond(nec(A1),nec(B1)):- !, betaConvert(A1,B1).
betaConvertCond(prop(X,A1),prop(X,B1)):- !, betaConvert(A1,B1).
betaConvertCond(or(A1,A2),or(B1,B2)):- !, betaConvert(A1,B1), betaConvert(A2,B2).
betaConvertCond(imp(A1,A2),imp(B1,B2)):- !, betaConvert(A1,B1), betaConvert(A2,B2).
betaConvertCond(whq(A1,A2),whq(B1,B2)):- !, betaConvert(A1,B1), betaConvert(A2,B2).
betaConvertCond(whq(T,A1,X,A2),whq(T,B1,X,B2)):- !, betaConvert(A1,B1), betaConvert(A2,B2).
betaConvertCond(C,C).


/* ========================================================================
   Wrapping arguments from stack
======================================================================== */

wrapArguments([],X,X).

wrapArguments([A|L],X,Y):- wrapArguments(L,app(X,A),Y).


/* ========================================================================
   Merge reduction
======================================================================== */

% Cannot reduce if one the arguments is a variable
%
reduceMerge(merge(Var,B),merge(Var,B)):- var(Var), !. 
reduceMerge(merge(B,Var),merge(B,Var)):- var(Var), !. 
reduceMerge(alfa(T,Var,B),alfa(T,Var,B)):- var(Var), !. 
reduceMerge(alfa(T,B,Var),alfa(T,B,Var)):- var(Var), !. 

% Reduce if both are basic DRSs
%
reduceMerge(merge(B:drs([],[C1]),B:drs([],[C2])),B:K):- 
   noncomp(C1,C2,K), !.

reduceMerge(merge(B:drs(D1,C1),B:drs(D2,C2)),B:drs(D3,C3)):- !,
   append(D1,D2,D3), append(C1,C2,C3).

reduceMerge(alfa(_,_:drs(D1,C1),B:drs(D2,C2)),B:drs(D3,C3)):- 
   option('--semantics',pdrs), !,
   append(D1,D2,D3), append(C1,C2,C3).

% Recursive case
%
reduceMerge(merge(B:drs(D1,C1),merge(B:drs(D2,C2),K)),Reduced):- !,
   append(D1,D2,D3), append(C1,C2,C3),
   reduceMerge(merge(B:drs(D3,C3),K),Reduced).

reduceMerge(alfa(_,_:drs(D1,C1),merge(B:drs(D2,C2),K)),Reduced):-
   option('--semantics',pdrs), !,
   append(D1,D2,D3), append(C1,C2,C3),
   reduceMerge(merge(B:drs(D3,C3),K),Reduced).

reduceMerge(M,M).
