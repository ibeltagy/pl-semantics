
:- module(alphaConversionDRT,[alphaConvertDRS/2]).

:- use_module(library(lists),[member/2]).
:- use_module(semlib(errors),[warning/2]).
:- use_module(knowledge(relations),[nn/3]).


/*========================================================================
   Alpha Conversion (introducing substitutions)
========================================================================*/

alphaConvertDRS(B1,B2):-
   alphaConvertDRS(B1,[]-_,B2), !.


/*========================================================================
   Variable
========================================================================*/

variable(X):- var(X), !.
variable(X):- functor(X,'$VAR',1), !.


/*========================================================================
   Alpha Conversion (terms)
========================================================================*/

alphaConvertVar(X,Vars,New):-
   variable(X), !,
   (
      member(sub(Z,Y),Vars),           %%% BOUND
      X==Z, !,
      New=Y
   ;
      New=X                            %%% FREE
   ).

alphaConvertVar(X,_,X).


/*========================================================================
   Alpha Conversion (symbols)
========================================================================*/

alphaConvertSym(f(_,_,V),_,V):- atomic(V), !.

alphaConvertSym(f(nn,[A1,B1],V),Vars,F):- !,
   alphaConvertDRS(A1,Vars-_,A2),
   alphaConvertDRS(B1,Vars-_,B2),
   interpretFunction(f(nn,[A2,B2],V),F).

alphaConvertSym(X,_,X).


/*========================================================================
   Alpha Conversion (DRSs)
========================================================================*/

alphaConvertDRS(X1,Vars-Vars,X2):-
   variable(X1), !,
   alphaConvertVar(X1,Vars,X2).

alphaConvertDRS(lam(X,B1),Vars-Vars,lam(Y,B2)):- !,
   alphaConvertDRS(B1,[sub(X,Y)|Vars]-_,B2).

alphaConvertDRS(B:drs(D,C),Vars,B:Drs):- !, 
   alphaConvertDRS(drs(D,C),Vars,Drs).

alphaConvertDRS(drs([],[]),Vars-Vars,drs([],[])):- !.

alphaConvertDRS(drs([],[B:I:C1|Conds1]),Vars1-Vars2,drs([],[B:I:C2|Conds2])):- !,
   alphaConvertCondition(C1,Vars1,C2), 
   alphaConvertDRS(drs([],Conds1),Vars1-Vars2,drs([],Conds2)).

alphaConvertDRS(drs([B:I:Ref|L1],C1),Vars1-Vars2,drs([B:I:New|L2],C2)):- !,
   alphaConvertDRS(drs(L1,C1),[sub(Ref,New)|Vars1]-Vars2,drs(L2,C2)).

alphaConvertDRS(alfa(Type,B1,B2),Vars1-Vars3,alfa(Type,B3,B4)):- !,
   alphaConvertDRS(B1,Vars1-Vars2,B3),
   alphaConvertDRS(B2,Vars2-Vars3,B4).

alphaConvertDRS(merge(B1,B2),Vars1-Vars3,merge(B3,B4)):- !,
   alphaConvertDRS(B1,Vars1-Vars2,B3),
   alphaConvertDRS(B2,Vars2-Vars3,B4).

alphaConvertDRS(smerge(B1,B2),Vars1-Vars3,smerge(B3,B4)):- !,
   alphaConvertDRS(B1,Vars1-Vars2,B3),
   alphaConvertDRS(B2,Vars2-Vars3,B4).

alphaConvertDRS(app(E1,E2),Vars-Vars,app(E3,E4)):- !,
   alphaConvertDRS(E1,Vars-_,E3),
   alphaConvertDRS(E2,Vars-_,E4).

alphaConvertDRS(sdrs([],[]),Vars-Vars,sdrs([],[])):- !.

alphaConvertDRS(sdrs([],[I:C1|L1]),Vars1-Vars2,sdrs([],[I:C2|L2])):- !,
   alphaConvertCondition(C1,Vars1,C2),
   alphaConvertDRS(sdrs([],L1),Vars1-Vars2,sdrs([],L2)).

alphaConvertDRS(sdrs([B1|L1],C1),Vars1-Vars3,sdrs([B2|L2],C2)):- !,
   alphaConvertDRS(B1,Vars1-Vars2,B2),
   alphaConvertDRS(sdrs(L1,C1),Vars2-Vars3,sdrs(L2,C2)).

alphaConvertDRS(lab(Ref,B1),Vars1-[sub(Ref,New)|Vars2],lab(New,B2)):- !,
   alphaConvertDRS(B1,Vars1-Vars2,B2).

alphaConvertDRS(sub(B1,B2),Vars1-Vars3,sub(B3,B4)):- !,
   alphaConvertDRS(B1,Vars1-Vars2,B3),
   alphaConvertDRS(B2,Vars2-Vars3,B4).

alphaConvertDRS(Sym,Vars-Vars,Sym):- atomic(Sym), !.

alphaConvertDRS(U,_,_):- !,
   warning('Unknown DRS expression: ~p',[U]), fail.


/*========================================================================
   Alpha Conversion (DRS-Conditions)
========================================================================*/

alphaConvertCondition(nec(B1),Vars,nec(B2)):- !,
   alphaConvertDRS(B1,Vars-_,B2).

alphaConvertCondition(pos(B1),Vars,pos(B2)):- !,
   alphaConvertDRS(B1,Vars-_,B2).

alphaConvertCondition(not(B1),Vars,not(B2)):- !,
   alphaConvertDRS(B1,Vars-_,B2).

alphaConvertCondition(prop(X,B1),Vars,prop(Y,B2)):- !,
   alphaConvertVar(X,Vars,Y),
   alphaConvertDRS(B1,Vars-_,B2).

alphaConvertCondition(imp(B1,B2),Vars,imp(B3,B4)):- !,
   alphaConvertDRS(B1,Vars-Vars1,B3),
   alphaConvertDRS(B2,Vars1-_,B4).

alphaConvertCondition(whq(B1,B2),Vars,whq(B3,B4)):- !,
   alphaConvertDRS(B1,Vars-Vars1,B3),
   alphaConvertDRS(B2,Vars1-_,B4).

alphaConvertCondition(whq(Type,B1,X,B2),Vars,whq(Type,B3,Y,B4)):- !,
   alphaConvertDRS(B1,Vars-Vars1,B3),
   alphaConvertVar(X,Vars1,Y),
   alphaConvertDRS(B2,Vars1-_,B4).

alphaConvertCondition(or(B1,B2),Vars,or(B3,B4)):- !,
   alphaConvertDRS(B1,Vars-_,B3),
   alphaConvertDRS(B2,Vars-_,B4).

alphaConvertCondition(pred(Arg1,Sym,Type,Sense),Vars,pred(Arg2,Sym,Type,Sense)):- !,
   alphaConvertVar(Arg1,Vars,Arg2).

alphaConvertCondition(rel(Arg1,Arg2,Sym),Vars,rel(Arg3,Arg4,Sym)):- !,
   alphaConvertVar(Arg1,Vars,Arg3),
   alphaConvertVar(Arg2,Vars,Arg4).

alphaConvertCondition(rel(Arg1,Arg2,Sym1,Sense),Vars,rel(Arg3,Arg4,Sym2,Sense)):- !,
   alphaConvertSym(Sym1,Vars,Sym2),
   alphaConvertVar(Arg1,Vars,Arg3),
   alphaConvertVar(Arg2,Vars,Arg4).

alphaConvertCondition(role(Arg1,Arg2,Sym,Dir),Vars,role(Arg3,Arg4,Sym,Dir)):- !,
   alphaConvertVar(Arg1,Vars,Arg3),
   alphaConvertVar(Arg2,Vars,Arg4).

alphaConvertCondition(named(X,Sym,Type,Sense),Vars,named(Y,Sym,Type,Sense)):- !,
   alphaConvertVar(X,Vars,Y).

alphaConvertCondition(card(X,Sym1,T),Vars,card(Y,Sym2,T)):- !,
   alphaConvertVar(X,Vars,Y),
   alphaConvertVar(Sym1,Vars,Sym2).

alphaConvertCondition(timex(X,Sym),Vars,timex(Y,Sym)):- !,
   alphaConvertVar(X,Vars,Y).

alphaConvertCondition(eq(X1,X2),Vars,eq(Y1,Y2)):- !,
   alphaConvertVar(X1,Vars,Y1),
   alphaConvertVar(X2,Vars,Y2).

alphaConvertCondition(U,_,_):- !,
   warning('Unknown condition: ~p',[U]), fail.


/*========================================================================
   Eta Conversion (DRSs)
========================================================================*/

etaConversion(Var,Var):- var(Var), !, fail.
etaConversion(Sym,Sym):- atomic(Sym).
etaConversion(lam(X,_:drs(_,C)),Sym):- member(_:_:pred(Y,Sym,_,_),C), X==Y, !.
etaConversion(_,thing).


/*========================================================================
  Function Interpretation: NN
========================================================================*/

interpretFunction(f(nn,_,Sym),Sym):-
   option('--nn',false), !, Sym = of.

interpretFunction(f(nn,[A1,B1],Sym),Sym):-
   etaConversion(A1,A2),
   etaConversion(B1,B2), !,
   ( nn(A2,B2,Sym), !
   ; nn(_ ,B2,Sym), !
   ; nn(A2,_, Sym), !
   ; Sym = of ).

interpretFunction(F,F).



