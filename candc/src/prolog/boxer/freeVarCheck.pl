
:- module(freeVarCheck,[freeVarCheckDrs/1]).

:- use_module(library(lists),[member/2]).


/*========================================================================
   Free Variable Check (main predicate)
========================================================================*/

freeVarCheckDrs(Drs):-
   freeVarCheckDrs(Drs,[]-_).


/*========================================================================
   Free Variable Check (DRSs)
========================================================================*/

freeVarCheckDrs(drs([_:X|D],C),L1-L2):- !,
   freeVarCheckDrs(drs(D,C),[X|L1]-L2).

freeVarCheckDrs(drs([],C),L-L):- !,
   freeVarCheckConds(C,L).

freeVarCheckDrs(merge(B1,B2),L1-L3):- !,
   freeVarCheckDrs(B1,L1-L2),
   freeVarCheckDrs(B2,L2-L3).

freeVarCheckDrs(smerge(B1,B2),L1-L3):- !,
   freeVarCheckDrs(B1,L1-L2),
   freeVarCheckDrs(B2,L2-L3).

freeVarCheckDrs(sdrs([lab(K,B)|O],C),L1-L3):- !,
   freeVarCheckDrs(B,[K|L1]-L2),
   freeVarCheckDrs(sdrs(O,C),L2-L3).

freeVarCheckDrs(sdrs([sub(lab(K1,B1),lab(K2,B2))|O],C),L1-L3):- !,
   freeVarCheckDrs(B1,[K1|L1]-L2),
   freeVarCheckDrs(B2,[K2|L2]-_),
   freeVarCheckDrs(sdrs(O,C),[K2|L2]-L3).

freeVarCheckDrs(sdrs([],C),L-L):- !,
   freeVarCheckConds(C,L).

freeVarCheckDrs(lab(K,B),L1-L2):- !,
   freeVarCheckDrs(B,[K|L1]-L2).

freeVarCheckDrs(alfa(_,B1,B2),L1-L3):- !,
   freeVarCheckDrs(B1,L1-L2),
   freeVarCheckDrs(B2,L2-L3).


/*========================================================================
   Free Variable Check (List of conditions)
========================================================================*/

freeVarCheckConds([],_):- !.

freeVarCheckConds([X|C],L):-
   freeVarCheckCond(X,L),
   freeVarCheckConds(C,L).


/*========================================================================
   Free Variable Check (Conditions)
========================================================================*/

freeVarCheckCond(_:X,L):- !,
   freeVarCheckCond(X,L).

freeVarCheckCond(not(B),L):- !,
   freeVarCheckDrs(B,L-_).

freeVarCheckCond(nec(B),L):- !,
   freeVarCheckDrs(B,L-_).

freeVarCheckCond(pos(B),L):- !,
   freeVarCheckDrs(B,L-_).

freeVarCheckCond(prop(X,B),L):- !,
   checkTerm(X,L),
   freeVarCheckDrs(B,L-_).

freeVarCheckCond(imp(B1,B2),L1):- !,
   freeVarCheckDrs(B1,L1-L2),
   freeVarCheckDrs(B2,L2-_).

freeVarCheckCond(whq(B1,B2),L1):- !,
   freeVarCheckDrs(B1,L1-L2),
   freeVarCheckDrs(B2,L2-_).

freeVarCheckCond(whq(_,B1,X,B2),L1):- !,
   freeVarCheckDrs(B1,L1-L2),
   checkTerm(X,L2),
   freeVarCheckDrs(B2,L2-_).

freeVarCheckCond(or(B1,B2),L):- !,
   freeVarCheckDrs(B1,L-_),
   freeVarCheckDrs(B2,L-_).

freeVarCheckCond(pred(Arg,_,_,_),L):- !,
   checkTerm(Arg,L).

freeVarCheckCond(rel(Arg1,Arg2,_,_),L):- !,
   checkTerm(Arg1,L), checkTerm(Arg2,L).

freeVarCheckCond(rel(Arg1,Arg2,_),L):- !,
   checkTerm(Arg1,L), checkTerm(Arg2,L).

freeVarCheckCond(card(Arg1,Arg2,_),L):- !,
   checkTerm(Arg1,L), checkTerm(Arg2,L).

freeVarCheckCond(named(Arg,_,_,_),L):- !,
   checkTerm(Arg,L).

freeVarCheckCond(timex(Arg,_),L):- !,
   checkTerm(Arg,L).

freeVarCheckCond(eq(Arg1,Arg2),L):- !,
   checkTerm(Arg1,L), checkTerm(Arg2,L).

/*========================================================================
   Check Term
========================================================================*/

checkTerm(X,L):- var(X), member(Y,L), X==Y, !.
checkTerm(X,_):- atomic(X), !.
