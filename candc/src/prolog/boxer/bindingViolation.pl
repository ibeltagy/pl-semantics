
:- module(bindingViolation,[bindingViolationDrs/1]).

:- use_module(library(lists),[member/2]).


/*========================================================================
   Succeeds if there is a binding violation in a DRS
========================================================================*/

bindingViolationDrs(sdrs([B|_],_)):- bindingViolationDrs(B), !.
bindingViolationDrs(sdrs([_|L],R)):- !, bindingViolationDrs(sdrs(L,R)).

bindingViolationDrs(lab(_,B)):- !, bindingViolationDrs(B).

bindingViolationDrs(drs(_,C)):- !, bindingViolationConds(C).

bindingViolationDrs(merge(B,_)):- bindingViolationDrs(B), !.
bindingViolationDrs(merge(_,B)):- !, bindingViolationDrs(B).

bindingViolationDrs(sub(B,_)):- bindingViolationDrs(B), !.
bindingViolationDrs(sub(_,B)):- !, bindingViolationDrs(B).

bindingViolationDrs(smerge(B,_)):- bindingViolationDrs(B), !.
bindingViolationDrs(smerge(_,B)):- !, bindingViolationDrs(B).

bindingViolationDrs(alfa(_,B,_)):- bindingViolationDrs(B), !.
bindingViolationDrs(alfa(_,_,B)):- !, bindingViolationDrs(B).


/*========================================================================
   Succeeds if there is a binding violation in a set of DRS condition
========================================================================*/

roleViolation(agent,patient).
roleViolation(agent,theme).
roleViolation(theme,patient).

/*========================================================================
   Succeeds if there is a binding violation in a set of DRS condition
========================================================================*/

bindingViolationConds(Conds):- 
   roleViolation(Role1,Role2),
   member(_:role(E1,X1,Role1,1),Conds),
   member(_:role(E2,X2,Role2,1),Conds),
   E1==E2, X1==X2, !.

bindingViolationConds(Conds):- 
   member(_:Cond,Conds), 
   bindingViolationCond(Cond), !.


/*========================================================================
   Succeeds if there is a binding violation in a DRS condition
========================================================================*/

bindingViolationCond(not(drs([],[_:eq(X,Y)]))):- !, X==Y.

bindingViolationCond(not(B)):- !, bindingViolationDrs(B).
bindingViolationCond(nec(B)):- !, bindingViolationDrs(B).
bindingViolationCond(pos(B)):- !, bindingViolationDrs(B).

bindingViolationCond(prop(_,B)):- !, bindingViolationDrs(B).

bindingViolationCond(imp(B,_)):- bindingViolationDrs(B), !.
bindingViolationCond(imp(_,B)):- !, bindingViolationDrs(B).

bindingViolationCond(or(B,_)):- bindingViolationDrs(B), !.
bindingViolationCond(or(_,B)):- !, bindingViolationDrs(B).

bindingViolationCond(whq(B,_)):- bindingViolationDrs(B), !.
bindingViolationCond(whq(_,B)):- !, bindingViolationDrs(B).

bindingViolationCond(whq(_,B,_,_)):- bindingViolationDrs(B), !.
bindingViolationCond(whq(_,_,_,B)):- !, bindingViolationDrs(B).

bindingViolationCond(rel(X,Y,_,_)):- !, X==Y.
bindingViolationCond(role(X,Y,_,_)):- !, X==Y.

