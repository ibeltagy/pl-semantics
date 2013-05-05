
:- module(drs2fdrs,[drs2fdrs/2,
	            eqDrs/2,         %%% should go in its own module!
                    instDrs/1,
                    instDrs/2]).

:- use_module(library(lists),[select/3,member/2]).
:- use_module(semlib(options),[option/2]).

/*========================================================================
   Main Predicates
========================================================================*/

drs2fdrs(B,Flat):- drs2fdrs(B,0,_,[]-Flat,_).

instDrs(B):- instDrs(B,0,_).

instDrs(B,N):- instDrs(B,0,N).


/*========================================================================
   Label
========================================================================*/

label(X,NonVar,X):- nonvar(NonVar), !.

label(X,Label,Y):-
   number_codes(X,Codes),
   atom_codes(Label,[108|Codes]),
   Y is X + 1.


/*========================================================================
   Variable
========================================================================*/

avar(Var):- var(Var), !.
avar(Var):- atom(Var), !.
avar(Var):- functor(Var,'$VAR',1).


/*========================================================================
   Flattening DRSs
========================================================================*/

drs2fdrs(Var,L1,L2,F-[Label:var(Var)|F],Label):-
   avar(Var), !, label(L1,Label,L2).

drs2fdrs(drs(Dom,Conds),L1,L3,F1-[Label:drs(Dom,CondsLabels)|F2],Label):-
   label(L1,Label,L2),
   conds2fconds(Conds,L2,L3,F1-F2,CondsLabels).

drs2fdrs(merge(A1,A2),L1,L4,F1-[Label:merge(Label1,Label2)|F3],Label):-
   label(L1,Label,L2),
   drs2fdrs(A1,L2,L3,F2-F3,Label1),
   drs2fdrs(A2,L3,L4,F1-F2,Label2).

drs2fdrs(smerge(A1,A2),L1,L4,F1-[Label:smerge(Label1,Label2)|F3],Label):-
   label(L1,Label,L2),
   drs2fdrs(A1,L2,L3,F2-F3,Label1),
   drs2fdrs(A2,L3,L4,F1-F2,Label2).

drs2fdrs(sdrs(C1,C2),L1,L4,F1-[Label:sdrs(Labels1,Labels2)|F3],Label):-
   label(L1,Label,L2),
   conds2fconds(C1,L2,L3,F2-F3,Labels1),
   conds2fconds(C2,L3,L4,F1-F2,Labels2).

drs2fdrs(alfa(Type,A1,A2),L1,L4,F1-[Label:alfa(Type,Label1,Label2)|F3],Label):-
   label(L1,Label,L2),
   drs2fdrs(A1,L2,L3,F2-F3,Label1),
   drs2fdrs(A2,L3,L4,F1-F2,Label2).

drs2fdrs(app(A1,A2),L1,L4,F1-[Label:app(Label1,Label2)|F3],Label):-
   label(L1,Label,L2),
   drs2fdrs(A1,L2,L3,F2-F3,Label1),
   drs2fdrs(A2,L3,L4,F1-F2,Label2).

drs2fdrs(lam(X,A),L1,L3,F1-[Label:lam(X,Label2)|F2],Label):-
   label(L1,Label,L2),
   drs2fdrs(A,L2,L3,F1-F2,Label2).


/*========================================================================
   Flattening DRS-Conditions
========================================================================*/

conds2fconds([],L,L,F-F,[]).

conds2fconds([I:imp(A1,A2)|Conds],L1,L5,F1-[Label:I:imp(Label1,Label2)|F4],[Label|L]):- !,
   label(L1,Label,L2),
   drs2fdrs(A1,L2,L3,F1-F2,Label1),
   drs2fdrs(A2,L3,L4,F2-F3,Label2),
   conds2fconds(Conds,L4,L5,F3-F4,L).

conds2fconds([I:or(A1,A2)|Conds],L1,L5,F1-[Label:I:or(Label1,Label2)|F4],[Label|L]):- !,
   label(L1,Label,L2),
   drs2fdrs(A1,L2,L3,F1-F2,Label1),
   drs2fdrs(A2,L3,L4,F2-F3,Label2),
   conds2fconds(Conds,L4,L5,F3-F4,L).

conds2fconds([I:whq(A1,A2)|Conds],L1,L5,F1-[Label:I:whq(Label1,Label2)|F4],[Label|L]):- !,
   label(L1,Label,L2),
   drs2fdrs(A1,L2,L3,F1-F2,Label1),
   drs2fdrs(A2,L3,L4,F2-F3,Label2),
   conds2fconds(Conds,L4,L5,F3-F4,L).

conds2fconds([I:whq(Type,A1,Var,A2)|Conds],L1,L5,F1-[Label:I:whq(Type,Label1,Var,Label2)|F4],[Label|L]):- !,
   label(L1,Label,L2),
   drs2fdrs(A1,L2,L3,F1-F2,Label1),
   drs2fdrs(A2,L3,L4,F2-F3,Label2),
   conds2fconds(Conds,L4,L5,F3-F4,L).

conds2fconds([I:not(A)|Conds],L1,L4,F1-[Label:I:not(Label1)|F3],[Label|L]):- !,
   label(L1,Label,L2),
   drs2fdrs(A,L2,L3,F1-F2,Label1),
   conds2fconds(Conds,L3,L4,F2-F3,L).

conds2fconds([I:nec(A)|Conds],L1,L4,F1-[Label:I:nec(Label1)|F3],[Label|L]):- !,
   label(L1,Label,L2),
   drs2fdrs(A,L2,L3,F1-F2,Label1),
   conds2fconds(Conds,L3,L4,F2-F3,L).

conds2fconds([I:pos(A)|Conds],L1,L4,F1-[Label:I:pos(Label1)|F3],[Label|L]):- !,
   label(L1,Label,L2),
   drs2fdrs(A,L2,L3,F1-F2,Label1),
   conds2fconds(Conds,L3,L4,F2-F3,L).

conds2fconds([I:prop(X,A)|Conds],L1,L4,F1-[Label:I:prop(X,Label1)|F3],[Label|L]):- !,
   label(L1,Label,L2),
   drs2fdrs(A,L2,L3,F1-F2,Label1),
   conds2fconds(Conds,L3,L4,F2-F3,L).

conds2fconds([sub(lab(K1,A1),lab(K2,A2))|Conds],L1,L7,F1-[Label:sub(K1,K2)|F4],[Label|L]):- !,
   label(L1,Label,L2),
   label(L2,K1,L3),
   label(L3,K2,L4),
   drs2fdrs(A1,L4,L5,F1-F2,K1),
   drs2fdrs(A2,L5,L6,F2-F3,K2),
   conds2fconds(Conds,L6,L7,F3-F4,L).

conds2fconds([lab(K,B)|Conds],L1,L4,F1-F3,[K|L]):- !,
   label(L1,K,L2),
   drs2fdrs(B,L2,L3,F1-F2,K),
   conds2fconds(Conds,L3,L4,F2-F3,L).

conds2fconds([I:Cond|Conds],L1,L3,F1-[Label:I:Cond|F2],[Label|L]):- !,
   label(L1,Label,L2),
   conds2fconds(Conds,L2,L3,F1-F2,L).



/*========================================================================
   Referent
========================================================================*/

ref(X,Ref,Code,Y):-
   var(Ref), !,
   number_codes(X,Codes),
   atom_codes(Ref,[Code|Codes]),
   Y is X + 1.

ref(X,_,_,X).


/*========================================================================
   Sort Referent: time (116), event (101), proposition (112), entity (120)
========================================================================*/

sortref(X,Conds,116):- member(_:pred(Y,now,a,1),Conds), X==Y, !.
sortref(X,Conds,116):- member(_:rel(_,Y,temp_overlap,1),Conds), X==Y, !.
sortref(X,Conds,116):- member(_:rel(_,Y,temp_before,1),Conds), X==Y, !.
sortref(X,Conds,116):- member(_:rel(Y,_,temp_before,1),Conds), X==Y, !.
sortref(X,Conds,116):- member(_:rel(_,Y,temp_included,1),Conds), X==Y, !.

sortref(X,Conds,101):- member(_:pred(Y,_,v,_),Conds), X==Y, !.
sortref(X,Conds,101):- member(_:rel(_,Y,temp_abut,1),Conds), X==Y, !.
sortref(X,Conds,101):- member(_:rel(Y,_,temp_abut,1),Conds), X==Y, !.
sortref(X,Conds,101):- member(_:rel(Y,_,temp_overlap,1),Conds), X==Y, !.

sortref(X,Conds,112):- member(_:prop(Y,_),Conds), X==Y, !.
sortref(_,_,120).


/*========================================================================
   Instantiating DRSs
========================================================================*/

instDrs(Var,L1,L2):-
   var(Var), !, 
   ref(L1,Var,102,L2).

instDrs(Var,L1,L2):-
   atom(Var), !,
   L2 = L1. 

instDrs(Var,L1,L2):-
   Var =.. ['$VAR',_], !,
   L2 = L1. 

instDrs(drs([_:Ref|Dom],Conds),L1,L3):-
   sortref(Ref,Conds,Sort),
   ref(L1,Ref,Sort,L2), 
   instDrs(drs(Dom,Conds),L2,L3).

instDrs(drs([],Conds),L1,L2):-
   instConds(Conds,L1,L2).

instDrs(merge(A1,A2),L1,L3):-
   instDrs(A1,L1,L2),
   instDrs(A2,L2,L3).

instDrs(smerge(A1,A2),L1,L3):-
   instDrs(A1,L1,L2),
   instDrs(A2,L2,L3).

instDrs(sdrs([],_),L,L).

instDrs(sdrs([X|L],C),L1,L3):-
   instDrs(X,L1,L2),
   instDrs(sdrs(L,C),L2,L3).

instDrs(lab(K,B),L1,L3):-
   ref(L1,K,107,L2),
   instDrs(B,L2,L3).

instDrs(sub(B1,B2),L1,L3):-
   instDrs(B1,L1,L2),
   instDrs(B2,L2,L3).

instDrs(alfa(_,A1,A2),L1,L3):-
   instDrs(A1,L1,L2),
   instDrs(A2,L2,L3).

instDrs(app(A1,A2),L1,L3):-
   instDrs(A1,L1,L2),
   instDrs(A2,L2,L3).

instDrs(lam(X,A),L1,L3):-
   ref(L1,X,118,L2),
   instDrs(A,L2,L3).


/*========================================================================
   Instantiating DRS-Conditions
========================================================================*/

instConds([],L,L).

instConds([_:imp(A1,A2)|Conds],L1,L4):- !,
   instDrs(A1,L1,L2),
   instDrs(A2,L2,L3),
   instConds(Conds,L3,L4).

instConds([_:or(A1,A2)|Conds],L1,L4):- !,
   instDrs(A1,L1,L2),
   instDrs(A2,L2,L3),
   instConds(Conds,L3,L4).

instConds([_:whq(A1,A2)|Conds],L1,L4):- !,
   instDrs(A1,L1,L2),
   instDrs(A2,L2,L3),
   instConds(Conds,L3,L4).

instConds([_:whq(_,A1,_,A2)|Conds],L1,L4):- !,
   instDrs(A1,L1,L2),
   instDrs(A2,L2,L3),
   instConds(Conds,L3,L4).

instConds([_:not(A)|Conds],L1,L3):- !,
   instDrs(A,L1,L2),
   instConds(Conds,L2,L3).

instConds([_:nec(A)|Conds],L1,L3):- !,
   instDrs(A,L1,L2),
   instConds(Conds,L2,L3).

instConds([_:pos(A)|Conds],L1,L3):- !,
   instDrs(A,L1,L2),
   instConds(Conds,L2,L3).

instConds([_:prop(_,A)|Conds],L1,L3):- !,
   instDrs(A,L1,L2),
   instConds(Conds,L2,L3).

instConds([_|Conds],L1,L2):- !,
   instConds(Conds,L1,L2).


/*========================================================================
   Eliminate Equality from DRS 
========================================================================*/

eqDrs(xdrs(A,B,C,DRS1),xdrs(A,B,C,DRS2)):-
   option('--elimeq',true), !,
   elimEqDrs(DRS1,DRS2).

eqDrs(DRS1,DRS2):-
   option('--elimeq',true), !,
   elimEqDrs(DRS1,DRS2).

eqDrs(DRS,DRS).


/*========================================================================
   Eliminate Equality
========================================================================*/

elimEqDrs(Var,Var):- avar(Var), !.

elimEqDrs(drs(Dom1,Conds1),drs(Dom2,Conds2)):-
   elimEqConds(Conds1,Conds2,Dom1,Dom2).

elimEqDrs(merge(A1,A2),merge(B1,B2)):-
   elimEqDrs(A1,B1),
   elimEqDrs(A2,B2).

elimEqDrs(smerge(A1,A2),smerge(B1,B2)):-
   elimEqDrs(A1,B1),
   elimEqDrs(A2,B2).

elimEqDrs(sub(A1,A2),sub(B1,B2)):-
   elimEqDrs(A1,B1),
   elimEqDrs(A2,B2).

elimEqDrs(sdrs([],C),sdrs([],C)).

elimEqDrs(sdrs([X1|L1],C1),sdrs([X2|L2],C2)):-
   elimEqDrs(X1,X2),
   elimEqDrs(sdrs(L1,C1),sdrs(L2,C2)).

elimEqDrs(alfa(T,A1,A2),alfa(T,B1,B2)):-
   elimEqDrs(A1,B1),
   elimEqDrs(A2,B2).

elimEqDrs(lab(X,A1),lab(X,B1)):-
   elimEqDrs(A1,B1).

elimEqDrs(lam(X,A1),lam(X,B1)):-
   elimEqDrs(A1,B1).

elimEqDrs(app(A1,A2),app(B1,B2)):-
   elimEqDrs(A1,B1),
   elimEqDrs(A2,B2).


/*========================================================================
   Instantiating DRS-Conditions
========================================================================*/

elimEqConds([],[],D,D).

elimEqConds([I:imp(A1,A2)|Conds1],[I:imp(B1,B2)|Conds2],D1,D2):- !,
   elimEqDrs(A1,B1),
   elimEqDrs(A2,B2),
   elimEqConds(Conds1,Conds2,D1,D2).

elimEqConds([I:or(A1,A2)|Conds1],[I:or(B1,B2)|Conds2],D1,D2):- !,
   elimEqDrs(A1,B1),
   elimEqDrs(A2,B2),
   elimEqConds(Conds1,Conds2,D1,D2).

elimEqConds([I:whq(A1,A2)|Conds1],[I:whq(B1,B2)|Conds2],D1,D2):- !,
   elimEqDrs(A1,B1),
   elimEqDrs(A2,B2),
   elimEqConds(Conds1,Conds2,D1,D2).

elimEqConds([I:whq(X,A1,T,A2)|Conds1],[I:whq(X,B1,T,B2)|Conds2],D1,D2):- !,
   elimEqDrs(A1,B1),
   elimEqDrs(A2,B2),
   elimEqConds(Conds1,Conds2,D1,D2).

elimEqConds([I:not(A1)|Conds1],[I:not(B1)|Conds2],D1,D2):- !,
   elimEqDrs(A1,B1),
   elimEqConds(Conds1,Conds2,D1,D2).

elimEqConds([I:nec(A1)|Conds1],[I:nec(B1)|Conds2],D1,D2):- !,
   elimEqDrs(A1,B1),
   elimEqConds(Conds1,Conds2,D1,D2).

elimEqConds([I:pos(A1)|Conds1],[I:pos(B1)|Conds2],D1,D2):- !,
   elimEqDrs(A1,B1),
   elimEqConds(Conds1,Conds2,D1,D2).

elimEqConds([I:prop(X,A1)|Conds1],[I:prop(X,B1)|Conds2],D1,D2):- !,
   elimEqDrs(A1,B1),
   elimEqConds(Conds1,Conds2,D1,D2).

elimEqConds([_:eq(X,Y)|Conds1],Conds2,D1,D2):- 
   select(_:Z,D1,D3), X==Z, !, X=Y,
   elimEqConds(Conds1,Conds2,D3,D2).

elimEqConds([_:eq(X,Y)|Conds1],Conds2,D1,D2):- 
   select(_:Z,D1,D3), Y==Z, !, X=Y,
   elimEqConds(Conds1,Conds2,D3,D2).

elimEqConds([C|Conds1],[C|Conds2],D1,D2):- !,
   elimEqConds(Conds1,Conds2,D1,D2).

