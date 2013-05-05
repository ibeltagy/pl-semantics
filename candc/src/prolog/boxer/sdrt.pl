
:- module(sdrt,[insertDRS/4]).

:- use_module(library(lists),[member/2]).
 

/* =========================================================================
   Subordinate SDRT relations
========================================================================= */

subordinate(elaboration).
subordinate(instance).
subordinate(topic).
subordinate(explanation).
subordinate(precondition).
subordinate(commentary).
subordinate(correction).


/* =========================================================================
   Coordinate SDRT relations
========================================================================= */

coordinate(continuation).
coordinate(narration).
coordinate(background).
coordinate(result).


/* =========================================================================
   Insert DRS into SDRS
========================================================================= */

insertDRS(Rel,OldSDRS,DRS,NewSDRS):-
   subordinate(Rel), !,
   subDRS(OldSDRS,Rel,DRS,NewSDRS).

insertDRS(Rel,OldSDRS,DRS,NewSDRS):-
   coordinate(Rel), !,
   cooDRS(OldSDRS,Rel,DRS,NewSDRS).
  

/* =========================================================================
   Insert subordinate DRS into SDRS
========================================================================= */

subDRS(sdrs([sub(L1,lab(L2,S1))],R),Rel,B,sdrs([sub(L1,lab(L2,S2))],R)):- !,
   subDRS(S1,Rel,B,S2).

subDRS(sdrs([lab(K1,B1)],R),Rel,B2,SDRS):- !,
   SDRS = sdrs([sub(lab(K1,B1),lab(K2,B2))],[[]:rel(K1,K2,Rel)|R]).

subDRS(DRS,Rel,B2,SDRS):-
   member(DRS,[drs(_,_),alfa(_,_,_)]), !,
   SDRS = sdrs([sub(lab(K1,DRS),lab(K2,B2))],[[]:rel(K1,K2,Rel)]).

subDRS(sdrs([X|L1],R1),Rel,New,sdrs([X|L2],R2)):-
   subDRS(sdrs(L1,R1),Rel,New,sdrs(L2,R2)).


/* =========================================================================
   Insert coordinate DRS into SDRS
========================================================================= */

cooDRS(sdrs([sub(L1,lab(L2,S1))],R),Rel,B,sdrs([sub(L1,lab(L2,S2))],R)):- !,
   cooDRS(S1,Rel,B,S2).

cooDRS(sdrs([lab(K1,B1)],R),Rel,B2,SDRS):- !,
   SDRS = sdrs([lab(K1,B1),lab(K2,B2)],[[]:rel(K1,K2,Rel)|R]).

cooDRS(DRS,Rel,B2,SDRS):- 
   member(DRS,[drs(_,_),alfa(_,_,_)]), !,
   SDRS = sdrs([lab(K1,DRS),lab(K2,B2)],[[]:rel(K1,K2,Rel)]).

cooDRS(sdrs([X|L1],R1),Rel,New,sdrs([X|L2],R2)):-
   cooDRS(sdrs(L1,R1),Rel,New,sdrs(L2,R2)).


