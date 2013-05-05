
:- module(thematicRoles,[labelRolesDrs/2]).

:- use_module(semlib(errors),[warning/2]).
:- use_module(library(lists),[member/2,select/3,append/3]).
:- use_module(semlib(options),[option/2]).
:- use_module(lex(verbnet),[vnpattern/3]).
:- use_module(lex(framenet),[fnpattern/4]).

/*========================================================================
   Find proto roles in DRS, then assign thematic roles
========================================================================*/

labelRolesDrs(B,B):-
   option('--roles',proto), !. 

labelRolesDrs(B1,B2):-
   collectRolesDrs(B1,[]-Roles,B2), 
   assignRoles(Roles,[],[]), !.

labelRolesDrs(B,B):-
   warning('role labelling failed',[]).


/*========================================================================
   Collect Roles (DRSs)
========================================================================*/

collectRolesDrs(drs(Dom,Conds1),Roles1-Roles2,drs(Dom,Conds2)):- !,
   collectRolesConds(Conds1,Roles1-Roles2,Conds2).

collectRolesDrs(alfa(Type,B1,B2),Roles1-Roles3,alfa(Type,B3,B4)):- !,
   collectRolesDrs(B1,Roles1-Roles2,B3),
   collectRolesDrs(B2,Roles2-Roles3,B4).

collectRolesDrs(merge(B1,B2),Roles1-Roles3,merge(B3,B4)):- !,
   collectRolesDrs(B1,Roles1-Roles2,B3),
   collectRolesDrs(B2,Roles2-Roles3,B4).

collectRolesDrs(smerge(B1,B2),Roles1-Roles3,smerge(B3,B4)):- !,
   collectRolesDrs(B1,Roles1-Roles2,B3),
   collectRolesDrs(B2,Roles2-Roles3,B4).


/*========================================================================
   Collect Roles (DRS-Conditions)
========================================================================*/

collectRolesConds([],R-R,[]).

collectRolesConds(Conds1,Roles1-Roles2,[I:pred(X,NewV,NewP,NewS)|Conds3]):- 
   select(I:pred(X,OldV,v,OldS),Conds1,Conds2), !,
   collectRolesConds(Conds2,[v(X,OldV,v,OldS,[],NewV,NewP,NewS)|Roles1]-Roles2,Conds3).

collectRolesConds([I:rel(X,Y,Old,S)|Conds1],Roles1-Roles3,[I:rel(X,Y,New,S)|Conds2]):-
   select(v(E,V1,P1,S1,R,V2,P2,S2),Roles1,Roles2), E == X, !,
   collectRolesConds(Conds1,[v(E,V1,P1,S1,[r(Old,New)|R],V2,P2,S2)|Roles2]-Roles3,Conds2).

collectRolesConds([I:prop(X,B1)|Conds1],Roles1-Roles3,[I:prop(X,B2)|Conds2]):- !,
   collectRolesDrs(B1,Roles1-Roles2,B2),
   collectRolesConds(Conds1,Roles2-Roles3,Conds2).

collectRolesConds([I:not(B1)|Conds1],Roles1-Roles3,[I:not(B2)|Conds2]):- !,
   collectRolesDrs(B1,Roles1-Roles2,B2),
   collectRolesConds(Conds1,Roles2-Roles3,Conds2).

collectRolesConds([I:pos(B1)|Conds1],Roles1-Roles3,[I:pos(B2)|Conds2]):- !,
   collectRolesDrs(B1,Roles1-Roles2,B2),
   collectRolesConds(Conds1,Roles2-Roles3,Conds2).

collectRolesConds([I:nec(B1)|Conds1],Roles1-Roles3,[I:nec(B2)|Conds2]):- !,
   collectRolesDrs(B1,Roles1-Roles2,B2),
   collectRolesConds(Conds1,Roles2-Roles3,Conds2).

collectRolesConds([I:whq(T,B1,X,B3)|Conds1],Roles1-Roles4,[I:whq(T,B2,X,B4)|Conds2]):- !,
   collectRolesDrs(B1,Roles1-Roles2,B2),
   collectRolesDrs(B3,Roles2-Roles3,B4),
   collectRolesConds(Conds1,Roles3-Roles4,Conds2).

collectRolesConds([I:imp(B1,B3)|Conds1],Roles1-Roles4,[I:imp(B2,B4)|Conds2]):- !,
   collectRolesDrs(B1,Roles1-Roles2,B2),
   collectRolesDrs(B3,Roles2-Roles3,B4),
   collectRolesConds(Conds1,Roles3-Roles4,Conds2).

collectRolesConds([I:or(B1,B3)|Conds1],Roles1-Roles4,[I:or(B2,B4)|Conds2]):- !,
   collectRolesDrs(B1,Roles1-Roles2,B2),
   collectRolesDrs(B3,Roles2-Roles3,B4),
   collectRolesConds(Conds1,Roles3-Roles4,Conds2).

collectRolesConds([C|Conds1],Roles1-Roles2,[C|Conds2]):-
   collectRolesConds(Conds1,Roles1-Roles2,Conds2).


/*========================================================================
   Assign Roles
========================================================================*/

assignRoles([],_,_).

assignRoles([v(_,V,P,S,R,V,P,S)|L],[_,_,_,_],_):-
   option('--roles',verbnet), !,
   assignNoRole(R),
   assignRoles(L,[],[]).

assignRoles([v(_,V,P,S,R,V,P,S)|L],[_,_,_,_],[_,_,_,_]):-
   option('--roles',framenet), !,
   assignNoRole(R),
   assignRoles(L,[],[]).

assignRoles([v(_,Verb,Pos,_,Roles,Verb,Pos,Sense)|L],Remove,_):-
   option('--roles',verbnet), 
   vnpattern(Verb,Sense,Pattern),
   subPattern(Pattern,Remove,SubPattern),
   map(SubPattern,Roles), !,
%  warning('mapped pattern ~p to VN roles ~p',[Pattern,Roles]),
   assignRoles(L,[],[]).

assignRoles([v(_,Verb,_,_,Roles,Frame,f,1)|L],RemoveVN,RemoveFN):-
   option('--roles',framenet), 
   vnpattern(Verb,Sense,VNPattern),
   subPattern(VNPattern,RemoveVN,SubVNPattern),
   trans(SubVNPattern,Roles,VNRoles), 
   fnpattern(Verb,Sense,Frame,FNPattern),
   subPattern(FNPattern,RemoveFN,SubFNPattern),
   map(SubFNPattern,VNRoles), !,
%  warning('mapped pattern ~p to FN roles ~p (frame: ~p)',[FNPattern,VNRoles,Frame]),
   assignRoles(L,[],[]).

assignRoles(L,Remove,_):-
   option('--roles',verbnet), !, 
   assignRoles(L,[_|Remove],[]).

assignRoles(L,[_,_,_,_],Remove):-
   option('--roles',framenet), !,
   assignRoles(L,[],[_|Remove]).

assignRoles(L,RemoveVN,RemoveFN):-
   option('--roles',framenet), !,
   assignRoles(L,[_|RemoveVN],RemoveFN).


/* ========================================================================
   No assignment
======================================================================== */

assignNoRole([]).

assignNoRole([r(X,X)|L]):-
   assignNoRole(L).


/* ========================================================================
   Map VerbNet Pattern to Roles
======================================================================== */

map([],L):- assignNoRole(L).
map([Proto:Role|L],R1):- select(r(Proto,Role),R1,R2), !, map(L,R2).
map([rel:Role|L],R1):- select(r(NoProto,Role),R1,R2), 
   \+ member(NoProto,[agent,patient,theme,
                      temp_overlap,temp_included,temp_abut]), map(L,R2).


trans([],L,L).
trans([Proto:Role|L],R1,[r(Role,Var)|R3]):- select(r(Proto,Var),R1,R2), !, trans(L,R2,R3).
trans([rel:Role|L],R1,[r(Role,Var)|R3]):- select(r(NoProto,Var),R1,R2), 
   \+ member(NoProto,[agent,patient,theme,
                      temp_overlap,temp_included,temp_abut]), trans(L,R2,R3).


/* ========================================================================
  Sub List
======================================================================== */

subPattern(List,[],List):- !.

subPattern(List,[_|L],SubList):-
   select(_,List,NewList),
   subPattern(NewList,L,SubList).