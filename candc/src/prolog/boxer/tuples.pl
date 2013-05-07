
:- module(tuples,[tuples/4,write_tuples/2]).

:- use_module(semlib(drs2tacitus),[label/4]).
:- use_module(semlib(errors),[warning/2]).
:- use_module(semlib(options),[option/2]).
:- use_module(knowledge(punctuation),[punctuation/2]).
:- use_module(library(lists),[member/2,last/2,select/3]).


/* =======================================================================
   Dynamic predicates
======================================================================= */

:- dynamic cond_ctr/1. cond_ctr(0).


/* =======================================================================
   Main: tuples (DRG)
======================================================================= */

tuples(Tags,DRS,N1,Sorted):- 
   label(N1,k,K0,N2),
   initcondcounter,
   tuples(DRS,K0,[]-_,Tags,[]-Tuples,N2-_),
   inverse(Tuples,Tuples1),
   surface(Tags,Tuples1,Extended),
   order(Extended,Extended,Ordered),
   sort(Ordered,Sorted).


/* =======================================================================
   Invert Tuples if needed (e.g. for relative clauses
======================================================================= */

inverse(Tuples0,Tuples5):-
   member(tuple( _, _:equality, ext, X,          _),Tuples0),
   select(tuple(L1, C:Role:1,   ext, X,         I1),Tuples0,Tuples1),    T1=tuple(L1,C:Role:-1,int, X,        I1),
   select(tuple(L2, C:Role:1,   int, Y,         I2),Tuples1,Tuples2),    T2=tuple(L2,C:Role:-1,ext, Y,        I2),
   select(tuple(L3, K,          role, C:Role:1, I3),Tuples2,Tuples3), !, T3=tuple(L3,K,        role,C:Role:-1,I3),
   Tuples4=[T1,T2,T3|Tuples3],
   inverse(Tuples4,Tuples5).

inverse(Tuples,Tuples).


/* =======================================================================
   Order edges for each discourse referent
======================================================================= */

order([],_,[]).

order([T|L1],Refs,Ordered):-
   T = tuple(_,_,_,X,_),
   split(L1,X,WithX,WithoutX),       %%% split tuple wrt X
   position([T|WithX],Refs,Pos),     %%% get positions for X
   sort(Pos,Sorted),
   localOrder(Sorted,1,L2,Ordered),  %%% normalise order starting with 1
   order(WithoutX,Refs,L2).


/* =======================================================================
   Determine local position (of relations)
======================================================================= */

position([],_,[]).

position([tuple([],N1,int,N2,W)|L1],Tuples,[tuple([P],N1,int,N2,W)|L2]):-
   member(tuple(_,N1,ext,X,_),Tuples),
   member(tuple([P|_],_,_,X,_),Tuples), !,
   position(L1,Tuples,L2).

position([tuple([],N1,int,N2,W)|L1],Tuples,[tuple([P],N1,int,N2,W)|L2]):-
   member(tuple(_,N1,ext,B,_),Tuples),
   member(tuple(_,B,referent,X,_),Tuples), 
   member(tuple([P|_],_,_,X,_),Tuples), !,
   position(L1,Tuples,L2).

position([tuple([],R1,int,N2,W)|L1],Tuples,[tuple([P],R1,int,N2,W)|L2]):-
   member(tuple(_,R1,ext,Y,_),Tuples),
   member(tuple(_,R2,int,Y,_),Tuples),
   member(tuple(_,R2,ext,Z,_),Tuples),
   member(tuple([P|_],_,_,Z,_),Tuples), !,
   position(L1,Tuples,L2).

position([T|L1],Tuples,[T|L2]):-
   position(L1,Tuples,L2).


/* =======================================================================
   Determine local order
======================================================================= */

localOrder([],_,L,L).

localOrder([tuple(A,B,C,D,E)|L],N,L1,[tuple(A,0,B,C,D,E)|L2]):- 
   A = [], !,
   localOrder(L,N,L1,L2).

localOrder([tuple(A,B,C,D,E)|L],N,L1,[tuple(A,N,B,C,D,E)|L2]):-
   M is N + 1,
   localOrder(L,M,L1,L2).


/* =======================================================================
   Split tuples into two sets based on third argument
======================================================================= */

split([],_,[],[]).

split([T|L1],X,[T|L2],L3):-
   T = tuple(_,_,_,X,_), !,
   split(L1,X,L2,L3).

split([T|L1],X,L2,[T|L3]):-
   split(L1,X,L2,L3).


/* =======================================================================
   Add surface tuples (ideally should be eliminated)
======================================================================= */

surface([],T,T).

surface([Index:_|W],T1,T2):-                % if token is
   member(tuple(I,_,_,_,_),T1),             % already part of a tuple
   member(Index,I), !,                      % then 
   surface(W,T1,T2).                        % take next token

surface([Index1:[tok:Word|Tags]|W],T1,[T|T2]):- 
   member(pos:POS,Tags),
   punctuation(POS,left),
   member(tuple([Index2|_],_,_,X,_),T1),
   Index1 is Index2 - 1,
   member(tuple(_,K,referent,X,_),T1), !,
   T = tuple([Index1],K,punctuation,X,[Word]),
   surface(W,T1,T2).

surface([Index1:[tok:Word|Tags]|W],T1,[T|T2]):- 
   member(pos:POS,Tags),
   punctuation(POS,right),
   member(tuple(Indices,_,_,X,_),T1),
   last(Indices,Index2),
   Index1 is Index2 + 1,
   member(tuple(_,K,referent,X,_),T1), !,
   T = tuple([Index1],K,punctuation,X,[Word]),
   surface(W,T1,T2).

surface([Index:[tok:Word|_]|W],T1,[T|T2]):- 
   I is div(Index,1000),
   event(T1,Index,I,K,E,Distance), 
   \+ (event(T1,Index,I,_,_,Smaller), Smaller < Distance), !,
   T = tuple([Index],K,surface,E,[Word]),
%  warning('surface tuple: ~p (~p)',[Word,Index]),
   surface(W,T1,T2).

surface([Index:[tok:Word|_]|W],T1,[T|T2]):- 
   T = tuple([Index],k,error,x,[Word]),
   warning('word not part of tuples: ~p',[Word]),
   surface(W,T1,T2).


/* =======================================================================
   Find an event tuple with distance to Index1 (slow!)
======================================================================= */

event(Tuples,Index1,I,K,E,Distance):-
   member(tuple(_,K,event,Event,_),Tuples),
   member(tuple([Index2|_],Event,instance,E,_),Tuples),
   I is div(Index2,1000),
   Distance is abs(Index1-Index2).

%event(Tuples,Index1,K,E,Distance):-
%   member(tuple(_,K,event,Event,_),Tuples),
%   member(tuple([Index2|_],Event,instance,E,_),Tuples),
%   Distance is abs(Index1-Index2),
%   Distance < 1000.


/* =======================================================================
   Counter for DRS-conditions
======================================================================= */

condcounter(CC):-
   retract(cond_ctr(X)),
   label(X,c,CC,N),
   assert(cond_ctr(N)).

initcondcounter:-
   retract(cond_ctr(_)),
   assert(cond_ctr(0)).


/* =======================================================================
   Converting DRSs into graph tuples

   tuples(+DRS,+CurrentDRSid,+Refs,+Words,?Tuples,?Counter)
   
   where: tuple(Index,Node1,Edge,Node2,Words)

======================================================================= */

tuples(sdrs(D,R),K,R1-R3,W,T1-T3,N1-N3):- !, tuples(D,K,R1-R2,W,T1-T2,N1-N2), tuples(R,K,R2-R3,W,T2-T3,N2-N3).
tuples(smerge(B1,B2),K,R1-R3,W,T1-T3,N1-N3):- !, tuples(B1,K,R1-R2,W,T1-T2,N1-N2), tuples(B2,K,R2-R3,W,T2-T3,N2-N3).
tuples(merge(B1,B2),K,R1-R3,W,T1-T3,N1-N3):- !, tuples(B1,K,R1-R2,W,T1-T2,N1-N2), tuples(B2,K,R2-R3,W,T2-T3,N2-N3).
tuples(alfa(_,B1,B2),K,R1-R3,W,T1-T3,N1-N3):- !, tuples(B1,K,R1-R2,W,T1-T2,N1-N2), tuples(B2,K,R2-R3,W,T2-T3,N2-N3).

tuples(lab(K0,sdrs([sub(lab(L,B),Sub)|D],R)),K,R1-R3,W,T1-[T|T3],N1-N3):- !, 
   T = tuple([],K0,dominates,L,[]),
   tuples(sub(lab(L,B),Sub),K,R1-R2,W,T1-T2,N1-N2), 
   tuples(lab(K0,sdrs(D,R)),K,R2-R3,W,T2-T3,N2-N3).

tuples(lab(K0,sdrs([lab(L,B)|D],R)),K,R1-R3,W,T1-[T|T3],N1-N3):- !, 
   T = tuple([],K0,dominates,L,[]),
   tuples(lab(L,B),K,R1-R2,W,T1-T2,N1-N2), 
   tuples(lab(K0,sdrs(D,R)),K,R2-R3,W,T2-T3,N2-N3).

tuples(lab(K,B),_,R1-R2,W,T1-T2,N1-N2):- !, tuples(B,K,R1-R2,W,T1-T2,N1-N2).
tuples(sub(B1,B2),K,R1-R3,W,T1-T3,N1-N3):- !, tuples(B1,K,R1-R2,W,T1-T2,N1-N2), tuples(B2,K,R2-R3,W,T2-T3,N2-N3).
tuples(_:drs([],C),K,R1-R2,W,T1-T2,N1-N2):- !, tuples(C,K,R1-R2,W,T1-T2,N1-N2).

tuples(B:drs([_:I:R|L],C),K,R1-R2,W,T1-[T|T2],N1-N2):- !,
    word(I,W,Word), !,
    T = tuple(I,K,referent,K:R,Word),
    tuples(B:drs(L,C),K,[K:R|R1]-R2,W,T1-T2,N1-N2).

tuples([lab(A,B)|L],K,R1-R3,W,T1-T3,N1-N3):- !, tuples(lab(A,B),K,R1-R2,W,T1-T2,N1-N2),tuples(L,K,R2-R3,W,T2-T3,N2-N3).

tuples([sub(A,B)|L],K,R1-R3,W,T1-T3,N1-N3):- !, tuples(sub(A,B),K,R1-R2,W,T1-T2,N1-N2),tuples(L,K,R2-R3,W,T2-T3,N2-N3).

tuples([_:I:pred(X,Sym,n,Sense)|L],K,R1-R2,W,T1-[E1,E2|T2],N1-N2):-
    word(I,W,Word), nonvar(X), member(Dom:X,R1), !,
    condcounter(CC),
    E1 = tuple([],K,          concept,  CC:Sym:Sense,[]),
    E2 = tuple(I,CC:Sym:Sense,instance, Dom:X,Word),
    tuples(L,K,R1-R2,W,T1-T2,N1-N2).

tuples([_:I:pred(X,Sym,v,Sense)|L],K,R1-R2,W,T1-[E1,E2|T2],N1-N2):- 
    word(I,W,Word), nonvar(X), member(Dom:X,R1), !,
    condcounter(CC),
    E1 = tuple([],K,          event,CC:Sym:Sense,[]),
    E2 = tuple(I,CC:Sym:Sense,instance,    Dom:X,Word),
    tuples(L,K,R1-R2,W,T1-T2,N1-N2).

tuples([_:I:pred(X,_,s,1)|L],K,R1-R2,W,T1-[E|T2],N1-N2):- 
    word(I,W,Word), nonvar(X), member(Dom:X,R1), !,
    E = tuple(I,K,function,       Dom:X,Word),
    tuples(L,K,R1-R2,W,T1-T2,N1-N2).

tuples([_:I:pred(X,Sym,a,Sense)|L],K,R1-R2,W,T1-[E1,E2|T2],N1-N2):- 
    word(I,W,Word), nonvar(X), member(Dom:X,R1), !,
    condcounter(CC),
    E1 = tuple([],K,          attribute,CC:Sym:Sense,[]),
    E2 = tuple(I,CC:Sym:Sense,arg,       Dom:X,Word),
    tuples(L,K,R1-R2,W,T1-T2,N1-N2).

tuples([_:I:named(X,Sym,Type,_)|L],K,R1-R2,W,T1-[E1,E2|T2],N1-N2):- 
    word(I,W,Word), nonvar(X), member(Dom:X,R1), !,
    condcounter(CC),
    E1 = tuple([],K,         named,CC:Sym:Type,[]),
    E2 = tuple(I,CC:Sym:Type,instance,  Dom:X,Word),
    tuples(L,K,R1-R2,W,T1-T2,N1-N2).

tuples([B:_:timex(X,date(_,I2:Y,I3:M,I4:D))|L],K,R1-R2,W,T,N):- !,
    tuples([B:I2:timex(X,year,Y),B:I3:timex(X,month,M),B:I4:timex(X,day,D)|L],K,R1-R2,W,T,N).

tuples([_:I:timex(X,Type,Sym)|L],K,R1-R2,W,T1-[E1,E2|T2],N1-N2):- 
    word(I,W,Word), nonvar(X), member(Dom:X,R1), !,
    condcounter(CC),
    E1 = tuple([],K,         Type,CC:Sym:Type,[]),
    E2 = tuple(I,CC:Sym:Type,arg,  Dom:X,Word),
    tuples(L,K,R1-R2,W,T1-T2,N1-N2).

tuples([_:I:eq(X,Y)|L],K,R1-R2,W,T1-[E1,E2,E3|T2],N1-N2):- 
    word(I,W,Word), nonvar(X), nonvar(Y), member(D1:X,R1), member(D2:Y,R1), !,
    condcounter(CC),
    E1 = tuple(I, K,    relation,CC:equality,Word),
    E2 = tuple([],CC:equality,int,D1:X,[]),
    E3 = tuple([],CC:equality,ext,D2:Y,[]),
    tuples(L,K,R1-R2,W,T1-T2,N1-N2).

tuples([B:I:rel(X,Y,subset_of,Sense)|L],K,R,W,T,N):-
   tuples([B:I:rel(Y,X,superset_of,Sense)|L],K,R,W,T,N).

tuples([_:I:rel(X,Y,Sym,Sense)|L],K,R1-R2,W,T1-[E1,E2,E3|T2],N1-N2):-
    word(I,W,Word), nonvar(X), nonvar(Y), member(D1:X,R1), member(D2:Y,R1), !,
    condcounter(CC),
    E1 = tuple([],K,relation,CC:Sym:Sense,[]),
    E2 = tuple([],CC:Sym:Sense,int,D1:X,[]),
    E3 = tuple( I,CC:Sym:Sense,ext,D2:Y,Word),
    tuples(L,K,R1-R2,W,T1-T2,N1-N2).

tuples([_:I:role(X,Y,Sym,Dir)|L],K,R1-R2,W,T1-[E1,E2,E3|T2],N1-N2):-
    word(I,W,Word), nonvar(X), nonvar(Y), member(D1:X,R1), member(D2:Y,R1), !,
    condcounter(CC),
    E1 = tuple([],K,role,CC:Sym:Dir,[]),
    E2 = tuple([],CC:Sym:Dir,int,D1:X,[]),
    E3 = tuple( I,CC:Sym:Dir,ext,D2:Y,Word),
    tuples(L,K,R1-R2,W,T1-T2,N1-N2).

tuples([I:rel(X,Y,Sym)|L],K,R1-R2,W,T1-[E|T2],N1-N2):-
    word(I,W,Word), !, 
    E = tuple(I,X,Sym,Y,Word),
    tuples(L,K,R1-R2,W,T1-T2,N1-N2).

tuples([_:I:card(X,Y,Type)|L],K,R1-R2,W,T1-[E1,E2|T2],N1-N2):-
    word(I,W,Word), nonvar(X), member(Dom:X,R1), !,
    condcounter(CC),
    E1 = tuple([],K,       cardinality,CC:Y:Type,[]),
    E2 = tuple(I,CC:Y:Type,arg,         Dom:X,Word),
    tuples(L,K,R1-R2,W,T1-T2,N1-N2).

tuples([_:_:prop(X,B)|L],K,R1-R1,W,T1-[T|T3],N1-N3):-
    nonvar(X), member(Dom:X,R1), !,
    T = tuple([],K,subordinates:prop,Dom:X,[]),
    tuples(B,Dom:X,R1-_,W,T1-T2,N1-N2),
    tuples(L,K,R1-_,W,T2-T3,N2-N3).

tuples([_:_:not(B)|L],K1,R1-R1,W,T1-[T|T3],N1-N4):- 
    option('--x',false), !,
    T = tuple([],K1,subordinates:neg,K2,[]),
    label(N1,k,K2,N2),
    tuples(B,K2,R1-_,W,T1-T2,N2-N3),
    tuples(L,K1,R1-_,W,T2-T3,N3-N4).

tuples([_:I:not(B)|L],K1,R1-R1,W,T1-[T|T3],N1-N4):- 
    option('--x',true), !,
    word(I,W,Word), 
    T = tuple([],K1,subordinates:neg,K2,Word),
    label(N1,k,K2,N2),
    tuples(B,K2,R1-_,W,T1-T2,N2-N3),
    tuples(L,K1,R1-_,W,T2-T3,N3-N4).

tuples([_:_:pos(B)|L],K1,R1-R1,W,T1-[T|T3],N1-N4):- !,
    T = tuple([],K1,subordinates:pos,K2,[]),
    label(N1,k,K2,N2),
    tuples(B,K2,R1-_,W,T1-T2,N2-N3),
    tuples(L,K1,R1-_,W,T2-T3,N3-N4).

tuples([_:_:nec(B)|L],K1,R1-R1,W,T1-[T|T3],N1-N4):- !,
    T = tuple([],K1,subordinates:nec,K2,[]),
    label(N1,k,K2,N2),
    tuples(B,K2,R1-_,W,T1-T2,N2-N3),
    tuples(L,K1,R1-_,W,T2-T3,N3-N4).

tuples([_:_:imp(B1,B2)|L],K1,R1-R1,W,T1-[Tu1,Tu2|T4],N1-N6):- !,
    Tu1 = tuple([],K1,subordinates:cond,K2,[]),
    label(N1,k,K2,N2),
    tuples(B1,K2,R1-R2,W,T1-T2,N2-N3),
    Tu2 = tuple([],K2,subordinates:impl,K3,[]),
    label(N3,k,K3,N4),
    tuples(B2,K3,R2-_,W,T2-T3,N4-N5),
    tuples(L,K1,R1-_,W,T3-T4,N5-N6).

tuples([_:_:whq(_,B1,_,B2)|L],K1,R1-R1,W,T1-[Tu1,Tu2|T4],N1-N6):- !,
    Tu1 = tuple([],K1,subordinates:que,K2,[]),
    label(N1,k,K2,N2),
    tuples(B1,K2,R1-R2,W,T1-T2,N2-N3),
    Tu2 = tuple([],K2,subordinates:bgr,K3,[]),
    label(N3,k,K3,N4),
    tuples(B2,K3,R2-_,W,T2-T3,N4-N5),
    tuples(L,K1,R1-_,W,T3-T4,N5-N6).

tuples([_:_:or(B1,B2)|L],K1,R1-_,W,T1-[Tu1,Tu2|T4],N1-N6):- !,
    Tu1 = tuple([],K1,subordinates:dis,K2,[]),
    label(N1,k,K2,N2),
    tuples(B1,K2,R1-_,W,T1-T2,N2-N3),
    Tu2 = tuple([],K2,coordinates:dis,K3,[]),
    label(N3,k,K3,N4),
    tuples(B2,K3,R1-_,W,T2-T3,N4-N5),
    tuples(L,K1,R1-_,W,T3-T4,N5-N6).

tuples([Err|L],K,R,W,T,N):- !, 
     warning('unknown tuple: ~p',[Err]),
     tuples(L,K,R,W,T,N).

tuples([],_,R-R,_,T-T,N-N).


/* =======================================================================
   Convert words in tuple format
======================================================================= */

word(I1,_,I2):- option('--x',true), !, I1 = I2.

word([],_,[]):- !.
word([Index|Is],W,[Tok|Ws]):- member(Index:[tok:Tok|_],W), !, word(Is,W,Ws).
word([_|Is],W,Ws):- word(Is,W,Ws).


/* =======================================================================
   Output tuples to stream
======================================================================= */

write_tuples([],_).

write_tuples([tuple(_Index,Order,Node1,Edge,Node2,Words)|L],Stream):- !,
   write_node(Node1,Stream),
   format(Stream,' ~w ',[Edge]),
   write_node(Node2,Stream),
   format(Stream,' ~w [ ',[Order]),
   write_tokens(Words,Stream),
   write_tuples(L,Stream).

write_tuples([T|L],Stream):-
   warning('unable to output tuple ~p',[T]),
   write_tuples(L,Stream).


/* =======================================================================
   Output tokens to stream
======================================================================= */

write_tokens([],Stream):- !, write(Stream,']'), nl(Stream).
write_tokens([X|L],Stream):- format(Stream,'~w ',[X]), write_tokens(L,Stream).


/* =======================================================================
   Output nodes to stream
======================================================================= */

write_node(A:B,Stream):- !, format(Stream,'~w:',[A]), write_node(B,Stream).
write_node(A,Stream):- format(Stream,'~w',[A]).


