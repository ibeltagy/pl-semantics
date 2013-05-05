
:- module(noncomp,[noncomp/2]).

:- use_module(library(lists),[member/2,append/3,select/3]).
:- use_module(semlib(errors),[warning/2]).


/*========================================================================
   Main predicate
========================================================================*/

noncomp(Var,Var):- var(Var), !.

noncomp(lab(X,B1),lab(X,B2)):- !, noncomp(B1,B2).
noncomp(sub(B1,B2),sub(B3,B4)):- !, noncomp(B1,B3), noncomp(B2,B4).
noncomp(sdrs([],C),sdrs([],C)):- !.
noncomp(sdrs([B1|L1],C1),sdrs([B2|L2],C2)):- !, noncomp(B1,B2), noncomp(sdrs(L1,C1),sdrs(L2,C2)).
noncomp(xdrs(W,P,E,A),xdrs(W,P,E,B)):- !, noncomp(A,B).
noncomp(drs(D1,C1),drs(D2,C4)):- !, glue(C1,C2), add(D1,C2,D2,C3), noncomp(C3,C4).
noncomp(smerge(A1,A2),smerge(B1,B2)):- !, noncomp(A1,B1), noncomp(A2,B2).
noncomp(merge(A1,A2),merge(B1,B2)):- !, noncomp(A1,B1), noncomp(A2,B2).
noncomp(app(A1,A2),app(B1,B2)):- !, noncomp(A1,B1), noncomp(A2,B2).
noncomp(alfa(T,A1,A2),alfa(T,B1,B2)):- !, noncomp(A1,B1), noncomp(A2,B2).
noncomp(lam(X,A),lam(X,B)):- !, noncomp(A,B).

noncomp([],[]):- !.
noncomp([I:pos(A)|L1],[I:pos(B)|L2]):- !, noncomp(A,B), noncomp(L1,L2).
noncomp([I:nec(A)|L1],[I:nec(B)|L2]):- !, noncomp(A,B), noncomp(L1,L2).
noncomp([I:not(A)|L1],[I:not(B)|L2]):- !, noncomp(A,B), noncomp(L1,L2).
noncomp([I:prop(X,A)|L1],[I:prop(X,B)|L2]):- !, noncomp(A,B), noncomp(L1,L2).
noncomp([I:imp(A1,A2)|L1],[I:imp(B1,B2)|L2]):- !, noncomp(A1,B1), noncomp(A2,B2), noncomp(L1,L2).
noncomp([I:or(A1,A2)|L1],[I:or(B1,B2)|L2]):- !, noncomp(A1,B1), noncomp(A2,B2), noncomp(L1,L2).
noncomp([I:whq(A1,A2)|L1],[I:whq(B1,B2)|L2]):- !, noncomp(A1,B1), noncomp(A2,B2), noncomp(L1,L2).
noncomp([I:whq(X,A1,Y,A2)|L1],[I:whq(X,B1,Y,B2)|L2]):- !, noncomp(A1,B1), noncomp(A2,B2), noncomp(L1,L2).
noncomp([I:C|L1],[I:C|L2]):- noncomp(L1,L2).


/*========================================================================
   Glue symbols together -- dates and numbers
========================================================================*/

glue(C1,C4):-
   select(I1:timex(X1,Date1),C1,C2), 
   select(I2:timex(X2,Date2),C2,C3), X1==X2,
   adjacent(I1,I2,I3),
   concat_dates(Date1,Date2,Date), !,
   glue([I3:timex(X1,Date)|C3],C4).

glue(C1,C4):-
   select(I1:card(X1,Num,eq),C1,C2), 
   num2day(Num,Day),
   Date1 = date([]:'+',[]:'XXXX',[]:'XX',I1:Day),
   select(I2:timex(X2,Date2),C2,C3), X1==X2,
   adjacent(I1,I2,I3),
   concat_dates(Date1,Date2,Date), !,
   glue([I3:timex(X1,Date)|C3],C4).

glue(C1,C4):-
   select(I1:card(X1,Num1,Type),C1,C2),
   number(Num1),
   select(I2:card(X2,Num2,Type),C2,C3), X1==X2,
   number(Num2),
   adjacent(I1,I2,I3), !,
   Num is Num1 * Num2,
   glue([I3:card(X1,Num,Type)|C3],C4).

glue(Conds,Conds).


/* ========================================================================
   NN Compounds
======================================================================== */

compound([I1:named(_,N1,per,S1), I2:named(_,N2,per,S2)],X,
       C-[I1:named(X,N1,per,S1), I2:named(X,N2,per,S2)|C]).

compound([I1:named(_,N1,ttl,S1), I2:named(_,N2,per,S2)],X,
       C-[I1:named(X,N1,ttl,S1), I2:named(X,N2,per,S2)|C]).

compound([I1:named(_,N1,ttl,S1), I2:named(_,N2,nam,S2)],X,
       C-[I1:named(X,N1,ttl,S1), I2:named(X,N2,per,S2)|C]).

compound([I1:named(_,N1,ttl,S1), I2:named(_,N2,org,S2)],X,
       C-[I1:named(X,N1,ttl,S1), I2:named(X,N2,per,S2)|C]).

compound([I1:named(_,N1,ttl,S1), I2:named(_,N2,loc,S2)],X,
       C-[I1:named(X,N1,ttl,S1), I2:named(X,N2,per,S2)|C]).

compound([I1:named(_,N1,per,S1), I2:named(_,N2,nam,S2)],X,
       C-[I1:named(X,N1,per,S1), I2:named(X,N2,per,S2)|C]).

compound([I1:named(_,N1,_,_),    I2:named(_,N2,org,S2)],X,
       C-[I:named(X,N,org,S2)|C]):- atomic_list_concat([N1,N2],'_',N), app(I1,I2,I).

compound([I1:named(_,N1,_,_),    I2:named(_,N2,loc,S2)],X,
       C-[I:named(X,N,loc,S2)|C]):- atomic_list_concat([N1,N2],'_',N), app(I1,I2,I).

compound([I1:named(_,N1,_,_),    I2:named(_,N2,nam,S2)],X,
       C-[I:named(X,N,nam,S2)|C]):- atomic_list_concat([N1,N2],'_',N), app(I1,I2,I).

compound([I1:named(_,N1,_,_),    I2:named(_,N2,per,S2)],X,
       C-[I:named(X,N,nam,S2)|C]):- atomic_list_concat([N1,N2],'_',N), app(I1,I2,I).

compound([I1:named(_,N1,_,_),    I2:named(_,N2,ttl,S2)],X,
       C-[I:named(X,N,nam,S2)|C]):- atomic_list_concat([N1,N2],'_',N), app(I1,I2,I).


/* ========================================================================
   NNN Compounds
======================================================================== */

compound([I1:named(_,P1,per,S), I2:named(_,P2,per,S), I3:named(_,P3,per,S)],X,
       C-[I1:named(X,P1,per,S),I2:named(X,P2,per,S),I3:named(X,P3,per,S)|C]).

compound([I1:named(_,P1,ttl,S), I2:named(_,P2,per,S), I3:named(_,P3,per,S)],X,
       C-[I1:named(X,P1,ttl,S),I2:named(X,P2,per,S),I3:named(X,P3,per,S)|C]).

compound([I1:named(_,P1,ttl,S), I2:named(_,P2,org,S), I3:named(_,P3,org,S)],X,
       C-[I1:named(X,P1,ttl,S),I2:named(X,P2,per,S),I3:named(X,P3,per,S)|C]).

compound([I1:named(_,P1,_,S),   I2:named(_,P2,_,S),   I3:named(_,P3,org,S)],X,
       C-[I:named(X,P,org,S)|C]):- atomic_list_concat([P1,P2,P3],'_',P), app(I1,I2,I3,I).

compound([I1:named(_,P1,_,S),   I2:named(_,P2,_,S),   I3:named(_,P3,loc,S)],X,
       C-[I:named(X,P,loc,S)|C]):- atomic_list_concat([P1,P2,P3],'_',P), app(I1,I2,I3,I).

compound([I1:named(_,P1,loc,S), I2:named(_,P2,loc,S), I3:named(_,P3,nam,S)],X,
       C-[I:named(X,P,loc,S)|C]):- atomic_list_concat([P1,P2,P3],'_',P), app(I1,I2,I3,I).

compound([I1:named(_,P1,org,S), I2:named(_,P2,org,S), I3:named(_,P3,nam,S)],X,
       C-[I:named(X,P,org,S)|C]):- atomic_list_concat([P1,P2,P3],'_',P), app(I1,I2,I3,I).

compound([I1:named(_,P1,_,S),   I2:named(_,P2,_,S),   I3:named(_,P3,_,S)], X,
       C-[I:named(X,P,nam,S)|C]):- atomic_list_concat([P1,P2,P3],'_',P), app(I1,I2,I3,I).


/* ========================================================================
   NNNN Compounds
======================================================================== */

compound([I1:named(_,P1,per,S), I2:named(_,P2,per,S), I3:named(_,P3,per,S), I4:named(_,P4,per,S)],X,
       C-[I1:named(X,P1,per,S),I2:named(X,P2,per,S),I3:named(X,P3,per,S),I4:named(X,P4,per,S)|C]).

compound([I1:named(_,P1,ttl,S), I2:named(_,P2,per,S), I3:named(_,P3,per,S), I4:named(_,P4,per,S)],X,
       C-[I1:named(X,P1,ttl,S),I2:named(X,P2,per,S),I3:named(X,P3,per,S),I4:named(X,P4,per,S)|C]).

compound([I1:named(_,P1,ttl,S), I2:named(_,P2,per,S), I3:pred(_,P3,n,S), I4:named(_,P4,per,S)],X,
       C-[I1:named(X,P1,ttl,S),I2:named(X,P2,per,S),I3:named(X,P3,per,S),I4:named(X,P4,per,S)|C]).

compound([I1:named(_,N1,_,S),   I2:named(_,N2,_,S),   I3:named(_,N3,_,S),   I4:named(_,N4,org,S)],X,
       C-[I:named(X,N,org,S)|C]):- atomic_list_concat([N1,N2,N3,N4],'_',N), app(I1,I2,I3,I4,I).

compound([I1:named(_,N1,_,S),   I2:named(_,N2,_,S),   I3:named(_,N3,_,S),   I4:named(_,N4,loc,S)],X,
       C-[I:named(X,N,loc,S)|C]):- atomic_list_concat([N1,N2,N3,N4],'_',N), app(I1,I2,I3,I4,I).

compound([I1:named(_,N1,_,S),   I2:named(_,N2,_,S),   I3:named(_,N3,_,S),   I4:named(_,N4,nam,S)],X,
       C-[I:named(X,N,nam,S)|C]):- atomic_list_concat([N1,N2,N3,N4],'_',N), app(I1,I2,I3,I4,I).

compound([I1:named(_,N1,_,S),   I2:named(_,N2,_,S),   I3:named(_,N3,_,S),   I4:named(_,N4,per,S)],X,
       C-[I:named(X,N,nam,S)|C]):- atomic_list_concat([N1,N2,N3,N4],'_',N), app(I1,I2,I3,I4,I).

compound([I1:named(_,N1,_,S),   I2:named(_,N2,_,S),   I3:named(_,N3,_,S),   I4:named(_,N4,ttl,S)],X,
       C-[I:named(X,N,nam,S)|C]):- atomic_list_concat([N1,N2,N3,N4],'_',N), app(I1,I2,I3,I4,I).


/* ========================================================================
   NNNNN Compounds
======================================================================== */

compound([I1:named(_,P1,ttl,S), I2:named(_,P2,_,S), I3:named(_,P3,_,S), I4:named(_,P4,_,S), I5:named(_,P5,per,S)],X,
       C-[I1:named(X,P1,ttl,S), I2:named(X,P2,per,S), I3:named(X,P3,per,S), I4:named(X,P4,per,S), I5:named(X,P5,per,S)|C]).

compound([I1:named(_,P1,_,S), I2:named(_,P2,_,S), I3:named(_,P3,_,S), I4:named(_,P4,_,S), I5:named(_,P5,per,S)],X,
       C-[I1:named(X,P1,per,S), I2:named(X,P2,per,S), I3:named(X,P3,per,S), I4:named(X,P4,per,S), I5:named(X,P5,per,S)|C]).

compound([I1:named(_,N1,_,S), I2:named(_,N2,_,S), I3:named(_,N3,_,S), I4:named(_,N4,_,S), I5:named(_,N5,org,S)],X,
       C-[I:named(X,N,org,S)|C]):- atomic_list_concat([N1,N2,N3,N4,N5],'_',N), app(I1,I2,I3,I4,I5,I).

compound([I1:named(_,N1,_,S), I2:named(_,N2,_,S), I3:named(_,N3,_,S), I4:named(_,N4,_,S), I5:named(_,N5,loc,S)],X,
       C-[I:named(X,N,loc,S)|C]):- atomic_list_concat([N1,N2,N3,N4,N5],'_',N), app(I1,I2,I3,I4,I5,I).

compound([I1:named(_,N1,_,S), I2:named(_,N2,_,S), I3:named(_,N3,_,S), I4:named(_,N4,_,S), I5:named(_,N5,nam,S)],X,
       C-[I:named(X,N,nam,S)|C]):- atomic_list_concat([N1,N2,N3,N4,N5],'_',N), app(I1,I2,I3,I4,I5,I).


/* ========================================================================
   NNNNNN Compounds
======================================================================== */

compound([I1:named(_,N1,_,S), I2:named(_,N2,_,S), I3:named(_,N3,_,S), I4:named(_,N4,_,S), I5:named(_,N5,_,S), I6:named(X,N6,T,S)],X,
       C-[I:named(X,N,T,S)|C]):- atomic_list_concat([N1,N2,N3,N4,N5,N6],'_',N), app(I1,I2,I3,I4,I5,I6,I).


/* ========================================================================
   Add NN Relation
======================================================================== */

nn([I1:pred(_,P1,n,S1), I2:pred(_,P2,n,S2)],   X,
 C-[I1:pred(Y,P1,n,S1), []:rel(Y,X,nn,0), I2:pred(X,P2,n,S2)|C], D-[[]:Y|D]).

nn([I1:named(_,P1,T1,S1), I2:pred(_,P2,n,S2)],   X,
 C-[I1:named(Y,P1,T1,S1), []:rel(Y,X,nn,0),   I2:pred(X,P2,n,S2)|C], D-[[]:Y|D]).

nn([I1:pred(_,P1,n,S1),   I2:named(_,P2,T2,S2)], X,
 C-[I1:pred(Y,P1,n,S1),   []:rel(Y,X,nn,0), I2:named(X,P2,T2,S2)|C], D-[[]:Y|D]).

nn([I1:timex(_,T1),       I2:pred(_,P2,n,S2)],   X,
 C-[I1:timex(Y,T1),       []:rel(Y,X,nn,0),   I2:pred(X,P2,n,S2)|C], D-[[]:Y|D]).

nn([I1:timex(_,T1),       I2:named(_,P2,T2,S2)], X,
 C-[I1:timex(Y,T1),       []:rel(Y,X,nn,0), I2:named(X,P2,T2,S2)|C], D-[[]:Y|D]).

nn([I1:pred(_,P1,n,S1),   I2:timex(_,T2)],       X,
 C-[I1:pred(Y,P1,n,S1),   []:rel(Y,X,nn,0),       I2:timex(X,T2)|C], D-[[]:Y|D]).

nn([I1:named(_,P1,T1,S1), I2:timex(_,T2)],       X,
 C-[I1:named(Y,P1,T1,S1), []:rel(Y,X,nn,0),       I2:timex(X,T2)|C], D-[[]:Y|D]).


/* ========================================================================
   Add NNN Relation
======================================================================== */

nn([N1,                N2, I:pred(_,P3,n,S3)], Y,C1-C2, D-[[]:X|D]):- compound([N1,N2],X,[[]:rel(X,Y,nn,0), I:pred(Y,P3,n,S3)|C1]-C2).

nn([N1,                N2, I:timex(_,T3)],     Y,C1-C2, D-[[]:X|D]):- compound([N1,N2],X,[[]:rel(X,Y,nn,0), I:timex(Y,T3)|C1]-C2).

nn([I:pred(_,P3,n,S3), N2, N3],                Y,C1-C2, D-[[]:X|D]):- compound([N2,N3],Y,[[]:rel(X,Y,nn,0), I:pred(X,P3,n,S3)|C1]-C2).

nn([I:timex(_,T3),     N2, N3],                Y,C1-C2, D-[[]:X|D]):- compound([N2,N3],Y,[[]:rel(X,Y,nn,0), I:timex(X,T3)|C1]-C2).

nn([I1:pred(_,P1,n,S1),   I2:pred(_,P2,n,S2),   I3:pred(_,P3,n,S3)],   X,
 C-[I1:pred(Y1,P1,n,S1),   []:rel(Y1,Y2,nn,0), I2:pred(Y2,P2,n,S2),   []:rel(Y2,X,nn,0), I3:pred(X,P3,n,S3)|C], D-[[]:Y1,[]:Y2|D]).

nn([I1:pred(_,P1,n,S1),   I2:pred(_,P2,n,S2),   I3:named(_,P3,T3,S3)], X,
 C-[I1:pred(Y1,P1,n,S1),   []:rel(Y1,Y2,nn,0), I2:pred(Y2,P2,n,S2),   []:rel(Y2,X,nn,0), I3:named(X,P3,T3,S3)|C], D-[[]:Y1,[]:Y2|D]).

nn([I1:pred(_,P1,n,S1),   I2:pred(_,P2,n,S2),   I3:timex(_,T3)],       X,
 C-[I1:pred(Y1,P1,n,S1),   []:rel(Y1,Y2,nn,0), I2:pred(Y2,P2,n,S2),   []:rel(Y2,X,nn,0), I3:timex(X,T3)|C], D-[[]:Y1,[]:Y2|D]).

nn([I1:pred(_,P1,n,S1),   I2:named(_,P2,T2,S2), I3:pred(_,P3,n,S3)],   X,
 C-[I1:pred(Y1,P1,n,S1),   []:rel(Y1,Y2,nn,0), I2:named(Y2,P2,T2,S2), []:rel(Y2,X,nn,0), I3:pred(X,P3,n,S3)|C], D-[[]:Y1,[]:Y2|D]).

nn([I1:pred(_,P1,n,S1),   I2:timex(_,T2),       I3:pred(_,P3,n,S3)],   X,
 C-[I1:pred(Y1,P1,n,S1),   []:rel(Y1,Y2,nn,0), I2:timex(Y2,T2),       []:rel(Y2,X,nn,0), I3:pred(X,P3,n,S3)|C], D-[[]:Y1,[]:Y2|D]).

nn([I1:named(_,P1,T1,S1), I2:pred(_,P2,n,S2),   I3:pred(_,P3,n,S3)],   X,
 C-[I1:named(Y1,P1,T1,S1), []:rel(Y1,X,nn,0),  I2:pred(Y2,P2,n,S2),   []:rel(Y2,X,nn,0), I3:pred(X,P3,n,S3)|C], D-[[]:Y1,[]:Y2|D]).

nn([I1:timex(_,T1),       I2:pred(_,P2,n,S2),   I3:pred(_,P3,n,S3)],   X,
 C-[I1:timex(Y1,T1),       []:rel(Y1,X,nn,0),  I2:pred(Y2,P2,n,S2),   []:rel(Y2,X,nn,0), I3:pred(X,P3,n,S3)|C], D-[[]:Y1,[]:Y2|D]).

nn([I1:named(_,P1,T1,S1), I2:pred(_,P2,n,S2),   I3:named(_,P3,T3,S3)], X,
 C-[I1:named(Y1,P1,T1,S1), []:rel(Y1,X,nn,0),  I2:pred(Y2,P2,n,S2),   []:rel(Y2,X,nn,0), I3:named(X,P3,T3,S3)|C], D-[[]:Y1,[]:Y2|D]).

nn([I1:named(_,P1,T1,S1), I2:timex(_,T2),       I3:named(_,P3,T3,S3)], X,
 C-[I1:named(Y1,P1,T1,S1), []:rel(Y1,X,nn,0),  I2:timex(Y2,T2),       []:rel(Y2,X,nn,0), I3:named(X,P3,T3,S3)|C], D-[[]:Y1,[]:Y2|D]).

nn([I1:named(_,P1,T1,S1), I2:timex(_,T2),       I3:pred(_,P3,T3,S3)],  X,
 C-[I1:named(Y1,P1,T1,S1), []:rel(Y1,X,nn,0),  I2:timex(Y2,T2),       []:rel(Y2,X,nn,0), I3:pred(X,P3,T3,S3)|C], D-[[]:Y1,[]:Y2|D]).

nn([I1:named(_,P1,T1,S1), I2:pred(_,P2,n,S2),   I3:timex(_,T3)],       X,
 C-[I1:named(Y1,P1,T1,S1), []:rel(Y1,X,nn,0),  I2:pred(Y2,P2,n,S2),   []:rel(Y2,X,nn,0), I3:timex(X,T3)|C], D-[[]:Y1,[]:Y2|D]).

nn([I1:timex(_,T1),       I2:pred(_,P2,n,S2),   I3:named(_,P3,T3,S3)], X,
 C-[I1:timex(Y1,T1),       []:rel(Y1,X,nn,0),  I2:pred(Y2,P2,n,S2),   []:rel(Y2,X,nn,0), I3:named(X,P3,T3,S3)|C], D-[[]:Y1,[]:Y2|D]).

nn([I1:timex(_,T1),       I2:named(_,P2,T2,S2),  I3:pred(_,P3,T3,S3)],  X,
 C-[I1:timex(Y1,T1),       []:rel(Y1,X,nn,0),  I2:named(Y2,P2,T2,S2),  []:rel(Y2,X,nn,0), I3:pred(X,P3,T3,S3)|C], D-[[]:Y1,[]:Y2|D]).

nn([I1:timex(_,T1),       I2:pred(_,P2,n,S2),   I3:timex(_,T3)],       X,
 C-[I1:timex(Y1,T1),       []:rel(Y1,X,nn,0),  I2:pred(Y2,P2,n,S2),   []:rel(Y2,X,nn,0), I3:timex(X,T3)|C], D-[[]:Y1,[]:Y2|D]).


/* ========================================================================
   Add NNNN Relation
======================================================================== */

nn([I:pred(_,P1,n,S1),N2,N3,N4], Y,C1-C2, D-[[]:X|D]):- 
   compound([N2,N3,N4],Y,[I:pred(X,P1,n,S1),[]:rel(X,Y,nn,0)|C1]-C2).

nn([I:timex(_,T1),N2,N3,N4],     Y,C1-C2, D-[[]:X|D]):-  
   compound([N2,N3,N4],Y,[I:timex(X,T1),[]:rel(X,Y,nn,0)|C1]-C2).

nn([N1,N2,N3,I:pred(_,P4,n,S4)], Y,C1-C2, D-[[]:X|D]):- 
   compound([N1,N2,N3],X,[I:pred(Y,P4,n,S4),[]:rel(X,Y,nn,0)|C1]-C2).

nn([N1,N2,N3,I:timex(_,T4)],     Y,C1-C2, D-[[]:X|D]):- 
   compound([N1,N2,N3],X,[I:timex(Y,T4),[]:rel(X,Y,nn,0)|C1]-C2).

nn([N1,N2,I3:pred(_,P3,T3,S3),I4:pred(_,P4,T4,S4)], X,C1-C2, D-[[]:Y1,[]:Y3|D]):- 
   compound([N1,N2],Y1,[[]:rel(Y1,X,nn,0),I3:pred(Y3,P3,T3,S3),[]:rel(Y3,X,nn,0),I4:pred(X,P4,T4,S4)|C1]-C2).

nn([N1,N2,I3:timex(_,T3),    I4:named(_,P4,T4,S4)], X,C1-C2, D-[[]:Y1,[]:Y3|D]):- 
   compound([N1,N2],Y1,[[]:rel(Y1,X,nn,0),I3:timex(Y3,T3),[]:rel(Y3,X,nn,0),I4:named(X,P4,T4,S4)|C1]-C2).

nn([I1:named(_,P1,T1,S1), I2:pred(_,P2,T2,S2), N3,N4], Y,C1-C2, D-[[]:X1,[]:X2|D]):- 
   compound([N3,N4],Y,[I1:named(X1,P1,T1,S1),[]:rel(X1,Y,nn,0),I2:pred(X2,P2,T2,S2),[]:rel(X2,Y,nn,0)|C1]-C2).

nn([I1:pred(_,P1,T1,S1), I2:pred(_,P2,T2,S2), N3,N4], Y,C1-C2, D-[[]:X1,[]:X2|D]):- 
   compound([N3,N4],Y,[I1:pred(X1,P1,T1,S1),[]:rel(X1,X2,nn,0),I2:pred(X2,P2,T2,S2),[]:rel(X2,Y,nn,0)|C1]-C2).

nn([I1:pred(_,P1,n,S1), I2:pred(_,P2,n,S2), I3:pred(_,P3,n,S3), I4:pred(_,P4,n,S4)],X, 
 C-[I1:pred(Y1,P1,n,S1), []:rel(Y1,Y2,nn,0),I2:pred(Y2,P2,n,S2),[]:rel(Y2,X,nn,0),I3:pred(Y3,P3,n,S3),[]:rel(Y3,X,nn,0),I4:pred(X,P4,n,S4)|C], D-[[]:Y1,[]:Y2,[]:Y3|D]).

nn([I1:timex(_,T1), I2:pred(_,P2,n,S2), I3:pred(_,P3,n,S3), I4:pred(_,P4,n,S4)], X,
 C-[I1:timex(Y1,T1), []:rel(Y1,X,nn,0),I2:pred(Y2,P2,n,S2),[]:rel(Y2,X,nn,0),I3:pred(Y3,P3,n,S3),[]:rel(Y3,X,nn,0),I4:pred(X,P4,n,S4)|C], D-[[]:Y1,[]:Y2,[]:Y3|D]).

nn([I1:named(_,P1,T1,S1), I2:pred(_,P2,n,S2), I3:pred(_,P3,n,S3), I4:pred(_,P4,n,S4)], X,
 C-[I1:named(Y1,P1,T1,S1), []:rel(Y1,Y2,nn,0),I2:pred(Y2,P2,n,S2),[]:rel(Y2,X,nn,0),I3:pred(Y3,P3,n,S3),[]:rel(Y3,X,nn,0),I4:pred(X,P4,n,S4)|C], D-[[]:Y1,[]:Y2,[]:Y3|D]).


/* ========================================================================
   Add NNNNN Relation
======================================================================== */

nn([N1,N2,N3,N4,I5:pred(_,P5,T5,S5)],Y,C1-C2,D-[[]:X|D]):- 
   compound([N1,N2,N3,N4],X,[[]:rel(X,Y,nn,0),I5:pred(Y,P5,T5,S5)|C1]-C2).

nn([N1,N2,N3,N4,I5:timex(_,T5)],Y,C1-C2,D-[[]:X|D]):- 
   compound([N1,N2,N3,N4],X,[[]:rel(X,Y,nn,0),I5:timex(Y,T5)|C1]-C2).

nn([I1:pred(_,P1,T1,S1),N2,N3,N4,N5],Y,C1-C2,D-[[]:X|D]):- 
   compound([N2,N3,N4,N5],X,[I1:pred(X,P1,T1,S1),[]:rel(X,Y,nn,0)|C1]-C2).

nn([I1:time(_,T1),N2,N3,N4,N5],Y,C1-C2,D-[[]:X|D]):- 
   compound([N2,N3,N4,N5],X,[I1:time(X,T1),[]:rel(X,Y,nn,0)|C1]-C2).

nn([I1:pred(_,P1,T1,S1), I2:pred(_,P2,T2,S2), N3,N4,N5], Y,C1-C2, D-[[]:X1,[]:X2|D]):- 
   compound([N3,N4,N5],Y,[I1:pred(X1,P1,T1,S1),[]:rel(X1,X2,nn,0),I2:pred(X2,P2,T2,S2),[]:rel(X2,Y,nn,0)|C1]-C2).

nn([I1:named(_,P1,T1,S1), I2:pred(_,P2,T2,S2), N3,N4,N5], Y,C1-C2, D-[[]:X1,[]:X2|D]):- 
   compound([N3,N4,N5],Y,[I1:named(X1,P1,T1,S1),[]:rel(X1,X2,nn,0),I2:pred(X2,P2,T2,S2),[]:rel(X2,Y,nn,0)|C1]-C2).

nn([N1,N2,N3,I4:pred(_,P4,T4,S4), I5:pred(_,P5,T5,S5)], Y,C1-C2, D-[[]:X1,[]:X2|D]):- 
   compound([N1,N2,N3],X1,[[]:rel(X1,Y,nn,0),I4:pred(X2,P4,T4,S4),[]:rel(X2,Y,nn,0),I5:pred(Y,P5,T5,S5)|C1]-C2).

nn([N1,N2,I3:pred(_,P3,T3,S3),N4,N5], Y,C1-C3, D-[[]:X1,[]:X2|D]):- 
   compound([N1,N2],X1,[[]:rel(X1,X2,nn,0),I3:pred(X2,P3,T3,S3)|C1]-C2), 
   compound([N4,N5],Y,[[]:rel(X2,Y,nn,0)|C2]-C3).

/* ========================================================================
   Add NNNNNN Relation
======================================================================== */

nn([N1,N2,N3,I4:pred(_,P4,T4,S4),N5,N6], Y,C1-C3, D-[[]:X1,[]:X2|D]):- 
   compound([N1,N2,N3],X1,[[]:rel(X1,X2,nn,0),I4:pred(X2,P4,T4,S4)|C1]-C2), 
   compound([N5,N6],Y,[[]:rel(X2,Y,nn,0)|C2]-C3).


/* ========================================================================
   Relevant DRS conditions
======================================================================== */

relevant(named(X,_,Type,_),X,Type).
relevant(pred(X,_,n,_),X,noun).
relevant(timex(X,_),X,timex).


/* ========================================================================
   Adding DRS conditions (NN compounds)
======================================================================== */

add(D1,C1,D2,C3):-                   %%% take a right-most 
   select(I1:P1,C1,C2),              %%% relevant DRS condition
   relevant(P1,X1,_),                %%% that could be a compound
   \+ (member(I2:P2,C2), 
       relevant(P2,X2,_), X2==X1,
       adjacent(I1,I2,_)), !,
   add1(D1,C2,D2,C3,X1,[I1:P1]).

add(D,C,D,C).


add1(D1,C1,D2,C3,X2,[I2:P2|L]):-     %%% find as many as possible
   select(I1:P1,C1,C2),              %%% relevant DRS conditions
   relevant(P1,X1,_), X2==X1,        %%% adjacent to the left 
   adjacent(I1,I2,_), !,             %%% of the compound
   add1(D1,C2,D2,C3,X2,[I1:P1,I2:P2|L]).

add1(D1,C1,D2,[I:P|C2],_,[I:P]):- !, 
   add(D1,C1,D2,C2).                 %%% not a compound!

add1(D1,C1,D2,C2,X,L):- 
   compound(L,X,C3-C2), !,           %%% simple compound
   add(D1,C1,D2,C3).

add1(D0,C1,D2,C2,X,L):- 
   nn(L,X,C3-C2,D0-D1), !,           %%% add NN relations
   add(D1,C1,D2,C3).

add1(D1,C1,D2,C3,_,L):- 
   append(L,C2,C3),
%  warning('compound ~p not analysed',[L]),
   add(D1,C1,D2,C2).


/*========================================================================
   Adjacent Indexes
========================================================================*/

adjacent([I],[J],[I,J]):- !, J is I + 1.
adjacent(Is,Js,Ks):- member(I,Is), member(J,Js), J is I + 1, !, append(Is,Js,Ks).


/* ========================================================================
   Appending Indices
======================================================================== */

app([A],[B],[A,B]):- !.
app(L1,L2,L):- append(L1,L2,L).

app([A],[B],[C],[A,B,C]):- !.
app(L1,L2,L3,L):- append(L1,L2,T1), append(T1,L3,L).

app([A],[B],[C],[D],[A,B,C,D]):- !.
app(L1,L2,L3,L4,L):- append(L1,L2,T1), append(T1,L3,T2), append(T2,L4,L).

app([A],[B],[C],[D],[E],[A,B,C,D,E]):- !.
app(L1,L2,L3,L4,L5,L):- append(L1,L2,T1), append(T1,L3,T2), append(T2,L4,T3), append(T3,L5,L).

app([A],[B],[C],[D],[E],[F],[A,B,C,D,E,F]):- !.
app(L1,L2,L3,L4,L5,L6,L):- append(L1,L2,T1), append(T1,L3,T2), append(T2,L4,T3), append(T3,L5,T4), append(T4,L6,L).


/*========================================================================
   Concatenate Dates
========================================================================*/

concat_dates(date([]:'+', []:'XXXX', Month,  Day),
             date([]:'+',  Year,   []:'XX', Day),
             date([]:'+',  Year,    Month,  Day)).

concat_dates(date([]:'+',  Year,   []:'XX', Day),
             date([]:'+', []:'XXXX', Month,  Day), 
             date([]:'+',  Year,    Month,  Day)).

concat_dates(date([]:'+', Year, []:'XX', Day),
             date([]:'+', Year,  Month, []:'XX'), 
             date([]:'+', Year,  Month,  Day)).

concat_dates(date([]:'+', Year,  Month, []:'XX'), 
             date([]:'+', Year, []:'XX', Day),
             date([]:'+', Year,  Month,  Day)).

concat_dates(date([]:'+', []:'XXXX', []:'XX', Day),
             date([]:'+', Year,  Month, []:'XX'), 
             date([]:'+', Year,  Month,  Day)).

concat_dates(date([]:'+', Year, []:'XX', []:'XX'), 
             date([]:'+', []:'XXXX', Month, Day),
             date([]:'+', Year, Month, Day) ).

concat_dates(date([]:'+', []:'XXXX', Month, Day),
             date([]:'+', Year, []:'XX', []:'XX'),
             date([]:'+', Year, Month, Day) ).

concat_dates(date([]:'+', []:'XXXX', Month, []:'XX'),
             date([]:'+', Year, []:'XX', Day),
             date([]:'+', Year, Month, Day) ).


/*========================================================================
   Day
========================================================================*/

num2day(Num,Day):-
   integer(Num), Num > 0, Num < 10,
   number_codes(Num,[Code]), !,
   atom_codes(Day,[48,Code]).

num2day(Num,Day):-
   integer(Num), Num > 9, Num < 32,
   atom_number(Day,Num).

