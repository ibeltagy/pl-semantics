
:- module(determiners,[semlex_det/3]).

/* =========================================================================
   Determiners: NP/N

   This file contains the lexical semantic specifications for determiners,
   i.e. tokens with CCG category NP/N.
========================================================================= */


/* -------------------------------------------------------------------------
   Indefinites
------------------------------------------------------------------------- */

semlex_det(Lemma,Index,Sem):-
   member(Lemma,[a,an,one,some,few,several]), !,
   Sem = lam(N,lam(P,merge(merge(B:drs([B:Index:X],[]),
                                 app(N,X)),
                           app(P,X)))).


/* -------------------------------------------------------------------------
   Universally quantifying
------------------------------------------------------------------------- */

semlex_det(Lemma,Index,Sem):-
   member(Lemma,[all,each,either,any,every,whichever,whatever]), !,
   Sem = lam(N,lam(P,B1:drs([],[B1:[]:imp(merge(B2:drs([B2:Index:X],[]),
                                          app(N,X)),
                                    app(P,X))]))).


/* -------------------------------------------------------------------------
   Negation
------------------------------------------------------------------------- */

semlex_det(no,Index,Sem):- !,
   Sem = lam(N,lam(P,B1:drs([],[B1:Index:not(merge(B2:drs([B2:Index:X],[]),
                                             merge(app(N,X),
                                                   app(P,X))))]))).


/* -------------------------------------------------------------------------
   Definites/Demonstratives
------------------------------------------------------------------------- */

semlex_det(Lemma,Index,Sem):-
   member(Lemma,[the,that,this,those,these,both]), !,
   Sem = lam(N,lam(P,alfa(def,merge(B:drs([B:Index:X],[]),
                                    app(N,X)),
                              app(P,X)))).


/* -------------------------------------------------------------------------
   WH
------------------------------------------------------------------------- */

semlex_det(Lemma,Index,Sem):-
   member(Lemma,[which,what]), !,
   Sem = lam(N,lam(P,B1:drs([],[B1:[]:whq([],
                                       merge(B2:drs([B2:Index:X],[]),app(N,X)),
                                       X,
                                       app(P,X))]))).


/* -------------------------------------------------------------------------
   "another"
------------------------------------------------------------------------- */

semlex_det(another,Index,Sem):- !,
   Sem = lam(N,lam(P,alfa(def,merge(B1:drs([B1:[]:Y],[]),
                                    app(N,Y)),
                              merge(merge(B2:drs([B2:Index:X],
                                                 [B2:[]:not(B3:drs([],[B3:[]:eq(X,Y)]))]),
                                          app(N,X)),
                                    app(P,X))))).


/* -------------------------------------------------------------------------
   "neither" (see Heim & Kratzer 1998 p. 154)
------------------------------------------------------------------------- */

semlex_det(neither,Index,Sem):- !,
   Sem = lam(N,lam(P,B1:drs([],[B1:[]:imp(merge(B2:drs([B2:Index:X],[]),app(N,X)),
                                                B3:drs([],[B3:Index:not(app(P,X))]))]))).


/* -------------------------------------------------------------------------
   Possessives
------------------------------------------------------------------------- */

semlex_det(my,Index,Sem):- !,
   Sem = lam(N,lam(P,alfa(pro,B1:drs([B1:[]:Y],[B1:[]:pred(Y,person,n,1)]),
                              alfa(def,merge(B2:drs([B2:[]:X],[B2:Index:rel(X,Y,of,0)]),
                                             app(N,X)),
                                       app(P,X))))).

semlex_det(your,Index,Sem):- !,
   Sem = lam(N,lam(P,alfa(pro,B1:drs([B1:[]:Y],[B1:[]:pred(Y,person,n,1)]),
                              alfa(def,merge(B2:drs([B2:[]:X],[B2:Index:rel(X,Y,of,0)]),
                                             app(N,X)),
                                       app(P,X))))).

semlex_det(his,Index,Sem):- !,
   Sem = lam(N,lam(P,alfa(pro,B1:drs([B1:[]:Y],[B1:[]:pred(Y,male,a,0)]),
                              alfa(def,merge(B2:drs([B2:[]:X],[B2:Index:rel(X,Y,of,0)]),
                                             app(N,X)),
                                       app(P,X))))).

semlex_det(her,Index,Sem):- !,
   Sem = lam(N,lam(P,alfa(pro,B1:drs([B1:[]:Y],[B1:[]:pred(Y,female,a,0)]),
                              alfa(def,merge(B2:drs([B2:[]:X],[B2:Index:rel(X,Y,of,0)]),
                                             app(N,X)),
                                       app(P,X))))).

semlex_det(its,Index,Sem):- !,
   Sem = lam(N,lam(P,alfa(pro,B1:drs([B1:[]:Y],[B1:[]:pred(Y,neuter,a,0)]),
                              alfa(def,merge(B2:drs([B2:[]:X],[B2:Index:rel(X,Y,of,0)]),
                                             app(N,X)),
                                       app(P,X))))).

semlex_det(our,Index,Sem):- !,
   Sem = lam(N,lam(P,alfa(pro,B1:drs([B1:[]:Y],[B1:[]:pred(Y,person,n,1)]),
                              alfa(def,merge(B2:drs([B2:[]:X],[B2:Index:rel(X,Y,of,0)]),
                                             app(N,X)),
                                       app(P,X))))).

semlex_det(their,Index,Sem):- !,
   Sem = lam(N,lam(P,alfa(pro,B1:drs([B1:[]:Y],[B1:[]:pred(Y,thing,n,12)]),
                              alfa(def,merge(B2:drs([B2:[]:X],[B2:Index:rel(X,Y,of,0)]),
                                             app(N,X)),
                                       app(P,X))))).


/* -------------------------------------------------------------------------
   Many/Much [as determiner]
------------------------------------------------------------------------- */

semlex_det(many,Index,Sem):- !,
   Sem = lam(P,lam(Q,merge(B:drs([B:[]:X],[B:Index:pred(X,quantity,n,1)]),
                           merge(app(P,X),app(Q,X))))).

semlex_det(much,Index,Sem):- !,
   Sem = lam(P,lam(Q,merge(B:drs([B:[]:X],[B:Index:pred(X,amount,n,3)]),
                           merge(app(P,X),app(Q,X))))).


/* -------------------------------------------------------------------------
   Wrongly classified determiners
------------------------------------------------------------------------- */

semlex_det(_Lemma,Index,Sem):-
   Sem = lam(N,lam(P,merge(merge(B:drs([B:Index:X],[]),app(N,X)),app(P,X)))).
