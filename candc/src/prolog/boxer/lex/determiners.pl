
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
   Sem = lam(N,lam(P,merge(merge(drs([Index:X],[]),
                                 app(N,X)),
                           app(P,X)))).


/* -------------------------------------------------------------------------
   Universally quantifying
------------------------------------------------------------------------- */

semlex_det(Lemma,Index,Sem):-
   member(Lemma,[all,each,either,any,every,whichever,whatever]), !,
   Sem = lam(N,lam(P,drs([],[Index:imp(merge(drs([Index:X],[]),
                                             app(N,X)),
                                       app(P,X))]))).


/* -------------------------------------------------------------------------
   Negation
------------------------------------------------------------------------- */

semlex_det(no,Index,Sem):- !,
   Sem = lam(N,lam(P,drs([],[Index:imp(merge(drs([Index:X],[]),
                                             app(N,X)),
                                       drs([],[Index:not(app(P,X))]))]))).


/* -------------------------------------------------------------------------
   Definites/Demonstratives
------------------------------------------------------------------------- */

semlex_det(Lemma,Index,Sem):-
   member(Lemma,[the,that,this,those,these,both]), !,
   Sem = lam(N,lam(P,alfa(def,merge(drs([Index:X],[]),
                                    app(N,X)),
                              app(P,X)))).


/* -------------------------------------------------------------------------
   WH
------------------------------------------------------------------------- */

semlex_det(Lemma,Index,Sem):-
   member(Lemma,[which,what]), !,
   Sem = lam(N,lam(P,drs([],[Index:whq([],
                                       merge(drs([Index:X],[]),app(N,X)),
                                       X,
                                       app(P,X))]))).


/* -------------------------------------------------------------------------
   "another"
------------------------------------------------------------------------- */

semlex_det(another,Index,Sem):- !,
   Sem = lam(N,lam(P,alfa(def,merge(drs([[]:Y],[]),
                                    app(N,Y)),
                              merge(merge(drs([Index:X],
                                              [Index:not(drs([],
                                                            [[]:eq(X,Y)]))]),
                                          app(N,X)),
                                    app(P,X))))).


/* -------------------------------------------------------------------------
   "neither" (see Heim & Kratzer 1998 p. 154)
------------------------------------------------------------------------- */

semlex_det(neither,Index,Sem):- !,
   Sem = lam(N,lam(P,drs([],[Index:imp(merge(drs([Index:X],[]),
                                             app(N,X)),
                                       drs([],[Index:not(app(P,X))]))]))).


/* -------------------------------------------------------------------------
   Possessives
------------------------------------------------------------------------- */

semlex_det(my,Index,Sem):- !,
   Sem = lam(N,lam(P,alfa(pro,drs([Index:Y],[Index:pred(Y,person,n,1)]),
                              alfa(def,merge(drs([Index:X],[Index:rel(X,Y,of,0)]),
                                             app(N,X)),
                                       app(P,X))))).

semlex_det(your,Index,Sem):- !,
   Sem = lam(N,lam(P,alfa(pro,drs([Index:Y],[Index:pred(Y,person,n,1)]),
                              alfa(def,merge(drs([Index:X],[Index:rel(X,Y,of,0)]),
                                             app(N,X)),
                                       app(P,X))))).

semlex_det(his,Index,Sem):- !,
   Sem = lam(N,lam(P,alfa(pro,drs([Index:Y],[Index:pred(Y,male,a,0)]),
                              alfa(def,merge(drs([Index:X],[Index:rel(X,Y,of,0)]),
                                             app(N,X)),
                                       app(P,X))))).

semlex_det(her,Index,Sem):- !,
   Sem = lam(N,lam(P,alfa(pro,drs([Index:Y],[Index:pred(Y,female,a,0)]),
                              alfa(def,merge(drs([Index:X],[Index:rel(X,Y,of,0)]),
                                             app(N,X)),
                                       app(P,X))))).

semlex_det(its,Index,Sem):- !,
   Sem = lam(N,lam(P,alfa(pro,drs([Index:Y],[Index:pred(Y,neuter,a,0)]),
                              alfa(def,merge(drs([Index:X],[Index:rel(X,Y,of,0)]),
                                             app(N,X)),
                                       app(P,X))))).

semlex_det(our,Index,Sem):- !,
   Sem = lam(N,lam(P,alfa(pro,drs([Index:Y],[Index:pred(Y,person,n,1)]),
                              alfa(def,merge(drs([Index:X],[Index:rel(X,Y,of,0)]),
                                             app(N,X)),
                                       app(P,X))))).

semlex_det(their,Index,Sem):- !,
   Sem = lam(N,lam(P,alfa(pro,drs([Index:Y],[Index:pred(Y,thing,n,12)]),
                              alfa(def,merge(drs([Index:X],[Index:rel(X,Y,of,0)]),
                                             app(N,X)),
                                       app(P,X))))).


/* -------------------------------------------------------------------------
   Many/Much [as determiner]
------------------------------------------------------------------------- */

semlex_det(many,Index,Sem):- !,
   Sem = lam(P,lam(Q,merge(drs([Index:X],[Index:pred(X,quantity,n,1)]),
                           merge(app(P,X),app(Q,X))))).

semlex_det(much,Index,Sem):- !,
   Sem = lam(P,lam(Q,merge(drs([Index:X],[Index:pred(X,amount,n,3)]),
                           merge(app(P,X),app(Q,X))))).


/* -------------------------------------------------------------------------
   Wrongly classified determiners
------------------------------------------------------------------------- */

semlex_det(_Lemma,Index,Sem):-
   Sem = lam(N,lam(P,merge(merge(drs([Index:X],[]),app(N,X)),app(P,X)))).






/* -------------------------------------------------------------------------
   Plurals (in progress)

semlex_det(Cat,Lemma,Index,Sem):-
   Cat = 'NP[nb]/N[pl]',
   member(Lemma,['some']), !,
   Sem = lam(N,lam(P,merge(drs([Index:G],[Index:pred(G,group,n,1),
                                          Index:imp(drs([Index:X],[Index:rel(X,G,member,0)]),
                                                    app(P,X))]),
                           app(N,G)))).

semlex_det(Cat,Lemma,Index,Sem):-
   Cat = 'NP[nb]/N[pl]',
   member(Lemma,['no']), !,
   Sem = lam(N,lam(P,merge(drs([Index:G],[Index:pred(G,group,n,1),
                                          Index:imp(drs([Index:X],[Index:rel(X,G,member,0)]),
                                                    drs([],[Index:not(app(P,X))]))]),
                           app(N,G)))).

semlex_det(Cat,Lemma,Index,Sem):-
   Cat = 'NP[nb]/N[pl]',
   member(Lemma,['the']), !,
   Sem = lam(N,lam(P,alfa(def,merge(drs([Index:G],[Index:pred(G,group,n,1)]),app(N,G)),
                              drs([],[Index:imp(drs([Index:X],[Index:rel(X,G,member,0)]),app(P,X))])))).

semlex_det(Cat,Lemma,Index,Sem):-
   Cat = 'NP[nb]/N[pl]',
   member(Lemma,['any']), !,
   Sem = lam(N,lam(P,drs([],[Index:imp(merge(drs([Index:G],[Index:pred(G,group,n,1)]),app(N,G)),
                                       drs([],[Index:imp(drs([Index:X],[Index:rel(X,G,member,0)]),app(P,X))]))]))).

semlex_det(Cat,Lemma,Index,Sem):-
   member(Cat,['NP[nb]/N[pl]']),
   member(Lemma,['few','several']), !,
   Sem = lam(N,lam(P,merge(drs([Index:G],[Index:pred(G,group,n,1),
                                          Index:imp(drs([Index:X],[Index:rel(X,G,member,0)]),
                                                        app(P,X))]),
                           app(N,G)))).

semlex_det(Cat,Lemma,Index,Sem):-
   member(Cat,['NP[nb]/N[pl]']),
   member(Lemma,['all']), !,
   Sem = lam(N,lam(P,drs([],[Index:imp(merge(drs([Index:G],[Index:pred(G,group,n,1)]),app(N,G)),
                                         drs([],[Index:imp(drs([Index:X],[Index:rel(X,G,member,0)]),app(P,X))]))]))).

semlex_det(Cat,Lemma,Index,Sem):-
   member(Cat,['NP[nb]/N[pl]']),
   member(Lemma,['these','those']), !,
   Sem = lam(N,lam(P,alfa(def,merge(drs([Index:G],[Index:pred(G,group,n,1)]),app(N,G)),
                              drs([],[Index:imp(drs([Index:X],[Index:rel(X,G,member,0)]),app(P,X))])))).

semlex_det(Cat,Lemma,Index,Sem):-
   member(Cat,['NP[nb]/N[pl]']),
   member(Lemma,['both']), !,
   Sem = lam(N,lam(P,alfa(def,merge(drs([Index:G],[Index:pred(G,group,n,1),
                                                     Index:card(G,2,eq)]),app(N,G)),
                              drs([],[Index:imp(drs([Index:X],[Index:rel(X,G,member,0)]),app(P,X))])))).

semlex_det(Cat,Lemma,Index,Sem):-
   Cat = 'NP[nb]/N[pl]', 
   member(Lemma,['my','your']), !,
   Sem = lam(N,lam(P,alfa(pro,drs([Index:Y],[Index:pred(Y,person,n,1)]),
                              alfa(def,merge(drs([Index:G],[Index:pred(G,group,n,1),
                                                            Index:imp(drs([Index:X],[Index:rel(X,G,member,0)]),
                                                                      drs([],[Index:rel(X,Y,of,0)]))]),app(N,G)),
                                       drs([],[Index:imp(drs([Index:X],[Index:rel(X,G,member,0)]),app(P,X))]))))).

semlex_det(Cat,Lemma,Index,Sem):-
   Cat = 'NP[nb]/N', 
   member(Lemma,['our']), !,
   Sem = lam(N,lam(P,alfa(pro,drs([Index:G],[Index:pred(G,group,n,1)]),
                              alfa(def,merge(drs([Index:X],[Index:imp(drs([Index:Y],[Index:rel(Y,G,member,0)]),
                                                                      drs([],[Index:pred(Y,person,n,1),
                                                                              Index:rel(X,Y,of,0)]))]),
                                             app(N,X)),
                                       app(P,X))))).

semlex_det(Cat,Lemma,Index,Sem):-
   Cat = 'NP[nb]/N[pl]', 
   member(Lemma,['our']), !,
   Sem = lam(N,lam(P,alfa(pro,drs([Index:G1],[Index:pred(G1,group,n,1)]),
                              alfa(def,drs([Index:G2],[Index:imp(drs([Index:X],[Index:rel(X,G2,member,0)]),
                                                                 merge(drs([Index:Y],[Index:pred(Y,person,n,1),
                                                                                      Index:rel(X,Y,of,0),
                                                                                      Index:rel(Y,G1,member,0)]),
                                                                       app(N,X)))]),
                                       drs([],[Index:imp(drs([Index:X],[Index:rel(X,G2,member,0)]),
                                                         app(P,X))]))))).

semlex_det(Cat,Lemma,Index,Sem):-
   Cat = 'NP[nb]/N[pl]', 
   member(Lemma,['its']), !,
   Sem = lam(N,lam(P,alfa(pro,drs([Index:Y],[Index:pred(Y,neuter,a,0)]),
                              alfa(def,merge(drs([Index:G],[Index:pred(G,group,n,1),
                                                            Index:imp(drs([Index:X],[Index:rel(X,G,member,0)]),
                                                                      drs([],[Index:rel(X,Y,of,0)]))]),app(N,G)),
                                       drs([],[Index:imp(drs([Index:X],[Index:rel(X,G,member,0)]),app(P,X))]))))).

semlex_det(Cat,Lemma,Index,Sem):-
   Cat = 'NP[nb]/N[pl]', 
   member(Lemma,['his']), !,
   Sem = lam(N,lam(P,alfa(pro,drs([Index:Y],[Index:pred(Y,male,a,0)]),
                              alfa(def,merge(drs([Index:G],[Index:pred(G,group,n,1),
                                                            Index:imp(drs([Index:X],[Index:rel(X,G,member,0)]),
                                                                      drs([],[Index:rel(X,Y,of,0)]))]),app(N,G)),
                                       drs([],[Index:imp(drs([Index:X],[Index:rel(X,G,member,0)]),app(P,X))]))))).

semlex_det(Cat,Lemma,Index,Sem):-
   Cat = 'NP[nb]/N[pl]', 
   member(Lemma,['her']), !,
   Sem = lam(N,lam(P,alfa(pro,drs([Index:Y],[Index:pred(Y,female,a,0)]),
                              alfa(def,merge(drs([Index:G],[Index:pred(G,group,n,1),
                                                            Index:imp(drs([Index:X],[Index:rel(X,G,member,0)]),
                                                                      drs([],[Index:rel(X,Y,of,0)]))]),app(N,G)),
                                       drs([],[Index:imp(drs([Index:X],[Index:rel(X,G,member,0)]),app(P,X))]))))).

semlex_det(Cat,Lemma,Index,Sem):-
   Cat = 'NP[nb]/N', 
   member(Lemma,['their']), !,
   Sem = lam(N,lam(P,alfa(pro,drs([Index:G],[Index:pred(G,group,n,1)]),
                              alfa(def,merge(drs([Index:X],[Index:imp(drs([Index:Y],[Index:rel(Y,G,member,0)]),
                                                                      drs([],[Index:rel(X,Y,of,0)]))]),
                                             app(N,X)),
                                       app(P,X))))).

semlex_det(Cat,Lemma,Index,Sem):-
   Cat = 'NP[nb]/N[pl]', 
   member(Lemma,['their']), !,
   Sem = lam(N,lam(P,alfa(pro,drs([Index:G1],[Index:pred(G1,group,n,1)]),
                              alfa(def,merge(drs([Index:G2],[Index:imp(drs([Index:X],[Index:rel(X,G2,member,0)]),
                                                                       drs([],[Index:rel(X,G1,of,0)]))]),app(N,G2)),
                                       drs([],[Index:imp(drs([Index:X],[Index:rel(X,G2,member,0)]),
                                                         app(P,X))]))))).


------------------------------------------------------------------------- */


