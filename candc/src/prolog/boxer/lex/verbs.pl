 
:- module(verbs,
          [semlex_verb/5,    % +Cat, +Sym, +Index, +Att--Att, -Sem
           closing/1,
           aux_modal_verb/1
          ]).

:- use_module(boxer(slashes)).
:- use_module(semlib(options),[option/2]).
:- use_module(library(lists),[member/2]).
:- use_module(lex(tense),[tense/4,aspect/5]).
:- use_module(boxer(categories),[category/4,
				 roles/4,att/3,
                                 category_type/5]).


/* =========================================================================
   I n t r a n s i t i v e   V e r b s
========================================================================= */

/* -------------------------------------------------------------------------
   VP Ellipsis (... so does NP ...)
------------------------------------------------------------------------- */

semlex_verb(Cat,do,Index,Att1-Att3,Sem):- 
   option('--vpe',true),
   member(Cat,[(s:dcl\(s:adj\np))/np]),
   roles(Sym,(s:dcl\np)/np,[Role],Att1-Att2), !,
   DRS = merge(B:drs([B:[]:E],
                     [B:Index:pred(E,Sym,v,98),
                      B:[]:role(E,X,Role,1)]),
               app(P,E)),
   tense(dcl,[],Att2-Att3,TDRS),
   Sem = lam(NP,lam(_ADJ,app(TDRS,lam(P,app(NP,lam(X,DRS)))))).

/* -------------------------------------------------------------------------
   Intransitive (VP Ellipsis)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,Index,Att1-Att3,Sem):-
   option('--vpe',true),
   option('--modal',true),
   modal_verb(pos,Sym),
   member(Cat,[s:Mood\np,s:Mood/np]), 
   roles(Sym,Cat,[Role],Att1-Att2), !,
   DRS = merge(B:drs([B:[]:E],
                     [B:Index:pred(E,Sym,v,98),
                      B:[]:role(E,X,Role,1)]),
               app(P,E)),
   tense(Mood,[],Att2-Att3,TDRS),
   Sem = lam(NP,app(TDRS,lam(P,B2:drs([],[B2:[]:pos(app(NP,lam(X,DRS)))])))).

semlex_verb(Cat,Sym,Index,Att1-Att3,Sem):-
   option('--vpe',true),
   option('--modal',true),
   modal_verb(nec,Sym),
   member(Cat,[s:Mood\np,s:Mood/np]), 
   roles(Sym,Cat,[Role],Att1-Att2), !,
   DRS = merge(B1:drs([B1:[]:E],
                      [B1:Index:pred(E,Sym,v,98),
                       B1:[]:role(E,X,Role,1)]),
               app(P,E)),
   tense(Mood,[],Att2-Att3,TDRS),
   Sem = lam(NP,app(TDRS,lam(P,B2:drs([],[B2:[]:nec(app(NP,lam(X,DRS)))])))).

semlex_verb(Cat,Sym,Index,Att1-Att3,Sem):- 
   option('--vpe',true),
   aux_modal_verb(Sym),
   member(Cat,[s:Mood\np,s:Mood/np]), 
   roles(Sym,Cat,[Role],Att1-Att2), !,
   ( Sym = do, \+ Mood = pss, \+ Mood = pt ; \+ Sym = do ), 
   ( Sym = can, \+ Mood = pss ; \+ Sym = can ), 
   !,   %%% VP Ellipsis
   DRS = merge(B:drs([B:[]:E],
                     [B:Index:pred(E,Sym,v,98),
                      B:[]:role(E,X,Role,1)]),
               app(P,E)),
   tense(Mood,[],Att2-Att3,TDRS),
   Sem = lam(NP,app(TDRS,lam(P,app(NP,lam(X,DRS))))).


/* -------------------------------------------------------------------------
   more or less (... than X)
------------------------------------------------------------------------- */

semlex_verb(s:adj\np,Sym,Index,Att-Att,Sem):- 
   member(Sym,[more,less]), !,
   att(Att,sense,Sense),
   Sem = lam(NP,lam(E,app(NP,lam(X,merge(B:drs([B:[]:S],[B:Index:pred(S,Sym,a,Sense),
                                                         B:[]:role(X,S,topic,-1)]),app(E,S)))))).


/* -------------------------------------------------------------------------
   Adjectival (possible, necessary)

semlex_verb(s:adj\np,possible,Index,Att-Att,Sem):- 
   option('--modal',true), !,
   Sem = lam(NP,lam(P,merge(B1:drs([B1:[]:E],
                                   [B1:[]:pos(app(NP,lam(X,B2:drs([],[B2:Index:role(E,X,theme,1)]))))]),
                            app(P,E)))).

semlex_verb(s:adj\np,necessary,Index,Att-Att,Sem):- 
   option('--modal',true), !,
   Sem = lam(NP,lam(P,merge(B1:drs([B1:[]:E],
                                   [B1:[]:nec(app(NP,lam(X,B2:drs([],[B2:Index:role(E,X,theme,1)]))))]),
                            app(P,E)))).
------------------------------------------------------------------------- */


/* -------------------------------------------------------------------------
   Intransitive (standard case)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,Index,Att1-Att3,Sem):- 
   member(Cat,[s:Mood\np,s:Mood/np]),
   roles(Sym,Cat,[Role],Att1-Att2), !,
   ( Mood=adj, !, Pos=a; \+ Mood=adj, Pos=v ),
   att(Att2,sense,Sense),
   DRS = merge(B:drs([B:[]:E],
                     [B:Index:pred(E,Sym,Pos,Sense),B:[]:role(E,X,Role,1)]),
               app(P,E)),
   tense(Mood,[],Att2-Att3,TDRS),
   Sem = lam(NP,app(TDRS,lam(P,app(NP,lam(X,DRS))))).


/* -------------------------------------------------------------------------
   Passive Phrasal Verbs ("An action has been called for by an official")
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,Index,Att-Att,Sem):-
   Cat = (s:pss\np)/(pp/np), !,
   att(Att,sense,Sense),
   Sem = lam(PP,lam(NP,lam(F,merge(B:drs([B:[]:E],
                                         [B:Index:pred(E,Sym,v,Sense)]),
                                   merge(app(app(PP,NP),E),
                                         app(F,E)))))).



/* =========================================================================
   T r a n s i t i v e   V e r b s
========================================================================= */

/* -------------------------------------------------------------------------
    Copula 
------------------------------------------------------------------------- */

% expriment, giving negation wide scope in 'there is no'
semlex_verb(Cat,be,Index,Att1-Att2,Sem):- 
   option('--x',true),
   option('--copula',true),
   category_type(Cat,_,npVnp,_,Mood), !,
   DRS = merge(B:drs([B:[]:E],
                     [B:[]:prop(E,app(NP2,lam(Y,app(NP1,lam(X,B2:drs([],[B2:Index:eq(X,Y)]))))))]),
               app(P,E)),
   tense(Mood,[],Att1-Att2,TDRS),
   Sem = lam(NP2,lam(NP1,app(TDRS,lam(P,DRS)))).

% old version
semlex_verb(Cat,be,Index,Att1-Att2,Sem):- 
   option('--copula',true),
   category_type(Cat,_,npVnp,_,Mood), !,
   DRS = merge(B:drs([B:[]:E],
                     [B:[]:prop(E,app(NP2,lam(Y,B2:drs([],[B2:Index:eq(X,Y)]))))]),
               app(P,E)),
   tense(Mood,[],Att1-Att2,TDRS),
   Sem = lam(NP2,lam(NP1,app(TDRS,lam(P,app(NP1,lam(X,DRS)))))).

semlex_verb(Cat,be,Index,Att1-Att2,Sem):-          %%% place holder for IS-A sense
    option('--copula',true),
    category_type(Cat,_,npVnp,_,Mood), !,
    DRS = merge(B:drs([B:[]:E],
                      [B:[]:prop(E,B2:drs([],[B2:[]:imp(merge(B3:drs([B3:[]:X],[]),app(NP1,lam(Y,B4:drs([],[B4:[]:eq(Y,X)])))),
                                                    app(NP2,lam(Y,B5:drs([],[B5:Index:eq(Y,X)]))))]))]),
                app(P,E)),
    tense(Mood,[],Att1-Att2,TDRS),
    Sem = lam(NP2,lam(NP1,app(TDRS,lam(P,DRS)))).

semlex_verb(Cat,be,Index,Att1-Att3,Sem):- 
   option('--semantics',tacitus),
   member(Cat,[(s:Mood\np)/np,(s:Mood/np)/np]),
   roles(be,Cat,[Role],Att1-Att2), !,
   DRS = merge(B:drs([B:[]:E],
                     [B:Index:pred(E,be,v,1),
                      B:[]:role(E,X,Role,1),
                      B:[]:eq(X,Y)]),
               app(P,E)),
   tense(Mood,[],Att2-Att3,TDRS),
   Sem = lam(NP2,lam(NP1,app(TDRS,
                             lam(P,app(NP1,
                                       lam(X,app(NP2,
                                                 lam(Y,DRS)))))))).


/* -------------------------------------------------------------------------
   Adjectival (np V pp)
------------------------------------------------------------------------- */

semlex_verb((s:adj\np)/pp,Sym,Index,Att-Att,Sem):- !,
   att(Att,sense,Sense),
   Sem = lam(PP,lam(NP,lam(P,app(NP,lam(X,merge(B1:drs([B1:[]:E],
                                                       [B1:Index:pred(E,Sym,a,Sense),
                                                        B1:[]:role(E,X,theme,1)]),
                                                merge(app(PP,E),app(P,E)))))))).


/* -------------------------------------------------------------------------
   Transitive (np V np) intensional

semlex_verb(Cat,Sym,Index,Att1-Att2,Sem):-
   member(Sym,[want,need]),
   category_type(Cat,Sym,npVnp,[Role,_],Mood), !, 
   closing(CC),
   att(Att1,sense,Sense),
   semlex_verb((s:b\np)/np,have,'VB',[],TV),
   DRS = merge(B:drs([B:[]:E,B:[]:A],
                     [B:Index:pred(E,Sym,v,Sense),
                      B:[]:role(E,X,Role,1),
                      B:[]:role(E,A,theme,1),
                      B:[]:prop(A,app(app(app(TV,NP2),lam(P,app(P,X))),CC))]),
               app(P,E)),
   tense(Mood,[],Att1-Att2,TDRS),
   Sem = lam(NP2,lam(NP1,app(TDRS,lam(P,app(NP1,lam(X,DRS)))))).
------------------------------------------------------------------------- */


/* -------------------------------------------------------------------------
   Transitive (np V np) extensional

   Interesting idea: adding presupposition:
   ADRS = alfa(def,drs([],[[]:not(drs([],[[]:eq(X,Y)]))]),DRS) 

------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,Index,Att1-Att3,Sem):-
   member(Cat, [(s:Mood\np)/np,(s:Mood/np)/np,(s:Mood\np)\np]), 
   roles(Sym,Cat,[Role2,Role1],Att1-Att2), !,
   ( Mood=adj, !, Pos=a; \+ Mood=adj, Pos=v ),
   att(Att2,sense,Sense),
   DRS = merge(B:drs([B:[]:E],
                     [B:Index:pred(E,Sym,Pos,Sense),
                      B:[]:role(E,X,Role1,1),
                      B:[]:role(E,Y,Role2,1)]),
               app(P,E)),
   tense(Mood,[],Att2-Att3,TDRS),
   Sem = lam(NP2,lam(NP1,app(TDRS,
                             lam(P,app(NP1,
                                       lam(X,app(NP2,
                                                 lam(Y,DRS)))))))).

/* -------------------------------------------------------------------------
   Transitive (there is NP)
   Not working currently because thr is stripped of NP category

semlex_verb((s:Mood\np_thr)/np,_,Index,Att1-Att2,Sem):- !,
   tense(Mood,Index,Att1-Att2,TDRS),
   Sem = lam(NP,lam(_,app(TDRS,lam(F,app(NP,lam(X,merge(B:drs([B:[]:E],
                                                            [B:Index:pred(E,be,v,5), 
                                                             B:[]:role(E,X,agent,1)]),
                                                        app(F,E)))))))).
------------------------------------------------------------------------- */


/* -------------------------------------------------------------------------
   Transitive (np V pp)
------------------------------------------------------------------------- */

semlex_verb(Cat,be,Index,Att1-Att2,Sem):-
   option('--copula',true),
   category_type(Cat,_,npVpp,_,Mood), !, 
   DRS = merge(B:drs([B:[]:E],
                     [B:[]:prop(E,app(PP,Y))]),
               app(P,E)),
   tense(Mood,Index,Att1-Att2,TDRS),
   Sem = lam(PP,lam(NP,app(TDRS,lam(P,app(NP,lam(Y,DRS)))))).

semlex_verb(Cat,Sym,Index,Att1-Att3,Sem):-
   member(Cat,[(s:Mood\np)/pp,(s:Mood/np)/pp]), 
   roles(Sym,Cat,[Role],Att1-Att2), !,
   att(Att2,sense,Sense),
   DRS = merge(merge(B:drs([B:[]:E],
                           [B:Index:pred(E,Sym,v,Sense),
                            B:[]:role(E,Y,Role,1)]),
                     app(PP,E)),
               app(P,E)),
   tense(Mood,[],Att2-Att3,TDRS),
   Sem = lam(PP,lam(NP,app(TDRS,lam(P,app(NP,lam(Y,DRS)))))).


/* -------------------------------------------------------------------------
   Transitive (pp V np)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,Index,Att1-Att3,Sem):-
   Cat = (s:Mood\pp)/np,
   roles(Sym,Cat,[Role],Att1-Att2), !,
   att(Att2,sense,Sense),
   DRS = merge(merge(B:drs([B:[]:E],
                           [B:Index:pred(E,Sym,v,Sense),
                            B:[]:role(E,Y,Role,1)]),
                     app(PP,E)),
               app(P,E)),
   tense(Mood,[],Att2-Att3,TDRS),
   Sem = lam(NP,lam(PP,app(TDRS,lam(P,app(NP,lam(Y,DRS)))))).


/* =========================================================================
   D i t r a n s i t i v e   V e r b s
========================================================================= */

/* -------------------------------------------------------------------------
   Adjectival  (np V np pp)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,Index,Att-Att,Sem):-
   category_type(Cat,Sym,npVnppp,[],_Mood), !, 
   att(Att,sense,Sense),
   Sem = lam(NP1,lam(PP,lam(NP2,lam(P,app(NP1,lam(Y,app(NP2,lam(X,merge(B1:drs([B1:[]:E],
                                                                               [B1:[]:prop(E,B2:drs([],
                                                                                                    [B2:Index:pred(X,Sym,a,Sense)])),
                                                                                B1:[]:rel(E,Y,rel,0)]),
                                                                        merge(app(PP,E),
                                                                              app(P,E))))))))))).

/* -------------------------------------------------------------------------
   Adjectival (np V pp np)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,Index,Att-Att,Sem):-
   category_type(Cat,Sym,npVppnp,[],_Mood), !, 
   att(Att,sense,Sense),
   Sem = lam(PP,lam(NP1,lam(NP2,lam(P,app(NP1,lam(Y,app(NP2,lam(X,merge(B1:drs([B1:[]:E],
                                                                               [B1:[]:prop(E,B2:drs([],
                                                                                                    [B2:Index:pred(X,Sym,a,Sense)])),
                                                                                B1:[]:rel(E,Y,rel,0)]),
                                                                        merge(app(PP,E),
                                                                              app(P,E))))))))))).

/* -------------------------------------------------------------------------
   Adjectival (np V pp pp)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,Index,Att-Att,Sem):-
   category_type(Cat,Sym,npVpppp,[],_Mood), !, 
   att(Att,sense,Sense),
   Sem = lam(PP1,lam(PP2,lam(NP,lam(P,app(NP,lam(X,merge(B1:drs([B1:[]:E],
                                                                [B1:[]:prop(E,B2:drs([],
                                                                                     [B2:Index:pred(X,Sym,a,Sense)]))]),
                                                         merge(app(PP1,E),
                                                               merge(app(PP2,E),
                                                                     app(P,E)))))))))).

/* -------------------------------------------------------------------------
   Ditransitive (np V np np)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,Index,Att1-Att3,Sem):-
   Cat = ((s:Mood\np)/np)/np, !,
   roles(Sym,Cat,[Role3,Role2,Role1],Att1-Att2),
   att(Att2,sense,Sense),
   DRS = merge(B:drs([B:[]:E],
                     [B:Index:pred(E,Sym,v,Sense),
                      B:[]:role(E,X,Role1,1),
                      B:[]:role(E,Y,Role2,1),
                      B:[]:role(E,Z,Role3,1)]),
               app(P,E)),
   tense(Mood,[],Att2-Att3,TDRS),
   Sem = lam(NP3,lam(NP2,lam(NP1,app(TDRS,
                                     lam(P,app(NP1,
                                               lam(X,app(NP2,
                                                         lam(Z,app(NP3,
                                                                   lam(Y,DRS))))))))))).

/* -------------------------------------------------------------------------
   Ditransitive (np V np pp)   
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,Index,Att1-Att3,Sem):-
%   category_type(Cat,Sym,npVnppp,[Role1,Role2],Mood), !, 
   Cat = ((s:Mood\np)/pp)/np,
   roles(Sym,Cat,[Role2,Role1],Att1-Att2), !,
   att(Att2,sense,Sense),
   DRS = merge(B:drs([B:[]:E],
                     [B:Index:pred(E,Sym,v,Sense),
                      B:[]:role(E,Y,Role1,1),
                      B:[]:role(E,X,Role2,1)]),
               merge(app(PP,E),
                     app(P,E))),
   tense(Mood,[],Att2-Att3,TDRS),
   Sem = lam(NP1,lam(PP,lam(NP2,app(TDRS,
                                    lam(P,app(NP2,
                                              lam(Y,app(NP1,
                                                        lam(X,DRS))))))))).

/* -------------------------------------------------------------------------
   Ditransitive (np V pp np)   
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,Index,Att1-Att3,Sem):-
%   category_type(Cat,Sym,npVppnp,[Role1,Role2],Mood), !, 
   Cat = ((s:Mood\np)/np)/pp,
   roles(Sym,Cat,[Role2,Role1],Att1-Att2), !,
   att(Att2,sense,Sense),
   DRS = merge(B:drs([B:[]:E],
                     [B:Index:pred(E,Sym,v,Sense),
                      B:[]:role(E,Y,Role1,1),
                      B:[]:role(E,X,Role2,1)]),
               merge(app(PP,E),
                     app(P,E))),
   tense(Mood,[],Att2-Att3,TDRS),
   Sem = lam(PP,lam(NP1,lam(NP2,app(TDRS,
                                    lam(P,app(NP2,
                                              lam(Y,app(NP1,
                                                        lam(X,DRS))))))))).

/* -------------------------------------------------------------------------
   Ditransitive (np V pp pp)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,Index,Att1-Att3,Sem):-
%   category_type(Cat,Sym,npVpppp,[Role],Mood), !,
   Cat = ((s:Mood\np)/pp)/pp,
   roles(Sym,Cat,[Role],Att1-Att2), !,
   att(Att2,sense,Sense),
   DRS = merge(B:drs([B:[]:E],
                     [B:Index:pred(E,Sym,v,Sense),
                      B:[]:role(E,Y,Role,1)]),
               merge(app(PP1,E),
                     merge(app(PP2,E),
                           app(P,E)))),
   tense(Mood,[],Att2-Att3,TDRS),
   Sem = lam(PP1,lam(PP2,lam(NP,app(TDRS,
                                    lam(P,app(NP,lam(Y,DRS))))))).

/* -------------------------------------------------------------------------
   Ditransitive (pp V np np)   
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,Index,Att1-Att2,Sem):-
   category_type(Cat,Sym,ppVnpnp,[Role1,Role2],Mood), !, 
   att(Att1,sense,Sense),
   DRS = merge(B:drs([B:[]:E],
                     [B:Index:pred(E,Sym,v,Sense),
                      B:[]:role(E,Y,Role1,1),
                      B:[]:role(E,X,Role2,1)]),
               merge(app(PP,E),
                     app(P,E))),
   tense(Mood,[],Att1-Att2,TDRS),
   Sem = lam(NP1,lam(NP2,lam(PP,app(TDRS,
                                    lam(P,app(NP2,
                                              lam(Y,app(NP1,
                                                        lam(X,DRS))))))))).

/* -------------------------------------------------------------------------
   Ditransitive (pp V np pp)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,Index,Att1-Att2,Sem):-
   category_type(Cat,Sym,ppVnppp,[Role],Mood), !, 
   att(Att1,sense,Sense),
   DRS = merge(B:drs([B:[]:E],
                     [B:Index:pred(E,Sym,v,Sense),
                      B:[]:role(E,Y,Role,1)]),
               merge(app(PP1,E),
                     merge(app(PP2,E),
                           app(P,E)))),
   tense(Mood,[],Att1-Att2,TDRS),
   Sem = lam(NP,lam(PP1,lam(PP2,app(TDRS,
                                    lam(P,app(NP,
                                              lam(Y,DRS))))))).


/* =========================================================================
   T r i t r a n s i t i v e   V e r b s
========================================================================= */

/* -------------------------------------------------------------------------
   Tritransitive (np V np pp pp)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,Index,Att1-Att3,Sem):-
%   category_type(Cat,Sym,npVnppppp,[Role1,Role2],Mood), !,
   Cat = (((s:Mood\np)/pp)/pp)/np,
   roles(Sym,Cat,[Role2,Role1],Att1-Att2), !,   
   att(Att2,sense,Sense),
   DRS = merge(B:drs([B:[]:E],
                     [B:Index:pred(E,Sym,v,Sense),
                      B:[]:role(E,Y,Role1,1),
                      B:[]:role(E,X,Role2,1)]),
               merge(merge(app(PP1,E),
                           app(PP2,E)),
                     app(P,E))),
   tense(Mood,[],Att2-Att3,TDRS),
   Sem = lam(NP1,lam(PP1,lam(PP2,lam(NP2,app(TDRS,
                                             lam(P,app(NP2,
                                                       lam(Y,app(NP1,
                                                                 lam(X,DRS)))))))))).

semlex_verb(Cat,Sym,Index,Att1-Att3,Sem):-
%   category_type(Cat,Sym,npVppnppp,[Role1,Role2],Mood), !,
   Cat = (((s:Mood\np)/pp)/np)/pp,
   roles(Sym,Cat,[Role2,Role1],Att1-Att2), !,   
   att(Att2,sense,Sense),
   DRS = merge(B:drs([B:[]:E],
                     [B:Index:pred(E,Sym,v,Sense),
                      B:[]:role(E,Y,Role1,1),
                      B:[]:role(E,X,Role2,1)]),
               merge(merge(app(PP1,E),
                           app(PP2,E)),
                     app(P,E))),
   tense(Mood,[],Att2-Att3,TDRS),
   Sem = lam(PP1,lam(NP1,lam(PP2,lam(NP2,app(TDRS,
                                             lam(P,app(NP2,
                                                       lam(Y,app(NP1,
                                                                 lam(X,DRS)))))))))).

    
/* =========================================================================
   P r o p o s i t i o n a l   c o m p l e m e n t   v e r b s
========================================================================= */

/* -------------------------------------------------------------------------
   Intransitive (np V s)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,Index,Att1-Att3,Sem):-
   Cat = (s:Mood\np)/s:Emb,
   member(Emb,[em,bem,qem,for]),
   roles(Sym,Cat,[Role2,Role1],Att1-Att2), !,   
   att(Att2,sense,Sense),
   DRS = merge(B:drs([B:[]:E],
                     [B:Index:pred(E,Sym,v,Sense),
                      B:[]:role(E,Y,Role1,1),
                      B:[]:role(E,K,Role2,1)]),
               app(F,E)),
   tense(Mood,[],Att2-Att3,TDRS),
   Sem = lam(S,lam(Q,app(TDRS,lam(F,app(Q,lam(Y,app(S,lam(K,DRS)))))))).

semlex_verb(Cat,Sym,Index,Att1-Att3,Sem):-
   Cat = (s:Mood\np)/s:_,
   roles(Sym,Cat,[Role2,Role1],Att1-Att2), !,   
   att(Att2,sense,Sense),
   closing(CC),
   DRS = merge(B:drs([B:[]:E,B:[]:A],
                     [B:Index:pred(E,Sym,v,Sense),
                      B:[]:role(E,Y,Role1,1),
                      B:[]:role(E,A,Role2,1),
                      B:[]:prop(A,app(S,CC))]),
               app(F,E)),
   tense(Mood,[],Att2-Att3,TDRS),
   Sem = lam(S,lam(Q,app(TDRS,lam(F,app(Q,lam(Y,DRS)))))).

/* -------------------------------------------------------------------------
   Intransitive (np V pp s)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,Index,Att1-Att2,Sem):-
   category_type(Cat,Sym,npVpps,[Role1,Role2],Mood), !,  
   att(Att1,sense,Sense),
   closing(CC),
   DRS = merge(B:drs([B:[]:E,B:[]:A],
                     [B:Index:pred(E,Sym,v,Sense),
                      B:[]:role(E,Y,Role1,1),
                      B:[]:role(E,A,Role2,1),
                      B:[]:prop(A,app(S,CC))]),
               merge(app(PP,E),app(F,E))),
   tense(Mood,[],Att1-Att2,TDRS),
   Sem = lam(PP,lam(S,lam(Q,app(TDRS,lam(F,app(Q,lam(Y,DRS))))))).

/* -------------------------------------------------------------------------
   Intransitive (s V np)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,Index,Att1-Att2,Sem):-
   att(Att1,pos,PoS),
   \+ PoS = 'IN',  %%% exclude prepositions!
   category_type(Cat,Sym,sVnp,[Role1,Role2],Mood), !,
   att(Att1,sense,Sense),
   closing(CC),
   DRS = merge(B:drs([B:[]:E,B:[]:A],
                     [B:Index:pred(E,Sym,v,Sense),
                      B:[]:role(E,Y,Role2,1),
                      B:[]:role(E,A,Role1,1),
                      B:[]:prop(A,app(S,CC))]),
               app(F,E)),
   tense(Mood,[],Att1-Att2,TDRS),
   Sem = lam(Q,lam(S,app(TDRS,lam(F,app(Q,lam(Y,DRS)))))).

/* -------------------------------------------------------------------------
    Copula (vp V np)
------------------------------------------------------------------------- */

semlex_verb(Cat,be,Index,Att1-Att2,Sem):-
   option('--copula',true),
   category_type(Cat,_,vpVnp,_,Mood), !,
   closing(CC),
   DRS = merge(B1:drs([B1:[]:E,B1:[]:A],
                      [B1:[]:prop(E,B2:drs([],[B2:Index:eq(Y,A)])),
                       B1:[]:prop(A,app(app(VP,lam(P,merge(B3:drs([B3:[]:X],[]),app(P,X)))),CC))]),
               app(F,E)),
   tense(Mood,[],Att1-Att2,TDRS),
   Sem = lam(Q,lam(VP,app(TDRS,lam(F,app(Q,lam(Y,DRS)))))).

/* -------------------------------------------------------------------------
   Intransitive (vp V np)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,Index,Att1-Att2,Sem):-
   category_type(Cat,Sym,vpVnp,[Role1,Role2],Mood), !,
   closing(CC),
   att(Att1,sense,Sense),
   DRS = merge(B1:drs([B1:[]:E,B1:[]:A],
                      [B1:Index:pred(E,Sym,v,Sense),
                       B1:[]:role(E,Y,Role2,1),
                       B1:[]:role(E,A,Role1,1),
                       B1:[]:prop(A,app(app(VP,lam(P,merge(B2:drs([B2:[]:X],[]),app(P,X)))),CC))]),
               app(F,E)),
   tense(Mood,[],Att1-Att2,TDRS),
   Sem = lam(Q,lam(VP,app(TDRS,lam(F,app(Q,lam(Y,DRS)))))).

/* -------------------------------------------------------------------------
   Transitive (s np V np)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,Index,Att1-Att2,Sem):-
   category_type(Cat,Sym,snpVnp,[Role1,Role2,Role3],Mood), !,
   closing(CC),
   att(Att1,sense,Sense),
   DRS = merge(B:drs([B:[]:E,B:[]:A],
                     [B:Index:pred(E,Sym,v,Sense),
                      B:[]:role(E,Y,Role2,1),
                      B:[]:role(E,Z,Role3,1),
                      B:[]:role(E,A,Role1,1),
                      B:[]:prop(A,app(S,CC))]),
               app(F,E)),
   tense(Mood,[],Att1-Att2,TDRS),
   Sem = lam(R,lam(Q,lam(S,app(TDRS,lam(F,app(R,lam(Z,app(Q,lam(Y,DRS))))))))).


/* -------------------------------------------------------------------------
   Transitive (np V np s)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,Index,Att1-Att2,Sem):-
   category_type(Cat,Sym,npVnpq,[Role1,Role2,Role3],Mood), !,
   closing(CC),
   att(Att1,sense,Sense),
   DRS = merge(B:drs([B:[]:E,B:[]:A],
                     [B:Index:pred(E,Sym,v,Sense),
                      B:[]:role(E,Y,Role2,1),
                      B:[]:role(E,X,Role1,1),
                      B:[]:role(E,A,Role3,1),
                      B:[]:prop(A,app(S,CC))]),
               app(F,E)),
   tense(Mood,[],Att1-Att2,TDRS),
   Sem = lam(NP2,lam(S,lam(NP1,app(TDRS,lam(F,app(NP1,lam(X,app(NP2,lam(Y,DRS))))))))).

semlex_verb(Cat,Sym,Index,Att1-Att2,Sem):-
   category_type(Cat,Sym,npVnps,[Role1,Role2,Role3],Mood), !,
   closing(CC),
   att(Att1,sense,Sense),
   DRS = merge(B:drs([B:[]:E,B:[]:A],
                     [B:Index:pred(E,Sym,v,Sense),
                      B:[]:role(E,Y,Role2,1),
                      B:[]:role(E,X,Role1,1),
                      B:[]:role(E,A,Role3,1),
                      B:[]:prop(A,app(S,CC))]),
               app(F,E)),
   tense(Mood,[],Att1-Att2,TDRS),
   Sem = lam(NP2,lam(S,lam(NP1,app(TDRS,lam(F,app(NP1,lam(X,app(NP2,lam(Y,DRS))))))))).

/* -------------------------------------------------------------------------
   Transitive (np V vp np)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,Index,Att1-Att2,Sem):-
   category_type(Cat,Sym,npVvpnp,[Role1,Role2],Mood), !,
   closing(CC),
   att(Att1,sense,Sense),
   DRS = merge(B:drs([B:[]:E,B:[]:Z],
                     [B:Index:pred(E,Sym,v,Sense),
                      B:[]:role(E,X,Role1,1),
                      B:[]:role(E,Z,Role2,1),
                      B:[]:prop(Z,app(app(VP,NP2),CC))]),
               app(F,E)),
   tense(Mood,[],Att1-Att2,TDRS),
   Sem = lam(VP,lam(NP2,lam(NP1,app(TDRS,lam(F,app(NP1,lam(X,DRS))))))).

/* -------------------------------------------------------------------------
   Transitive (np V vp pp)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,Index,Att1-Att2,Sem):-
   category_type(Cat,Sym,npVvppp,[Role],Mood), !,
   att(Att1,sense,Sense),
   tense(Mood,[],Att1-Att2,TDRS),
   Sem = lam(VP,lam(PP,lam(NP,app(TDRS,lam(F,app(NP,lam(X,merge(B:drs([B:[]:E],
                                                                      [B:Index:pred(E,Sym,v,Sense),
                                                                       B:[]:role(E,X,Role,1)]),
                                                                merge(app(PP,E),
                                                                      merge(app(app(VP,lam(P,app(P,E))),F),
                                                                            app(F,E))))))))))).

/* -------------------------------------------------------------------------
   Transitive (np V pp vp)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,Index,Att1-Att2,Sem):-
   category_type(Cat,Sym,npVppvp,[Role],Mood), !,
   att(Att1,sense,Sense),
   tense(Mood,[],Att1-Att2,TDRS),
   Sem = lam(PP,lam(AP,lam(NP,app(TDRS,lam(F,app(NP,lam(X,merge(B:drs([B:[]:E],
                                                                      [B:Index:pred(E,Sym,v,Sense),
                                                                       B:[]:role(E,X,Role,1)]),
                                                                merge(app(PP,E),
                                                                      merge(app(app(AP,lam(P,app(P,E))),F),
                                                                            app(F,E))))))))))).


/* =========================================================================
   A u x i l i a r y ,  M o d a l ,  C o n t r o l   V e r b s
========================================================================= */

/* =========================================================================
   Infinitivals
========================================================================= */

semlex_verb(Cat,Sym,Index,Att-Att,Sem):-
   option('--semantics',drgno),
   Cat = (s:to\np)/(s:b\np), !,
   CC = lam(F,merge(B2:drs([],[B2:Index:pred(F,Sym,s,1)]),app(E,F))),
%   Sem = lam(VP,lam(NP,lam(E,B1:drs([B1:[]:P],
%                                    [B1:[]:prop(P,app(app(VP,NP),CC))])))).
   Sem =  lam(V0,lam(V1,lam(E,merge(B3:drs([B3:Index:P4],
                                            [B3:[]:prop(P4,app(app(V0,V1),CC))]),app(E,P4))))).

semlex_verb(Cat,_Sym,Index,Att-Att,Sem):-
   Cat = (s:to\np)/(s:b\np), !,
   closing(CC),
%   Sem = lam(VP,lam(NP,lam(E,B:drs([B:Index:P],
%                                   [B:[]:prop(P,app(app(VP,NP),E))])))).


   Sem =  lam(V0,lam(V1,lam(V2,merge(B3:drs([B3:Index:P4],
                                            [B3:[]:prop(P4,app(app(V0,V1),CC))]),app(V2,P4))))).


/* -------------------------------------------------------------------------
   Standard case (NP aux VP)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,Index,Att1-Att2,Sem):-
   option('--modal',true), 
   modal_verb(pos,Sym),
   Cat = (s:Mood\np)/(s:Aspect\np), 
   \+ Mood = Aspect, !,
   aspect(Aspect,Mood,Index,Att1-Att2,TDRS),
   Sem = lam(VP,lam(NP,lam(E,B:drs([],[B:[]:pos(app(app(TDRS,app(VP,NP)),E))])))).

semlex_verb(Cat,Sym,Index,Att1-Att2,Sem):-
   option('--modal',true),
   modal_verb(nec,Sym),
   Cat = (s:Mood\np)/(s:Aspect\np), 
   \+ Mood = Aspect, !,
   aspect(Aspect,Mood,Index,Att1-Att2,TDRS),
   Sem = lam(VP,lam(NP,lam(E,B:drs([],[B:[]:nec(app(app(TDRS,app(VP,NP)),E))])))).

semlex_verb(Cat,Sym,Index,Att1-Att2,Sem):-
   aux_verb(Sym),
   Cat = (s:Mood\np)/(s:Aspect\np), \+ Aspect=to,
   \+ Mood = Aspect, !,
   aspect(Aspect,Mood,Index,Att1-Att2,TDRS),
   Sem = lam(VP,lam(NP,app(TDRS,app(VP,NP)))).


/* -------------------------------------------------------------------------
   Inversion case (aux NP VP)
------------------------------------------------------------------------- */

semlex_verb(Cat,_Sym,Index,Att1-Att2,Sem):-
   Cat = (s:Mood/(s:Aspect\np))/np, 
   \+ Mood = Aspect, \+ Mood = for, !,
   aspect(Aspect,Mood,Index,Att1-Att2,TDRS),
   Sem = lam(NP,lam(VP,app(TDRS,app(VP,NP)))).


/* -------------------------------------------------------------------------
   Control Verbs (intransitive)
------------------------------------------------------------------------- */

% e.g. "is about to swim"
semlex_verb(Cat,Sym,Index,Att1-Att2,Sem):- 
   Cat = (s:adj\np)/(s:to\np), 
   roles(Sym,Cat,[Role2,Role1],Att1-Att2), !,
   att(Att2,sense,Sense),
   DRS = merge(B:drs([B:[]:E],
                     [B:Index:pred(E,Sym,a,Sense),
                      B:[]:role(E,X,Role1,1),
                      B:[]:role(E,K,Role2,1)]),
               app(F,E)),
   Sem = lam(VP,lam(NP,lam(F,app(NP,lam(X,app(app(VP,lam(P,app(P,X))),lam(K,DRS))))))).

% e.g. "wants to swim" 
%
semlex_verb(Cat,Sym,Index,Att1-Att3,Sem):- 
   Cat = (s:Mood\np)/(s:to\np), \+ Mood = to,
   roles(Sym,Cat,[Role2,Role1],Att1-Att2), !,
   att(Att2,sense,Sense),
   DRS = merge(B:drs([B:[]:E],
                     [B:Index:pred(E,Sym,v,Sense),
                      B:[]:role(E,X,Role1,1),
                      B:[]:role(E,K,Role2,1)]),
               app(F,E)),
   tense(Mood,[],Att2-Att3,TDRS),
   Sem = lam(VP,lam(NP,app(TDRS,lam(F,app(NP,lam(X,app(app(VP,lam(P,app(P,X))),lam(K,DRS)))))))).

% e.g. "help finish his homework"
%
semlex_verb(Cat,Sym,Index,Att1-Att3,Sem):- 
   Cat = (s:b\np)/(s:b\np),   att(Att1,pos,'VB'),
   roles(Sym,(s:b\np)/(s:to\np),[Role2,Role1],Att1-Att2), !,
   closing(CC),
   att(Att2,sense,Sense),
   DRS = merge(B:drs([B:[]:E,
                      B:[]:K],
                     [B:Index:pred(E,Sym,v,Sense),
                      B:[]:role(E,X,Role1,1),
                      B:[]:role(E,K,Role2,1),
                      B:[]:prop(K,app(app(VP,lam(P,app(P,X))),CC))]),
               app(F,E)),
   tense(b,[],Att2-Att3,TDRS),
   Sem = lam(VP,lam(NP,app(TDRS,lam(F,app(NP,lam(X,DRS)))))).

% e.g. "helping walking home"
%
semlex_verb(Cat,Sym,Index,Att1-Att3,Sem):- 
   Cat = (s:ng\np)/(s:ng\np),   att(Att1,pos,'VBG'),
   roles(Sym,(s:ng\np)/(s:to\np),[Role2,Role1],Att1-Att2), !,
   closing(CC),
   att(Att2,sense,Sense),
   DRS = merge(B:drs([B:[]:E,
                      B:[]:K],
                     [B:Index:pred(E,Sym,v,Sense),
                      B:[]:role(E,X,Role1,1),
                      B:[]:role(E,K,Role2,1),
                      B:[]:prop(K,app(app(VP,lam(P,app(P,X))),CC))]),
               app(F,E)),
   tense(ng,[],Att2-Att3,TDRS),
   Sem = lam(VP,lam(NP,app(TDRS,lam(F,app(NP,lam(X,DRS)))))).

% e.g. "were left stranded"
%
semlex_verb(Cat,Sym,Index,Att1-Att3,Sem):- 
   Cat = (s:pss\np)/(s:pss\np),   att(Att1,pos,'VBN'),
   roles(Sym,(s:pss\np)/(s:to\np),[Role2,Role1],Att1-Att2), !,
   closing(CC),
   att(Att2,sense,Sense),
   DRS = merge(B:drs([B:[]:E,
                      B:[]:K],
                     [B:Index:pred(E,Sym,v,Sense),
                      B:[]:role(E,X,Role1,1),
                      B:[]:role(E,K,Role2,1),
                      B:[]:prop(K,app(app(VP,lam(P,app(P,X))),CC))]),
               app(F,E)),
   tense(ng,[],Att2-Att3,TDRS),
   Sem = lam(VP,lam(NP,app(TDRS,lam(F,app(NP,lam(X,DRS)))))).

semlex_verb(Cat,Sym,Index,Att1-Att3,Sem):- 
   Cat = (s:Mood\np)/(s:Arg\np), \+ Mood = Arg,
   roles(Sym,Cat,[Role2,Role1],Att1-Att2), !,
   closing(CC),
   att(Att2,sense,Sense),
   DRS = merge(B:drs([B:[]:E,
                      B:[]:K],
                     [B:Index:pred(E,Sym,v,Sense),
                      B:[]:role(E,X,Role1,1),
                      B:[]:role(E,K,Role2,1),
                      B:[]:prop(K,app(app(VP,lam(P,app(P,X))),CC))]),
               app(F,E)),
   tense(Mood,[],Att2-Att3,TDRS),
   Sem = lam(VP,lam(NP,app(TDRS,lam(F,app(NP,lam(X,DRS)))))).


/* -------------------------------------------------------------------------
   Subject Control Verbs
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,Index,Att1-Att3,Sem):-
   member(Sym,[promise,offer,use]),
   Cat = ((s:Mood\np)/(s:to\np))/np, \+ Mood = to,
   roles(Sym,((s:Mood\np)/np)/np,[Role1,Role2,Role3],Att1-Att2), !,
   att(Att2,sense,Sense),
   DRS = app(app(V,lam(P,app(P,X))),lam(Z,merge(B1:drs([B1:[]:E],
                                                       [B1:Index:pred(E,Sym,v,Sense),
                                                        B1:[]:role(E,X,Role3,1),
                                                        B1:[]:role(E,Y,Role2,1),
                                                        B1:[]:role(E,Z,Role1,1)]),app(F,E)))),
   tense(Mood,[],Att2-Att3,TDRS),
   Sem = lam(Q2,lam(V,lam(Q1,app(TDRS,lam(F,app(Q1,lam(X,app(Q2,lam(Y,DRS))))))))).

semlex_verb(Cat,Sym,Index,Att1-Att2,Sem):-
   member(Sym,[promise,offer,use]),
   category(socv,Cat,[Role1,Role2],Mood), !,
   closing(CC),
   att(Att1,sense,Sense),
   Modal = B1:drs([B1:[]:A],
                  [B1:[]:role(E,A,theme,1),
                   B1:[]:prop(A,app(app(V,lam(R,app(R,X))),CC))]),
   DRS = merge(B2:drs([B2:[]:E],
                      [B2:Index:pred(E,Sym,v,Sense),
                       B2:[]:role(E,Y,Role2,1),
                       B2:[]:role(E,X,Role1,1)]),
               merge(Modal,app(F,E))),
   tense(Mood,[],Att1-Att2,TDRS),
   Sem = lam(Q2,lam(V,lam(Q1,app(TDRS,lam(F,app(Q1,lam(X,app(Q2,lam(Y,DRS))))))))).


/* -------------------------------------------------------------------------
   Object Control Verbs (ECM, exceptional case marking)
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,Index,Att1-Att3,Sem):-
   Cat = ((s:Mood\np)/(s:to\np))/np, \+ Mood = to,
   roles(Sym,((s:Mood\np)/np)/np,[Role1,Role2,Role3],Att1-Att2), !,
   att(Att2,sense,Sense),
   DRS = app(app(V,lam(P,app(P,Y))),lam(Z,merge(B1:drs([B1:[]:E],
                                                       [B1:Index:pred(E,Sym,v,Sense),
                                                        B1:[]:role(E,X,Role3,1),
                                                        B1:[]:role(E,Y,Role2,1),
                                                        B1:[]:role(E,Z,Role1,1)]),app(F,E)))),
   tense(Mood,[],Att2-Att3,TDRS),
   Sem = lam(Q2,lam(V,lam(Q1,app(TDRS,lam(F,app(Q1,lam(X,app(Q2,lam(Y,DRS))))))))).

%semlex_verb(Cat,Sym,Index,Att1-Att3,Sem):-
%   Cat = ((s:Mood\np)/(s:to\np))/np, \+ Mood = to,
%   roles(Sym,(s:Mood\np)/np,[Role1,Role2],Att1-Att2), !,
%   att(Att2,sense,Sense),
%   DRS = app(app(V,lam(P,app(P,Y))),lam(Z,merge(B1:drs([B1:[]:E],
%                                                       [B1:Index:pred(E,Sym,v,Sense),
%                                                        B1:[]:role(E,X,Role2,1),
%                                                        B1:[]:role(E,Z,Role1,1)]),app(F,E)))),
%   tense(Mood,[],Att2-Att3,TDRS),
%   Sem = lam(Q2,lam(V,lam(Q1,app(TDRS,lam(F,app(Q1,lam(X,app(Q2,lam(Y,DRS))))))))).

semlex_verb(Cat,Sym,Index,Att1-Att2,Sem):-
   category(socv,Cat,[Role1,Role2],Mood), !,   %%% not to-complement (see above)
   closing(CC),
   att(Att1,sense,Sense),
   Modal = B1:drs([B1:[]:A],
                  [B1:[]:role(E,A,theme,1),
                   B1:[]:prop(A,app(app(V,lam(R,app(R,Y))),CC))]),
   DRS = merge(B1:drs([B1:[]:E],
                      [B1:Index:pred(E,Sym,v,Sense),
                       B1:[]:role(E,Y,Role2,1),
                       B1:[]:role(E,X,Role1,1)]),
               merge(Modal,app(F,E))),
   tense(Mood,[],Att1-Att2,TDRS),
   Sem = lam(Q2,lam(V,lam(Q1,app(TDRS,lam(F,app(Q1,lam(X,app(Q2,lam(Y,DRS))))))))).


/* -------------------------------------------------------------------------
   Object Control Verbs subcatting PP
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,Index,Att1-Att3,Sem):-
   Cat = ((s:Mood\np)/(s:to\np))/pp, \+ Mood = to,
   roles(Sym,((s:Mood\np)/np)/pp,[Role1,Role2],Att1-Att2), !,
   att(Att2,sense,Sense),
   DRS = app(app(V,lam(P,app(P,X))),lam(Z,merge(B1:drs([B1:[]:E],
                                                       [B1:Index:pred(E,Sym,v,Sense),
                                                        B1:[]:role(E,X,Role2,1),
                                                        B1:[]:role(E,Z,Role1,1)]),merge(app(PP,E),app(F,E))))),
   tense(Mood,[],Att2-Att3,TDRS),
   Sem = lam(PP,lam(V,lam(Q,app(TDRS,lam(F,app(Q,lam(X,DRS))))))).
                    

/* -------------------------------------------------------------------------
   make NP ADJ to VP
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,Index,Att1-Att2,Sem):-
   Cat = (((s:Mood\np)/(s:to\np))/(s:adj\np))/np, !,
   closing(CC),
   att(Att1,sense,Sense),
   Modal = merge(B1:drs([B1:[]:A],
                        [B1:[]:role(E,A,theme,1),
                         B1:[]:prop(A,app(app(V,lam(R,app(R,Y))),CC))]),
                 app(app(AP,lam(P,app(P,A))),CC)),
   DRS = merge(B2:drs([B2:[]:E],
                      [B2:Index:pred(E,Sym,v,Sense),
                       B2:[]:role(E,Y,patient,1),
                       B2:[]:role(E,X,agent,1)]),
               merge(Modal,app(F,E))),
   tense(Mood,[],Att1-Att2,TDRS),
   Sem = lam(Q2,lam(AP,lam(V,lam(Q1,app(TDRS,lam(F,app(Q1,lam(X,app(Q2,lam(Y,DRS)))))))))).

semlex_verb(Cat,Sym,Index,Att1-Att2,Sem):-
   Cat = (((s:Mood\np)/s:_)/(s:adj\np))/np, !,
   closing(CC),
   att(Att1,sense,Sense),
   Modal = merge(B1:drs([B1:[]:A],
                        [B1:[]:role(E,A,theme,1),
                         B1:[]:prop(A,app(S,CC))]),
                 app(app(AP,lam(P,app(P,A))),CC)),
   DRS = merge(B1:drs([B1:[]:E],
                      [B1:Index:pred(E,Sym,v,Sense),
                       B1:[]:role(E,Y,patient,1),
                       B1:[]:role(E,X,agent,1)]),
               merge(Modal,app(F,E))),
   tense(Mood,[],Att1-Att2,TDRS),
   Sem = lam(Q2,lam(AP,lam(S,lam(Q1,app(TDRS,lam(F,app(Q1,lam(X,app(Q2,lam(Y,DRS)))))))))).



/* -------------------------------------------------------------------------
   Funny transitive case (rare -- often in incorrect parses)
------------------------------------------------------------------------- */

semlex_verb(Cat,_Sym,Index,Att1-Att2,Sem):-
   Cat = ((s:Mood\np)\np)/(s:Aspect\np), !,
   aspect(Aspect,Mood,Index,Att1-Att2,TDRS),
   Sem = lam(VP,lam(_,lam(NP,app(TDRS,app(VP,NP))))).

/* -------------------------------------------------------------------------
   Copula "How ADJ be NP PP"
------------------------------------------------------------------------- */

semlex_verb(Cat,_Sym,Index,Att1-Att2,Sem):-
   Cat = ((s:q/pp)/(s:Aspect\np))/np, !,
   aspect(Aspect,q,Index,Att1-Att2,TDRS),
   Sem = lam(NP,lam(VP,lam(PP,lam(F,app(app(TDRS,app(VP,NP)),lam(E,merge(app(PP,E),app(F,E)))))))).



/* =========================================================================
   E x p l e t i v e   v e r b s
========================================================================= */

/* -------------------------------------------------------------------------
   Cleft  "it was NP who VP"
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,Index,Att1-Att3,Sem):- 
   member(Cat,[((s:Mood\np_exp)/(np\np))/np,
               ((s:Mood\np)/(np\np))/np]),
   roles(Sym,(s:Mood\np)/np,[Role2,Role1],Att1-Att2), !,
   att(Att2,sense,Sense),
   DRS = merge(B:drs([B:[]:E],
                     [B:Index:pred(E,Sym,v,Sense),
                      B:[]:role(E,X,Role1,1),
                      B:[]:role(E,Y,Role2,1)]),
               app(P,E)),
   tense(Mood,[],Att2-Att3,TDRS),
   Sem = lam(NP2,lam(RC,lam(NP1,app(TDRS,
                             lam(P,app(NP1,
                                       lam(X,app(app(RC,NP2),
                                                 lam(Y,DRS))))))))).


%   Sem = lam(NP,lam(M,lam(_,app(TDRS,lam(F,alfa(fac,merge(B1:drs([B1:[]:X],[]),
%                                                          app(app(M,lam(P,app(P,X))),lam(U,merge(B2:drs([B2:[]:E],[B2:[]:role(E,U,theme,1)]),
%                                                                                                 app(F,E))))),
%                                                    app(NP,lam(U,B3:drs([],[B3:Index:eq(U,X)]))))))))).

/* -------------------------------------------------------------------------
   Copula  "it is ADJ to-VP"
------------------------------------------------------------------------- */

semlex_verb(Cat,_Sym,Index,Att1-Att2,Sem):-
   Cat = ((s:Mood\np)/(s:to\np))/(s:adj\np), !,
   tense(Mood,Index,Att1-Att2,TDRS),
   Sem = lam(AP,lam(VP,lam(NP,app(TDRS,lam(E,DRS))))),
   DRS = app(app(AP,NP), 
             lam(X,app(app(VP,lam(P,merge(B1:drs([B1:[]:X],[]),app(P,X)))),
                       lam(F,merge(B3:drs([],[B3:[]:role(X,F,theme,1)]),app(E,X)))))).


/* -------------------------------------------------------------------------
   Copula  "it is so ADJ that S"
           "it is ADJ that S"
           "it is ADJ whether S"
           "it is ADJ for S"
------------------------------------------------------------------------- */

semlex_verb(Cat,Sym,Index,Att-Att,Sem):-
   Cat = ((s:adj\np)/s:em)/(s:adj\np), !,
   att(Att,sense,Sense),
   Sem = lam(AP,lam(S,lam(Q,lam(E,app(app(AP,Q),lam(Y,merge(app(S,lam(X,B2:drs([],[B2:Index:pred(Y,Sym,a,Sense),
                                                                                   B2:[]:role(Y,X,theme,1)]))),
                                                            app(E,Y)))))))).

semlex_verb(Cat,_,Index,Att1-Att2,Sem):-
   member(Cat,[((s:Mood\np_exp)/s:em)/(s:adj\np),
               ((s:Mood\np)/s:em)/(s:adj\np)]), !,
   tense(Mood,Index,Att1-Att2,TDRS),
   Sem = lam(AP,lam(S,lam(Q,app(TDRS,lam(E,app(app(AP,Q),lam(Y,merge(app(S,lam(X,B2:drs([],[B2:[]:role(Y,X,theme,1)]))),
                                                                     app(E,Y))))))))).

semlex_verb(Cat,_,Index,Att1-Att2,Sem):-
   member(Cat,[((s:Mood\np_exp)/s:qem)/(s:adj\np),
               ((s:Mood\np)/s:qem)/(s:adj\np)]), !,
   tense(Mood,Index,Att1-Att2,TDRS),
   Sem = lam(AP,lam(S,lam(Q,app(TDRS,lam(E,app(app(AP,Q),lam(Y,merge(app(S,lam(X,B2:drs([],[B2:[]:role(Y,X,theme,1)]))),
                                                                     app(E,Y))))))))).

semlex_verb(Cat,_,Index,Att1-Att2,Sem):-
   member(Cat,[((s:Mood\np_exp)/s:for)/(s:adj\np), 
               ((s:Mood\np)/s:for)/(s:adj\np)]), !,
   tense(Mood,Index,Att1-Att2,TDRS),
   Sem = lam(AP,lam(S,lam(Q,app(TDRS,lam(E,app(app(AP,Q),lam(Y,merge(app(S,lam(X,B2:drs([],[B2:[]:role(Y,X,theme,1)]))),
                                                                     app(E,Y))))))))).


/* -------------------------------------------------------------------------
   Copula  "it is PP that S"

semlex_verb(Cat,_,_Index,Att-Att,Sem):-
   Cat = ((s:dcl\np_exp)/s:em)/pp, !,
   Sem = lam(PP,lam(S,lam(_,lam(F,app(S,lam(E,merge(app(PP,E),app(F,E)))))))). 
------------------------------------------------------------------------- */


/* -------------------------------------------------------------------------
   Copula  "it is NP that S"

semlex_verb(Cat,_,Index,Att1-Att2,Sem):-
   Cat = ((s:Mood\np_exp)/s:em)/np, !,
   tense(Mood,[],Att1-Att2,TDRS),
   closing(CC),
   Sem = lam(NP,lam(S,lam(_,app(TDRS,lam(E,merge(B1:drs([B1:Index:F,B1:[]:K],
                                            [B1:[]:role(F,K,theme,1),
                                             B1:[]:prop(K,app(S,CC))]),
                                        merge(app(NP,lam(X,B2:drs([],[B2:[]:eq(X,K)]))),
                                              app(E,F)))))))).
------------------------------------------------------------------------- */


/* -------------------------------------------------------------------------
   Control "it takes/makes/is NP to VP" 

semlex_verb(Cat,_,Index,Att1-Att2,Sem):-
   Cat = ((s:Mood\np_exp)/(s:to\np))/np, !,
   closing(CC),
   tense(Mood,[],Att1-Att2,TDRS),
   Sem = lam(NP,lam(VP,lam(_,app(TDRS,lam(F,merge(B1:drs([B1:[]:E],
                                             [B1:[]:imp(merge(B2:drs([B2:[]:X],[]),
                                                              app(app(VP,lam(P,app(P,X))),CC)),
                                                        app(NP,lam(Y,B3:drs([],[B3:Index:eq(Y,X)]))))]),
                                         app(F,E))))))).
------------------------------------------------------------------------- */


/* -------------------------------------------------------------------------
   Modal "it may/could/would VP"

semlex_verb(Cat,Sym,Index,Att1-Att2,Sem):-
   option('--modal',true),
   modal_verb(pos,Sym),
   Cat = (s:Mood\np_exp)/(s:Aspect\np), !,
   aspect(Aspect,Mood,[],Att1-Att2,TDRS),
   Sem = lam(VP,lam(NP,lam(E,B:drs([],[B:Index:pos(app(app(TDRS,app(VP,NP)),E))])))).

semlex_verb(Cat,Sym,Index,Att1-Att2,Sem):-
   option('--modal',true),
   modal_verb(nec,Sym),
   Cat = (s:Mood\np_exp)/(s:Aspect\np), !,
   aspect(Aspect,Mood,[],Att1-Att2,TDRS),
   Sem = lam(VP,lam(NP,lam(E,B:drs([],[B:Index:nec(app(app(TDRS,app(VP,NP)),E))])))).

semlex_verb(Cat,_Sym,Index,Att1-Att2,Sem):-
   option('--modal',false),
   Cat = (s:Mood\np_exp)/(s:Aspect\np), !,
   aspect(Aspect,Mood,Index,Att1-Att2,TDRS),
   Sem = lam(VP,lam(NP,lam(E,app(app(TDRS,app(VP,NP)),E)))).
------------------------------------------------------------------------- */


/* -------------------------------------------------------------------------
   Comparative "V it COMP (for) S"

semlex_verb(Cat,Sym,Index,Att-Att,Sem):-
   Cat = (((s:_\np)/s:_)/(s:adj\np))/np_exp, !,
   closing(CC),
   att(Att,sense,Sense),
   Sem = lam(_,lam(AP,lam(S,lam(NP,lam(F,
         app(NP,lam(X,merge(B1:drs([B1:[]:K,B1:[]:E],
                                   [B1:Index:pred(E,Sym,v,Sense),
                                    B1:[]:role(E,X,agent,1),
                                    B1;[]:role(E,K,theme,1),
                                    B1:[]:prop(K,app(S,CC))]),
                            merge(app(app(AP,lam(P,app(P,K))),lam(G,B2:drs([],[B2:[]:role(E,G,result,1)]))),
                                  app(F,E)))))))))).
------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------
   Comparative "make/find it COMP to VP"

semlex_verb(Cat,Sym,Index,Att-Att,Sem):-
   Cat = (((s:_\np)/(s:to\np))/(s:adj\np))/np_exp, !,
   closing(CC),
   att(Att,sense,Sense),
   DRS = app(app(VP,lam(P,merge(B1:drs([B1:[]:X],[]),app(P,X)))),CC),
   Sem = lam(_,lam(AP,lam(VP,lam(NP,lam(F,
         app(NP,lam(X,merge(B2:drs([B2:[]:K,B2:[]:E],
                                   [B2:Index:pred(E,Sym,v,Sense),
                                    B2:[]:role(E,X,agent,1),
                                    B2:[]:role(E,K,theme,1),
                                    B2:[]:prop(K,DRS)]),
                            merge(app(app(AP,lam(P,app(P,K))),lam(G,B3:drs([],[B3:[]:role(E,G,result,1)]))),
                                  app(F,E)))))))))).
------------------------------------------------------------------------- */


/* =========================================================================
   Closing
========================================================================= */

closing(lam(_,_:drs([],[]))).


/* =========================================================================
   Modal Verbs
========================================================================= */

aux_modal_verb(V):- aux_verb(V).
aux_modal_verb(V):- modal_verb(_,V).

aux_verb(be).
aux_verb(do).
aux_verb(have).
aux_verb(to).

modal_verb(pos, ca).     %%% can't
modal_verb(pos, can).
modal_verb(pos, may).
modal_verb(pos, might).
modal_verb(pos, could).

modal_verb(nec, sha).    %%% shan't
modal_verb(nec, shall).
modal_verb(nec, will).
modal_verb(nec, must).
modal_verb(nec, should).
modal_verb(nec, would).

