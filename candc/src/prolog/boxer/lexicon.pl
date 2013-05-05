
:- module(lexicon,[semlex/5]).

/* -------------------------------------------------------------------------
   Notational conventions used in lambda expressions
     
        X, Y, Z       individuals
        G             groups
        P             properties (also event-types)
        NP            noun phrases
        N             nouns
        PP            prepositional phrases
        VP            verb phrases
        D             determiner 

   Senses used for standard predicates

       a 1           topic (not n, causes inconsistencies)
       n 12          thing
       n 1           person 
       n 1           event
       n 1           group
       n 2           reason
       n 2           manner
       n 1           proposition
       n 1           location


------------------------------------------------------------------------- */

:- use_module(boxer(slashes)).
:- use_module(boxer(string2digit),[string2digit/2]).
:- use_module(boxer(categories),[category/3]).
:- use_module(semlib(options),[option/2]).
%:- use_module(knowledge(nationality),[nationality/2]).
:- use_module(knowledge(ne),[netype/2]).
:- use_module(knowledge(title),[title/1,title/2]).
:- use_module(knowledge(dates),[month/2,dofm/2,decade/2,year/2]).
:- use_module(lex(determiners),[semlex_det/3]).
:- use_module(lex(verbs),[semlex_verb/5]).
:- use_module(library(lists),[member/2]).


/* =========================================================================
   Punctuation
========================================================================= */

semlex(t:ynq\s:dcl,'?',_,_,Sem):- !,
   Sem = lam(S,app(S,lam(_,drs([],[])))).

semlex(t:M\s:M,_,_,_,Sem):- !,
   Sem = lam(S,app(S,lam(_,drs([],[])))).

semlex(C\C,',',_,_,Sem):- !,
   Sem = lam(X,X).


/* =========================================================================
   Coordination (hack to deal with bug in C&C output)
========================================================================= */

semlex(comma,_,_,_,Sem):- !,
   Sem = lam(K2,lam(K1,merge(K1,K2))).

semlex(semi,_,_,_,Sem):- !,
   Sem = lam(K2,lam(K1,merge(K1,K2))).


/* =========================================================================
   Coordination
========================================================================= */

semlex(conj,Lemma,_,Index,Sem):-
   member(Lemma,[either,or]), !,
   Sem = lam(K2,lam(K1,drs([],[Index:or(K1,K2)]))).

semlex(conj,Lemma,_,Index,Sem):- 
   member(Lemma,[neither,nor,not]), !,
   Sem = lam(K2,lam(K1,merge(K1,drs([],[Index:not(K2)])))).

semlex(conj:s,_,_,_,Sem):- !,
   Sem = lam(K2,lam(K1,merge(K1,K2))).

semlex(conj,_,_,_,Sem):- !,
   Sem = lam(K2,lam(K1,merge(K1,K2))).

semlex(Punct,Punct,Punct:_:_,_,Sem):- 
   member(Punct,[',', ';', ':']), !,
   Sem = lam(K2,lam(K1,merge(K1,K2))).

semlex(conj:app,_,_,Index,Sem):- !,
   Sem = lam(X1,lam(X2,lam(P,app(X2,
                                 lam(Y,merge(app(X1,
                                                 lam(X,drs([],
%                                                           [Index:eq(Y,X)]))),
                                                           [Index:rel(Y,X,rel,0)]))),
                                             app(P,Y))))))).


/* =========================================================================
   Compound Coordination
========================================================================= */

semlex(conj/conj,instead,_,Index,Sem):- !,  % instead of
   Sem = lam(C,lam(K1,lam(K2,app(app(C,drs([],[Index:not(K1)])),K2)))).

semlex(conj/conj,rather,_,Index,Sem):- !,   % rather than
   Sem = lam(C,lam(K2,lam(K1,app(app(C,K2),drs([],[Index:not(K1)]))))).

semlex(conj/conj,_,_,_,lam(U,U)):- !.
semlex(conj\conj,_,_,_,lam(U,U)):- !.


/* =========================================================================
   Quotes
========================================================================= */

semlex(q, _,_,Index,Sem):- !,
   Sem = lam(X,drs([],[Index:pred(X,quotation,n,2)])).

semlex((n/q)/n, _,_,Index,Sem):- !,
   Sem = lam(N,lam(Q,lam(X,merge(drs([],[Index:pred(X,quotation,n,2)]),
                                 merge(app(N,X),app(Q,X)))))).

semlex((np/q)/np, _,_,Index,Sem):- !,
   Sem = lam(NP,lam(Q,lam(P,app(NP,lam(X,merge(drs([],[Index:pred(X,quotation,n,2)]),
                                               merge(app(Q,X),app(P,X)))))))).

semlex((s:dcl/q)/s:dcl, _,_,Index,Sem):- !,
   Sem = lam(S,lam(Q,lam(F,app(S,lam(E,merge(drs([],[Index:pred(E,quotation,n,2)]),
                                             merge(app(Q,E),app(F,E)))))))).


/* =========================================================================
   Noun Phrases
========================================================================= */

/* -------------------------------------------------------------------------
   Expletive 'there' and other "special" nouns
------------------------------------------------------------------------- */

semlex(n:_, many,  'NN':_:_, Index,lam(X,drs([],[Index:pred(X,quantity,n,1)]))):- !.

semlex(n:_, much,  'NN':_:_ ,Index,lam(X,drs([],[Index:pred(X,amount,n,3)]))):- !.

semlex(n:_, '%',   'NN':_:_, Index,lam(X,drs([],[Index:pred(X,percent,n,1)]))):- !.

semlex(n:_, there, 'EX':_:_ ,Index,lam(X,drs([Index:Y],
                                          [Index:pred(Y,location,n,1),
                                           Index:rel(Y,X,rel,0)]))):- !.


/* -------------------------------------------------------------------------
   Nouns
------------------------------------------------------------------------- */

semlex(n:_,Sym,'CD':'I-DAT':_,Index,Sem):- 
   dofm(Sym,DID), !,
   Sem = lam(X,drs([],[Index:timex(X,date([]:'+',[]:'XXXX',[]:'XX',Index:DID))])).

semlex(n:_,YID,'CD':'I-DAT':_,Index,Sem):- 
   year(YID,Year), !,
   Sem = lam(X,drs([],[Index:timex(X,date([]:'+',Index:Year,[]:'XX',[]:'XX'))])).

semlex(n:_,Sym,'NNP':'I-DAT':_,Index,Sem):- 
   month(Sym,MID), !,
   Sem = lam(X,drs([],[Index:timex(X,date([]:'+',[]:'XXXX',Index:MID,[]:'XX'))])).

semlex(n:_,Sym,'NNS':_:_,Index,Sem):- 
   decade(Sym,DID), !,
   Sem = lam(X,drs([],[Index:timex(X,date([]:'+',Index:DID,[]:'XX',[]:'XX'))])).

semlex(n:num,Sym,_,Index,Sem):- 
   string2digit(Sym,Digit), !,
   Sem = lam(X,drs([],[Index:card(X,Digit,eq)])).

semlex(n:_,Sym,'CD':_:_,Index,Sem):- 
   string2digit(Sym,Digit), !,
   Sem = lam(X,drs([],[Index:card(X,Digit,eq)])).

semlex(n:_,Sym,'NNP':NE:Sense,Index,Sem):- !,
   netype(NE,Type),
   Sem = lam(X,drs([],[Index:named(X,Sym,Type,Sense)])).

semlex(n:_,Sym,'NNPS':NE:Sense,Index,Sem):- !,
   netype(NE,Type),
   Sem = lam(X,drs([],[Index:named(X,Sym,Type,Sense)])).

semlex(n:_, most,'JJS':_:Sense,Index,Sem):- !,
   Sem = lam(X,drs([],[Index:pred(X,most,n,Sense)])).

semlex(n:_,Sym,'JJS':_:_,Index,Sem):- !,
   Sem = lam(X,drs([],[Index:imp(drs([[]:Y],[[]:not(drs([],[[]:eq(Y,X)]))]),drs([],[Index:rel(X,Y,Sym,0)]))])).

semlex(n:_, Sym,_:_:Sense,Index,Sem):- !,
   Sem = lam(X,drs([],[Index:pred(X,Sym,n,Sense)])).


/* -------------------------------------------------------------------------
   Relational nouns
------------------------------------------------------------------------- */

semlex(n:_/pp,Sym,Pos:Ne:Sense,Index,Sem):-
   member(Pos,['NNP','NNPS']), !,
   netype(Ne,Type),   
   Sem = lam(P,lam(X,merge(drs([],[Index:named(X,Sym,Type,Sense)]),
                           app(P,X)))).


semlex(n:_/pp,Sym,_:_:Sense,Index,Sem):- !,
   Sem = lam(P,lam(X,merge(drs([],[Index:pred(X,Sym,n,Sense)]),
                           app(P,X)))).

semlex(n:_/pp,Sym,_:_:Sense,Index,Sem):- !,
   Sem = lam(P,lam(X,merge(drs([Index:Y],
                               [Index:pred(Y,Sym,n,Sense),
                                Index:rel(X,Y,actor,1),
                                Index:pred(X,person,n,1)]),
                           app(P,Y)))).

semlex((n:_/pp)/pp,Sym,_:_:Sense,Index,Sem):- !,
   Sem = lam(P1,lam(P2,lam(X,merge(drs([],[Index:pred(X,Sym,n,Sense)]),
                                   merge(app(P1,X),app(P2,X)))))).

semlex(((n:_/pp)/pp)/pp,Sym,_:_:Sense,Index,Sem):- !,
   Sem = lam(P1,lam(P2,lam(P3,lam(X,merge(drs([],[Index:pred(X,Sym,n,Sense)]),
                                          merge(app(P1,X),merge(app(P2,X),app(P3,X)))))))).

semlex(n:_/(s:_\np),Sym,_:_:Sense,Index,Sem):-
   Sem = lam(VP,lam(X,merge(drs([],[Index:pred(X,Sym,n,Sense)]),
                            app(app(VP,lam(P,app(P,X))),lam(_,drs([],[])))))).

semlex((n:_/(s:_\np))/pp,Sym,_:_:Sense,Index,Sem):-
   Sem = lam(P,lam(VP,lam(X,merge(drs([],[Index:pred(X,Sym,n,Sense)]),
                                  merge(app(app(VP,lam(P,app(P,X))),lam(_,drs([],[]))),
                                        app(P,X)))))).

semlex((n:_/pp)/(s:_\np),Sym,_:_:Sense,Index,Sem):-
   Sem = lam(VP,lam(P,lam(X,merge(drs([],[Index:pred(X,Sym,n,Sense)]),
                                  merge(app(app(VP,lam(P,app(P,X))),lam(_,drs([],[]))),
                                        app(P,X)))))).


/* -------------------------------------------------------------------------
   Determiners
------------------------------------------------------------------------- */

semlex( np/n:_, Token,_,Index,Sem):- !,
   semlex_det(Token,Index,Sem).


/* -------------------------------------------------------------------------
   Possessives
------------------------------------------------------------------------- */

semlex(np/(n:_/pp),_,_,Index,Sem):- !,
   Sem = lam(RN,lam(P,alfa(pro,drs([Index:Y],[Index:pred(Y,male,a,0)]),
                               alfa(def,merge(drs([Index:X],[]),
                                              app(app(RN,lam(U,drs([],[Index:rel(U,Y,of,0)]))),X)),
                                        app(P,X))))).


/* -------------------------------------------------------------------------
   Determiners (as many as X)
------------------------------------------------------------------------- */

semlex( ((np/n:_)/pp)/(s:adj\np),_,_,Index,Sem):- !,
   Sem = lam(_,lam(PP,lam(N,lam(P,merge(merge(drs([Index:X],[]),
                                              merge(app(N,X),app(PP,X))),
                                        app(P,X)))))).


/* -------------------------------------------------------------------------
   Many/Much [as NP]
------------------------------------------------------------------------- */

semlex( np, many,_,Index,Sem):- !,
   Sem = lam(P,merge(drs([Index:X],[Index:pred(X,quantity,n,1)]),app(P,X))).

semlex( np, much,_,Index,Sem):- !,
   Sem = lam(P,merge(drs([Index:X],[Index:pred(X,amount,n,3)]),app(P,X))).


/* -------------------------------------------------------------------------
   Pronouns (non-reflexives)
------------------------------------------------------------------------- */

semlex( np, Lemma,_,Index,Sem):-
   member(Lemma,['I',i,me,mine]), !,
   Sem = lam(P,alfa(pro,drs([Index:X],[Index:pred(X,person,n,1)]),app(P,X))).

semlex( np, Lemma,_,Index,Sem):-
   member(Lemma,['we','us','\'s','ours']), !,
   Sem = lam(P,alfa(pro,drs([Index:X],[Index:pred(X,person,n,1)]),app(P,X))).

semlex( np, Lemma,_,Index,Sem):-
   member(Lemma,['we','us','\'s','ours']), !,
   Sem = lam(P,alfa(pro,drs([Index:G],[Index:pred(G,group,n,1)]),
                        drs([],[Index:imp(drs([Index:X],[Index:rel(X,G,member,0)]),
                                          merge(drs([],[Index:pred(X,person,n,1)]),       
                                                app(P,X)))]))).

semlex( np, Lemma,_,Index,Sem):-
   member(Lemma,[whom,'you','yours']), !,
   Sem = lam(P,alfa(pro,drs([Index:X],[Index:pred(X,person,n,1)]),app(P,X))).

semlex( np, Lemma,_,Index,Sem):-
   member(Lemma,['he','his','him']), !,
   Sem = lam(P,alfa(pro,drs([Index:X],[Index:pred(X,male,a,0)]),app(P,X))).

semlex( np, Lemma,_,Index,Sem):-
   member(Lemma,['she','hers','her']), !,
   Sem = lam(P,alfa(pro,drs([Index:X],[Index:pred(X,female,a,0)]),app(P,X))).

semlex( np, Lemma,_,Index,Sem):-
   member(Lemma,['it']), !,
   Sem = lam(P,alfa(pro,drs([Index:X],[Index:pred(X,neuter,a,0)]),app(P,X))).

semlex( np, Lemma,_,Index,Sem):-
   member(Lemma,['they','them','theirs']), !,
   Sem = lam(P,alfa(pro,drs([Index:X],[Index:pred(X,thing,n,12)]),app(P,X))).

semlex( np, Lemma,_,Index,Sem):-
   member(Lemma,['they','them','theirs']), !,
   Sem = lam(P,alfa(pro,drs([Index:G],[Index:pred(G,group,n,1)]),
                        drs([],[Index:imp(drs([Index:X],[Index:rel(X,G,member,0)]),
                                          app(P,X))]))).


/* -------------------------------------------------------------------------
   Reflexive Pronouns 
------------------------------------------------------------------------- */

semlex( np, Lemma,_,Index,Sem):-
   member(Lemma,['myself','yourself','ourselves']), !,
   Sem = lam(P,alfa(ref,drs([Index:X],[Index:pred(X,person,n,1)]),app(P,X))).

semlex( np, Lemma,_,Index,Sem):-
   member(Lemma,['himself']), !,
   Sem = lam(P,alfa(ref,drs([Index:X],[Index:pred(X,male,a,0)]),app(P,X))).

semlex( np, Lemma,_,Index,Sem):-
   member(Lemma,['herself']), !,
   Sem = lam(P,alfa(ref,drs([Index:X],[Index:pred(X,female,a,0)]),app(P,X))).

semlex( np, Lemma,_,Index,Sem):-
   member(Lemma,['itself']), !,
   Sem = lam(P,alfa(ref,drs([Index:X],[Index:pred(X,neuter,a,0)]),app(P,X))).

semlex( np, Lemma,_,Index,Sem):-
   member(Lemma,['themselves']), !,
   Sem = lam(P,alfa(ref,drs([Index:X],[Index:pred(X,group,n,1)]),app(P,X))).


/* -------------------------------------------------------------------------
   Demonstratives and Quantificational Noun Phrases
------------------------------------------------------------------------- */

semlex( np, Lemma,_,Index,Sem):-
   member(Lemma,['none','neither',nothing]), !,
   Sem = lam(P,drs([],[Index:imp(drs([Index:X],[Index:pred(X,thing,n,12)]),drs([],[Index:not(app(P,X))]))])).

semlex( np, Lemma,_,Index,Sem):-
   member(Lemma,[something,some,'both','most','more','many','less','half','another']), !,
   Sem = lam(P,merge(drs([Index:X],[Index:pred(X,thing,n,12)]),app(P,X))).

semlex( np, Lemma,_,Index,Sem):-
   member(Lemma,['this','that','those','these']), !,
   Sem = lam(P,alfa(def,drs([Index:X],[Index:pred(X,thing,n,12)]),app(P,X))).

semlex( np, Lemma,_,Index,Sem):-
   member(Lemma,['all','any','each','either',everything,anything]), !,
   Sem = lam(P,drs([],[Index:imp(drs([Index:X],[Index:pred(X,thing,n,12)]),app(P,X))])).

semlex( np, Lemma,_,Index,Sem):-
   member(Lemma,[everybody,everyone,anybody,anyone]), !,
   Sem = lam(P,drs([],[Index:imp(drs([Index:X],[Index:pred(X,person,n,1)]),app(P,X))])).

semlex( np, Lemma,_,Index,Sem):-
   member(Lemma,[nobody,noone,'no-one']), !,
   Sem = lam(P,drs([],[Index:imp(drs([Index:X],[Index:pred(X,person,n,1)]),drs([],[Index:not(app(P,X))]))])).

semlex( np, Lemma,_,Index,Sem):-
   member(Lemma,[someone,somebody]), !,
   Sem = lam(P,merge(drs([Index:X],[Index:pred(X,person,n,1)]),app(P,X))).


/* -------------------------------------------------------------------------
   NP (semantically empty)
------------------------------------------------------------------------- */

semlex( np_exp, _Lemma,_,Index,Sem):- !,
   Sem = lam(P,merge(drs([Index:X],[]),app(P,X))).

semlex( np_thr, _Lemma,_,Index,Sem):- !,
   Sem = lam(P,merge(drs([Index:X],[]),app(P,X))).


/* -------------------------------------------------------------------------
   NP Why
------------------------------------------------------------------------- */

semlex( np, Lemma,_,Index,Sem):-
   Lemma = 'why', !,
   Sem = lam(P,drs([],[Index:whq([des:rea],
                                 drs([Index:X],[Index:pred(X,reason,n,2)]),
                                 X,
                                 app(P,X))])).


/* -------------------------------------------------------------------------
   NP (all others)
------------------------------------------------------------------------- */

semlex(np,Sym,Pos:Ne:Sense,Index,Sem):- 
   member(Pos,['NNP','NNPS']), !,
   netype(Ne,Type),
   Sem = lam(P,alfa(nam,drs([Index:X],[Index:named(X,Sym,Type,Sense)]),app(P,X))).

semlex(np,Sym,_:_:Sense,Index,Sem):- 
   Sem = lam(P,merge(drs([Index:X],[Index:pred(X,Sym,n,Sense)]),app(P,X))).


/* -------------------------------------------------------------------------
   NP/PP
------------------------------------------------------------------------- */

semlex(np/pp, Sym,_:_:Sense,Index,Sem):- !,
   Sem = lam(PP,lam(P,merge(drs([Index:X],[Index:pred(X,Sym,n,Sense)]),
                            merge(app(P,X),app(PP,X))))).



/* -------------------------------------------------------------------------
   Question words: whose
------------------------------------------------------------------------- */

semlex(Cat,whose,_,Index,Sem):-
   member(Cat,[(s:wq/(s:dcl\np))/n:_,
               (s:wq/(s:q/np))/n:_,
               (s:wq\(s:dcl/np))/n:_]), !, 
   Sem = lam(N,lam(V,app(V,lam(P,drs([],[Index:whq([ins:hum],
                                                   merge(merge(drs([Index:Y],[]),app(N,Y)),
                                                            drs([Index:X],[Index:pred(X,person,n,1),Index:rel(Y,X,of,0)])),
                                                   X, 
                                                   app(P,Y))]))))).


/* -------------------------------------------------------------------------
   Question words: which/what N
------------------------------------------------------------------------- */

semlex(Cat,_Sym,_,Index,Sem):-
   member(Cat,[(s:wq/(s:dcl\np))/n:_,
               (s:wq/(s:q/np))/n:_,
               (s:qem/(s:dcl\np))/n:_,
               (s:qem/(s:dcl/np))/n:_,
               (s:wq\(s:dcl/np))/n:_]), !, 
   Sem = lam(P1,lam(V2,app(V2,lam(P3,drs([],[Index:whq([],
                                             merge(drs([Index:X4],[]),app(P1,X4)),
                                             X4,
                                             app(P3,X4))]))))).
%   Sem = lam(N,lam(V,lam(F,drs([],[Index:whq([],
%                                             merge(drs([Index:X],[]),app(N,X)),
%                                             X,
%                                             app(app(V,lam(P,app(P,X))),F))])))).

semlex(Cat,_Sym,_,Index,Sem):-
   Cat = (s:wq/(s:q/pp))/n:_, !,  % WH-DET N + YNQ
   Sem = lam(N,lam(V,lam(E,drs([],[Index:whq([],
                                              merge(drs([Index:X],[]),app(N,X)),
                                              X,
                                              app(app(V,lam(Y,drs([],[Index:rel(Y,X,rel,0)]))),E))])))).


/* -------------------------------------------------------------------------
   Question words: how much/many 
------------------------------------------------------------------------- */

semlex(Cat,_Sym,_,Index,Sem):-
   member(Cat,[(s:wq/(s:q/np))/np,
               (s:wq/(s:dcl\np))/np]), !, 
   Sem = lam(NP,lam(VP,lam(E,drs([],[Index:whq([num:cou],
                                               merge(drs([Index:X,Index:Y],[Index:card(X,Y,eq)]),
                                                     app(NP,lam(U,drs([],[Index:eq(U,X)])))),
                                               Y,
                                               app(app(VP,lam(P,app(P,X))),E))])))).



/* -------------------------------------------------------------------------
   Question words: how much/many N 
------------------------------------------------------------------------- */

semlex(Cat,_Sym,_,Index,Sem):-
   member(Cat,[((s:wq/(s:q/np))/n:_)/(np/n:_),        
               ((s:wq/(s:dcl\np))/n:_)/(np/n:_)]), !, 
   Sem = lam(D,lam(N,lam(VP,lam(E,drs([],[Index:whq([num:cou],
                                                    merge(drs([Index:X,Index:Y],[Index:card(X,Y,eq)]),
                                                          app(app(D,N),lam(Z,drs([],[Index:eq(X,Z)])))),
                                                    Y,
                                                    app(app(VP,lam(P,app(P,X))),E))]))))).

semlex(Cat,_Sym,_,Index,Sem):-
   member(Cat,[((s:wq/(s:q/pp))/n:_)/(np/n:_),        
               ((s:wq/(s:dcl\pp))/n:_)/(np/n:_)]), !, 
   Sem = lam(D,lam(N,lam(VP,lam(E,drs([],[Index:whq([num:cou],
                                                    merge(drs([Index:X,Index:Y],[Index:card(X,Y,eq)]),
                                                          app(app(D,N),lam(Z,drs([],[Index:eq(X,Z)])))),
                                                    Y,
                                                    app(app(VP,lam(Y,drs([],[Index:rel(Y,X,rel,0)]))),E))]))))).

semlex(Cat,_Sym,_,Index,Sem):-
   member(Cat,[(((s:wq/pp)/((s:q/pp)/np))/n:_)/(np/n:_)]), !,
   Sem = lam(D,lam(N,lam(TV,lam(PP,lam(E,drs([],[Index:whq([num:cou],
                                                           merge(drs([Index:X,Index:Y],[Index:card(X,Y,eq)]),
                                                                 app(app(D,N),lam(Z,drs([],[Index:eq(X,Z)])))),
                                                           Y,
                                                           app(app(app(TV,lam(P,app(P,X))),PP),E))])))))).


semlex(((s:wq/pp)/n:_)/(np/n:_),_Sym,_,Index,Sem):- !,  % American English dialect (How many feet in a mile?)
   Sem = lam(D,lam(N,lam(PP,lam(_,drs([],[Index:whq([num:cou],
                                                    merge(drs([Index:X,Index:Y],[Index:card(X,Y,eq)]),
                                                          app(app(D,N),lam(Z,drs([],[Index:eq(X,Z)])))),
                                                    Y,                                           
                                                    app(PP,X))]))))).
         

semlex(Cat,_Sym,_,Index,Sem):-
   member(Cat,[(((s:wq/(s:pss\np))/((s:q/(s:pss\np))/np))/n:_)/(np/n:_)]),
   Sem = lam(D,lam(N,lam(_,lam(VP,lam(E,drs([],[Index:whq([num:cou],
                                                          merge(drs([Index:X,Index:Y],[Index:card(X,Y,eq)]),
                                                                app(app(D,N),lam(Z,drs([],[Index:eq(X,Z)])))),
                                                          Y,
                                                          app(app(VP,lam(P,app(P,X))),E))])))))).



/* -------------------------------------------------------------------------
   Question words: how ADJ 
------------------------------------------------------------------------- */

semlex(Cat,_Sym,_,Index,Sem):-
   member(Cat,[(s:wq/(s:q/(s:adj\np)))/(s:adj\np),
               ((s:wq/pp)/((s:q/pp)/(s:adj\np)))/(s:adj\np),
               (s:qem/(s:dcl/(s:adj\np)))/(s:adj\np)]), !, % How ADJ
   Sem = lam(A,lam(U,app(U,lam(NP,lam(E,app(NP,lam(X,drs([],[Index:whq([mea:mis],
                                                                       merge(drs([Index:Y],[]),
                                                                             app(app(A,lam(P,app(P,Y))),E)),
                                                                       Y,
                                                                       drs([],[Index:rel(Y,X,of,0)]))])))))))).


semlex(Cat,_Sym,_,Index,Sem):-
   Cat = (s:wq/(s:q/pp))/(s:adj\np), !, % How often does...
   Sem = lam(A,lam(VP,lam(F,drs([],[Index:whq([mea:mis],
                                              merge(drs([Index:X],[]),
                                                    app(app(A,lam(P,app(P,X))),lam(_,drs([],[])))),
                                              X,
                                              app(app(VP,lam(Y,drs([],[Index:rel(Y,X,rel,0)]))),F))])))).


/* -------------------------------------------------------------------------
   Question words: basic question words 
------------------------------------------------------------------------- */

semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[s:wq/(s:dcl\np),  
               s:wq/(s:q/np),  
               s:wq\(s:dcl/np)]), 
   ( Sym = what,      Pred = thing,        QType=[],        Sense=12;
     Sym = whatever,  Pred = thing,        QType=[],        Sense=12;
     Sym = which,     Pred = thing,        QType=[],        Sense=12;
     Sym = whichever, Pred = thing,        QType=[],        Sense=12;
     Sym = where,     Pred = location,     QType=[loc:any], Sense=1;
     Sym = why,       Pred = reason,       QType=[des:rea], Sense=2;
     Sym = how,       Pred = manner,       QType=[des:man], Sense=2; 
     Sym = who,       Pred = person,       QType=[ins:hum], Sense=1;      
     Sym = whoever,   Pred = person,       QType=[ins:hum], Sense=1;      
     Sym = whom,      Pred = person,       QType=[ins:hum], Sense=1;      
     Sym = when,      Pred = unit_of_time, QType=[tim:any], Sense=1 
   ), !,
   Sem = lam(VP,lam(F,drs([],[Index:whq(QType,
                                        drs([Index:X],[Index:pred(X,Pred,n,Sense)]),
                                        X,
                                        app(app(VP,lam(P,app(P,X))),F))]))).


semlex(Cat,Sym,_,Index,Sem):-
   Cat = s:wq/(s:q/pp), 
   ( Sym=where, Pred=location,     Rel=loc_rel,  QType=[loc:any], Sense=1;
     Sym=why,   Pred=reason,       Rel=rel,      QType=[des:rea], Sense=2;
     Sym=how,   Pred=manner,       Rel=rel,      QType=[des:man], Sense=2;
     Sym=when,  Pred=unit_of_time, Rel=temp_rel, QType=[tim:any], Sense=1
   ), !, 
   Sem = lam(VP,lam(F,drs([],[Index:whq(QType,
                                        drs([Index:X],[Index:pred(X,Pred,n,Sense)]),
                                        X,
                                        app(app(VP,lam(E,drs([],[Index:rel(E,X,Rel,0)]))),F))]))).

semlex(Cat,_Sym,_,Index,Sem):-
   member(Cat,[np/(s:dcl\np),
               np/(s:dcl/np)]), !,
   Sem = lam(VP,lam(P,drs([],[Index:whq([],
                                        drs([Index:X],[Index:pred(X,thing,n,12)]),
                                        X,
                                        merge(app(app(VP,lam(R,app(R,X))),lam(_,drs([],[]))),app(P,X)))]))). 

semlex(Cat,_Sym,_,Index,Sem):-
   member(Cat,[np/((s:to\np)/np)]), !,
   Sem = lam(TV,lam(P,drs([],[Index:whq([],
                                        drs([Index:X],[Index:pred(X,thing,n,12)]),
                                        X,
                                        merge(app(app(app(TV,lam(R,app(R,X))),lam(Q,merge(drs([[]:Z],[[]:pred(Z,thing,n,12)]),
                                                                                          app(Q,Z)))),
                                                  lam(_,drs([],[]))),app(P,X)))]))). 

semlex(Cat,_Sym,_,Index,Sem):-
   member(Cat,[(np/(s:dcl\np))/n:_,
               (np/(s:dcl/np))/n:_]), !,
   Sem = lam(N,lam(VP,lam(P,drs([],[Index:whq([],
                                              merge(drs([Index:X],[]),app(N,X)),
                                              X,
                                              merge(app(app(VP,lam(R,app(R,X))),lam(_,drs([],[]))),app(P,X)))])))). 


semlex(s:wq/s:q,Sym,_,Index,Sem):- 
   ( Sym=how,   Pred=manner,       QType=[des:man], Sense=2 ;
     Sym=where, Pred=location,     QType=[loc:any], Sense=1 ;
     Sym=when,  Pred=unit_of_time, QType=[tim:any], Sense=1 ;
     Sym=why,   Pred=reason,       QType=[des:rea], Sense=2 ;
     Sym=what,  Pred=thing,        QType=[], Sense=12 
   ), !,
   Sem = lam(YNQ,lam(E,app(YNQ,lam(F,drs([],[Index:whq(QType,
                                                       drs([Index:X],[Index:pred(X,Pred,n,Sense)]),    
                                                       X,
                                                       merge(drs([],[Index:rel(F,X,rel,0)]),app(E,F)))]))))). 

semlex(s:qem/(s:to\np),_,_,Index,Sem):- !,
   Sem = lam(VP,lam(E,app(app(VP,lam(P,merge(drs([Index:X],[]),app(P,X)))),lam(F,merge(drs([],[Index:pred(F,manner,n,2)]),app(E,F)))))). % how to

% whose
semlex(Cat,whose,_,Index,Sem):- 
   member(Cat,[s:wq/(s:q/np),
               s:wq\(s:dcl/np),
               s:wq/(s:dcl\np)]), !,
   Sem = lam(VP,app(VP,lam(P,drs([],[Index:whq([],
                                               drs([Index:X,Index:Y],[Index:pred(X,thing,n,12),Index:pred(Y,person,n,1),Index:rel(X,Y,of,0)]),
                                               X,
                                               app(P,X))])))).

semlex(Cat,_,_,Index,Sem):- 
   member(Cat,[s:qem/(s:dcl\np),
               s:_/(s:dcl\np),
               s:qem/(s:dcl/np)]), !,   
   Sem = lam(VP,app(VP,lam(P,drs([],[Index:whq([],
                                               drs([Index:X],[Index:pred(X,thing,n,12)]),
                                               X,
                                               app(P,X))])))).

% how
semlex((s:qem/s:dcl)/(s:adj\np),_,_,Index,Sem):- !,
   Sem = lam(VP,lam(S,app(VP,lam(P,drs([],[Index:whq([des:man],
                                                     merge(drs([Index:X],[Index:pred(X,manner,n,2)]),app(P,X)),
                                                     X,
                                                     app(S,lam(E,drs([],[Index:rel(E,X,rel,0)]))))]))))).

% how much energy was lost (??)
% how many years has GP been campaigning
semlex(Cat,_,_,Index,Sem):-
   member(Cat,[((s:qem/(s:dcl\np))/n:_)/(s:adj\np),
               ((s:qem/(s:dcl/np))/n:_)/(s:adj\np)]), !,
   Sem = lam(VPADJ,lam(N,lam(VPDCL,lam(E,app(app(VPADJ,lam(P,drs([],[Index:whq([num:cou],
                                                                                merge(drs([Index:X],[]),
                                                                                      merge(app(N,X),
                                                                                            app(P,X))),
                                                                     X,
                                                                     app(app(VPDCL,lam(P,app(P,X))),E))]))),
                                                                         lam(_,drs([],[]))))))).

% why does he always wait
semlex((s:X\s:X)/s:q,_,_,Index,Sem):- !,
   Sem = lam(W,lam(S,lam(F,app(S,lam(E,merge(drs([Index:Y],[Index:prop(Y,drs([],[Index:whq([des:rea],
                                                                                           drs([Index:Z],[Index:pred(Z,reason,n,2)]),
                                                                                           Z,
                                                                                           app(W,lam(E,drs([],[Index:rel(E,Z,rel,0)]))))])),
                                                            Index:rel(E,Y,rel,0)]),
                                             app(F,E))))))).

/* =========================================================================
   Relative pronouns,pied-piping ("N under which S", "NP under which S")
========================================================================= */

semlex(((np\np)/s:dcl)\((s:F\s:F)/np),_Sym,_,_Index,Sem):- !,
   Sem = lam(Prep,lam(S,lam(NP,lam(Q,app(app(app(Prep,lam(P,app(NP,lam(X,merge(app(Q,X),app(P,X)))))),S),lam(_,drs([],[]))))))). 

semlex(((np\np)/s:dcl)\((np\np)/np),_Sym,_,_Index,Sem):- !,
   Sem = lam(Prep,lam(S,lam(NP,lam(Q,app(app(app(Prep,lam(P,app(NP,lam(X,merge(app(Q,X),app(P,X)))))),S),lam(_,drs([],[]))))))). 

semlex(((n:Y\n:Y)/s:dcl)\((n:X\n:X)/np),_Sym,_,_Index,Sem):- !,
   Sem = lam(Prep,lam(S,lam(N,app(app(Prep,S),N)))).

semlex(((np\np)/s:dcl)\((n:X\n:X)/np),_Sym,_,_Index,Sem):- !,
   Sem = lam(Prep,lam(S,lam(NP,lam(P,app(NP,app(app(Prep,S),P)))))).



/* =========================================================================
   Verbs
========================================================================= */

semlex(Cat,Sym,Pos:_:_,Index,Sem):- 
   semlex_verb(Cat,Sym,Pos,Index,Sem), !.



/* -------------------------------------------------------------------------
   Copula modifiers
------------------------------------------------------------------------- */

semlex(Cat,Sym,_,Index,Sem):-
   Cat = ((s:adj\np)/s:em)/(s:adj\np),  % .. are so similar to themselves that ...
   Sem = lam(VP,lam(S,lam(Q,lam(F,app(app(VP,Q),lam(E,merge(drs([[]:A],[Index:rel(E,A,Sym,0),
                                                                        Index:prop(A,app(S,lam(_,drs([],[]))))]),app(F,E)))))))).


/* =========================================================================
   Adjectives
========================================================================= */

/* -------------------------------------------------------------------------
   Wrongly Classified Adjectives + "own"
------------------------------------------------------------------------- */

semlex(Cat,many,_,Index,Sem):-
   category(adj,Cat,_), !,
   Sem = lam(P,lam(X,merge(drs([],[Index:pred(X,quantity,n,1)]),app(P,X)))).

semlex(Cat,much,_,Index,Sem):-
   category(adj,Cat,_), !,
   Sem = lam(P,lam(X,merge(drs([],[Index:pred(X,amount,n,3)]),app(P,X)))).

semlex(Cat,only,_,Index,Sem):-
   category(adj,Cat,_), !,
   Sem = lam(P,lam(X,alfa(fac,app(P,X),drs([],[Index:imp(merge(drs([Index:Y],[]),app(P,Y)),drs([],[Index:eq(Y,X)]))])))).


/* -------------------------------------------------------------------------
   Presuppositional Adjectives
------------------------------------------------------------------------- */

semlex(Cat,Sym,_,Index,Sem):-
   member(Sym,[other,previous,different]),
   category(adj,Cat,_), !,
   Sem = lam(P,lam(X,alfa(def,
                          merge(drs([[]:Y],[]),app(P,Y)),
                          merge(drs([],[Index:not(drs([],[[]:eq(X,Y)]))]),
                                app(P,X))))).


/* -------------------------------------------------------------------------
   Gerunds
------------------------------------------------------------------------- */

semlex(Cat,Sym,'VBG':_:_,Index,Sem):-
   category(adj,Cat,_), !,
   Sem = lam(P,lam(X,merge(drs([Index:E],
                               [Index:pred(E,Sym,v,0),
                                Index:rel(E,X,agent,0)]), 
                           app(P,X)))).

/* -------------------------------------------------------------------------
   Passive participles 
------------------------------------------------------------------------- */

semlex(Cat,Sym,'VBN':_:_,Index,Sem):-
   category(adj,Cat,_), !,
   Sem = lam(P,lam(X,merge(drs([Index:E],
                               [Index:pred(E,Sym,v,0),
                                Index:rel(E,X,patient,0)]), 
                           app(P,X)))).

/* -------------------------------------------------------------------------
   Passive participles (in compounds)
------------------------------------------------------------------------- */

semlex((n:F/n:F)\pp,Sym,'VBN':_:_,Index,Sem):-
   Sem = lam(Q,lam(P,lam(X,merge(app(P,X),
                                 merge(drs([Index:E],
                                           [Index:pred(E,Sym,v,0),
                                            Index:rel(E,X,patient,0)]), 
                                       app(Q,E)))))).

/* -------------------------------------------------------------------------
   Compound adjectives: 10-year-old
------------------------------------------------------------------------- */

semlex((n:F/n:F)\pp,Sym,'JJ':_:_,Index,Sem):-
   Sem = lam(Q,lam(P,lam(X,merge(app(P,X),
                                 merge(drs([Index:E],
                                           [Index:pred(E,Sym,v,0),
                                            Index:rel(E,X,theme,0)]), 
                                       app(Q,E)))))).


/* -------------------------------------------------------------------------
   Noun Noun Compounds
------------------------------------------------------------------------- */

semlex(Cat,'%','NN':Ner:Sense,Index,Sem):-
   category(adj,Cat,_), !,
   semlex(Cat,percent,'NN':Ner:Sense,Index,Sem).

semlex(Cat,'$',_,Index,Sem):- 
   Cat = n:num/n:num, !,
   Sem = lam(P,lam(X,merge(drs([],[Index:pred(X,dollar,n,1)]),app(P,X)))).

semlex(n:_/n:_,'&',_:Ne:Sense,Index,Sem):- 
   netype(Ne,org), !,
   Sem = lam(P,lam(X,merge(drs([],[Index:named(X,'&',org,Sense)]),
                           app(P,X)))).

semlex(Cat,Sym,'CD':'I-DAT':_,Index,Sem):- 
   category(adj,Cat,_), 
   dofm(Sym,DID), !,
   Sem = lam(P,lam(X,merge(drs([],[Index:timex(X,date([]:'+',[]:'XXXX',[]:'XX',Index:DID))]),
                           app(P,X)))).

semlex(Cat,Sym,'NNP':'I-DAT':_,Index,Sem):- 
   category(adj,Cat,_),
   month(Sym,MID), !,
   Sem = lam(P,lam(X,merge(drs([],[Index:timex(X,date([]:'+',[]:'XXXX',Index:MID,[]:'XX'))]),
                           app(P,X)))).

semlex(Cat,YID,'CD':'I-DAT':_,Index,Sem):- 
   category(adj,Cat,_),
   year(YID,Year), !,
   Sem = lam(P,lam(X,merge(drs([],[Index:timex(X,date([]:'+',Index:Year,[]:'XX',[]:'XX'))]),
                           app(P,X)))).

semlex(Cat,Sym,Pos:_:Sense,Index,Sem):-
   member(Pos,['NN','NNS']),
   category(adj,Cat,_), !,
   Sem = lam(P,lam(X,merge(drs([],
                               [Index:pred(X,Sym,n,Sense)]),
                           app(P,X)))).

semlex(n:F/n:F,Title,Pos:_:Sense,Index,Sem):-
   member(Pos,['NNP','NNPS']),
   title(Title,Sym), !,
   Sem = lam(P,lam(X,merge(drs([],[Index:named(X,Sym,ttl,Sense)]),
                           app(P,X)))).

semlex(Cat,Sym,Pos:Ner:Sense,Index,Sem):-
   member(Pos,['NNP','NNPS']),
   category(adj,Cat,_), !,
   netype(Ner,Type),
   Sem = lam(P,lam(X,merge(drs([],[Index:named(X,Sym,Type,Sense)]),
                           app(P,X)))).

/* -------------------------------------------------------------------------
   Singular Superlatives
------------------------------------------------------------------------- */

semlex(Cat,Sym,'JJS':_:_,Index,Sem):-
   category(adj,Cat,_), !,
   Sem = lam(P,lam(X,merge(app(P,X),
                           drs([],[Index:imp(merge(drs([Index:Y],[[]:not(drs([],[[]:eq(X,Y)]))]),
                                                   app(P,Y)),
                                             drs([],[Index:rel(X,Y,Sym,0)]))])))).


/* -------------------------------------------------------------------------
   Cardinal Adjectives
------------------------------------------------------------------------- */

semlex(Cat,Sym,_,Index,Sem):-
   category(adj,Cat,_), 
   string2digit(Sym,Digit), !,
   Sem = lam(P,lam(X,merge(drs([],[Index:card(X,Digit,eq)]),app(P,X)))).


/* -------------------------------------------------------------------------
   Composite Adjectives:  10-hour story
------------------------------------------------------------------------- */

semlex(Cat,Sym,_:_:Sense,Index,Sem):-
   category(adj,Cat,_), 
   atomic_list_concat([Prefix,Suffix],'-',Sym),
   member(Suffix,[acre,year,yard,foot,pound,day,minute,page,point,man,inch,
                  degree,week,member,mile,week,km,dollar,kilometer,
                  'square-foot',seat,meter,story,hour,time,ton,month]),
   string2digit(Prefix,Number), !, 
   Sem = lam(P,lam(X,merge(drs([Index:Y],
                               [Index:card(Y,Number,eq),
                                Index:pred(Y,Suffix,n,Sense),
                                []:rel(Y,X,nn,1)]),app(P,X)))).


/* -------------------------------------------------------------------------
   Singular Intersective Adjectives
------------------------------------------------------------------------- */

semlex(Cat,Sym,_:_:Sense,Index,Sem):-
   category(adj,Cat,_), !,
   Sem = lam(P,lam(X,merge(drs([],[Index:pred(X,Sym,a,Sense)]),app(P,X)))).

semlex(Cat,Sym,_:_:Sense,Index,Sem):-
   member(Cat,[(n:F/pp)\(n:F/pp)]), !,
   Sem = lam(PP,lam(P,lam(X,merge(drs([],[Index:pred(X,Sym,a,Sense)]),app(app(PP,P),X))))).


/* -------------------------------------------------------------------------
   Adjectives introducing a degree
------------------------------------------------------------------------- */

semlex(d/n:_,Sym,_,Index,Sem):- !,
%%   Sem = lam(P,lam(X,lam(D,merge(drs([],[Index:pred(D,degree,n,1),Index:rel(X,D,Sym,0)]),app(P,X))))).
   Sem = lam(Y,lam(X,drs([],[Index:rel(X,Y,Sym,0)]))).


/* =========================================================================
   Other Modifiers
========================================================================= */

/* -------------------------------------------------------------------------
   Superlative: at least/most/best (no idea how to specify semantics)
------------------------------------------------------------------------- */

semlex(Cat,_Sym,_,_Index,Sem):-
   member(Cat,[(C/C)/(s:asup\np), 
               (C\C)/(s:asup\np), 
               (C\C)\(s:asup\np),
               (C/C)\(s:asup\np)]), !,
   Sem = lam(_,lam(N,N)).


/* -------------------------------------------------------------------------
   Comparative (more than)
------------------------------------------------------------------------- */

semlex(Cat,_,_,_Index,Sem):- 
   member(Cat,[(n:_/n:_)\(s:adj\np),
               (n:_/n:_)/(s:adj\np)]),
   Sem = lam(VP,lam(N,lam(X,app(app(VP,lam(P,merge(app(P,X),app(N,X)))),lam(_,drs([],[])))))).

semlex(Cat,Sym,_:_:Sense,Index,Sem):-
   member(Cat,[(np/np)\(s:adj\np),
               (np/np)/(s:adj\np)]), !,
   Sem = lam(VP,lam(NP,lam(P,app(app(VP,NP),lam(X,merge(drs([],[Index:pred(X,Sym,a,Sense)]),app(P,X))))))).

semlex(Cat,Sym,_:_:Sense,Index,Sem):-
   member(Cat,[((np/np)/(np/np))\(s:adj\np),
               ((np\np)\(np\np))/(s:dcl\np)]), !, 
   Sem = lam(VP,lam(NPNP,lam(NP,lam(P,app(app(VP,app(NPNP,NP)),lam(X,merge(drs([],[Index:pred(X,Sym,a,Sense)]),app(P,X)))))))).

semlex(Cat,Sym,_,Index,Sem):-
   Cat = ((((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np)))/(((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np))))\(s:adj\np), !,
   Sem = lam(_VP,lam(_MM,lam(_M,lam(Z,lam(Q,lam(F,app(app(Z,Q),lam(E,merge(drs([],[Index:pred(E,Sym,v,0)]),app(F,E)))))))))).      


/* -------------------------------------------------------------------------
   Superlatives: (the) most/least ... 
------------------------------------------------------------------------- */

semlex(Cat,most,_,Index,Sem):-  
   member(Cat,[(n:_/n:_)/(d/n)]), !,
   Sem = lam(R,lam(P,lam(X,merge(app(P,X),
                                 drs([],[Index:imp(merge(drs([[]:Y],[[]:not(drs([],[[]:eq(X,Y)]))]),app(P,Y)),
                                                        app(app(R,Y),X))]))))).

semlex(Cat,least,_,Index,Sem):-  
   member(Cat,[(n:_/n:_)/(d/n)]), !,
   Sem = lam(R,lam(P,lam(X,merge(app(P,X),
                                 drs([],[Index:imp(merge(drs([[]:Y],[[]:not(drs([],[[]:eq(X,Y)]))]),app(P,Y)),
                                                        app(app(R,X),Y))]))))).



/* -------------------------------------------------------------------------
   Compound numerals
------------------------------------------------------------------------- */

semlex(Cat,Sym,_,Index,Sem):-  
   member(Cat,[(n:_/n:_)/(n:_/n:_),
               (n:_\n:_)/(n:_\n:_),
               (n:_/n:_)\(n:_/n:_)]), 
   string2digit(Sym,Digit), !,
   Sem = lam(Z,lam(P,lam(Y,app(app(Z,lam(X,merge(drs([],[Index:card(X,Digit,eq)]),app(P,X)))),Y)))).


/* -------------------------------------------------------------------------
   Compound superlative adjectives
------------------------------------------------------------------------- */

semlex(Cat,Sym,'JJS':_:_,Index,Sem):-        
   member(Cat,[(n:_/n:_)/(n:_/n:_),        %%%% Example: ... fastest growing segment
               (n:_/n:_)\(n:_/n:_)]), !,   %%%% Example: ... third largest bank (incorrect semantics!)
   Sem = lam(Z,lam(P,lam(X,merge(app(app(Z,P),X),
                                 drs([],[Index:imp(merge(drs([Index:Y],[[]:not(drs([],[[]:eq(X,Y)]))]),
                                                              app(app(Z,P),Y)),
                                                        drs([],[Index:rel(X,Y,Sym,0)]))]))))).

/* -------------------------------------------------------------------------
   Intensifiers
------------------------------------------------------------------------- */

semlex(Cat,Sym,Pos:Ne:Sense,Index,Sem):-  
   member(Pos,['NNP','NNPS']),
   member(Cat,[(n:_/n:_)/(n:_/n:_),
               (n:_\n:_)/(n:_\n:_),
               (n:_/n:_)\(n:_/n:_)]), !,
   netype(Ne,Type),
   Sem = lam(Z,lam(P,lam(X,merge(drs([],[Index:named(X,Sym,Type,Sense)]),
                                 app(app(Z,P),X))))).

semlex(Cat,Sym,_:_:Sense,Index,Sem):-  
   member(Cat,[(n:_/n:_)/(n:_/n:_),
               (n:_\n:_)/(n:_\n:_),
               (n:_/n:_)\(n:_/n:_)]), !,
   Sem = lam(Z,lam(P,lam(X,merge(drs([],[Index:pred(X,Sym,a,Sense)]),
                                 app(app(Z,P),X))))).

semlex(Cat,_Sym,_,_Index,Sem):-  
   member(Cat,[((n:_/n:_)/(n:_/n:_))/((n:_/n:_)/(n:_/n:_))]), !,
   Sem = lam(I,lam(Z,lam(P,lam(Y,app(app(app(I,Z),P),Y))))).  % just a place-holder!

semlex(Cat,Sym,_:_:Sense,Index,Sem):-  
   Cat = ((n:_/n:_)/(n:_/n:_))\(s:adj\np), !,
   Sem = lam(Q,lam(Z,lam(P,lam(Y,app(app(Z,lam(X,merge(drs([],[Index:pred(X,Sym,a,Sense)]),
                                                       merge(app(app(Q,lam(P,app(P,X))),lam(_,drs([],[]))),
                                                             app(P,X))))),Y))))).  


/* -------------------------------------------------------------------------
   Compound adjectives (actually, the hyphen in compound adjectives)
------------------------------------------------------------------------- */

semlex(pp\n:loc,'-',_,Index,Sem):- !,
   Sem = lam(N,lam(X,alfa(def,merge(drs([Index:Y],[]),app(N,Y)),
                              drs([],[Index:rel(X,Y,loc_rel,0)])))).

semlex(pp\n:nom,'-',_,Index,Sem):- !,
   Sem = lam(N,lam(X,merge(drs([Index:Y],[Index:rel(X,Y,rel,0)]),
                           app(N,Y)))).

semlex(pp\n:num,'-',_,Index,Sem):- !,
   Sem = lam(N,lam(X,merge(drs([Index:Y],[Index:rel(X,Y,attribute,0)]),
                           app(N,Y)))).


/* -------------------------------------------------------------------------
   Definite Prepositions
------------------------------------------------------------------------- */

% except
%
semlex(Cat,except,_,Index,Sem):- 
   Cat = (n:_\n:_)/pp, !,
   Sem = lam(PP,lam(P,lam(X,merge(app(P,X),drs([],[Index:not(app(PP,X))]))))).

semlex(Cat,Sym,_:_:Sense,Index,Sem):- 
   Cat = (n:_\n:_)/pp, !,
   Sem = lam(PP,lam(P,lam(X,merge(app(P,X),merge(drs([],[Index:pred(X,Sym,a,Sense)]),app(PP,X)))))).


% Range constructions (e.g., "10 to 20")
%
semlex(Cat,to,_,Index,Sem):- 
   Cat = (n:_\n:_)/n:_, !,
   Sem = lam(N,lam(P,lam(X,drs([],[Index:or(app(P,X),app(N,X))])))).

% seven cents a share
%
semlex(Cat,Sym,_,Index,Sem):- 
   member(Sym,[a,an]),
   Cat = (n:_\n:_)/n:_, !,
   Sem = lam(N,lam(P,lam(X,drs([],[Index:imp(merge(drs([Index:Y],[]),app(N,Y)),
                                             merge(drs([],[Index:rel(X,Y,rel,0)]),app(P,X)))])))).

semlex(Cat,Sym,_,Index,Sem):- 
   member(Sym,[the,that,this,these,those]),
   Cat = (n:_\n:_)/n:_, !,
   Sem = lam(N,lam(P,lam(X,alfa(def,merge(drs([Index:Y],[]),app(N,Y)),
                                    merge(drs([],[Index:rel(X,Y,rel,0)]),app(P,X)))))).

semlex(Cat,Sym,_,Index,Sem):- 
   Cat = (n:_\n:_)/n:_, !,
   Sem = lam(N,lam(P,lam(X,merge(merge(drs([Index:Y],[]),app(N,Y)),
                                 merge(drs([],[Index:rel(X,Y,Sym,0)]),app(P,X)))))).

semlex(Cat,'$',_,Index,Sem):- 
   Cat = (n:_/n:_)/n:num, !,
   Sem = lam(N,lam(P,lam(X,merge(merge(drs([Index:Y],[Index:pred(Y,dollar,n,1)]),app(N,Y)),
                                 merge(drs([],[Index:rel(X,Y,rel,0)]),app(P,X)))))).

semlex(Cat,Sym,_,Index,Sem):- 
   Cat = (n:_/n:_)/n:_, !,
   Sem = lam(N,lam(P,lam(X,merge(merge(drs([Index:Y],[Index:pred(Y,Sym,n,1)]),app(N,Y)),
                                 merge(drs([],[Index:rel(X,Y,rel,0)]),app(P,X)))))).

semlex(((n:_/n:_)\(n:_/n:_))/(n:_/n:_),_,_,Index,Sem):- !,
   Sem = lam(M2,lam(M1,lam(P,lam(X,drs([],[Index:or(app(app(M1,P),X),
                                                    app(app(M2,P),X))]))))).

semlex(((n:_/n:_)\(n:_/n:_))/n:_,_,_,Index,Sem):- !,
   Sem = lam(N,lam(M,lam(P,lam(X,drs([],[Index:or(app(app(M,P),X),
                                                  merge(app(N,X),app(P,X)))]))))).

semlex(((n:_/pp)\(n:_/pp))/n:_,_,_,Index,Sem):- !,
   Sem = lam(N,lam(RN,lam(PP,lam(X,merge(drs([Index:Y],[Index:rel(X,Y,rel,0)]),
                                         merge(app(N,Y),app(app(RN,PP),X))))))).


/* -------------------------------------------------------------------------
   Complementizers (Wh)
------------------------------------------------------------------------- */

semlex(s:qem/s:dcl,Sym,_,Index,Sem):- 
   ( Sym=how,   Pred=manner,       QType=[des:man], Sense=2, Rel=manner_rel;
     Sym=where, Pred=location,     QType=[loc:any], Sense=1, Rel=loc_rel;
     Sym=when,  Pred=unit_of_time, QType=[tim:any], Sense=1, Rel=time_rel;
     Sym=why,   Pred=reason,       QType=[des:rea], Sense=2, Rel=reason_rel), !,
   Sem = lam(S,lam(F,drs([],[Index:whq(QType,
                                       drs([Index:X],[Index:pred(X,Pred,n,Sense)]),
                                       X,
                                       app(S,lam(E,merge(drs([],[Index:rel(E,X,Rel,0)]),
                                                         app(F,E)))))]))).

/* -------------------------------------------------------------------------
   Complementizers 
------------------------------------------------------------------------- */

semlex(Cat,_Sym,_,_Index,Sem):-
   category(comp,Cat,_), !,
   Sem = lam(U,U).

semlex(Cat,_,_,Index,Sem):-
   Cat = np/s:dcl,
   Sem = lam(S,lam(P,merge(drs([Index:X],
                               [Index:prop(X,app(S,lam(_,drs([],[]))))]),
                           app(P,X)))).


/* -------------------------------------------------------------------------
   Locative Adverbs
------------------------------------------------------------------------- */

semlex(Cat,Sym,_,Index,Sem):-
   category(vpadv,Cat,_),
   member(Sym,[somewhere]), !,
   Sem = lam(X,lam(Q,lam(F,app(app(X,Q),lam(E,merge(drs([Index:Z],[Index:pred(Z,location,n,1),
                                                                   Index:rel(E,Z,loc_rel,0)]),app(F,E))))))).

semlex(Cat,Sym,_,Index,Sem):-
   category(vpadv,Cat,_),
   member(Sym,[anywhere,everywhere]), !,
   Sem = lam(X,lam(Q,lam(F,app(app(X,Q),lam(E,drs([],[Index:imp(drs([Index:Z],[Index:pred(Z,location,n,1)]),
                                                                merge(drs([],[Index:rel(E,Z,loc_rel,0)]),app(F,E)))])))))).

semlex(Cat,Sym,_,Index,Sem):-
   category(vpadv,Cat,_),
   member(Sym,[nowhere]), !,
   Sem = lam(X,lam(Q,lam(F,app(app(X,Q),lam(E,drs([],[Index:imp(drs([Index:Z],[Index:pred(Z,location,n,1)]),
                                                                drs([],[Index:not(merge(drs([],[Index:rel(E,Z,loc_rel,0)]),app(F,E)))]))])))))).


/* -------------------------------------------------------------------------
   Not 
------------------------------------------------------------------------- */

semlex(Cat,Sym,_,Index,Sem):-
   category(vpadv,Cat,_),
   notSymbol(Sym), !,
   Sem = lam(V,lam(Q,lam(F,app(Q,lam(X,drs([],[Index:not(app(app(V,lam(P,app(P,X))),F))])))))). %%% subject wide scope (preferred)
%  Sem = lam(X,lam(Q,lam(F,drs([],[Index:not(app(app(X,Q),F))])))).                             %%% negation wide scope (dispreferred)


/* -------------------------------------------------------------------------
   Cardinals that function as VP modifiers
------------------------------------------------------------------------- */

semlex(Cat,Sym,'CD':_:_,Index,Sem):-
   category(vpadv,Cat,_),
   string2digit(Sym,Digit), !,
   Sem = lam(X,lam(Q,lam(F,app(app(X,Q),lam(E,merge(drs([Index:X],[Index:card(X,Digit,eq),
                                                                   Index:rel(E,X,rel,0)]),app(F,E))))))).

/* -------------------------------------------------------------------------
   NPs that function as VP modifiers
------------------------------------------------------------------------- */

semlex(Cat,Sym,Pos:_:Sense,Index,Sem):-
   category(vpadv,Cat,_),
   member(Pos,['NN','NNP','NNS','NNPS']), !,
   Sem = lam(X,lam(Q,lam(F,app(app(X,Q),lam(E,merge(drs([Index:X],[Index:pred(X,Sym,n,Sense),
                                                                   Index:rel(E,X,rel,0)]),app(F,E))))))).

/* -------------------------------------------------------------------------
   Comparative (more)
------------------------------------------------------------------------- */

semlex(Cat,Sym,_,Index,Sem):-
   Sym = more,
   Cat = (s:adj\np)/(s:adj\np), !,
   Sem = lam(X,lam(Q,lam(F,app(app(X,Q),lam(D1,merge(drs([Index:D2],[Index:rel(D1,D2,more,0)]),app(F,D1))))))).


/* -------------------------------------------------------------------------
   Adverbs (VP modifying)
------------------------------------------------------------------------- */

semlex(Cat,Sym,_:_:Sense,Index,Sem):-
   category(vpadv,Cat,_), !,
   Sem = lam(X,lam(Q,lam(F,app(app(X,Q),lam(E,merge(drs([],[Index:pred(E,Sym,a,Sense)]),app(F,E))))))).


/* -------------------------------------------------------------------------
   "hard to take", "easy to please"
------------------------------------------------------------------------- */

semlex((s:adj\np)/((s:to\np)/np),Sym,_,Index,Sem):-
%   Sem = lam(TV,lam(Q,lam(F,app(app(app(TV,lam(P,merge(drs([[]:X],[[]:pred(X,thing,n,12)]),app(P,X)))),Q),lam(E,merge(drs([],[Index:pred(E,Sym,v,0)]),app(F,E))))))).
   Sem = lam(TV,lam(Q,lam(F,app(app(app(TV,Q),lam(P,merge(drs([[]:X],[[]:pred(X,thing,n,12)]),app(P,X)))),lam(E,merge(drs([],[Index:pred(E,Sym,a,0)]),app(F,E))))))).



/* -------------------------------------------------------------------------
   Definite prepositions 
------------------------------------------------------------------------- */

% "the" as apposition trigger
%
semlex(Cat,the,_,Index,Sem):- 
   Cat = (np\np)/n:_, !,
   Sem = lam(N,lam(Q,lam(P,app(Q,lam(X,merge(drs([Index:Y],[Index:eq(X,Y)]),
                                             merge(app(N,Y),app(P,X)))))))).
% seven cents a share
%
semlex(Cat,Sym,_,Index,Sem):-
   member(Sym,[a,an]),
   Cat = (np\np)/n:_, !,
   Sem = lam(N,lam(Q,lam(P,drs([],[Index:imp(merge(drs([Index:Y],[]),
                                                   app(N,Y)),
                                             app(Q,lam(X,merge(drs([],[Index:rel(X,Y,rel,0)]),
                                                               app(P,X)))))])))).

semlex(Cat,Sym,_,Index,Sem):- 
   Cat = (np\np)/n:_, !,
   Sem = lam(N,lam(Q,lam(P,app(Q,lam(X,merge(drs([Index:Y],[Index:rel(X,Y,Sym,0)]),
                                             merge(app(N,Y),app(P,X)))))))).


/* -------------------------------------------------------------------------
   Prepositional Phrases
------------------------------------------------------------------------- */

semlex(pp,Sym,_,Index,Sem):- !,
   Sem = lam(X,drs([Index:Y],[Index:pred(Y,thing,n,12),
                              Index:rel(X,Y,Sym,0)])).


/* -------------------------------------------------------------------------
   Prepositions
------------------------------------------------------------------------- */

semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[(n:_\n:_)/np,
               (n:_/n:_)/np,
               (n:_/n:_)\np,
               (n:_\n:_)\np]), !,
   Sem = lam(Q,lam(P,lam(X,merge(app(P,X),app(Q,lam(Y,drs([],[Index:rel(X,Y,Sym,0)]))))))).

semlex(Cat,Sym,_,Index,Sem):-
   Cat = ((n:F/(s:to\np))\(n:F/(s:to\np)))/np, !,
   Sem = lam(Q,lam(N,lam(VP,lam(X,merge(app(app(N,VP),X),app(Q,lam(Y,drs([],[Index:rel(X,Y,Sym,0)])))))))).

semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[((n:_/n:_)\(n:_/n:_))/np,
               ((n:_/pp)\(n:_/pp))/np]), !,
   Sem = lam(Q,lam(Z,lam(P,lam(Y,app(app(Z,lam(X,merge(app(Q,lam(Y,drs([],[Index:rel(X,Y,Sym,0)]))),
                                                       app(P,X)))),Y))))).  

semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[((np\np)\(np\np))/np,
               ((np\np)/(np\np))/np]), !,
   Sem = lam(Q1,lam(R,lam(Q2,lam(P,merge(app(Q1,lam(X,app(Q2,lam(Y,drs([],[Index:rel(Y,X,Sym,0)]))))),app(app(R,Q2),P)))))).


semlex(Cat,_Sym,_,Index,Sem):-
   Cat = (np/np)/n:_, !,
   Sem = lam(N,lam(Q,lam(P,merge(merge(drs([Index:Y],[]),app(N,Y)),app(Q,lam(X,merge(drs([],[Index:rel(X,Y,rel,0)]),app(P,X)))))))).


semlex(Cat,except,_:_:_,Index,Sem):-
   Cat = (np\np)/pp, !,
   Sem = lam(PP,lam(NP,lam(P,app(NP,lam(X,merge(drs([],[Index:not(app(PP,X))]),app(P,X))))))).

semlex(Cat,Sym,_:_:Sense,Index,Sem):-
   Cat = (np\np)/pp, !,
   Sem = lam(PP,lam(NP,lam(P,app(NP,lam(X,merge(drs([],[Index:pred(X,Sym,n,Sense)]),merge(app(PP,X),app(P,X)))))))).


semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[(np\np)/np,(np\np)\np,(np/np)/np]),
   member(Sym,['-lrb-','-lcb-','-lsb-','(','[']), !,
   Sem = lam(Q1,lam(Q2,lam(P,app(Q2,lam(X,app(Q1,lam(Y,merge(drs([],[Index:rel(X,Y,rel,0)]),app(P,X))))))))).

semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[(np\np)/np,(np\np)\np,(np/np)/np]), !,
   Sem = lam(Q1,lam(Q2,lam(P,app(Q2,lam(X,app(Q1,lam(Y,merge(drs([],[Index:rel(X,Y,Sym,0)]),app(P,X))))))))).



semlex(Cat,Sym,_,Index,Sem):- 
   member(Cat,[((np\np)/(s:to\np))/np,
               ((np\np)/(s:ng\np))/np]), !,
   Sem = lam(NP1,lam(VP,lam(NP2,lam(P,app(NP2,lam(X,merge(drs([Index:Y],[Index:rel(X,Y,Sym,0),
                                                                         Index:prop(Y,app(app(VP,NP1),lam(_,drs([],[]))))]),
                                                          app(P,X)))))))).

semlex(Cat,Sym,_,Index,Sem):- 
   member(Cat,[((np\np)/pp)/np]), !,
   Sem = lam(Q1,lam(PP,lam(Q2,lam(P,app(Q2,lam(X,app(Q1,lam(Y,merge(drs([],[Index:rel(X,Y,Sym,0)]),
                                                                    merge(app(PP,X),app(P,X))))))))))).

semlex((n:_/pp)/(s:adj\np),Sym,_,Index,Sem):-
   Sem = lam(VP,lam(PP,lam(X,app(app(VP,lam(P,app(P,X))),lam(E,merge(drs([],[Index:rel(X,E,Sym,0)]),app(PP,E))))))).

semlex(((s:wq/s:q)\(s:wq/s:q))/np,Sym,_,Index,Sem):- !,
   Sem = lam(NP,lam(U,lam(YNQ,lam(F,app(app(U,YNQ),lam(E,merge(app(NP,lam(X,drs([],[Index:rel(E,X,Sym,0)]))),app(F,E)))))))).


semlex(Cat,_,_,Index,Sem):-
   member(Cat,[((s:adj\np)/(s:adj\np))/n:_]), !, %  a bit
   Sem = lam(N,lam(A,lam(Q,lam(F,app(app(A,Q),lam(E,merge(merge(drs([Index:X],[Index:rel(E,X,rel,0)]),
                                                                 app(N,X)),
                                                           app(F,E)))))))).

semlex(Cat,by,_,Index,Sem):-
   Cat = ((s:pss\np)\(s:pss\np))/np, !,
   Sem = lam(Q2,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,app(Q2,lam(Y,merge(drs([],[Index:rel(E,Y,agent,0)]),app(F,E)))))))))).
                    
semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[((s:adj\np)\(s:adj\np))/np, % than
               ((s:adj\np)/(s:adj\np))/np, 
               ((s:X\np)\(s:X\np))/np, 
               ((s:X\np)/(s:X\np))/np, 
               ((s:X\np)\(s:X\np))\np, 
               ((s:X\np)/(s:X\np))\np]), !,
   Sem = lam(Q2,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,app(Q2,lam(Y,merge(drs([],[Index:rel(E,Y,Sym,0)]),app(F,E)))))))))).

% from 8% the week before
semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[(((s:X\np)\(s:X\np))\np)/np,
               (((s:X\np)\(s:X\np))/np)/np]), !,
   Sem = lam(Q3,lam(Q2,lam(V,lam(Q1,lam(F,app(app(V,Q1),lam(E,app(Q3,lam(Z,merge(drs([],[Index:rel(E,Z,Sym,0)]),
                                                                                 app(Q2,lam(Y,merge(drs([],[Index:rel(E,Y,rel,0)]),
                                                                                                    app(F,E)))))))))))))).

semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[(((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np)))/np,
               (((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))/np,
               (((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))\np]), !,
   Sem = lam(Q,lam(AV,lam(VP,lam(NP,lam(F,app(app(app(AV,VP),NP),lam(E,merge(app(Q,lam(Z,drs([],[Index:rel(E,Z,Sym,0)]))),
                                                                             app(F,E))))))))).

semlex(Cat,_Sym,_,Index,Sem):-
   member(Cat,[(((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np)))/n:_,
               (((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))/n:_,
               (((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))\n:_,
               (((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))/pp]), !,
   Sem = lam(N,lam(AV,lam(VP,lam(NP,lam(F,app(app(app(AV,VP),NP),lam(E,merge(merge(drs([Index:Z],[Index:rel(E,Z,rel,0)]),
                                                                                   app(N,Z)),
                                                                             app(F,E))))))))).

semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[(((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np)))/s:dcl,
               (((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))/s:dcl,
               (((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))\s:dcl]), !,
   Sem = lam(S,lam(AV,lam(VP,lam(NP,lam(F,app(app(app(AV,VP),NP),lam(E,merge(app(S,lam(Z,drs([],[Index:rel(E,Z,Sym,0)]))),
                                                                             app(F,E))))))))).

semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[(((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))/(s:pss\np),
               (((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))/(s:adj\np)]), !,
   Sem = lam(VP1,lam(AV,lam(VP2,lam(NP,lam(F,app(app(app(AV,VP2),NP),lam(E,merge(app(app(VP1,lam(P,merge(drs([[]:Z],[[]:pred(Z,thing,n,12)]),
                                                                                                         app(P,Z)))),lam(Z,drs([],[Index:rel(E,Z,Sym,0)]))),
                                                                                 app(F,E))))))))).

semlex(Cat,Sym,_,Index,Sem):-
   Cat = pp/np, !,
   Sem = lam(Q,lam(X,app(Q,lam(Y,drs([],[Index:rel(X,Y,Sym,0)]))))).

semlex(Cat,'$',_,Index,Sem):-
   Cat = pp/n:num, !,
   Sem = lam(N,lam(X,merge(drs([Index:Y],[Index:pred(Y,dollar,n,1),Index:rel(X,Y,rel,0)]),app(N,Y)))).

semlex(Cat,Sym,_,Index,Sem):-
   Cat = pp/n:_, !,
   Sem = lam(N,lam(X,merge(drs([Index:Y],[Index:rel(X,Y,Sym,0)]),app(N,Y)))).

semlex(Cat,Sym,_,Index,Sem):-
   Cat = (pp\np)/np, !,
   Sem = lam(Q1,lam(Q2,lam(X,merge(app(Q2,lam(Z,drs([],[Index:rel(X,Z,rel,0)]))),
                                   app(Q1,lam(Y,drs([],[Index:rel(X,Y,Sym,0)]))))))).

semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[pp/(s:ng\np),pp/(s:adj\np),pp/(s:b\np)]), !,
   Sem = lam(VP,lam(X,app(app(VP,lam(P,merge(drs([Index:Y],[Index:rel(X,Y,Sym,0)]),app(P,Y)))),lam(_,drs([],[]))))).


/* -------------------------------------------------------------------------
   Discourse connectors: if
------------------------------------------------------------------------- */

semlex(Cat,if,_,Index,Sem):-
  member(Cat,[((s:X\np)\(s:X\np))/s:dcl]), !,
  Sem = lam(S,lam(V,lam(Q,lam(F,drs([],[Index:imp(app(S,lam(_,drs([],[]))),
                                                  app(app(V,Q),F))]))))).


/* -------------------------------------------------------------------------
   Discourse connectors: where
------------------------------------------------------------------------- */

semlex(Cat,where,_,Index,Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/s:dcl]), !,
   Sem = lam(S,lam(V,lam(Q,lam(F,merge(merge(drs([Index:T],[Index:pred(T,location,n,1)]),
                                             app(app(V,Q),lam(E,merge(drs([],[Index:rel(E,T,loc_rel,0)]),app(F,E))))),
                                       app(S,lam(E,drs([],[Index:rel(E,T,loc_rel,0)])))))))).


/* -------------------------------------------------------------------------
   Discourse connectors: when
------------------------------------------------------------------------- */

semlex(Cat,when,_,Index,Sem):-
   option('--tense',true),
   member(Cat,[((s:X\np)\(s:X\np))/s:dcl]), !,
   Sem = lam(S,lam(V,lam(Q,lam(F,merge(merge(drs([Index:T],[Index:pred(T,time,n,1)]),
                                             app(S,lam(E,drs([],[Index:rel(E,T,temp_included,1)])))),
                                       app(app(V,Q),lam(E,merge(drs([],[Index:rel(E,T,temp_included,1)]),app(F,E))))))))).


/* -------------------------------------------------------------------------
   Discourse connectors: as does NP
------------------------------------------------------------------------- */

semlex(Cat,'as',_:_:Sense,Index,Sem):-
   Cat = ((s:X\np)\(s:X\np))/s:inv, !,
   Sem = lam(S,lam(V,lam(Q,lam(F,smerge(app(app(V,Q),lam(E,app(F,E))),
                                        app(S,lam(E,drs([],[Index:pred(E,as,a,Sense)])))))))).


/* -------------------------------------------------------------------------
   Discourse connectors
------------------------------------------------------------------------- */

semlex(Cat,Sym,_,Index,Sem):-
   option('--theory',sdrt),
   member(Cat,[((s:X\np)\(s:X\np))/s:_,
               ((s:X\np)/(s:X\np))/s:_]), !,
   Sem = lam(S,lam(V,lam(Q,lam(F,sdrs([sub(lab(K1,B1),lab(K2,B2))],[Index:rel(K1,K2,Sym)]))))),
   B1 = app(app(V,Q),F),
   B2 = app(S,lam(_,drs([],[]))).

semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/s,
               ((s:X\np)/(s:X\np))/s,
               ((s:X\np)\(s:X\np))/s:_,
               ((s:X\np)/(s:X\np))/s:_]), !,
   Sem = lam(S,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,merge(drs([Index:Z],
                                                              [Index:rel(E,Z,Sym,0),
                                                               Index:prop(Z,app(S,lam(_,drs([],[]))))]),
                                                          app(F,E)))))))).




semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[((s:X\np)/s:_)/(s:X\np)]), !,
   Sem = lam(V,lam(S,lam(Q,lam(F,app(app(V,Q),lam(E,merge(drs([Index:Z],
                                                              [Index:rel(E,Z,Sym,0),
                                                               Index:prop(Z,app(S,lam(_,drs([],[]))))]),
                                                          app(F,E)))))))).

semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[(((s:X\np)\(s:X\np))\np)/s:dcl ]), !,
   Sem = lam(S,lam(NP,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,merge(drs([Index:Z],
                                                                     [Index:rel(E,Z,Sym,0),
                                                                      Index:prop(Z,app(S,lam(_,drs([],[]))))]),
                                                                 merge(app(NP,lam(U,drs([],[[]:rel(E,U,rel,0)]))),
                                                                       app(F,E)))))))))).


semlex((np\np)/s:_,Sym,_,Index,Sem):- !,
   Sem = lam(S,lam(Q,lam(P,app(Q,lam(X,merge(drs([Index:Z],[Index:rel(X,Z,Sym,0),
                                                            Index:prop(Z,app(S,lam(_,drs([],[]))))]),
                                             app(P,X))))))).

semlex((n:F\n:F)/s:_, Sym,_,Index,Sem):- !,
   Sem = lam(S,lam(P,lam(X,merge(drs([Index:Z],[Index:rel(X,Z,Sym,0),
                                                Index:prop(Z,app(S,lam(_,drs([],[]))))]),
                                 app(P,X))))).


semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[((np\np)\(np\np))/s:dcl]), !, 
   Sem = lam(S,lam(M,lam(NP,lam(Q,app(app(M,NP),
                                      lam(X,merge(app(S,lam(E,drs([],[Index:rel(E,X,Sym,0)]))),
                                                  app(Q,X)))))))).


/* -------------------------------------------------------------------------
   Prepositions:  "VP prep VPing"
------------------------------------------------------------------------- */

semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/(s:ng\np),
               ((s:X\np)/(s:X\np))/(s:ng\np)]), !,
   Sem = lam(VA,lam(VM,lam(Q,lam(F,app(Q,
                                       lam(Z,app(app(VM,lam(P,app(P,Z))),
                                                 lam(E,merge(app(F,E),
                                                             app(app(VA,lam(P,app(P,Z))),
                                                                 lam(G,drs([],[Index:rel(G,E,Sym,0)])))))))))))).



%%
%% Need to check this: discourse referent is introduced without properties
%% Similar semantics as above case might do.
%%
semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/(s:adj\np), 
               ((s:X\np)\(s:X\np))/(s:dcl\np), % which
               ((s:X\np)\(s:X\np))/(s:dcl/np), % who
               ((s:X\np)\(s:X\np))/(s:pss\np), 
               ((s:X\np)\(s:X\np))/(s:pt\np), 
               ((s:X\np)\(s:X\np))/(s:b\np),
               ((s:X\np)\(s:X\np))/(s:to\np), 

               ((s:X\np)/(s:X\np))/(s:adj\np), 
               ((s:X\np)/(s:X\np))/(s:dcl\np), 
               ((s:X\np)/(s:X\np))/(s:pss\np), 
               ((s:X\np)/(s:X\np))/(s:pt\np), 
               ((s:X\np)/(s:X\np))/(s:b\np), 
               ((s:X\np)/(s:X\np))/(s:to\np), 

               ((s:X\np)/(s:X\np))\(s:adj\np), 

               ((s:adj\np)\(s:adj\np))/(s:pss\np), 
               ((s:adj\np)\(s:adj\np))/(s:adj\np), 
               ((s:adj\np)/(s:adj\np))/(s:adj\np), 
               ((s:adj\np)\(s:adj\np))/(s:pss\np), 
               ((s:adj\np)\(s:adj\np))/(s:to\np), 
               ((s:adj\np)/(s:to\np))/(s:adj\np), 
               ((s:adj\np)\(s:adj\np))/(s:ng\np), 
               ((s:adj\np)\(s:adj\np))/(s:dcl\np), 
               ((s:adj\np)/(s:for\np))/(s:adj\np), 

               ((s:b\np)/(s:to\np))/(s:adj\np), 
               ((s:b\np)/(s:ng\np))/(s:adj\np), 
               ((s:b\np)/(s:dcl\np))/(s:adj\np), 
               ((s:pt\np)/(s:to\np))/(s:adj\np), 
               ((s:pt\np)/(s:ng\np))/(s:adj\np), 
               ((s:dcl\np)/(s:b\np))/(s:adj\np), 
               ((s:dcl\np)/(s:to\np))/(s:adj\np), 
               ((s:dcl\np)/(s:adj\np))/(s:adj\np)]), !,
   Sem = lam(VP,lam(V,lam(Q,lam(F,app(app(V,Q),
                                      lam(E,app(app(VP,
                                                    lam(P,merge(drs([Index:Z],[]),app(P,Z)))),
                                                lam(Y,merge(drs([],[Index:rel(E,Y,Sym,0)]),app(F,E)))))))))).


/* -------------------------------------------------------------------------
   Control Prepositions (NP)
------------------------------------------------------------------------- */

semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[(((s:X\np)\(s:X\np))/(s:ng\np))/np,
               (((s:X\np)\(s:X\np))/(s:pt\np))/np,
               (((s:X\np)\(s:X\np))/(s:pss\np))/np,
               (((s:X\np)\(s:X\np))/(s:b\np))/np,
               (((s:X\np)\(s:X\np))/(s:to\np))/np]), !,
   Sem =  lam(NP,lam(VP,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,app(app(VP,lam(P,app(NP,lam(Y,merge(drs([],[Index:rel(E,Y,Sym,0)]),
                                                                                                 app(P,Y)))))),F)))))))).

/* -------------------------------------------------------------------------
   Control Prepositions (N)
   Example: "in", as in: "I had a plan in place to respond."
%------------------------------------------------------------------------- */

semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[(((s:X\np)\(s:X\np))/(s:ng\np))/n:_,
               (((s:X\np)\(s:X\np))/(s:pt\np))/n:_,
               (((s:X\np)\(s:X\np))/(s:pss\np))/n:_,
               (((s:X\np)\(s:X\np))/(s:b\np))/n:_,
               (((s:X\np)\(s:X\np))/(s:to\np))/n:_]), !,
   Sem = lam(N,lam(VP,lam(Y,lam(Q,lam(F,app(Q,lam(U,app(app(Y,lam(P,app(P,U))),lam(E,merge(drs([Index:Z,Index:K],
                                                                     [Index:rel(E,Z,Sym,0),
								      Index:prop(K,app(app(VP,lam(P,app(P,U))),
                                                                                       lam(_,drs([],[])))),
                                                                      Index:rel(E,K,theme,0)]),
                                                                 merge(app(N,Z),app(F,E)))))))))))).


semlex(Cat,Sym,_,Index,Sem):-
   Cat = (((s:X\np)\(s:X\np))/pp)/np, !,
   Sem =  lam(NP,lam(PP,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,merge(app(NP,lam(Y,drs([],[Index:rel(E,Y,Sym,0)]))),
                                                                   merge(app(PP,E),app(F,E)))))))))).

semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[((s:_\s:_)/pp)/np,
               ((s:_/s:_)/pp)/np]), !,   
   Sem = lam(Q,lam(PP,lam(S,lam(F,app(S,lam(E,merge(app(Q,lam(Y,drs([],[Index:rel(E,Y,Sym,0)]))),
                                                    merge(app(PP,E),app(F,E))))))))).

semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[(s:wq\s:wq)/np,
               (s:X/s:X)\np,
               (s:X/s:X)/np,
               (s:X\s:X)/np]), !,   
   Sem = lam(Q,lam(S,lam(F,app(S,lam(E,merge(app(Q,lam(Y,drs([],[Index:rel(E,Y,Sym,0)]))),app(F,E))))))).


semlex(Cat,this,_,Index,Sem):-
   member(Cat,[(s:X/s:X)/n:_,
               (s:X\s:X)/n:_]), !,
   Sem = lam(P,lam(S,lam(F,app(S,lam(E,merge(drs([Index:Y],
                                                 [Index:pred(Y,current,a,1),
                                                  Index:rel(E,Y,rel,0)]),
                                             merge(app(P,Y),
                                                   app(F,E)))))))).

semlex(Cat,_Sym,_,Index,Sem):-
   member(Cat,[(s:X/s:X)/n:_,
               (s:X\s:X)/n:_]), !,
   Sem = lam(P,lam(S,lam(F,app(S,lam(E,merge(drs([Index:Y],
                                                 [Index:rel(E,Y,rel,0)]),
                                             merge(app(P,Y),
                                                   app(F,E)))))))).


% With violence escalating in Kosovo, S
semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[((s:X/s:X)/(s:ng\np))/np,
               ((s:X/s:X)/(s:pt\np))/np,
               ((s:X/s:X)/(s:b\np))/np,
               ((s:X/s:X)/(s:adj\np))/np]), !,   
   Sem = lam(Q,lam(VP,lam(S,lam(F,app(S,lam(E,app(app(VP,lam(P,app(Q,lam(Y,merge(drs([],[Index:rel(E,Y,Sym,0)]),merge(app(P,Y),app(F,E))))))),lam(_,drs([],[]))))))))).

semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[((s:X/s:X)\np)/np]), !,
   Sem = lam(Q1,lam(Q2,lam(S,lam(F,app(S,lam(E,app(Q2,lam(Y,app(Q1,lam(Z,merge(drs([],[Index:rel(Y,Z,Sym,0)]),app(F,E)))))))))))).

semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[((s:X/s:X)\np)/s:dcl]), !,
   Sem = lam(S1,lam(Q2,lam(S,lam(F,app(S,lam(E,app(Q2,lam(Y,app(S1,lam(E,merge(drs([],[Index:rel(E,Y,Sym,0)]),app(F,E)))))))))))).

% Where on the body ...
semlex(Cat,Sym,_,Index,Sem):-
   Cat = ((s:wq/(s:q/pp))\(s:wq/(s:q/pp)))/np,
   Sem = lam(NP,lam(Q,lam(VP,lam(F,app(app(Q,VP),lam(E,merge(app(NP,lam(X,drs([],[Index:rel(E,X,Sym,0)]))),
                                                             app(F,E)))))))). 


/* -------------------------------------------------------------------------
   instead (of)
------------------------------------------------------------------------- */

semlex(Cat,instead,_,Index,Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/pp,
               ((s:X\np)/(s:X\np))/pp,
               ((s:adj\np)\(s:adj\np))/pp]), !,
   Sem = lam(PP,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,drs([],[Index:not(merge(app(PP,E),app(F,E)))]))))))). 


/* -------------------------------------------------------------------------
   Double prepositions, such as "out of", "together with"
------------------------------------------------------------------------- */

semlex(Cat,Sym,Pos:_:_,Index,Sem):-
   member(Pos,['VBG','VBN']),
   member(Cat,[((s:X\np)\(s:X\np))/pp,
               ((s:X\np)/(s:X\np))/pp,
               ((s:adj\np)\(s:adj\np))/pp]), !,
   Sem = lam(PP,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,merge(drs([Index:Y],[Index:pred(Y,Sym,v,0)]),
                                                           merge(app(PP,Y),app(F,E))))))))). 

semlex(Cat,_Sym,_,_Index,Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/pp,
               ((s:X\np)/(s:X\np))/pp,
               ((s:adj\np)\(s:adj\np))/pp]), !,
   Sem = lam(PP,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,merge(app(PP,E),app(F,E)))))))). 


/* -------------------------------------------------------------------------
   Double prepositions, such as "Cycling in the north of France, ..."
------------------------------------------------------------------------- */

semlex(Cat,Sym,Pos:_:_,Index,Sem):-
   member(Pos,['VBG','VBN']),
   member(Cat,[(s:X/s:X)/pp,
               (s:X\s:X)/pp]), !, 
   Sem = lam(PP,lam(S,lam(F,app(S,lam(E,merge(drs([Index:Y],[Index:pred(Y,Sym,v,0)]),
                                              merge(app(PP,Y),
                                                    app(F,E)))))))). 

semlex(Cat,Sym,_:_:Sense,Index,Sem):-
   member(Cat,[(s:X/s:X)/pp,
               (s:X\s:X)/pp]), !, 
   Sem = lam(PP,lam(S,lam(F,app(S,lam(E,merge(drs([],[Index:pred(E,Sym,a,Sense)]),
                                              merge(app(PP,E),
                                                    app(F,E)))))))). 



/* -------------------------------------------------------------------------
   VP adverb modifier (negation)
------------------------------------------------------------------------- */

semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[((s:X\np)/(s:X\np))/((s:X\np)/(s:X\np)),
               ((s:X\np)/(s:X\np))\((s:X\np)/(s:X\np)),
               ((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)),
               ((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np))]), 
   notSymbol(Sym), !,
   Sem = lam(AV,lam(VP,lam(NP,lam(F,drs([],[Index:not(app(app(app(AV,VP),NP),lam(E,app(F,E))))]))))).


/* -------------------------------------------------------------------------
   VP adverb modifier (Cardinals that function as modifiers)
------------------------------------------------------------------------- */

semlex(Cat,Sym,'CD':_:_,Index,Sem):-
   member(Cat,[((s:X\np)/(s:X\np))/((s:X\np)/(s:X\np)),
               ((s:X\np)/(s:X\np))\((s:X\np)/(s:X\np)),
               ((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)),  
               ((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np))]), 
   string2digit(Sym,Digit), !,
   Sem = lam(AV,lam(VP,lam(NP,lam(F,app(app(app(AV,VP),NP),lam(E,merge(drs([Index:Y],[Index:card(Y,Digit,eq),
                                                                                      Index:rel(E,Y,rel,0)]),
                                                                       app(F,E)))))))).

/* -------------------------------------------------------------------------
   VP adverb modifier (NPs that function as modifiers)
------------------------------------------------------------------------- */

semlex(Cat,Sym,Pos:_:Sense,Index,Sem):-
   member(Cat,[((s:X\np)/(s:X\np))/((s:X\np)/(s:X\np)),
               ((s:X\np)/(s:X\np))\((s:X\np)/(s:X\np)),
               ((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)),  
               ((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np))]), 
   member(Pos,['NN','NNS','NNP','NNPS']), !,
   Sem = lam(AV,lam(VP,lam(NP,lam(F,app(app(app(AV,VP),NP),lam(E,merge(drs([Index:Y],[Index:pred(Y,Sym,n,Sense),
                                                                                      Index:rel(E,Y,rel,0)]),
                                                                       app(F,E)))))))).


/* -------------------------------------------------------------------------
   VP adverb modifier (intersective)
------------------------------------------------------------------------- */

semlex(Cat,Sym,_:_:Sense,Index,Sem):-
   member(Cat,[((s:X\np)/(s:X\np))/((s:X\np)/(s:X\np)),
               ((s:X\np)/(s:X\np))\((s:X\np)/(s:X\np)),
               ((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)),  
               ((s:adj\np)\(s:adj\np))/((s:adj\np)\(s:adj\np)),  
               ((s:adj\np)/(s:adj\np))/((s:adj\np)/(s:adj\np)),
               ((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np))]), 
   Sem = lam(AV,lam(VP,lam(NP,lam(F,app(app(app(AV,VP),NP),lam(E,merge(drs([],[Index:pred(E,Sym,a,Sense)]),app(F,E)))))))).



% VP adverb modifier (negation)
semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[(((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np)))/(((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np))),
               (((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))/(((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np))),
               (((s:adj\np)/(s:adj\np))/((s:adj\np)/(s:adj\np)))/(((s:adj\np)/(s:adj\np))/((s:adj\np)/(s:adj\np)))]), 
   notSymbol(Sym), !,
   Sem = lam(M,lam(AV,lam(VP,lam(NP,lam(F,drs([],[Index:not(app(app(app(app(M,AV),VP),NP),lam(E,app(F,E))))])))))).

semlex(Cat,Sym,_:_:Sense,Index,Sem):-
   member(Cat,[(((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))/((s:X\np)\(s:X\np))]), !,
   Sem = lam(AV1,lam(AV2,lam(VP,lam(NP,lam(F,app(app(app(AV2,app(AV1,VP)),NP),
                                             lam(E,merge(drs([],[Index:pred(E,Sym,a,Sense)]),app(F,E))))))))).

% VP adverb modifier (intersective)
semlex(Cat,Sym,_:_:Sense,Index,Sem):-
   member(Cat,[(((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np)))/(((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np))), 
               (((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))/(((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))]),
   Sem = lam(M,lam(AV,lam(VP,lam(NP,lam(F,app(app(app(app(M,AV),VP),NP),lam(E,merge(drs([],[Index:pred(E,Sym,a,Sense)]),app(F,E))))))))).


semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[(s:X/s:X)/(s:_\np),
               (s:X\s:X)/(s:_\np)]), !,
   Sem = lam(VP,lam(S,lam(G,app(app(VP,lam(P,merge(drs([Index:Y],[Index:pred(Y,thing,n,12)]),app(P,Y)))),
                                lam(F,merge(app(G,F),app(S,lam(E,drs([],[Index:rel(E,F,Sym,0)]))))))))).


/* -------------------------------------------------------------------------
   Preposition (in front of WH, as in "From where ...")
------------------------------------------------------------------------- */

semlex((s:wq/(s:q/pp))/(s:wq/(s:q/np)),Sym,_,Index,Sem):-
   Sem = lam(Q,lam(W,lam(F,app(app(Q,V),F)))),
   V = lam(N,lam(E,app(N,lam(X,app(app(W,lam(Y,drs([],[Index:rel(Y,X,Sym,0)]))),E))))).

semlex((s:wq/(s:q/np))/(s:wq/(s:q/np)),_Sym,_,_Index,Sem):-
   Sem = lam(X,X).


/* -------------------------------------------------------------------------
   Prep + NP + VP (... results in shareholders receiving ...)
------------------------------------------------------------------------- */

semlex((pp/(s:_\np))/np,Sym,_,Index,Sem):- !,
   Sem = lam(NP,lam(VP,lam(E,app(app(VP,NP),lam(F,drs([],[Index:rel(E,F,Sym,0)])))))).


/* -------------------------------------------------------------------------
   Possessive 
------------------------------------------------------------------------- */

semlex(Cat,_Lemma,_,Index,Sem):-
   member(Cat,[(np:nb/n:_)/(n:_/n:_),
               (np/n:_)/(n:_/n:_)]), !,
   Sem = lam(S,lam(P,lam(Q,merge(drs([Index:U],[]),
                                 merge(app(app(S,lam(X,merge(app(P,X),drs([Index:Y],[Index:rel(X,Y,of,0)])))),U),
                                       app(Q,U)))))).



semlex(Cat,_,_,Index,Sem):- 
   member(Cat,[(np/n:_)\np, 
               (np:nb/n:_)\np]), !,
   Sem = lam(NP,lam(N,lam(P,app(NP,lam(Y,alfa(def,merge(drs([Index:X],[Index:rel(X,Y,of,0)]),
                                                        app(N,X)),
                                                  app(P,X))))))).

semlex(Cat,_,_,Index,Sem):- 
   member(Cat,[(np/(n:_/pp))\np, 
               (np:nb/(n:_/pp))\np]), !,
   Sem = lam(NP,lam(RN,lam(P,app(NP,lam(Y,alfa(def,merge(drs([Index:X],[]),
                                                         app(app(RN,lam(Z,drs([],[Index:rel(Z,Y,of,0)]))),X)),
                                                   app(P,X))))))).


semlex(Cat,_,_,Index,Sem):- 
   member(Cat,[((np:nb/n)/(n:_/n:_))\np,
               ((np/n:_)/(n:_/n:_))\np]), !,
   Sem = lam(N,lam(S,lam(P,lam(Q,merge(drs([Index:U],[]),
                                       merge(app(app(S,lam(X,merge(app(P,X),
                                                                   app(N,lam(Y,drs([],[Index:rel(X,Y,of,0)])))))),U),
                                             app(Q,U))))))).

semlex((n:_/n:_)\n:_,_,_,Index,Sem):- !,
   Sem = lam(N1,lam(N2,lam(X,merge(drs([[]:Y],[]),
                                   merge(app(N1,Y),
                                         merge(app(N2,X),
                                               drs([],[Index:rel(X,Y,of,0)]))))))).
semlex(Cat,_,_,Index,Sem):- 
   member(Cat,[((s:wq/(s:q/np))/n:_)\(s:wq/(s:q/np)),
               ((s:wq\(s:dcl/np))/n:_)\(s:wq\(s:dcl/np)),
               ((s:wq/(s:dcl\np))/n:_)\(s:wq/(s:dcl\np))]), !,

   XXX = lam(U,lam(E,app(U,lam(Y,alfa(def,merge(drs([Index:X],[Index:rel(X,Y,of,0)]),
                                          app(N,X)),
                                    app(app(V,lam(Q,app(Q,X))),E)))))),
                                  
   Sem = lam(NP,lam(N,lam(V,lam(P,app(app(NP,XXX),P))))).

%  Sem = lam(NP,lam(N,lam(V,app(V,lam(P2,app(NP,lam(V2,app(V2,lam(Y,alfa(def,merge(drs([Index:X],[Index:rel(X,Y,of,0)]),app(N,X)),app(P2,X))))))))))).


/* -------------------------------------------------------------------------
   Emphasising Pronouns
------------------------------------------------------------------------- */

semlex(np\np, himself, _,Index,Sem):- !,
   Sem = lam(Q,lam(P,app(Q,lam(X,merge(drs([],[Index:pred(X,male,a,0)]),app(P,X)))))).

semlex(np\np, herself, _,Index,Sem):- !,
   Sem = lam(Q,lam(P,app(Q,lam(X,merge(drs([],[Index:pred(X,female,a,0)]),app(P,X)))))).

semlex(np\np, itself, _,Index,Sem):- !,
   Sem = lam(Q,lam(P,app(Q,lam(X,merge(drs([],[Index:pred(X,neuter,a,0)]),app(P,X)))))).

semlex(np\np, Sym, _,Index,Sem):-
   member(Sym,[myself,yourself,ourselves,themselves]), !,
   Sem = lam(Q,lam(P,app(Q,lam(X,merge(drs([],[Index:pred(X,person,n,1)]),app(P,X)))))).


/* -------------------------------------------------------------------------
   NP modifiers: floating quantifiers 
------------------------------------------------------------------------- */

semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[np\np, np/np]), 
   member(Sym,[all,each]), !,
   Sem = lam(Q,lam(P,drs([],[Index:not(app(Q,lam(X,drs([],[Index:not(app(P,X))]))))]))).


/* -------------------------------------------------------------------------
   NP modifiers: deictics (not implemented yet)
------------------------------------------------------------------------- */

semlex(Cat,Sym,_,_Index,Sem):-
   member(Cat,[np\np, np/np]), 
   member(Sym,[there,here,ago,such,now]), !,
   Sem = lam(Q,lam(P,app(Q,lam(X,app(P,X))))).


/* -------------------------------------------------------------------------
   NP modifiers: only
------------------------------------------------------------------------- */

semlex(Cat,only,_,Index,Sem):-
   member(Cat,[np\np, np/np]), !,
   Sem = lam(NP,lam(P,alfa(fac,merge(drs([Index:Z],[]),
                                     app(NP,lam(X,merge(app(P,X),
                                                        drs([],[[]:eq(Z,X)]))))),
                               drs([],[Index:imp(merge(drs([Index:Y],[]),app(P,Y)),
                                                 drs([],[Index:eq(Z,Y)]))])))).


/* -------------------------------------------------------------------------
   NP modifiers: negation
------------------------------------------------------------------------- */

semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[np\np, np/np]), 
   notSymbol(Sym), !,
   Sem = lam(NP,lam(P,drs([],[Index:not(app(NP,P))]))).

semlex(Cat,Sym,_,Index,Sem):-
   Cat = (((np\np)/(np\np))/((np\np)/(np\np))),
   notSymbol(Sym), !,
   Sem = lam(A1,lam(A2,lam(NP,lam(P,drs([],[Index:not(app(app(app(A1,A2),NP),P))]))))).


/* -------------------------------------------------------------------------
   NP modifiers
------------------------------------------------------------------------- */

semlex(Cat,Sym,Pos:Ne:Sense,Index,Sem):-
   member(Pos,['NNP','NNPS']),
   member(Cat,[np\np, np/np]), !,
   netype(Ne,Type),
   Sem = lam(Q,lam(P,app(Q,lam(X,merge(drs([],[Index:named(X,Sym,Type,Sense)]),
                                       app(P,X)))))).

semlex(Cat,Sym,Pos:_:Sense,Index,Sem):-
   member(Pos,['IN','RB','JJ','JJR','RBR']),
   member(Cat,[np\np, np/np]), !,
   Sem = lam(Q,lam(P,app(Q,lam(X,merge(drs([],[Index:pred(X,Sym,a,Sense)]),
                                       app(P,X)))))).

semlex(Cat,Sym,_:_:Sense,Index,Sem):-
   member(Cat,[np\np, np/np]), !,
   Sem = lam(Q,lam(P,app(Q,lam(X,merge(drs([Index:Y],[Index:pred(Y,Sym,n,Sense),
                                                      Index:rel(X,Y,rel,0)]),
                                       app(P,X)))))).


/* -------------------------------------------------------------------------
   NP modifiers (superlative contruction)
------------------------------------------------------------------------- */

semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[d/np]), !,
   Sem = lam(X,lam(Y,drs([],[Index:rel(Y,X,Sym,0)]))).


/* -------------------------------------------------------------------------
   NP modifier modifiers: deitics
------------------------------------------------------------------------- */

semlex(Cat,Sym,_,_Index,Sem):-
   member(Cat,[(np\np)/(np\np),
               (np\np)\(np\np),
               (np/np)/(np/np)]), 
   member(Sym,[there,here,ago,such,now]), !,
   Sem = lam(M,lam(Q,lam(P,app(app(M,Q),P)))). 


/* -------------------------------------------------------------------------
   NP modifier modifiers (proper names)
------------------------------------------------------------------------- */

semlex(Cat,Sym,Pos:Ne:Sense,Index,Sem):-
   member(Pos,['NNP','NNPS']),
   member(Cat,[(np\np)/(np\np),
               (np\np)\(np\np),
               (np/np)/(np/np)]), !, 
   netype(Ne,Type),
   Sem = lam(M,lam(Q,lam(P,app(app(M,Q),lam(X,merge(drs([],[Index:named(X,Sym,Type,Sense)]),
                                                    app(P,X))))))).

/* -------------------------------------------------------------------------
   NP modifier modifiers (not)
------------------------------------------------------------------------- */

semlex(Cat,not,_,Index,Sem):-
   member(Cat,[(np\np)/(np\np),
               (np\np)\(np\np),
               (np/np)/(np/np)]), !, 
%   Sem = lam(M,lam(Q,lam(P,app(app(M,Q),lam(X,drs([],[Index:not(app(P,X))])))))).
   Sem = lam(M,lam(Q,lam(P,drs([],[Index:not(app(app(M,Q),lam(X,app(P,X))))])))).


/* -------------------------------------------------------------------------
   NP modifier modifiers
------------------------------------------------------------------------- */

semlex(Cat,Sym,_:_:Sense,Index,Sem):-
   member(Cat,[(np\np)/(np\np),
               (np\np)\(np\np),
               (np/np)/(np/np)]), !, 
   Sem = lam(M,lam(Q,lam(P,app(app(M,Q),lam(X,merge(drs([],[Index:pred(X,Sym,n,Sense)]),
                                                    app(P,X))))))).



/* -------------------------------------------------------------------------
   NP modifier modifiers, superlative ("most notably")
------------------------------------------------------------------------- */

semlex(Cat,_Sym,_,Index,Sem):-
   member(Cat,[(np/np)/(d/np)]), !, 
   Sem = lam(R,lam(Q,lam(P,app(Q,lam(X,merge(drs([],[Index:imp(drs([[]:Y],[[]:not(drs([],[[]:eq(X,Y)]))]),
                                                               app(app(R,X),Y))]),
                                             app(P,X))))))).


/* -------------------------------------------------------------------------
   NPs that function as S modifiers
------------------------------------------------------------------------- */

semlex(Cat,Sym,Pos:Ne:Sense,Index,Sem):-
   category(smod,Cat,Sym), 
   member(Pos,['NNP','NNPS']), !,
   netype(Ne,Type),
   Sem = lam(S,lam(F,app(S,lam(E,merge(drs([Index:X],[Index:named(X,Sym,Type,Sense),
                                                      Index:rel(E,X,rel,0)]),app(F,E)))))).

semlex(Cat,Sym,Pos:_:Sense,Index,Sem):-
   category(smod,Cat,Sym), 
   member(Pos,['NN','NNS']), !,
   Sem = lam(S,lam(F,app(S,lam(E,merge(drs([Index:X],[Index:pred(X,Sym,n,Sense),
                                                      Index:rel(E,X,rel,0)]),app(F,E)))))).

semlex(Cat,Sym,_,Index,Sem):-
   category(smod,Cat,Sym), 
   member(Sym,[everywhere,nowhere,anywhere,somewhere]), !,
   Sem = lam(S,lam(F,app(S,lam(E,merge(drs([Index:X],[Index:pred(X,location,n,1),
                                                      Index:rel(E,X,loc_rel,0)]),app(F,E)))))).



/* -------------------------------------------------------------------------
   S modifiers
------------------------------------------------------------------------- */

semlex(Cat,Sym,_,Index,Sem):-
   notSymbol(Sym),
   category(smod,Cat,Sym), !,
   Sem = lam(S,lam(F,drs([],[Index:not(app(S,F))]))).

semlex(Cat,Sym,_:_:Sense,Index,Sem):-
   category(smod,Cat,Sym), !,
   Sem = lam(S,lam(F,app(S,lam(E,merge(drs([],[Index:pred(E,Sym,a,Sense)]),app(F,E)))))).


/* -------------------------------------------------------------------------
   S modifier modifiers
------------------------------------------------------------------------- */

semlex(Cat,Sym,_,Index,Sem):- 
   notSymbol(Sym), 
   member(Cat,[(s:X/s:X)/(s:X/s:X),
               (s:X/s:X)\(s:X/s:X),
               (s:X\s:X)/(s:X\s:X),
               (s:X\s:X)\(s:X\s:X)]), !, 
   Sem = lam(M,lam(S,lam(F,drs([],[Index:not(app(app(M,S),F))])))).

semlex(Cat,_Sym,_,_Index,Sem):- 
   member(Cat,[(s:X/s:X)/(s:X/s:X),
               (s:X/s:X)\(s:X/s:X),
               (s:X\s:X)/(s:X\s:X),
               (s:X\s:X)\(s:X\s:X)]), !, 
   Sem = lam(M,lam(S,lam(F,app(app(M,S),F)))).

semlex(Cat,Sym,_,Index,Sem):- 
   member(Cat,[((s:X/s:X)/(s:X/s:X))/np,
               ((s:X/s:X)\(s:X/s:X))/np,
               ((s:X\s:X)/(s:X\s:X))/np,
               ((s:X\s:X)\(s:X\s:X))/np]), !, 
   Sem = lam(Q,lam(M,lam(S,lam(F,app(app(M,S),lam(E,merge(app(Q,lam(Y,drs([],[Index:rel(E,Y,Sym,0)]))),app(F,E)))))))).

semlex(Cat,Sym,_,Index,Sem):- 
   member(Cat,[((s:X/s:X)/(s:X/s:X))/s:dcl,
               ((s:X/s:X)\(s:X/s:X))/s:dcl,
               ((s:X\s:X)/(s:X\s:X))/s:dcl,
               ((s:X\s:X)\(s:X\s:X))/s:dcl]), !, 
   Sem = lam(S1,lam(M,lam(S2,lam(F,merge(drs([Index:E,Index:Z,Index:Y],
                                       [Index:prop(E,drs([],[Index:rel(Z,Y,Sym,0)])),
                                        Index:prop(Z,app(S1,lam(_,drs([],[])))),
                                        Index:prop(Y,app(app(M,S2),lam(_,drs([],[]))))]),
                                   app(F,E)))))).


/* -------------------------------------------------------------------------
   Punctuation
------------------------------------------------------------------------- */

semlex(Cat,Sym,_,_Index,Sem):-
   category(punctuation,Cat,Sym), !,
   Sem = lam(X,X).



/* -------------------------------------------------------------------------
   Funny modifiers: for
------------------------------------------------------------------------- */

semlex((s:for/(s:to\np))/np,_,_,_,lam(NP,lam(VP,app(VP,NP)))):- !.


/* -------------------------------------------------------------------------
   Mostly Temporal modifiers: "every month", "this week", "Nov. 29"
------------------------------------------------------------------------- */

semlex(Cat,this,_,Index,Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/n:_,
               ((s:X\np)/(s:X\np))/n:_]), !,
   Sem = lam(N,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,alfa(def,merge(drs([Index:Y],
                                                                       [Index:pred(Y,current,a,1)]),
                                                                   app(N,Y)),
                                                             merge(drs([],[Index:rel(E,Y,rel,0)]),
                                                                   app(F,E))))))))).

semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/n:_,
               ((s:X\np)/(s:X\np))/n:_]), 
   member(Sym,[the,that,these,those]), !,
   Sem = lam(N,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,alfa(def,merge(drs([Index:Y],[]),
                                                                   app(N,Y)),
                                                             merge(drs([],[Index:rel(E,Y,rel,0)]),
                                                                   app(F,E))))))))).

semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/n:_,
               ((s:X\np)/(s:X\np))/n:_]), 
   member(Sym,[every,each,all,any,either]), !,
   Sem = lam(N,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,merge(drs([],[Index:imp(merge(drs([Index:Y],[]),app(N,Y)),
                                                                            drs([],[Index:rel(E,Y,rel,0)]))]),
                                                          app(F,E)))))))).

semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/n:_,
               ((s:X\np)/(s:X\np))/n:_]), 
   member(Sym,[a,an,some]), !,
   Sem = lam(N,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,merge(drs([Index:Y],[Index:rel(E,Y,rel,0)]),
                                                          merge(app(N,Y),app(F,E))))))))).

semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/n:_,
               ((s:X\np)/(s:X\np))/n:_]), 
   member(Sym,[no]), !,
   Sem = lam(N,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,drs([],[Index:not(merge(drs([Index:Y],[Index:rel(E,Y,rel,0)]),
                                                                            merge(app(N,Y),app(F,E))))]))))))).

semlex(Cat,Sym,'NNP':'I-DAT':_,Index,Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/n:num,
               ((s:X\np)/(s:X\np))/n:num]), 
   month(Sym,MID), !,
   Sem = lam(P,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,merge(drs([Index:Y],[Index:timex(Y,date([]:'+',[]:'XXXX',Index:MID,[]:'XX')),
                                                                         Index:rel(E,Y,temp_rel,0)]),
                                                          merge(app(P,Y),app(F,E))))))))).

semlex(Cat,Sym,_:_:Sense,Index,Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/n:_,
               ((s:X\np)/(s:X\np))/n:_]), !,
   Sem = lam(P,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,merge(drs([Index:Y],[Index:pred(Y,Sym,n,Sense),
                                                               Index:rel(E,Y,rel,0)]),
                                                          merge(app(P,Y),app(F,E))))))))).




/* -------------------------------------------------------------------------
   Noun subcategorising for sentence
------------------------------------------------------------------------- */

semlex(Cat,Sym,_:_:Sense,Index,Sem):-
   Cat = n:_/s:_, !,
   Sem = lam(S,lam(X,drs([Index:B],
                         [Index:pred(X,Sym,n,Sense),
                          Index:rel(X,B,theme,0),
                          Index:prop(B,app(S,lam(_,drs([],[]))))]))).

semlex(Cat,Sym,_:_:Sense,Index,Sem):-
   Cat = (n:_/pp)/s:_, !, 
   Sem = lam(S,lam(P,lam(X,merge(drs([Index:B],
                                     [Index:pred(X,Sym,n,Sense),
                                      Index:rel(X,B,theme,0),
                                      Index:prop(B,app(S,lam(_,drs([],[]))))]),app(P,X))))).

semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[pp/s:dcl,
               pp/s:qem,
               pp/s]), !, 
   Sem = lam(S,lam(E,app(S,lam(X,drs([],[Index:rel(E,X,Sym,0)]))))).


/* -------------------------------------------------------------------------
   NP modifying noun
------------------------------------------------------------------------- */

semlex(n:_/np,Sym,_:_:Sense,Index,Sem):- !,
   Sem = lam(NP,lam(X,merge(drs([],[Index:pred(X,Sym,n,Sense)]),
                            app(NP,lam(Y,drs([],[Index:rel(X,Y,rel,0)])))))).


/* -------------------------------------------------------------------------
   PP modifiers
------------------------------------------------------------------------- */

semlex(Cat,Sym,_:_:Sense,Index,Sem):-
   member(Cat,[pp/pp, 
               pp\pp]), !, 
   Sem = lam(P,lam(E,merge(app(P,E),
                           drs([],[Index:pred(E,Sym,a,Sense)])))).

semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[(pp\pp)\np,
               (pp\pp)/np,
               (pp/pp)/np]), !, 
   Sem = lam(NP,lam(P,lam(E,merge(app(P,E),
                                  app(NP,lam(X,drs([],[Index:rel(E,X,Sym,0)]))))))).

semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[(pp\pp)\s:dcl,
               (pp\pp)/s:dcl,
               (pp/pp)/s:dcl]), !, 
   Sem = lam(S,lam(P,lam(E,merge(app(P,E),
                                 app(S,lam(X,drs([],[Index:rel(E,X,Sym,0)]))))))).

semlex(Cat,Sym,_,Index,Sem):-  
   notSymbol(Sym),
   member(Cat,[(pp\pp)/(pp\pp),
               (pp/pp)/(pp/pp)]), !,
   Sem = lam(Z,lam(P,lam(Y,app(app(Z,lam(X,drs([],[Index:not(app(P,X))]))),Y)))).

semlex(Cat,Sym,_:_:Sense,Index,Sem):-  
   member(Cat,[(pp\pp)/(pp\pp),
               (pp/pp)/(pp/pp)]), !,
   Sem = lam(Z,lam(P,lam(Y,app(app(Z,lam(X,merge(drs([],[Index:pred(X,Sym,a,Sense)]),
                                                 app(P,X)))),Y)))).

/* -------------------------------------------------------------------------
   Preposition: the (as in "the week before")
------------------------------------------------------------------------- */

semlex(Cat,the,_,Index,Sem):-
   member(Cat,[(pp\pp)/n:_]), !, 
   Sem = lam(N,lam(P,lam(E,merge(app(P,E),
                                 alfa(def,merge(drs([[]:X],[]),
                                                app(N,X)),
                                          drs([],[Index:rel(E,X,rel,0)])))))).

semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[(pp\pp)/n:_]), !, 
   Sem = lam(N,lam(P,lam(E,merge(app(P,E),
                                 alfa(def,merge(drs([[]:X],[]),
                                                app(N,X)),
                                          drs([],[Index:rel(E,X,Sym,0)])))))).


/* -------------------------------------------------------------------------
   Discourse connectors: when
------------------------------------------------------------------------- */

semlex(Cat,when,_,Index,Sem):-
   option('--tense',true),
   member(Cat,[(s:X/s:X)/s:dcl,
               (s:wq/s:wq)/s:dcl]), !, 
   Sem = lam(S1,lam(S2,lam(F,smerge(merge(drs([Index:T],[Index:pred(T,time,n,1)]),
                                          app(S1,lam(E,drs([],[Index:rel(E,T,temp_included,1)])))),
                                    app(S2,lam(E,merge(drs([],[Index:rel(E,T,temp_included,1)]),app(F,E)))))))).

semlex(Cat,when,_,Index,Sem):-
   option('--tense',true),
   member(Cat,[(s:X\s:X)/s:dcl,
               (s:wq\s:wq)/s:dcl]), !, 
   Sem = lam(S2,lam(S1,lam(F,smerge(merge(drs([Index:T],[Index:pred(T,time,n,1)]),
                                          app(S1,lam(E,drs([],[Index:rel(E,T,temp_included,1)])))),
                                    app(S2,lam(E,merge(drs([],[Index:rel(E,T,temp_included,1)]),app(F,E)))))))).


/* -------------------------------------------------------------------------
   Discourse connectors: if
------------------------------------------------------------------------- */

semlex(Cat,if,_,Index,Sem):-
   member(Cat,[(s:X/s:X)/s:dcl,
               (s:wq/s:wq)/s:dcl]), !, 
   Sem = lam(S1,lam(S2,lam(F,drs([],[Index:imp(app(S1,lam(_,drs([],[]))),
                                               app(S2,F))])))).

semlex(Cat,if,_,Index,Sem):-
   member(Cat,[(s:X\s:X)/s:dcl,
               (s:wq\s:wq)/s:dcl]), !, 
   Sem = lam(S2,lam(S1,lam(F,drs([],[Index:imp(app(S1,lam(_,drs([],[]))),
                                               app(S2,F))])))).


/* -------------------------------------------------------------------------
   Discourse connectors: all others
------------------------------------------------------------------------- */

semlex(Cat,Sym,_,Index,Sem):-
   option('--theory',sdrt),
   Cat = (s:X/s:X)/s:_, !,
%  Sem = lam(S1,lam(S2,lam(F,sdrs([lab(K1,B1),lab(K2,B2)],[Index:rel(K1,K2,Sym)])))),
   Sem = lam(S2,lam(S1,lam(F,sdrs([sub(lab(K1,B1),lab(K2,B2))],[Index:rel(K1,K2,Sym)])))),
   B1 = app(S1,lam(_,drs([],[]))),
   B2 = app(S2,F).

semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[(s:X/s:X)/s:dcl,
               (s:X/s:X)/s:inv,
               (s:wq/s:wq)/s:dcl]), !, 
   Sem = lam(S1,lam(S2,lam(F,merge(drs([Index:E,Index:Z,Index:Y],
                                       [Index:prop(E,drs([],[Index:rel(Z,Y,Sym,0)])),
                                        Index:prop(Z,app(S1,lam(_,drs([],[])))),
                                        Index:prop(Y,app(S2,lam(_,drs([],[]))))]),
                                   app(F,E))))).

semlex(Cat,Sym,_,Index,Sem):-
   member(Cat,[(s:X\s:X)/s:dcl,
               (s:wq\s:wq)/s:dcl]), !, 
   Sem = lam(S2,lam(S1,lam(F,merge(drs([Index:E,Index:Z,Index:Y],
                                       [Index:prop(E,drs([],[Index:rel(Z,Y,Sym,0)])),
                                        Index:prop(Z,app(S1,lam(_,drs([],[])))),
                                        Index:prop(Y,app(S2,lam(_,drs([],[]))))]),
                                   app(F,E))))).


/* -------------------------------------------------------------------------
   Non-Restrictive Relative Pronous
------------------------------------------------------------------------- */

semlex(Cat,_Sym,_,_Index,Sem):-
   member(Cat,[(np\np)/(s:_\np),
               (np\np)/(s:_/np)]), !,
   Sem = lam(VP,lam(Q,lam(P,app(Q,lam(X,merge(app(app(VP,
                                                      lam(P,app(P,X))),
                                                  lam(_,drs([],[]))),
                                              app(P,X))))))).


/* -------------------------------------------------------------------------
   Restrictive Relative Pronous
------------------------------------------------------------------------- */

semlex(Cat,_Sym,_,_Index,Sem):-
   member(Cat,[(n:_\n:_)/(s:_\np),
               (n:_\n:_)/(s:_/np)]), !,
   Sem = lam(VP,lam(N,lam(X,merge(app(N,X),
                                  app(app(VP,
                                          lam(P,app(P,X))),
                                      lam(_,drs([],[]))))))).


/* -------------------------------------------------------------------------
   PP Relative Pronous
------------------------------------------------------------------------- */

semlex(Cat,_Sym,_,_Index,Sem):-
   Cat = pp/(s:_/np), !,
   Sem = lam(VP,lam(X,app(app(VP,lam(P,app(P,X))),lam(_,drs([],[]))))).


/* -------------------------------------------------------------------------
   Other kind of relative pronous (pied piping)
------------------------------------------------------------------------- */

semlex(Cat,_Sym,_,Index,Sem):-
   Cat=((np\np)/(s:dcl\np))\(np/np),
   Sem = lam(M,lam(VP,lam(NP,lam(P,app(NP,lam(Y,merge(drs([Index:Z],[Index:eq(Y,Z)]),
                                                      merge(app(P,Y),
                                                            app(app(VP,app(M,lam(Q,app(Q,Z)))),lam(_,drs([],[]))))))))))).


/* -------------------------------------------------------------------------
   whose
------------------------------------------------------------------------- */

semlex(((np\np)/(s:dcl\np))/n:_,_,_,Index,Sem):- !,
   Sem = lam(N,lam(VP,lam(Q,lam(P,app(Q,lam(X,merge(app(app(VP,lam(P,merge(drs([Index:Y],[Index:rel(Y,X,of,0)]),
                                                                           merge(app(N,Y),app(P,Y))))),lam(_,drs([],[]))),
                                                    app(P,X)))))))). 

semlex(((n:F\n:F)/(s:dcl\np))/n:_,_,_,Index,Sem):- !,
   Sem = lam(N,lam(VP,lam(P,lam(X,merge(app(P,X),app(app(VP,lam(P,merge(drs([Index:Y],[Index:rel(Y,X,of,0)]),
                                                                        merge(app(N,Y),app(P,Y))))),lam(_,drs([],[])))))))).


/* -------------------------------------------------------------------------
   Interjections and Sentential Categories
------------------------------------------------------------------------- */

semlex(Cat,Sym,_:_:Sense,Index,Sem):-
   category(s,Cat,_), !,
   Sem = lam(E,merge(drs([Index:X],[Index:pred(X,Sym,n,Sense)]),app(E,X))).


/* =========================================================================
   Aux Predicates
========================================================================= */

notSymbol( not    ).
notSymbol( 'n\'t' ).
notSymbol( '\'t'  ).
notSymbol( nor    ).

