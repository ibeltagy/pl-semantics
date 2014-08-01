
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
:- use_module(boxer(string2digit),[string2digit/2,string2score/2]).
:- use_module(boxer(categories),[category/3,att/3,sense/4,roles/4,rel/3]).
:- use_module(semlib(options),[option/2]).
:- use_module(knowledge(ne),[neClass/2,neClassType/3]).
:- use_module(knowledge(dates),[month/2,dofm/2,decade/2,year/2]).
:- use_module(knowledge(punctuation),[punctuation/2]).
%:-use_module(knowledge(title),[title/2]).
%:-use_module(knowledge(negprefix),[negprefix/4]).
%:-use_module(knowledge(negsuffix),[negsuffix/4]).
%:-use_module(knowledge(nationality),[nationality/2]).
:- use_module(lex(determiners),[semlex_det/4]).
:- use_module(lex(verbs),[semlex_verb/5,closing/1]).
:- use_module(library(lists),[member/2]).
:- use_module(boxer(coordination),[coordMacro/2,argCard/2]).
:- use_module(boxer(resolveDRT),[goldAntecedent/2]).


/* =========================================================================
   Punctuation
========================================================================= */

semlex(t:_\s:_,_,_,Att-Att,Sem):- !,
   closing(CC),
   Sem = lam(S,app(S,CC)).

semlex(Cat,_,_,Att-Att,Sem):- 
   att(Att,pos,POS),
   punctuation(POS,_),
   member(Cat,[C\C, C/C,
               (s:X/s:X)/(s:Y/s:Y), (s:X/s:X)/(s:Y\s:Y),
               (s:X/s:X)\(s:Y/s:Y), (s:X/s:X)\(s:Y\s:Y),
               (s:X\s:X)/(s:Y\s:Y), (s:X\s:X)/(s:Y/s:Y),
               (s:X\s:X)\(s:Y/s:Y), (s:X\s:X)\(s:Y\s:Y)]), !,
   Sem = lam(P,P).

semlex(Cat,_,_,Att-Att,Sem):- 
   att(Att,pos,POS),
   punctuation(POS,_),
   member(Cat,[(((s:X\np)/(s:X\np))\(s:Y/s:Y)),
               (((s:X\np)\(s:X\np))\(s:Y/s:Y))]), !,
   Sem = lam(SMOD,lam(VP,lam(NP,app(SMOD,app(VP,NP))))).

semlex(Cat,_,Index,Att-Att,Sem):-
   att(Att,pos,POS),
   punctuation(POS,left),
   member(Cat,[(np\np)/np,(np\np)\np,(np/np)/np,(np/np)\np]), !,
   Sem = lam(Q1,lam(Q2,lam(P,app(Q2,lam(X,app(Q1,lam(Y,merge(B:drs([],[B:Index:rel(X,Y,rel,0)]),app(P,X))))))))).

semlex(Cat,_,Index,Att-Att,Sem):-
   att(Att,pos,POS),
   punctuation(POS,_),
   member(Cat,[(s:X\s:X)/np,(s:X\s:X)\np,(s:X/s:X)/np,(s:X/s:X)\np]), !,
   Sem = lam(Q1,lam(Q2,lam(P,app(Q2,lam(Z,app(Q1,lam(Y,merge(B:drs([],[B:Index:rel(Z,Y,rel,0)]),app(P,Z))))))))).

semlex(Cat,_,Index,Att-Att,Sem):-
   att(Att,pos,POS),
   punctuation(POS,left),
   member(Cat,[(n\n)/n,(n/n)/n]), !,
   Sem = lam(N,lam(P,lam(X,merge(merge(B:drs([B:[]:Y],[B:Index:rel(X,Y,rel,0)]),app(N,Y)),app(P,X))))).

semlex(Cat,_,Index,Att-Att,Sem):-
   att(Att,pos,POS),
   punctuation(POS,_),
   member(Cat,[((s:X\np)\(s:X\np))/np, 
               ((s:X\np)/(s:X\np))/np, 
               ((s:X\np)\(s:X\np))\np, 
               ((s:X\np)/(s:X\np))\np]), !,
   Sem = lam(Q2,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,app(Q2,lam(Y,merge(B:drs([],[B:Index:rel(E,Y,rel,0)]),app(F,E)))))))))).


/* =========================================================================
   Coordination (disjunction and negation)
========================================================================= */

semlex(conj:n,Sym,Index,Att-Att,Sem):-
   Sym = or,
   option('--semantics',drg), !,
   Sem = lam(P2,lam(P1,lam(X,B:drs([],[B:Index:pred(X,Sym,s,1),
                                       B:[]:or(app(P1,X),app(P2,X))])))).

semlex(conj:Cat,Lemma,Index,Att-Att,Sem):-
   member(Lemma,[either,or]), !,
   argCard(Cat,N),
   coordMacro(N,Coord),
   Sem = app(Coord,lam(K2,lam(K1,B:drs([],[B:Index:or(K1,K2)])))).

semlex(conj:Cat,Lemma,Index,Att-Att,Sem):- 
   member(Lemma,[neither,nor,not,of,than]), !,
   argCard(Cat,N),
   coordMacro(N,Coord),
   Sem = app(Coord,lam(K2,lam(K1,merge(K1,B:drs([],[B:Index:not(K2)]))))).


/* =========================================================================
   Coordination (conjuction)
========================================================================= */

semlex(conj:n,Sym,Index,Att-Att,Sem):- 
   option('--semantics',drg), !,
   Sem = lam(P2,lam(P1,lam(X,merge(B:drs([B:[]:Y,B:[]:Z],
                                         [B:Index:pred(X,Sym,s,1),
                                          B:[]:rel(Y,X,subset_of,1),
                                          B:[]:rel(Z,X,subset_of,1)]),
                                   merge(app(P1,Y),
                                         app(P2,Z)))))).

semlex(conj:n,_Sym,_,Att-Att,Sem):- !,
   Sem = lam(P2,lam(P1,lam(X,merge(B:drs([B:[]:Y,B:[]:Z],
                                         [B:[]:rel(Y,X,subset_of,1),
                                          B:[]:rel(Z,X,subset_of,1)]),
                                   merge(app(P1,Y),
                                         app(P2,Z)))))).

semlex(conj:(n/n),Sym,Index,Att-Att,Sem):- 
   option('--semantics',drg), !,
   Sem = lam(A2,lam(A1,lam(P,lam(X,merge(B:drs([B:[]:Y,B:[]:Z],
                                               [B:Index:pred(X,Sym,s,1),
                                                B:[]:rel(Y,X,subset_of,1),
                                                B:[]:rel(Z,X,subset_of,1)]),
                                   merge(app(app(A1,P),Y),
                                         app(app(A2,P),Z))))))).

semlex(conj:(n/n),_,_,Att-Att,Sem):- !,
   Sem = lam(A2,lam(A1,lam(P,lam(X,merge(B:drs([B:[]:Y,B:[]:Z],
                                               [B:[]:rel(Y,X,subset_of,1),
                                                B:[]:rel(Z,X,subset_of,1)]),
                                   merge(app(app(A1,P),Y),
                                         app(app(A2,P),Z))))))).

semlex(conj:np,_,Index,Att-Att,Sem):- !,  % collective
   Sem = lam(X2,lam(X1,lam(P,merge(merge(B:drs([B:Index:X],[]),
                                         merge(app(X1,lam(Y,B1:drs([],[B1:[]:rel(Y,X,subset_of,1)]))),
                                               app(X2,lam(Z,B2:drs([],[B2:[]:rel(Z,X,subset_of,1)]))))),
                                   app(P,X))))).

semlex(conj:app,_,Index,Att-Att,Sem):- !,
   Sem = lam(X1,lam(X2,lam(P,app(X2,
                                 lam(Y,merge(app(X1,lam(X,B:drs([],[B:Index:rel(Y,X,rel,2)]))),
                                             app(P,Y))))))).

semlex(conj:(s:_\np),Sym,Index,Att-Att,Sem):-    
   option('--theory',sdrt), 
   member(Sym,[but]), !,                        % VP coordination (contrastive)
   Sem = lam(V1,lam(V2,lam(X,lam(E,sdrs([lab(K1,app(app(V2,X),E)),
                                         lab(K2,app(app(V1,X),E))],
                                        [Index:rel(K1,K2,continuation),
                                         []:rel(K1,K2,contrast)]))))).

semlex(conj:(s:_\np),_Sym,Index,Att-Att,Sem):-    % VP coordination
   option('--theory',sdrt), !,
   Sem = lam(V1,lam(V2,lam(X,lam(E,sdrs([lab(K1,app(app(V2,X),E)),
                                         lab(K2,app(app(V1,X),E))],
                                        [Index:rel(K1,K2,continuation),
                                         []:rel(K1,K2,parallel)]))))).

semlex(conj:(s:_),Sym,Index,Att-Att,Sem):- 
   option('--theory',sdrt),
   member(Sym,[but]), !,                        % S coordination (contrastive)
   Sem = lam(S1,lam(S2,lam(E,sdrs([lab(K1,app(S2,E)),
                                   lab(K2,app(S1,E))],
                                  [Index:rel(K1,K2,continuation),
                                   []:rel(K1,K2,contrast)])))).

semlex(conj:(s:_),_Sym,Index,Att-Att,Sem):-     % S coordination
   option('--theory',sdrt), !,
   Sem = lam(S1,lam(S2,lam(E,sdrs([lab(K1,app(S2,E)),
                                   lab(K2,app(S1,E))],
                                  [Index:rel(K1,K2,continuation),
                                   []:rel(K1,K2,parallel)])))).


semlex(conj:CCat,_,_,Att-Att,Sem):- !,
   argCard(CCat,N),
   coordMacro(N,Coord),
   Sem = app(Coord,lam(K2,lam(K1,merge(K1,K2)))).


%semlex(conj:s,_,_,Att-Att,Sem):- !,
%   Sem = lam(K2,lam(K1,merge(K1,K2))).


% not used anymore??
%semlex(Punct,Punct,_,Att-Att,Sem):- 
%   att(Att,pos,Punct),
%   member(Punct,[',', ';', ':']), !,
%   Sem = lam(K2,lam(K1,merge(K1,K2))).




/* =========================================================================
   Compound Coordination
========================================================================= */

%semlex(conj:F/conj:F,instead,Index,Att-Att,Sem):- !,  % instead of
%   Sem = lam(C,lam(K1,lam(K2,app(app(C,B:drs([],[B:Index:not(K1)])),K2)))).

%semlex(conj:F/conj:F,rather,Index,Att-Att,Sem):- !,   % rather than
%   Sem = lam(C,lam(K1,lam(K2,app(app(C,B:drs([],[B:Index:not(K1)])),K2)))).

semlex(conj:F/conj:F,_,_,Att-Att,lam(U,U)):- !.
semlex(conj:F\conj:F,_,_,Att-Att,lam(U,U)):- !.


/* =========================================================================
   Quotes
========================================================================= */

semlex(q,_,Index,Att-Att,Sem):- !,
   Sem = lam(X,B:drs([],[B:Index:pred(X,quotation,n,2)])).

semlex((n/q)/n,_,Index,Att-Att,Sem):- !,
   Sem = lam(N,lam(Q,lam(X,merge(B:drs([],[B:Index:pred(X,quotation,n,2)]),
                                 merge(app(N,X),app(Q,X)))))).

semlex((np/q)/np,_,Index,Att-Att,Sem):- !,
   Sem = lam(NP,lam(Q,lam(P,app(NP,lam(X,merge(B:drs([],[B:Index:pred(X,quotation,n,2)]),
                                               merge(app(Q,X),app(P,X)))))))).

semlex((s:dcl/q)/s:dcl,_,Index,Att-Att,Sem):- !,
   Sem = lam(S,lam(Q,lam(F,app(S,lam(E,merge(B:drs([],[B:Index:pred(E,quotation,n,2)]),
                                             merge(app(Q,E),app(F,E)))))))).


/* =========================================================================
   Noun Phrases
========================================================================= */

/* -------------------------------------------------------------------------
   Expletive 'there' and other "special" nouns
------------------------------------------------------------------------- */

semlex(n,many,Index,Att-Att,Sem):-
   att(Att,pos,'NN'), !,
   Sem = lam(X,B:drs([],[B:Index:pred(X,quantity,n,1)])).

semlex(n,much,Index,Att-Att,Sem):-
   att(Att,pos,'NN'), !,
   Sem = lam(X,B:drs([],[B:Index:pred(X,amount,n,3)])).

semlex(n,'%',Index,Att-Att,Sem):-
   att(Att,pos,'NN'), !,
   Sem = lam(X,B:drs([],[B:Index:pred(X,percent,n,1)])).

semlex(n,'there',Index,Att-Att,Sem):-
   att(Att,pos,'EX'), !,
   Sem = lam(X,B:drs([B:[]:Y],[B:Index:pred(Y,location,n,1),
                               B:[]:rel(Y,X,rel,0)])).


/* -------------------------------------------------------------------------
   Nouns
------------------------------------------------------------------------- */

semlex(n,other,Index,Att-Att,Sem):-       % OTHERS
   \+ option('--semantics',drg), 
   att(Att,pos,'NNS'), !,
   Sem = lam(X,merge(B0:drs([],[B0:Index:pred(X,person,n,1)]),
                     alfa(def,
                          B1:drs([B1:[]:Y],[B1:[]:pred(Y,person,n,1)]),
                          B2:drs([],[B2:[]:not(B3:drs([],[B3:[]:eq(X,Y)]))])))).

semlex(n,Sym,Index,Att-Att,Sem):-         % DAY OF MONTH
   att(Att,pos,'CD'),
   att(Att,namex,NE), neClass(NE,tim),
   dofm(Sym,DID), !,
   Sem = lam(X,B:drs([],[B:Index:timex(X,date([]:'+',[]:'XXXX',[]:'XX',Index:DID))])).

semlex(n,Sym,Index,Att-Att,Sem):-         % YEAR
   att(Att,pos,'CD'),
   att(Att,namex,NE), neClass(NE,tim),
   year(Sym,Year), !,
   Sem = lam(X,B:drs([],[B:Index:timex(X,date([]:'+',Index:Year,[]:'XX',[]:'XX'))])).

semlex(n,Sym,Index,Att-Att,Sem):-         % MONTH
   att(Att,pos,'CD'),
   att(Att,namex,NE), neClass(NE,tim),
   month(Sym,MID), !,
   Sem = lam(X,B:drs([],[B:Index:timex(X,date([]:'+',[]:'XXXX',Index:MID,[]:'XX'))])).

semlex(n,Sym,Index,Att1-Att2,Sem):-       % GPE-NOUNS
   att(Att1,namex,'gpe-nam'), 
   ( att(Att1,pos,'NNS');  att(Att1,pos,'NN') ), !,
   rel(from,Att1-Att2,Relation),
   Sem = lam(X,B:drs([B:[]:Y],[B:Index:pred(X,person,n,1),
                               B:[]:named(Y,Sym,gpe,nam),
                               B:[]:rel(X,Y,Relation,1)])).

semlex(n,Sym,Index,Att-Att,Sem):-         % DECADE
   att(Att,pos,'NNS'),
   decade(Sym,DID), !,
   Sem = lam(X,B:drs([],[B:Index:timex(X,date([]:'+',Index:DID,[]:'XX',[]:'XX'))])).

semlex(n,Sym,Index,Att-Att,Sem):-         % SCORE
   att(Att,pos,'CD'),
   string2score(Sym,Score), !,
   Sem = lam(X,B:drs([],[B:Index:named(X,Score,sco,num)])).  % preliminary representation

semlex(n,Sym,Index,Att-Att,Sem):-         % QUANTITY
   att(Att,pos,'CD'),
   string2digit(Sym,Digit), !,
   Sem = lam(X,B:drs([],[B:Index:card(X,Digit,eq)])).

semlex(n,Sym,Index,Att-Att,Sem):-         % NAME (SG)
   att(Att,pos,'NNP'), !,
   att(Att,namex,NE), neClassType(NE,Class,Type),
   Sem = lam(X,B:drs([],[B:Index:named(X,Sym,Class,Type)])).

semlex(n,Sym,Index,Att-Att,Sem):-         % NAME (PL)
   att(Att,pos,'NNPS'), !,
   att(Att,namex,NE), neClassType(NE,Class,Type),
   Sem = lam(X,B:drs([],[B:Index:named(X,Sym,Class,Type)])).

semlex(n, most,Index,Att-Att,Sem):- 
   att(Att,pos,'JJS'), !,
   att(Att,sense,Sense),
   Sem = lam(X,B:drs([],[B:Index:pred(X,most,n,Sense)])).

semlex(n,Sym,Index,Att-Att,Sem):- 
   \+ option('--semantics',drg),
   att(Att,pos,'JJS'), !,
   Sem = lam(X,B1:drs([],[B1:[]:imp(B2:drs([B2:[]:Y],[B2:[]:not(B3:drs([],[B3:[]:eq(Y,X)]))]),B4:drs([],[B4:Index:rel(X,Y,Sym,0)]))])).

%semlex(n,Sym,_,Index,Att-Att,Sem):- 
%   option('--x',true),
%   negprefix(_, Sym, Prefix, Core), !,
%   Sem = lam(X,B1:drs([],[B1:Index:not(B2:drs([],[B2:Index:pred(X,Prefix,n,71),B2:Index:pred(X,Core,n,1)]))])).

%semlex(n,Sym,_,Index,Att-Att,Sem):- 
%   option('--x',true),
%   negsuffix(_, Sym, Suffix, Core), !,
%   Sem = lam(X,B1:drs([],[B1:Index:not(B2:drs([],[B2:Index:pred(X,Suffix,n,72),B2:Index:pred(X,Core,n,1)]))])).

semlex(n,Sym,Index,Att-Att,Sem):-
   option('--plural',true),
   att(Att,pos,'NNS'), !,
   att(Att,sense,Sense),
   Sem = lam(X,B:drs([],[B:Index:pred(X,Sym,n,Sense),B:[]:card(X,2,ge)])).

semlex(n,Sym,Index,Att-Att,Sem):- !,
   att(Att,sense,Sense),
   Sem = lam(X,B:drs([],[B:Index:pred(X,Sym,n,Sense)])).


/* -------------------------------------------------------------------------
   Relational nouns
------------------------------------------------------------------------- */

semlex(n/pp,Sym,Index,Att-Att,Sem):-
   att(Att,pos,Pos), 
   member(Pos,['NNP','NNPS']), !,
   att(Att,namex,NE), neClassType(NE,Class,Type),
   Sem = lam(P,lam(X,merge(B:drs([],[B:Index:named(X,Sym,Class,Type)]),
                           app(P,X)))).


semlex(n/pp,Sym,Index,Att-Att,Sem):- !,
   att(Att,sense,Sense),
   Sem = lam(P,lam(X,merge(B:drs([],[B:Index:pred(X,Sym,n,Sense)]),
                           app(P,X)))).

semlex(n/pp,Sym,Index,Att-Att,Sem):- !,
   att(Att,sense,Sense),
   Sem = lam(P,lam(X,merge(B:drs([[]:Y],
                               [B:Index:pred(Y,Sym,n,Sense),
                                B:[]:role(X,Y,actor,1),
                                B:[]:pred(X,person,n,1)]),
                           app(P,Y)))).

semlex((n/pp)/pp,Sym,Index,Att-Att,Sem):- !,
   att(Att,sense,Sense),
   Sem = lam(P1,lam(P2,lam(X,merge(B:drs([],[B:Index:pred(X,Sym,n,Sense)]),
                                   merge(app(P1,X),app(P2,X)))))).

semlex(((n/pp)/pp)/pp,Sym,Index,Att-Att,Sem):- !,
   att(Att,sense,Sense),
   Sem = lam(P1,lam(P2,lam(P3,lam(X,merge(B:drs([],[B:Index:pred(X,Sym,n,Sense)]),
                                          merge(app(P1,X),merge(app(P2,X),app(P3,X)))))))).

semlex(n/(s:_\np),Sym,Index,Att-Att,Sem):- !,
   att(Att,sense,Sense),
   closing(CC),
   Sem = lam(VP,lam(X,merge(B:drs([],[B:Index:pred(X,Sym,n,Sense)]),
                            app(app(VP,lam(P,app(P,X))),CC)))).

semlex((n/(s:_\np))/pp,Sym,Index,Att-Att,Sem):- !,
   att(Att,sense,Sense),
   closing(CC),
   Sem = lam(P,lam(VP,lam(X,merge(B:drs([],[B:Index:pred(X,Sym,n,Sense)]),
                                  merge(app(app(VP,lam(P,app(P,X))),CC),
                                        app(P,X)))))).

semlex((n/pp)/(s:_\np),Sym,Index,Att-Att,Sem):- !,
   att(Att,sense,Sense),
   closing(CC),
   Sem = lam(VP,lam(P,lam(X,merge(B:drs([],[B:Index:pred(X,Sym,n,Sense)]),
                                  merge(app(app(VP,lam(P,app(P,X))),CC),
                                        app(P,X)))))).


/* -------------------------------------------------------------------------
   Determiners
------------------------------------------------------------------------- */

semlex(np/n,Token,Index,Att1-Att2,Sem):- !, semlex_det(Token,Index,Att1-Att2,Sem).


/* -------------------------------------------------------------------------
   Possessives
------------------------------------------------------------------------- */

semlex(np/(n/pp),_,Index,Att-Att,Sem):- !,
   Sem = lam(RN,lam(P,alfa(pro,B1:drs([B1:[]:Y],[B1:[]:pred(Y,male,n,2)]),
                               alfa(def,merge(B2:drs([B2:[]:X],[]),
                                              app(app(RN,lam(U,B3:drs([],[B3:Index:rel(U,Y,of,0)]))),X)),
                                        app(P,X))))).


/* -------------------------------------------------------------------------
   Determiners (as many as X)
------------------------------------------------------------------------- */

semlex(((np/n)/pp)/(s:adj\np),_,Index,Att-Att,Sem):- !,
   closing(CC),
   Sem = lam(AP,lam(PP,lam(N,lam(P,merge(merge(B:drs([B:Index:X],[]),
                                               merge(app(N,X),
                                                     merge(app(PP,X),
                                                           app(app(AP,lam(Q,app(Q,X))),CC)))),
                                         app(P,X)))))).


/* -------------------------------------------------------------------------
   Many/Much [as NP]
------------------------------------------------------------------------- */

semlex(np,many,Index,Att-Att,Sem):- !,
   Sem = lam(P,merge(B:drs([B:[]:X],[B:Index:pred(X,quantity,n,1)]),app(P,X))).

semlex(np,much,Index,Att-Att,Sem):- !,
   Sem = lam(P,merge(B:drs([B:[]:X],[B:Index:pred(X,amount,n,3)]),app(P,X))).


/* -------------------------------------------------------------------------
   There insertion
------------------------------------------------------------------------- */

semlex(np,'there',Index,Att-Att,Sem):-
   att(Att,pos,'EX'), !,
   Sem = lam(P,merge(B:drs([B:Index:X],[]),app(P,X))).


/* -------------------------------------------------------------------------
   Pronouns (non-reflexives)
------------------------------------------------------------------------- */

semlex( np, Lemma,Index,Att-Att,Sem):-
   member(Lemma,['I',i,me,mine]), !,
   goldAntecedent(Index,Att),
   Sem = lam(P,alfa(pro,B:drs([B:[]:X],[B:Index:pred(X,person,n,1)]),app(P,X))).

semlex( np, Lemma,Index,Att-Att,Sem):-
   member(Lemma,['we','us','\'s','ours']), !,
   goldAntecedent(Index,Att),
   Sem = lam(P,alfa(pro,B:drs([B:[]:X],[B:Index:pred(X,person,n,1)]),app(P,X))).

semlex( np, Lemma,Index,Att-Att,Sem):-
   member(Lemma,['we','us','\'s','ours']), !,
   goldAntecedent(Index,Att),
   Sem = lam(P,alfa(pro,B1:drs([B1:[]:G],[B1:Index:pred(G,group,n,1)]),
                        B2:drs([],[B2:[]:imp(B3:drs([B3:[]:X],[B3:[]:rel(X,G,member_of,0)]),
                                          merge(B4:drs([],[B4:[]:pred(X,person,n,1)]),       
                                                app(P,X)))]))).

semlex( np, Lemma,Index,Att-Att,Sem):-
   member(Lemma,[whom,you,thou,yours]), !,
   goldAntecedent(Index,Att),
   Sem = lam(P,alfa(pro,B:drs([B:[]:X],[B:Index:pred(X,person,n,1)]),app(P,X))).

semlex( np, Lemma,Index,Att-Att,Sem):-
   member(Lemma,['he','his','him']), !,
   goldAntecedent(Index,Att),
   Sem = lam(P,alfa(pro,B:drs([B:[]:X],[B:Index:pred(X,male,n,2)]),app(P,X))).

semlex( np, Lemma,Index,Att-Att,Sem):-
   member(Lemma,['she','hers','her']), !,
   goldAntecedent(Index,Att),
   Sem = lam(P,alfa(pro,B:drs([B:[]:X],[B:Index:pred(X,female,n,2)]),app(P,X))).

semlex( np, Lemma,Index,Att-Att,Sem):-
   member(Lemma,['it','\'t']), !,
   goldAntecedent(Index,Att),
   Sem = lam(P,alfa(pro,B:drs([B:[]:X],[B:Index:pred(X,thing,n,12)]),app(P,X))).

semlex( np, Lemma,Index,Att-Att,Sem):-
   member(Lemma,['they','them','theirs']), !,
   goldAntecedent(Index,Att),
   Sem = lam(P,alfa(pro,B:drs([B:[]:X],[B:Index:pred(X,thing,n,12)]),app(P,X))).

semlex( np, Lemma,Index,Att-Att,Sem):-
   member(Lemma,['they','them','theirs']), !,
   goldAntecedent(Index,Att),
   Sem = lam(P,alfa(pro,B1:drs([B1:[]:G],[B1:Index:pred(G,group,n,1)]),
                        B2:drs([],[B2:[]:imp(B2:drs([B2:[]:X],[B2:[]:rel(X,G,member_of,0)]),
                                          app(P,X))]))).


/* -------------------------------------------------------------------------
   Reflexive Pronouns 
------------------------------------------------------------------------- */

semlex( np, Lemma,Index,Att-Att,Sem):-
   member(Lemma,[myself,yourself,thyself,ourselves]), !,
   Sem = lam(P,alfa(ref,B:drs([B:[]:X],[B:Index:pred(X,person,n,1)]),app(P,X))).

semlex( np, Lemma,Index,Att-Att,Sem):-
   member(Lemma,['himself']), !,
   Sem = lam(P,alfa(ref,B:drs([B:[]:X],[B:Index:pred(X,male,n,2)]),app(P,X))).

semlex( np, Lemma,Index,Att-Att,Sem):-
   member(Lemma,['herself']), !,
   Sem = lam(P,alfa(ref,B:drs([B:[]:X],[B:Index:pred(X,female,n,2)]),app(P,X))).

semlex( np, Lemma,Index,Att-Att,Sem):-
   member(Lemma,['itself']), !,
   Sem = lam(P,alfa(ref,B:drs([B:[]:X],[B:Index:pred(X,thing,n,12)]),app(P,X))).

semlex( np, Lemma,Index,Att-Att,Sem):-
   member(Lemma,['themselves']), !,
   Sem = lam(P,alfa(ref,B:drs([B:[]:X],[B:Index:pred(X,group,n,1)]),app(P,X))).


/* -------------------------------------------------------------------------
   Demonstratives and Quantificational Noun Phrases
------------------------------------------------------------------------- */

semlex( np, Lemma,Index,Att-Att,Sem):-
   member(Lemma,['none','neither',nothing]), !,
   Sem = lam(P,B1:drs([],[B1:Index:not(merge(B2:drs([B2:[]:X],[B2:Index:pred(X,thing,n,12)]),app(P,X)))])).

semlex( np, Lemma,Index,Att-Att,Sem):-
   member(Lemma,[something,some,'both','most','more','many','less','half','another']), !,
   Sem = lam(P,merge(B:drs([B:[]:X],[B:Index:pred(X,thing,n,12)]),app(P,X))).

semlex( np, Lemma,Index,Att-Att,Sem):-
   member(Lemma,['this','that','those','these']), !,
   Sem = lam(P,alfa(def,B:drs([B:[]:X],[B:Index:pred(X,thing,n,12)]),app(P,X))).

semlex( np, Lemma,Index,Att-Att,Sem):-
   member(Lemma,['all','any','each','either',everything,anything]), !,
   Sem = lam(P,B1:drs([],[B1:[]:imp(B2:drs([B2:[]:X],[B2:Index:pred(X,thing,n,12)]),app(P,X))])).

semlex( np, Lemma,Index,Att-Att,Sem):-
   member(Lemma,[everybody,everyone,anybody,anyone]), !,
   Sem = lam(P,B1:drs([],[B1:[]:imp(B2:drs([B2:[]:X],[B2:Index:pred(X,person,n,1)]),app(P,X))])).

semlex( np, Lemma,Index,Att-Att,Sem):-
   member(Lemma,[nobody,noone,'no-one']), !,
%  Sem = lam(P,B1:drs([],[B1:Index:not(merge(B2:drs([B2:[]:X],[B2:Index:pred(X,people,n,1)]),app(P,X)))])).
   Sem = lam(P,B1:drs([],[B1:Index:not(merge(B2:drs([B2:[]:X],[B2:Index:pred(X,person,n,1)]),app(P,X)))])).

semlex( np, Lemma,Index,Att-Att,Sem):-
   member(Lemma,[someone,somebody]), !,
   Sem = lam(P,merge(B:drs([B:[]:X],[B:Index:pred(X,person,n,1)]),app(P,X))).


/* -------------------------------------------------------------------------
   NP (semantically empty)

semlex( np_exp, _Lemma,Index,Att-Att,Sem):- !,
   Sem = lam(P,merge(B:drs([B:Index:X],[]),app(P,X))).

semlex( np_thr, _Lemma,Index,Att-Att,Sem):- !,
   Sem = lam(P,merge(B:drs([B:Index:X],[]),app(P,X))).
------------------------------------------------------------------------- */


/* -------------------------------------------------------------------------
   NP Why
------------------------------------------------------------------------- */

semlex( np, Lemma,Index,Att-Att,Sem):-
   Lemma = 'why', !,
   Sem = lam(P,B1:drs([],[B1:[]:duplex(whq,
                                     B2:drs([B2:[]:X],[B2:Index:pred(X,reason,n,2)]),
                                     X,
                                     app(P,X))])).


/* -------------------------------------------------------------------------
   NP (all others)
------------------------------------------------------------------------- */

semlex(np,Sym,Index,Att-Att,Sem):- 
   att(Att,pos,Pos), member(Pos,['NNP','NNPS']), !,
   att(Att,namex,Ne), neClassType(Ne,Class,Type),
   Sem = lam(P,alfa(nam,B:drs([B:[]:X],[B:Index:named(X,Sym,Class,Type)]),app(P,X))).

semlex(np,Sym,Index,Att-Att,Sem):- !,
   att(Att,sense,Sense),
   Sem = lam(P,merge(B:drs([B:[]:X],[B:Index:pred(X,Sym,n,Sense)]),app(P,X))).


/* -------------------------------------------------------------------------
   NP/PP
------------------------------------------------------------------------- */

semlex(np/pp, Sym,Index,Att-Att,Sem):- !,
   att(Att,sense,Sense),
   Sem = lam(PP,lam(P,merge(B:drs([B:[]:X],[B:Index:pred(X,Sym,n,Sense)]),
                            merge(app(P,X),app(PP,X))))).



/* -------------------------------------------------------------------------
   Question words: whose
------------------------------------------------------------------------- */

semlex(Cat,whose,Index,Att-Att,Sem):-
   member(Cat,[(s:wq/(s:dcl\np))/n,
               (s:wq/(s:q/np))/n,
               (s:wq\(s:dcl/np))/n]), !, 
   Sem = lam(N,lam(V,app(V,lam(P,B1:drs([],[B1:[]:duplex(whq,
                                                   merge(merge(B2:drs([B2:[]:Y],[]),app(N,Y)),
                                                               B3:drs([B3:[]:X],[B3:Index:pred(X,person,n,1),[]:rel(Y,X,of,0)])),
                                                   X, 
                                                   app(P,Y))]))))).


/* -------------------------------------------------------------------------
   Question words: which/what N
------------------------------------------------------------------------- */

semlex(Cat,_Sym,Index,Att-Att,Sem):-
   member(Cat,[(s:wq/(s:dcl\np))/n,
               (s:wq/(s:q/np))/n,
               (s:qem/(s:dcl\np))/n,
               (s:qem/(s:dcl/np))/n,
               (s:wq\(s:dcl/np))/n]), !, 
   Sem = lam(P1,lam(V2,app(V2,lam(P3,B1:drs([],[B1:[]:duplex(whq,
                                             merge(B2:drs([B2:Index:X4],[]),app(P1,X4)),
                                             X4,
                                             app(P3,X4))]))))).

semlex(Cat,_Sym,Index,Att-Att,Sem):-
   Cat = (s:wq/(s:q/pp))/n, !,  % WH-DET N + YNQ
   Sem = lam(N,lam(V,lam(E,B1:drs([],[B1:[]:duplex(whq,
                                              merge(B2:drs([B2:[]:X],[]),app(N,X)),
                                              X,
                                              app(app(V,lam(Y,B3:drs([],[B3:Index:rel(Y,X,rel,0)]))),E))])))).


/* -------------------------------------------------------------------------
   Question words: how much/many 
------------------------------------------------------------------------- */

semlex(Cat,_Sym,Index,Att-Att,Sem):-
   member(Cat,[(s:wq/(s:q/np))/np,
               (s:wq/(s:dcl\np))/np]), !, 
   Sem = lam(NP,lam(VP,lam(E,B1:drs([],[B1:[]:duplex(whq,
                                               merge(B2:drs([B2:[]:X,B2:[]:Y],[B2:Index:card(X,Y,eq)]),
                                                     app(NP,lam(U,B3:drs([],[B3:[]:eq(U,X)])))),
                                               Y,
                                               app(app(VP,lam(P,app(P,X))),E))])))).



/* -------------------------------------------------------------------------
   Question words: how much/many N 
------------------------------------------------------------------------- */

semlex(Cat,_Sym,Index,Att-Att,Sem):-
   member(Cat,[((s:wq/(s:q/np))/n)/(np/n),        
               ((s:wq/(s:dcl\np))/n)/(np/n)]), !, 
   Sem = lam(D,lam(N,lam(VP,lam(E,B1:drs([],[B1:[]:duplex(whq,
                                                    merge(B2:drs([B2:[]:X,B2:[]:Y],[B2:Index:card(X,Y,eq)]),
                                                          app(app(D,N),lam(Z,B3:drs([],[B3:[]:eq(X,Z)])))),
                                                    Y,
                                                    app(app(VP,lam(P,app(P,X))),E))]))))).

semlex(Cat,_Sym,Index,Att-Att,Sem):-
   member(Cat,[((s:wq/(s:q/pp))/n)/(np/n),        
               ((s:wq/(s:dcl\pp))/n)/(np/n)]), !, 
   Sem = lam(D,lam(N,lam(VP,lam(E,B1:drs([],[B1:[]:duplex(whq,
                                                    merge(B2:drs([B2:[]:X,B2:[]:Y],[B2:Index:card(X,Y,eq)]),
                                                          app(app(D,N),lam(Z,B3:drs([],[B3:[]:eq(X,Z)])))),
                                                    Y,
                                                    app(app(VP,lam(Y,B4:drs([],[B4:[]:rel(Y,X,rel,0)]))),E))]))))).

semlex(Cat,_Sym,Index,Att-Att,Sem):-
   member(Cat,[(((s:wq/pp)/((s:q/pp)/np))/n)/(np/n)]), !,
   Sem = lam(D,lam(N,lam(TV,lam(PP,lam(E,B1:drs([],[B1:[]:duplex(whq,
                                                           merge(B2:drs([B2:[]:X,B2:[]:Y],[B2:Index:card(X,Y,eq)]),
                                                                 app(app(D,N),lam(Z,B3:drs([],[B3:[]:eq(X,Z)])))),
                                                           Y,
                                                           app(app(app(TV,lam(P,app(P,X))),PP),E))])))))).


semlex(((s:wq/pp)/n)/(np/n),_Sym,Index,Att-Att,Sem):- !,  % American English dialect (How many feet in a mile?)
   Sem = lam(D,lam(N,lam(PP,lam(_,B1:drs([],[B1:[]:duplex(whq,
                                                    merge(B2:drs([B2:[]:X,B2:[]:Y],[Index:card(X,Y,eq)]),
                                                          app(app(D,N),lam(Z,B3:drs([],[B3:[]:eq(X,Z)])))),
                                                    Y,                                           
                                                    app(PP,X))]))))).
         

semlex(Cat,_Sym,Index,Att-Att,Sem):-
   member(Cat,[(((s:wq/(s:pss\np))/((s:q/(s:pss\np))/np))/n)/(np/n)]),
   Sem = lam(D,lam(N,lam(_,lam(VP,lam(E,B1:drs([],[B1:[]:duplex(whq,
                                                          merge(B2:drs([B2:[]:X,B2:[]:Y],[B2:Index:card(X,Y,eq)]),
                                                                app(app(D,N),lam(Z,B3:drs([],[B3:[]:eq(X,Z)])))),
                                                          Y,
                                                          app(app(VP,lam(P,app(P,X))),E))])))))).



/* -------------------------------------------------------------------------
   Question words: how ADJ 
------------------------------------------------------------------------- */

semlex(Cat,_Sym,Index,Att-Att,Sem):-
   member(Cat,[(s:wq/(s:q/(s:adj\np)))/(s:adj\np),
               ((s:wq/pp)/((s:q/pp)/(s:adj\np)))/(s:adj\np),
               (s:qem/(s:dcl/(s:adj\np)))/(s:adj\np)]), !, % How ADJ
   Sem = lam(A,lam(U,app(U,lam(NP,lam(E,app(NP,lam(X,B1:drs([],[B1:[]:duplex(whq,
                                                                       merge(B2:drs([B2:Index:Y],[]),
                                                                             app(app(A,lam(P,app(P,Y))),E)),
                                                                       Y,
                                                                       B3:drs([],[B3:[]:rel(Y,X,of,0)]))])))))))).


semlex(Cat,_Sym,Index,Att-Att,Sem):-
   Cat = (s:wq/(s:q/pp))/(s:adj\np), !, % How often does...
   closing(CC),
   Sem = lam(A,lam(VP,lam(F,B1:drs([],[B1:[]:duplex(whq,
                                              merge(B2:drs([B2:Index:X],[]),
                                                    app(app(A,lam(P,app(P,X))),CC)),
                                              X,
                                              app(app(VP,lam(Y,B3:drs([],[B3:[]:rel(Y,X,rel,0)]))),F))])))).


/* -------------------------------------------------------------------------
   Question words: basic question words 
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[s:wq/(s:dcl\np),  
               s:wq/(s:q/np),  
               s:wq\(s:dcl/np)]), 
   ( Sym = what,      Pred = thing,        Sense=12;
     Sym = whatever,  Pred = thing,        Sense=12;
     Sym = which,     Pred = thing,        Sense=12;
     Sym = whichever, Pred = thing,        Sense=12;
     Sym = where,     Pred = location,     Sense=1;
     Sym = why,       Pred = reason,       Sense=2;
     Sym = how,       Pred = manner,       Sense=2; 
     Sym = who,       Pred = person,       Sense=1;      
     Sym = whoever,   Pred = person,       Sense=1;      
     Sym = whom,      Pred = person,       Sense=1;      
     Sym = when,      Pred = unit_of_time, Sense=1 
   ), !,
   Sem = lam(VP,lam(F,B1:drs([],[B1:[]:duplex(whq,
                                        B2:drs([B2:[]:X],[B2:Index:pred(X,Pred,n,Sense)]),
                                        X,
                                        app(app(VP,lam(P,app(P,X))),F))]))).


semlex(Cat,Sym,Index,Att-Att,Sem):-
   Cat = s:wq/(s:q/pp), 
   ( Sym=where, Pred=location,     Rel=loc_rel,  Sense=1;
     Sym=why,   Pred=reason,       Rel=rel,      Sense=2;
     Sym=how,   Pred=manner,       Rel=rel,      Sense=2;
     Sym=when,  Pred=unit_of_time, Rel=temp_rel, Sense=1
   ), !, 
   Sem = lam(VP,lam(F,B1:drs([],[B1:[]:duplex(whq,
                                        B2:drs([B2:[]:X],[B2:Index:pred(X,Pred,n,Sense)]),
                                        X,
                                        app(app(VP,lam(E,B3:drs([],[B3:[]:rel(E,X,Rel,0)]))),F))]))).

semlex(Cat,_Sym,Index,Att-Att,Sem):-
   member(Cat,[np/(s:dcl\np),
               np/(s:dcl/np)]), !,
   closing(CC),
   Sem = lam(VP,lam(P,B1:drs([],[B1:[]:duplex(whq,
                                        B2:drs([B2:[]:X],[B2:Index:pred(X,thing,n,12)]),
                                        X,
                                        merge(app(app(VP,lam(R,app(R,X))),CC),app(P,X)))]))). 

semlex(Cat,_Sym,Index,Att-Att,Sem):-
   member(Cat,[np/((s:to\np)/np)]), !,
   closing(CC),
   Sem = lam(TV,lam(P,B1:drs([],[B1:[]:duplex(whq,
                                        B2:drs([B2:[]:X],[B2:Index:pred(X,thing,n,12)]),
                                        X,
                                        merge(app(app(app(TV,lam(R,app(R,X))),lam(Q,merge(B3:drs([B3:[]:Z],[B3:[]:pred(Z,thing,n,12)]),
                                                                                          app(Q,Z)))),CC),app(P,X)))]))). 

semlex(Cat,_Sym,Index,Att-Att,Sem):-
   member(Cat,[(np/(s:dcl\np))/n,
               (np/(s:dcl/np))/n]), !,
   closing(CC),
   Sem = lam(N,lam(VP,lam(P,B1:drs([],[B1:[]:duplex(whq,
                                              merge(B2:drs([B2:Index:X],[]),app(N,X)),
                                              X,
                                              merge(app(app(VP,lam(R,app(R,X))),CC),app(P,X)))])))). 


semlex(s:wq/s:q,Sym,Index,Att-Att,Sem):- 
   ( Sym=how,   Pred=manner,       Sense=2 ;
     Sym=where, Pred=location,     Sense=1 ;
     Sym=when,  Pred=unit_of_time, Sense=1 ;
     Sym=why,   Pred=reason,       Sense=2 ;
     Sym=what,  Pred=thing,        Sense=12 
   ), !,
   Sem = lam(YNQ,lam(E,app(YNQ,lam(F,B1:drs([],[B1:[]:duplex(whq,
                                                       B2:drs([B2:[]:X],[B2:Index:pred(X,Pred,n,Sense)]),    
                                                       X,
                                                       merge(B3:drs([],[B3:[]:rel(F,X,rel,0)]),app(E,F)))]))))). 

semlex(s:qem/(s:to\np),_,Index,Att-Att,Sem):- !,
   Sem = lam(VP,lam(E,app(app(VP,lam(P,merge(B1:drs([B1:[]:X],[]),app(P,X)))),lam(F,merge(B2:drs([],[B2:Index:pred(F,manner,n,2)]),app(E,F)))))). % how to

% whose
semlex(Cat,whose,Index,Att-Att,Sem):- 
   member(Cat,[s:wq/(s:q/np),
               s:wq\(s:dcl/np),
               s:wq/(s:dcl\np)]), !,
   Sem = lam(VP,app(VP,lam(P,B1:drs([],[B1:[]:duplex(whq,
                                               B2:drs([B2:[]:X,[]:Y],[B2:Index:pred(X,thing,n,12),B2:[]:pred(Y,person,n,1),B2:[]:rel(X,Y,of,0)]),
                                               X,
                                               app(P,X))])))).

semlex(Cat,_,Index,Att-Att,Sem):- 
   member(Cat,[s:qem/(s:dcl\np),
               s:_/(s:dcl\np),
               s:qem/(s:dcl/np)]), !,   
   Sem = lam(VP,app(VP,lam(P,B1:drs([],[B1:[]:duplex(whq,
                                               B2:drs([B2:[]:X],[B2:Index:pred(X,thing,n,12)]),
                                               X,
                                               app(P,X))])))).

% how
semlex((s:qem/s:dcl)/(s:adj\np),_,Index,Att-Att,Sem):- !,
   Sem = lam(VP,lam(S,app(VP,lam(P,B1:drs([],[B1:[]:duplex(whq,
                                                     merge(B2:drs([B2:[]:X],[B2:Index:pred(X,manner,n,2)]),app(P,X)),
                                                     X,
                                                     app(S,lam(E,B3:drs([],[B3:[]:rel(E,X,rel,0)]))))]))))).

% how much energy was lost (??)
% how many years has GP been campaigning
semlex(Cat,_,Index,Att-Att,Sem):-
   member(Cat,[((s:qem/(s:dcl\np))/n)/(s:adj\np),
               ((s:qem/(s:dcl/np))/n)/(s:adj\np)]), !,
   closing(CC),
   Sem = lam(VPADJ,lam(N,lam(VPDCL,lam(E,app(app(VPADJ,lam(P,B1:drs([],[B1:[]:duplex(whq,
                                                                                merge(B2:drs([B2:Index:X],[]),
                                                                                      merge(app(N,X),
                                                                                            app(P,X))),
                                                                     X,
                                                                     app(app(VPDCL,lam(P,app(P,X))),E))]))),CC))))).

% why does he always wait
semlex((s:X\s:X)/s:q,_,Index,Att-Att,Sem):- !,
   Sem = lam(W,lam(S,lam(F,app(S,lam(E,merge(B1:drs([B1:[]:Y],[B1:[]:prop(Y,B2:drs([],[B2:[]:duplex(whq,
                                                                                           B3:drs([B3:[]:Z],[B3:Index:pred(Z,reason,n,2)]),
                                                                                           Z,
                                                                                           app(W,lam(E,B4:drs([],[B4:[]:rel(E,Z,rel,0)]))))])),
                                                            B1:[]:rel(E,Y,rel,0)]),
                                             app(F,E))))))).


/* =========================================================================
   Relative pronouns, pied-piping ("N under which S", "NP under which S")
========================================================================= */

semlex(((np\np)/s:dcl)\((s:F\s:F)/np),_Sym,_Index,Att-Att,Sem):- !,
   closing(CC),
   Sem = lam(Prep,lam(S,lam(NP,lam(Q,app(app(app(Prep,lam(P,app(NP,lam(X,merge(app(P,X),app(Q,X)))))),S),CC))))). 

semlex(((np\np)/s:dcl)\((np\np)/np),_Sym,_Index,Att-Att,Sem):- !,
   closing(CC),
   Sem = lam(Prep,lam(S,lam(NP,lam(Q,app(app(app(Prep,lam(P,app(NP,lam(X,merge(app(P,X),app(Q,X)))))),S),CC))))). 

semlex(((n\n)/s:dcl)\((n\n)/np),_Sym,_Index,Att-Att,Sem):- !,
   Sem = lam(Prep,lam(S,lam(N,app(app(Prep,S),N)))).

semlex(((np\np)/s:dcl)\((n\n)/np),_Sym,_Index,Att-Att,Sem):- !,
   Sem = lam(Prep,lam(S,lam(NP,lam(P,app(NP,app(app(Prep,S),P)))))).



/* =========================================================================
   Verbs
========================================================================= */

semlex(Cat,Sym,Index,Att,Sem):- semlex_verb(Cat,Sym,Index,Att,Sem), !.


/* =========================================================================
   Adjectives
========================================================================= */

/* -------------------------------------------------------------------------
   Wrongly Classified Adjectives + "own"
------------------------------------------------------------------------- */

semlex(n/n,Sym,_Index,Att-Att,Sem):-
   member(Sym,[one,few]),
   option('--x',true), !,                   
   Sem = lam(P,lam(X,app(P,X))).

semlex(Cat,many,Index,Att-Att,Sem):-
   category(adj,Cat,_), !,
   Sem = lam(P,lam(X,merge(B:drs([],[B:Index:pred(X,quantity,n,1)]),app(P,X)))).

semlex(Cat,much,Index,Att-Att,Sem):-
   category(adj,Cat,_), !,
   Sem = lam(P,lam(X,merge(B:drs([],[B:Index:pred(X,amount,n,3)]),app(P,X)))).

semlex(n/n,only,Index,Att-Att,Sem):- !,
   Sem = lam(P,lam(X,merge(app(P,X),B1:drs([],[B1:[]:imp(merge(B2:drs([B2:Index:Y],[]),app(P,Y)),B3:drs([],[B3:[]:eq(X,Y)]))])))).


/* -------------------------------------------------------------------------
   Negation Adjectives
------------------------------------------------------------------------- */

%semlex(Cat,Sym,_,Index,Att-Att,Sem):-
%   category(adj,Cat,_), 
%   option('--x',true),
%   negprefix(_, Sym, Prefix, Core), !,
%   Sem = lam(P,lam(X,B1:drs([],[B1:Index:not(merge(B2:drs([],[B2:Index:pred(X,Prefix,a,71),
%                                                              B2:Index:pred(X,Core,a,1)]),app(P,X)))]))).

%semlex(Cat,Sym,_,Index,Att-Att,Sem):-
%   category(adj,Cat,_), 
%   option('--x',true),
%   negsuffix(_, Sym, Suffix, Core), !,
%   Sem = lam(P,lam(X,B1:drs([],[B1:Index:not(merge(B2:drs([],[B2:Index:pred(X,Suffix,a,72),
%                                                              B2:Index:pred(X,Core,a,1)]),app(P,X)))]))).


/* -------------------------------------------------------------------------
   Presuppositional Adjectives
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-Att,Sem):-
   \+ option('--semantics',drg), 
   member(Sym,[other,previous,different]),
   category(adj,Cat,_), !,
   Sem = lam(P,lam(X,merge(app(P,X),
                           alfa(def,
                                merge(B1:drs([B1:[]:Y],[]),app(P,Y)),
                                      B2:drs([],[B2:[]:not(B3:drs([],[B3:Index:eq(X,Y)]))]))))).


/* -------------------------------------------------------------------------
   Present participles, Gerunds
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att1-Att2,Sem):-
   member(Cat,[n/n,n\n]),
   att(Att1,pos,'VBG'), 
   roles(Sym,s:dcl\np,[Role],Att1-Att2), !,
   Sem = lam(P,lam(X,merge(B:drs([B:[]:E],
                                 [B:Index:pred(E,Sym,v,0),
                                  B:[]:role(X,E,Role,-1)]), 
                           app(P,X)))).


/* -------------------------------------------------------------------------
   Past Participles
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att1-Att2,Sem):-
   member(Cat,[n/n,n\n]),
   att(Att1,pos,'VBN'), 
   roles(Sym,s:pss\np,[Role],Att1-Att2), !,
   Sem = lam(P,lam(X,merge(B:drs([B:[]:E],
                                 [B:Index:pred(E,Sym,v,0),
                                  B:[]:role(X,E,Role,-1)]), 
                           app(P,X)))).


/* -------------------------------------------------------------------------
   Noun Noun Compounds
------------------------------------------------------------------------- */

semlex(Cat,'%',Index,Att1-Att2,Sem):-
   att(Att1,pos,'NN'),
   category(adj,Cat,_), !,
   semlex(Cat,percent,Index,Att1-Att2,Sem).

semlex(Cat,'$',Index,Att-Att,Sem):- 
   Cat = n/n, !,
   Sem = lam(P,lam(X,merge(B:drs([],[B:Index:pred(X,dollar,n,1)]),app(P,X)))).

semlex(n/n,'&',Index,Att-Att,Sem):- 
   att(Att,namex,Ne), neClassType(Ne,org,Type), !,
   Sem = lam(P,lam(X,merge(B:drs([],[B:Index:named(X,'&',org,Type)]),
                           app(P,X)))).

semlex(Cat,Sym,Index,Att-Att,Sem):- 
   att(Att,pos,'CD'),
   category(adj,Cat,_), 
   string2score(Sym,Score), !,
   Sem = lam(P,lam(X,merge(B:drs([],[B:Index:named(X,Score,sco,num)]),
                           app(P,X)))).

semlex(Cat,Sym,Index,Att-Att,Sem):- 
   att(Att,pos,'CD'),
   att(Att,namex,NE), neClass(NE,tim),
   category(adj,Cat,_), 
   dofm(Sym,DID), !,
   Sem = lam(P,lam(X,merge(B:drs([],[B:Index:timex(X,date([]:'+',[]:'XXXX',[]:'XX',Index:DID))]),
                           app(P,X)))).

semlex(Cat,Sym,Index,Att-Att,Sem):- 
   att(Att,pos,'NNP'),
   att(Att,namex,NE), neClass(NE,tim),
   category(adj,Cat,_),
   month(Sym,MID), !,
   Sem = lam(P,lam(X,merge(B:drs([],[B:Index:timex(X,date([]:'+',[]:'XXXX',Index:MID,[]:'XX'))]),
                           app(P,X)))).

semlex(Cat,YID,Index,Att-Att,Sem):- 
   att(Att,pos,'CD'),
   att(Att,namex,NE),
   neClass(NE,tim),
   category(adj,Cat,_),
   year(YID,Year), !,
   Sem = lam(P,lam(X,merge(B:drs([],[B:Index:timex(X,date([]:'+',Index:Year,[]:'XX',[]:'XX'))]),
                           app(P,X)))).

%semlex(Cat,Token,Index,Att1-Att2,Sem):-   %%% TITLES
%   att(Att1,pos,Pos),
%   member(Pos,['NN','NNS']),
%   title(Token,Lemma),
%   category(adj,Cat,_), !,
%   att(Att1,sense,Sense),
%   rel(with,Att1-Att2,Relation),
%   Sem = lam(P,lam(X,merge(B:drs([B:[]:Y],
%                                 [B:Index:pred(Y,Lemma,n,Sense),
%                                  B:[]:rel(X,Y,Relation,0)]),
%                           app(P,X)))).

semlex(Cat,Sym,Index,Att1-Att2,Sem):-    %%% NOUN-{NAME|NOUN} COMPOUNDS
   att(Att1,pos,Pos),
   member(Pos,['NN','NNS']),
   category(adj,Cat,_), !,
   ( att(Att1,relation,Rel), \+ Rel=unknown, !, Att2=Att1
   ; Rel = f(noun,[Sym,P],Relation), Att2=[relation:Relation|Att1] ),
   att(Att1,sense,Sense),
   Sem = lam(P,lam(X,merge(B:drs([B:[]:Y],
                                 [B:Index:pred(Y,Sym,n,Sense),
                                  B:[]:rel(X,Y,Rel,0)]),
                           app(P,X)))).

semlex(n/n,Sym,Index,Att1-Att2,Sem):-   %%% GPE-ADJECTIVES (e.g. French, German, etc.)
   att(Att1,pos,'JJ'),
   att(Att1,namex,'gpe-nam'), !,
   rel(of,Att1-Att2,Relation),
   Sem = lam(P,lam(X,merge(B:drs([B:[]:Y],
                                 [B:Index:named(Y,Sym,gpe,nam),
                                  B:[]:rel(X,Y,Relation,1)]),
                           app(P,X)))).

/* NEEDS TO BE CHECKED; THERE ARE MANY JJ,Location tagged words

semlex(n/n,Sym,Index,Att1-Att2,Sem):-   %%% LOC-ADJECTIVES (e.g. Antarctic, etc.)
   att(Att1,pos,'JJ'),
   att(Att1,namex,Ne), neClassType(Ne,loc,Type), !,
   rel(in,Att1-Att2,Relation),
   Sem = lam(P,lam(X,merge(B:drs([B:[]:Y],
                                 [B:Index:named(Y,Sym,loc,Type),
                                  B:[]:rel(X,Y,Relation,1)]),
                           app(P,X)))).
*/

semlex(n/n,Sym,Index,Att1-Att2,Sem):-    %%% NAME-{NOUN|NAME} COMPOUNDS
   att(Att1,pos,Pos),
   member(Pos,['NNP','NNPS']), !,
   att(Att1,namex,Ner), neClassType(Ner,Class,Type), 
   ( att(Att1,relation,Rel), \+ Rel=unknown, !, Att2=Att1
   ; Rel = f(name,[Sym,P],Relation), Att2=[relation:Relation|Att1] ),
   Sem = lam(P,lam(X,merge(B:drs([B:[]:Y],
                                 [B:Index:named(Y,Sym,Class,Type),
                                  B:[]:rel(X,Y,Rel,0)]),
                           app(P,X)))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   att(Att,pos,Pos),
   member(Pos,['NNP','NNPS']),
   category(adj,Cat,_), !,
   att(Att,namex,Ner), neClassType(Ner,Class,Type), 
   Sem = lam(P,lam(X,merge(B:drs([],[B:Index:named(X,Sym,Class,Type)]),
                           app(P,X)))).


/* -------------------------------------------------------------------------
   Singular Superlatives
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-Att,Sem):-
   \+ option('--semantics',drg),
   att(Att,pos,'JJS'),
   category(adj,Cat,_), !,
   Sem = lam(P,lam(X,merge(app(P,X),
                           B1:drs([],[B1:[]:imp(merge(B2:drs([B2:[]:Y],[B2:[]:not(B3:drs([],[B3:[]:eq(X,Y)]))]),
                                                      app(P,Y)),
                                                B4:drs([],[B4:Index:rel(X,Y,Sym,0)]))])))).


/* -------------------------------------------------------------------------
   Cardinal Adjectives
------------------------------------------------------------------------- */

semlex(n/n,Sym,Index,Att-Att,Sem):-
   string2digit(Sym,Digit), !,
   Sem = lam(P,lam(X,merge(B:drs([],[B:Index:card(X,Digit,eq)]),app(P,X)))).

semlex(n\n,Sym,Index,Att-Att,Sem):-
   string2digit(Sym,Digit), !,
   Sem = lam(P,lam(X,merge(app(P,X),B:drs([],[B:Index:card(X,Digit,eq)])))).


/* -------------------------------------------------------------------------
   Composite Adjectives:  10-hour story

semlex(Cat,Sym,Index,Att1-Att2,Sem):-
   category(adj,Cat,_), 
   atomic_list_concat([Prefix,Suffix],'-',Sym),
   member(Suffix,[acre,year,yard,foot,pound,day,minute,page,point,man,inch,
                  degree,week,member,mile,week,km,dollar,kilometer,
                  'square-foot',seat,meter,story,hour,time,ton,month]),
   string2digit(Prefix,Number), !, 
   att(Att1,sense,Sense),
   rel(Suffix,Att1-Att2,Relation),
   Sem = lam(P,lam(X,merge(B:drs([B:[]:Y],
                                 [B:[]:card(Y,Number,eq),
                                  B:Index:pred(Y,Suffix,n,Sense),
                                  B:[]:rel(X,Y,Relation,0)]),app(P,X)))).
------------------------------------------------------------------------- */


/* -------------------------------------------------------------------------
   Singular Intersective Adjectives
------------------------------------------------------------------------- */

% thematic role analysis
%
semlex(Cat,Sym,Index,Att1-Att2,Sem):-
   category(adj,Cat,_), !,
   att(Att1,sense,Sense),
   roles(Sym,s:adj\np,[Role],Att1-Att2), !,
   Sem = lam(P,lam(X,merge(B:drs([B:[]:E],[B:[]:role(X,E,Role,-1),B:Index:pred(E,Sym,a,Sense)]),app(P,X)))).

semlex(Cat,Sym,Index,Att1-Att2,Sem):-
   member(Cat,[(n/pp)\(n/pp)]), !,
   att(Att1,sense,Sense),
   roles(Sym,s:adj\np,[Role],Att1-Att2), !,
   Sem = lam(PP,lam(P,lam(X,merge(B:drs([B:[]:E],[B:[]:role(X,E,Role,-1),B:Index:pred(E,Sym,a,Sense)]),app(app(PP,P),X))))).


% classic analysis
%
semlex(Cat,Sym,Index,Att-Att,Sem):-
   category(adj,Cat,_), !,
   att(Att,sense,Sense),
   Sem = lam(P,lam(X,merge(B:drs([],[B:Index:pred(X,Sym,a,Sense)]),app(P,X)))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[(n/pp)\(n/pp)]), !,
   att(Att,sense,Sense),
   Sem = lam(PP,lam(P,lam(X,merge(B:drs([],[B:Index:pred(X,Sym,a,Sense)]),app(app(PP,P),X))))).


/* -------------------------------------------------------------------------
   Adjectives introducing a degree
------------------------------------------------------------------------- */

semlex(d/n,Sym,Index,Att-Att,Sem):- !,
%%   Sem = lam(P,lam(X,lam(D,merge(B:drs([],[B:Index:pred(D,degree,n,1),Index:rel(X,D,Sym,0)]),app(P,X))))).
   Sem = lam(Y,lam(X,B:drs([],[B:Index:rel(X,Y,Sym,0)]))).


/* =========================================================================
   Other Modifiers
========================================================================= */

/* -------------------------------------------------------------------------
   Superlative: at least/most/best (no idea how to specify semantics)
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[(s:_/s:_)/(s:asup\np),(np/np)/(s:asup\np)]), !,
   Sem = lam(VP,lam(NP,lam(P,app(app(VP,NP),lam(X,merge(B:drs([],[B:Index:pred(X,Sym,a,0)]),app(P,X))))))).

semlex(Cat,_Sym,_Index,Att-Att,Sem):-
   member(Cat,[(C/C)/(s:asup\np), 
               (C\C)/(s:asup\np), 
               (C\C)\(s:asup\np),
               (C/C)\(s:asup\np)]), !,
   Sem = lam(_,lam(N,N)).


/* -------------------------------------------------------------------------
   Comparatives
------------------------------------------------------------------------- */

% more than 10 dogs
%
semlex(Cat,Sym,Index,Att-Att,Sem):-  
   Cat = ((n/n)/(n/n))\(s:adj\np), !,
   att(Att,sense,Sense),
   Sem = lam(M,lam(C,lam(P,lam(X,merge(app(P,X),
             app(app(M,lam(Q,app(Q,X))),
                   lam(E,merge(B1:drs([B1:[]:Z],[]),
                         app(app(C,lam(Y,B2:drs([],[B2:Index:rel(E,Y,Sym,Sense)]))),Z))))))))).

% more than $ 600 million
%
semlex(Cat,Sym,Index,Att-Att,Sem):- 
   Cat = (n/n)\(s:adj\np), !,
   att(Att,sense,Sense),
   Sem = lam(M,lam(P,lam(X,merge(B1:drs([B1:[]:Y],[]),
                                 merge(app(P,Y),
                                       app(app(M,lam(Q,app(Q,X))),lam(E,B:drs([],[B:Index:rel(E,Y,Sym,Sense)])))))))).
%   closing(CC),
%   Sem = lam(VP,lam(N,lam(X,app(app(VP,lam(P,merge(app(P,X),app(N,X)))),CC)))).

% this category is sometimes assigned to opening brackets
%
semlex(Cat,_Sym,_Index,Att-Att,Sem):- 
   Cat = (n/n)/(s:adj\np), !,
   closing(CC),
   Sem = lam(VP,lam(N,lam(X,app(app(VP,lam(P,merge(app(P,X),app(N,X)))),CC)))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[(np/np)\(s:adj\np),
               (np/np)/(s:adj\np)]), !,
   att(Att,sense,Sense),
   Sem = lam(VP,lam(NP,lam(P,app(app(VP,NP),lam(X,merge(B:drs([],[B:Index:pred(X,Sym,a,Sense)]),app(P,X))))))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[((np/np)/(np/np))\(s:adj\np),
               ((np\np)\(np\np))/(s:dcl\np)]), !, 
   att(Att,sense,Sense),
   Sem = lam(VP,lam(NPNP,lam(NP,lam(P,app(app(VP,app(NPNP,NP)),lam(X,merge(B:drs([],[B:Index:pred(X,Sym,a,Sense)]),app(P,X)))))))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   Cat = ((((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np)))/(((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np))))\(s:adj\np), !,
   att(Att,sense,Sense),
   Sem = lam(_AP,lam(MM,lam(M,lam(VP,app(app(MM,M),lam(Q,lam(F,app(app(VP,Q),lam(E,merge(B:drs([],[B:Index:pred(E,Sym,a,Sense)]),app(F,E))))))))))).


/* -------------------------------------------------------------------------
   Superlatives: (the) most/least ... 
------------------------------------------------------------------------- */

semlex(Cat,most,Index,Att-Att,Sem):-  
   member(Cat,[(n/n)/(d/n)]), !,
   Sem = lam(R,lam(P,lam(X,merge(app(P,X),
                                 B1:drs([],[B1:Index:imp(merge(B2:drs([B2:[]:Y],[B2:[]:not(B3:drs([],[B3:[]:eq(X,Y)]))]),app(P,Y)),
                                                        app(app(R,Y),X))]))))).

semlex(Cat,least,Index,Att-Att,Sem):-  
   member(Cat,[(n/n)/(d/n)]), !,
   Sem = lam(R,lam(P,lam(X,merge(app(P,X),
                                 B1:drs([],[B1:Index:imp(merge(B2:drs([B2:[]:Y],[B2:[]:not(B3:drs([],[B3:[]:eq(X,Y)]))]),app(P,Y)),
                                                        app(app(R,X),Y))]))))).



/* -------------------------------------------------------------------------
   Compound numerals
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-Att,Sem):-  
   member(Cat,[(n/n)/(n/n),
               (n\n)/(n\n),
               (n/n)\(n/n)]), 
   string2digit(Sym,Digit), !,
   Sem = lam(Z,lam(P,lam(X,merge(B:drs([],[B:Index:card(X,Digit,eq)]),
                                 app(app(Z,P),X))))).


/* -------------------------------------------------------------------------
   Compound superlative adjectives
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-Att,Sem):-        
   \+ option('--semantics',drg),
   att(Att,pos,'JJS'),
   member(Cat,[(n/n)/(n/n),        %%%% Example: ... fastest growing segment
               (n/n)\(n/n)]), !,   %%%% Example: ... third largest bank (incorrect semantics!)
   Sem = lam(Z,lam(P,lam(X,merge(app(app(Z,P),X),
                                 B1:drs([],[B1:[]:imp(merge(B2:drs([B2:[]:Y],[B2:[]:not(B3:drs([],[B3:[]:eq(X,Y)]))]),
                                                              app(app(Z,P),Y)),
                                                        B4:drs([],[B4:Index:rel(X,Y,Sym,0)]))]))))).

/* -------------------------------------------------------------------------
   Intensifiers
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-Att,Sem):-  
   att(Att,pos,Pos),
   member(Pos,['NNP','NNPS']),
   member(Cat,[(n/n)/(n/n),
               (n\n)/(n\n),
               (n/n)\(n/n)]), !,
   att(Att,namex,Ne), neClassType(Ne,Class,Type),
   Sem = lam(Z,lam(P,lam(X,merge(B:drs([],[B:Index:named(X,Sym,Class,Type)]),
                                 app(app(Z,P),X))))).

semlex(Cat,Sym,Index,Att-Att,Sem):-  
   member(Cat,[(n/n)/(n/n),
               (n\n)/(n\n),
               (n/n)\(n/n)]), !,
   att(Att,sense,Sense),
   Sem = lam(Z,lam(P,lam(X,merge(B:drs([],[B:Index:pred(X,Sym,a,Sense)]),
                                 app(app(Z,P),X))))).

semlex(Cat,Sym,Index,Att-Att,Sem):-  
   Cat = ((n/n)/(n/n))/((n/n)/(n/n)), !,
   att(Att,sense,Sense),
   Sem = lam(M,lam(A,lam(P,lam(X,app(app(app(M,A),lam(Y,merge(B:drs([],[B:Index:pred(Y,Sym,a,Sense)]),app(P,Y)))),X))))).


/* -------------------------------------------------------------------------
   Compound adjectives (actually, the hyphen in compound adjectives)
------------------------------------------------------------------------- */

semlex(pp\n,'-',Index,Att-Att,Sem):- !,
   Sem = lam(N,lam(X,alfa(def,merge(B1:drs([B1:[]:Y],[]),app(N,Y)),
                                    B2:drs([],[B2:Index:rel(X,Y,loc_rel,0)])))).

semlex(pp\n,'-',Index,Att-Att,Sem):- !,
   Sem = lam(N,lam(X,merge(B:drs([B:[]:Y],[B:Index:rel(X,Y,rel,0)]),
                           app(N,Y)))).

semlex(pp\n,'-',Index,Att-Att,Sem):- !,
   Sem = lam(N,lam(X,merge(B:drs([B:[]:Y],[B:Index:rel(X,Y,attribute,0)]),
                           app(N,Y)))).


/* -------------------------------------------------------------------------
   Definite Prepositions
------------------------------------------------------------------------- */

% except
%
semlex(Cat,except,Index,Att-Att,Sem):- 
   Cat = (n\n)/pp, !,
   Sem = lam(PP,lam(P,lam(X,merge(app(P,X),B:drs([],[B:Index:not(app(PP,X))]))))).

semlex(Cat,Sym,Index,Att-Att,Sem):- 
   Cat = (n\n)/pp, !,
   att(Att,sense,Sense),
   Sem = lam(PP,lam(P,lam(X,merge(app(P,X),merge(B:drs([],[B:Index:pred(X,Sym,a,Sense)]),app(PP,X)))))).

% Range constructions (e.g., "10 to 20")
%
semlex(Cat,to,Index,Att-Att,Sem):- 
   Cat = (n\n)/n, !,
   Sem = lam(N,lam(P,lam(X,B:drs([],[B:Index:or(app(P,X),app(N,X))])))).

% seven cents a share
%
semlex(Cat,Sym,Index,Att1-Att2,Sem):- 
   member(Sym,[a,an]),
   Cat = (n\n)/n, !,
   rel(for,Att1-Att2,Relation),
   Sem = lam(N,lam(P,lam(X,B1:drs([],[B1:[]:imp(merge(B2:drs([B2:[]:Y],[]),app(N,Y)),
                                                merge(B3:drs([],[B3:Index:rel(X,Y,Relation,0)]),app(P,X)))])))).

semlex(Cat,Sym,Index,Att-Att,Sem):- 
   member(Sym,[the,that,this,these,those]),
   Cat = (n\n)/n, !,
   Sem = lam(N,lam(P,lam(X,alfa(def,merge(B1:drs([B1:[]:Y],[]),app(N,Y)),
                                    merge(B2:drs([],[B2:Index:rel(X,Y,rel,0)]),app(P,X)))))).

semlex(Cat,Sym,Index,Att-Att,Sem):- 
   Cat = (n\n)/n, !,
   Sem = lam(N,lam(P,lam(X,merge(merge(B1:drs([B1:[]:Y],[]),app(N,Y)),
                                 merge(B2:drs([],[B2:Index:rel(X,Y,Sym,0)]),app(P,X)))))).

semlex(Cat,'$',Index,Att-Att,Sem):- 
   Cat = (n/n)/n, !,
   Sem = lam(N,lam(P,lam(X,merge(merge(B1:drs([B1:[]:Y],[B1:Index:pred(Y,dollar,n,1)]),app(N,Y)),
                                 merge(B2:drs([],[B2:[]:rel(X,Y,rel,0)]),app(P,X)))))).

semlex(Cat,Sym,Index,Att-Att,Sem):- 
   Cat = (n/n)/n, !,
   att(Att,sense,Sense),
   Sem = lam(N,lam(P,lam(X,merge(merge(B1:drs([B1:[]:Y],[B1:Index:pred(Y,Sym,n,Sense)]),app(N,Y)),
                                 merge(B2:drs([],[B2:[]:rel(X,Y,rel,0)]),app(P,X)))))).

semlex(((n/n)\(n/n))/(n/n),_,Index,Att-Att,Sem):- !,
   Sem = lam(M2,lam(M1,lam(P,lam(X,B:drs([],[B:Index:or(app(app(M1,P),X),
                                                        app(app(M2,P),X))]))))).

semlex(((n/n)\(n/n))/n,_,Index,Att-Att,Sem):- !,
   Sem = lam(N,lam(M,lam(P,lam(X,B:drs([],[B:Index:or(app(app(M,P),X),
                                                      merge(app(N,X),app(P,X)))]))))).

semlex(((n/pp)\(n/pp))/n,_,Index,Att-Att,Sem):- !,
   Sem = lam(N,lam(RN,lam(PP,lam(X,merge(B:drs([B:[]:Y],[B:Index:rel(X,Y,rel,0)]),
                                         merge(app(N,Y),app(app(RN,PP),X))))))).


/* -------------------------------------------------------------------------
   Complementizers (Wh)
------------------------------------------------------------------------- */

semlex(s:qem/s:dcl,Sym,Index,Att-Att,Sem):- 
   ( Sym=how,   Pred=manner,       Sense=2, Rel=manner_rel;
     Sym=where, Pred=location,     Sense=1, Rel=loc_rel;
     Sym=when,  Pred=unit_of_time, Sense=1, Rel=time_rel;
     Sym=why,   Pred=reason,       Sense=2, Rel=reason_rel), !,
   Sem = lam(S,lam(F,B1:drs([],[B1:[]:duplex(whq,
                                       B2:drs([B2:[]:X],[B2:Index:pred(X,Pred,n,Sense)]),
                                       X,
                                       app(S,lam(E,merge(B3:drs([],[B3:[]:rel(E,X,Rel,0)]),
                                                         app(F,E)))))]))).


/* -------------------------------------------------------------------------
   Complementizers 
------------------------------------------------------------------------- */

semlex(Cat,_Sym,Index,Att-Att,Sem):-
   member(Cat,[s:em/s:dcl,s:bem/s:b,s:em/s:b,s:qem/s:dcl]), !,
   closing(CC),
   Sem = lam(S,lam(E,merge(B:drs([B:Index:K],
                                 [B:[]:prop(K,app(S,CC))]),
                           app(E,K)))).

semlex(Cat,_,Index,Att-Att,Sem):- 
   Cat = (s:for/(s:to\np))/np, !,
   closing(CC),
   Sem = lam(NP,lam(VP,lam(E,merge(B:drs([B:Index:K],
                                         [B:[]:prop(K,app(app(VP,NP),CC))]),
                                   app(E,K))))).

semlex(Cat,_Sym,_Index,Att-Att,Sem):-
   category(comp,Cat,_), !,
   Sem = lam(S,lam(F,app(S,F))).

semlex(Cat,_,Index,Att-Att,Sem):-
   Cat = np/s:dcl, !,
   closing(CC),
   Sem = lam(S,lam(P,merge(B:drs([B:Index:X],
                                 [B:[]:prop(X,app(S,CC))]),
                           app(P,X)))).


/* -------------------------------------------------------------------------
   Locative Adverbs
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att1-Att2,Sem):-
   category(vpadv,Cat,_),
   member(Sym,[somewhere]), !,
   rel(in,Att1-Att2,Relation),
   Sem = lam(X,lam(Q,lam(F,app(app(X,Q),lam(E,merge(B:drs([B:[]:Z],[B:Index:pred(Z,location,n,1),
                                                                    B:[]:rel(E,Z,Relation,0)]),app(F,E))))))).

semlex(Cat,Sym,Index,Att1-Att2,Sem):-
   category(vpadv,Cat,_),
   member(Sym,[anywhere,everywhere]), !,
   rel(in,Att1-Att2,Relation),
   Sem = lam(X,lam(Q,lam(F,app(app(X,Q),lam(E,B1:drs([],[B1:[]:imp(B2:drs([B2:[]:Z],[B2:Index:pred(Z,location,n,1)]),
                                                                   merge(B3:drs([],[B3:[]:rel(E,Z,Relation,0)]),app(F,E)))])))))).

semlex(Cat,Sym,Index,Att1-Att2,Sem):-
   category(vpadv,Cat,_),
   member(Sym,[nowhere]), !,
   rel(in,Att1-Att2,Relation),
   Sem = lam(X,lam(Q,lam(F,app(app(X,Q),lam(E,B1:drs([],[B1:Index:not(merge(B2:drs([B2:[]:Z],[B2:[]:pred(Z,location,n,1),
                                                                                              B2:[]:rel(E,Z,Relation,0)]),app(F,E)))])))))).


/* -------------------------------------------------------------------------
   Not 
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-Att,Sem):-
   category(vpadv,Cat,_),
   option('--semantics',drg),
   notSymbol(Sym), !,
   Sem = lam(X,lam(Q,lam(F,B1:drs([],[B1:[]:not(app(app(X,Q),lam(E,merge(B2:drs([],[B2:Index:pred(E,Sym,s,1)]),app(F,E)))))])))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   category(vpadv,Cat,_),
   notSymbol(Sym), 
   option('--x',true), !,
%  Sem = lam(V,lam(Q,lam(F,app(Q,lam(X,B:drs([],[B:Index:not(app(app(V,lam(P,app(P,X))),F))])))))). %%% subject wide scope (preferred?)
   Sem = lam(X,lam(Q,lam(F,B:drs([],[B:Index:not(app(app(X,Q),F))])))).                             %%% negation wide scope (dispreferred?)

semlex(Cat,Sym,Index,Att-Att,Sem):-
   category(vpadv,Cat,_),
   notSymbol(Sym), !,
   Sem = lam(V,lam(Q,lam(F,app(Q,lam(X,B:drs([],[B:Index:not(app(app(V,lam(P,app(P,X))),F))])))))). %%% subject wide scope (preferred?)
%  Sem = lam(X,lam(Q,lam(F,B:drs([],[B:Index:not(app(app(X,Q),F))])))).                             %%% negation wide scope (dispreferred?)


/* -------------------------------------------------------------------------
   Cardinals that function as VP modifiers (often wrongly analysed)
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att1-Att2,Sem):-
   att(Att1,pos,'CD'),
   category(vpadv,Cat,_),
   string2digit(Sym,Digit), !,
   rel(for,Att1-Att2,Relation),
   Sem = lam(X,lam(Q,lam(F,app(app(X,Q),lam(E,merge(B:drs([B:[]:X],[B:Index:card(X,Digit,eq),
                                                                    B:[]:rel(E,X,Relation,0)]),app(F,E))))))).

/* -------------------------------------------------------------------------
   NPs that function as VP modifiers
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att1-Att2,Sem):-
   category(vpadv,Cat,_),
   att(Att1,pos,Pos),
   member(Pos,['NNP','NNPS']), !,
   att(Att1,namex,NE), neClassType(NE,Class,Type),
   rel(on,Att1-Att2,Relation),
   Sem = lam(X,lam(Q,lam(F,app(app(X,Q),lam(E,merge(B:drs([B:[]:X],[B:Index:named(X,Sym,Class,Type),
                                                                    B:[]:rel(E,X,Relation,0)]),app(F,E))))))).

semlex(Cat,Sym,Index,Att1-Att2,Sem):-
   category(vpadv,Cat,_),
   att(Att1,pos,Pos),
   member(Pos,['NN','NNS']), !,
   att(Att1,sense,Sense),
   rel(on,Att1-Att2,Relation),
   Sem = lam(X,lam(Q,lam(F,app(app(X,Q),lam(E,merge(B:drs([B:[]:X],[B:Index:pred(X,Sym,n,Sense),
                                                                    B:[]:rel(E,X,Relation,0)]),app(F,E))))))).


/* -------------------------------------------------------------------------
   Comparative (more)

semlex(Cat,Sym,Index,Att-Att,Sem):-
   Sym = more,
   Cat = (s:adj\np)/(s:adj\np), !,
   Sem = lam(X,lam(Q,lam(F,app(app(X,Q),lam(D1,merge(B:drs([B:[]:D2],[B:Index:rel(D1,D2,more,0)]),app(F,D1))))))).
------------------------------------------------------------------------- */


/* -------------------------------------------------------------------------
   Adverbs (VP modifying)
------------------------------------------------------------------------- */

%semlex(Cat,Sym,Index,Att-Att,Sem):-
%   category(vpadv,Cat,_), 
%   option('--x',true),
%   negprefix(_, Sym, Prefix, Core), !,
%   Sem = lam(X,lam(Q,lam(F,B1:drs([],[B1:Index:not(app(app(X,Q),lam(E,merge(B2:drs([],[B2:Index:pred(E,Prefix,a,71),B2:Index:pred(E,Core,a,1)]),app(F,E)))))])))).

%semlex(Cat,Sym,Index,Att-Att,Sem):-
%   category(vpadv,Cat,_), 
%   option('--x',true), 
%   negsuffix(_, Sym, Suffix, Core), !,
%   Sem = lam(X,lam(Q,lam(F,B1:drs([],[B1:Index:not(app(app(X,Q),lam(E,merge(B2:drs([],[B2:Index:pred(E,Suffix,a,72),B2:Index:pred(E,Core,a,1)]),app(F,E)))))])))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   category(vpadv,Cat,_), !, 
   att(Att,sense,Sense),
   Sem = lam(X,lam(Q,lam(F,app(app(X,Q),lam(E,merge(B:drs([],[B:Index:pred(E,Sym,r,Sense)]),app(F,E))))))).


/* -------------------------------------------------------------------------
   "hard to take", "easy to please"
------------------------------------------------------------------------- */

semlex((s:adj\np)/((s:to\np)/np),Sym,Index,Att-Att,Sem):- !,
   closing(CC),
   Sem = lam(TV,lam(Q,lam(F,merge(B1:drs([B1:[]:K],[B1:Index:pred(K,Sym,a,0),
                                                    B1:[]:prop(K,app(app(app(TV,Q),lam(P,merge(B2:drs([B2:[]:X],[B2:[]:pred(X,thing,n,12)]),app(P,X)))),CC))]),
                                  app(F,K))))).


/* -------------------------------------------------------------------------
   Definite prepositions 
------------------------------------------------------------------------- */

% "the" as apposition trigger
%
semlex(Cat,the,Index,Att-Att,Sem):- 
   Cat = (np\np)/n, !,
   Sem = lam(N,lam(Q,lam(P,app(Q,lam(X,merge(B:drs([B:[]:Y],[B:Index:eq(X,Y)]),
                                             merge(app(N,Y),app(P,X)))))))).

% temporal
%
semlex(Cat,Sym,Index,Att1-Att2,Sem):- 
   member(Sym,[this,that,those,these]),
   Cat = (np\np)/n, !,
   rel(in,Att1-Att2,Relation),
   Sem = lam(N,lam(Q,lam(P,app(Q,lam(X,merge(B:drs([B:[]:Y],[B:Index:rel(X,Y,Relation,0)]),
                                             merge(app(N,Y),app(P,X)))))))).

% seven cents a share
%
semlex(Cat,Sym,Index,Att1-Att2,Sem):-
   member(Sym,[a,an]),
   Cat = (np\np)/n, !,
   rel(for,Att1-Att2,Relation),
   Sem = lam(N,lam(Q,lam(P,B1:drs([],[B1:[]:imp(merge(B2:drs([B2:[]:Y],[]),
                                                      app(N,Y)),
                                                app(Q,lam(X,merge(B3:drs([],[B3:Index:rel(X,Y,Relation,0)]),
                                                               app(P,X)))))])))).

semlex(Cat,Sym,Index,Att-Att,Sem):- 
   Cat = (np\np)/n, !,
   Sem = lam(N,lam(Q,lam(P,app(Q,lam(X,merge(B:drs([B:[]:Y],[B:Index:rel(X,Y,Sym,0)]),
                                             merge(app(N,Y),app(P,X)))))))).


/* -------------------------------------------------------------------------
   Prepositional Phrases
------------------------------------------------------------------------- */

semlex(pp,Sym,Index,Att-Att,Sem):- !,
   Sem = lam(X,B:drs([B:[]:Y],[B:Index:pred(Y,thing,n,12),
                               B:[]:rel(X,Y,Sym,0)])).


/* -------------------------------------------------------------------------
   Prepositions
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[(n\n)/np,
               (n/n)/np,
               (n/n)\np,
               (n\n)\np]), !,
   Sem = lam(Q,lam(P,lam(X,merge(app(P,X),app(Q,lam(Y,B:drs([],[B:Index:rel(X,Y,Sym,0)]))))))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   Cat = ((n/(s:to\np))\(n/(s:to\np)))/np, !,
   Sem = lam(Q,lam(N,lam(VP,lam(X,merge(app(app(N,VP),X),app(Q,lam(Y,B:drs([],[B:Index:rel(X,Y,Sym,0)])))))))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[((n/n)\(n/n))/np,
               ((n/pp)\(n/pp))/np]), !,
   Sem = lam(Q,lam(Z,lam(P,lam(Y,app(app(Z,lam(X,merge(app(Q,lam(Y,B:drs([],[B:Index:rel(X,Y,Sym,0)]))),
                                                       app(P,X)))),Y))))).  

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[((np\np)\(np\np))/np,
               ((np\np)/(np\np))/np]), !,
   Sem = lam(Q1,lam(R,lam(Q2,lam(P,merge(app(Q1,lam(X,app(Q2,lam(Y,B:drs([],[B:Index:rel(Y,X,Sym,0)]))))),app(app(R,Q2),P)))))).

semlex(Cat,_Sym,Index,Att-Att,Sem):-
   Cat = (np/np)/n, !,
   Sem = lam(N,lam(Q,lam(P,merge(merge(B1:drs([B1:[]:Y],[]),app(N,Y)),app(Q,lam(X,merge(B2:drs([],[B2:Index:rel(X,Y,rel,0)]),app(P,X)))))))).

semlex(Cat,except,Index,Att-Att,Sem):-
   Cat = (np\np)/pp, !,
   Sem = lam(PP,lam(NP,lam(P,app(NP,lam(X,merge(B:drs([],[B:Index:not(app(PP,X))]),app(P,X))))))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   Cat = (np\np)/pp, !,
   att(Att,sense,Sense),
%  Sem = lam(PP,lam(NP,lam(P,app(NP,lam(X,merge(B:drs([],[B:Index:pred(X,Sym,n,Sense)]),merge(app(PP,X),app(P,X)))))))).
   Sem = lam(PP,lam(NP,lam(P,app(NP,lam(X,merge(B:drs([B:[]:Y],[B:Index:rel(X,Y,Sym,Sense)]),merge(app(PP,Y),app(P,X)))))))).

semlex(Cat,Tok,Index,Att-Att,Sem):-
   Tok = without, Sym = with,
   member(Cat,[(np\np)/np,(np/np)/np]), !,
   Sem = lam(Q1,lam(Q2,lam(P,app(Q2,lam(X,merge(B1:drs([],[B1:Index:not(app(Q1,lam(Y,B2:drs([],[B2:Index:rel(X,Y,Sym,0)]))))]),app(P,X))))))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[(np\np)/np,(np\np)\np,(np/np)/np]),
   att(Att,scope,inv), !,
   Sem = lam(Q1,lam(Q2,lam(P,app(Q1,lam(Y,app(Q2,lam(X,merge(B:drs([],[B:Index:rel(X,Y,Sym,0)]),app(P,X))))))))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[(np\np)/np,(np\np)\np,(np/np)/np]), !,
   Sem = lam(Q1,lam(Q2,lam(P,app(Q2,lam(X,app(Q1,lam(Y,merge(B:drs([],[B:Index:rel(X,Y,Sym,0)]),app(P,X))))))))).

% permafrost three meters below the surface
semlex(Cat,Sym,Index,Att-Att,Sem):-
   Cat = ((np\np)\np)/np, !,
   Sem = lam(Q1,lam(Q2,lam(Q3,lam(P,app(Q3,lam(X,app(Q2,lam(Y,app(Q1,lam(Z,merge(B:drs([],[B:[]:rel(X,Y,rel,0),
                                                                                           B:Index:rel(Y,Z,Sym,0)]),
                                                                                 app(P,X)))))))))))).

semlex(Cat,Sym,Index,Att-Att,Sem):- 
   member(Cat,[((np\np)/(s:to\np))/np,
               ((np\np)/(s:ng\np))/np]), !,
   closing(CC),
   Sem = lam(NP1,lam(VP,lam(NP2,lam(P,app(NP2,lam(X,merge(B:drs([B:[]:Y],[B:Index:rel(X,Y,Sym,0),
                                                                          B:[]:prop(Y,app(app(VP,NP1),CC))]),
                                                          app(P,X)))))))).

semlex(Cat,Sym,Index,Att-Att,Sem):- 
   member(Cat,[((np\np)/pp)/np]), !,
   Sem = lam(Q1,lam(PP,lam(Q2,lam(P,app(Q2,lam(X,app(Q1,lam(Y,merge(B:drs([],[B:Index:rel(X,Y,Sym,0)]),
                                                                    merge(app(PP,X),app(P,X))))))))))).

semlex((n/pp)/(s:adj\np),Sym,Index,Att-Att,Sem):-
   Sem = lam(VP,lam(PP,lam(X,app(app(VP,lam(P,app(P,X))),lam(E,merge(B:drs([],[B:Index:rel(X,E,Sym,0)]),app(PP,E))))))).

semlex(((s:wq/s:q)\(s:wq/s:q))/np,Sym,Index,Att-Att,Sem):- !,
   Sem = lam(NP,lam(U,lam(YNQ,lam(F,app(app(U,YNQ),lam(E,merge(app(NP,lam(X,B:drs([],[B:Index:rel(E,X,Sym,0)]))),app(F,E)))))))).


semlex(Cat,_,Index,Att-Att,Sem):-
   member(Cat,[((s:adj\np)/(s:adj\np))/n]), !, %  a bit
   Sem = lam(N,lam(A,lam(Q,lam(F,app(app(A,Q),lam(E,merge(merge(B:drs([B:[]:X],[B:Index:rel(E,X,rel,0)]),
                                                                 app(N,X)),
                                                           app(F,E)))))))).

% Passive Clause
%
semlex(Cat,by,Index,Att1-Att2,Sem):-
   Cat = ((s:pss\np)\(s:pss\np))/np, 
   roles(by,Cat,[Role],Att1-Att2), !,
   Sem = lam(Q2,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,app(Q2,lam(Y,merge(B:drs([],[B:Index:role(E,Y,Role,1)]),app(F,E)))))))))).
     
               
semlex(Cat,Tok,Index,Att-Att,Sem):-
   Tok = without, Sym = with,
   member(Cat,[((s:X\np)\(s:X\np))/np, 
               ((s:X\np)/(s:X\np))/np]), !,
   Sem = lam(Q2,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,merge(B1:drs([],[B1:Index:not(app(Q2,lam(Y,B2:drs([],[B2:Index:rel(E,Y,Sym,0)]))))]),
                                                           app(F,E)))))))).


/* ONGOING WORK ON COMPARATIVES

% COMP than NP
semlex(Cat,than,Index,Att-Att,Sem):-
   member(Cat,[((s:adj\np)\(s:adj\np))/np]), !,
   F1 = lam(E1,merge(B1:drs([],[B1:[]:role(E1,N1,measure,1)]),app(F,E1))),
   F2 = lam(E2,B2:drs([],[B2:[]:role(E2,N2,measure,1)])),
   Sem = lam(Q2,lam(AP,lam(Q1,lam(F,merge(B:drs([B:[]:N1,B:[]:N2],[B:Index:rel(N2,N1,temp_before,1)]),
                                          merge(app(app(AP,Q1),F1),
                                                app(app(AP,Q2),F2))))))).

*/

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/np,
	       ((s:X\np)/(s:X\np))/np,
               ((s:X\np)\(s:X\np))\np,
               ((s:X\np)/(s:X\np))\np]),
   att(Att,scope,inv), !,
   Sem = lam(Q2,lam(V,lam(Q,lam(F,app(Q2,lam(Y,app(app(V,Q),lam(E,merge(B:drs([],[B:Index:rel(E,Y,Sym,0)]),app(F,E)))))))))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/np, 
               ((s:X\np)/(s:X\np))/np, 
               ((s:X\np)\(s:X\np))\np, 
               ((s:X\np)/(s:X\np))\np]), !,
   Sem = lam(Q2,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,app(Q2,lam(Y,merge(B:drs([],[B:Index:rel(E,Y,Sym,0)]),app(F,E)))))))))).

% from 8% the week before
semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[(((s:X\np)\(s:X\np))\np)/np,
               (((s:X\np)\(s:X\np))/np)/np]), !,
   Sem = lam(Q3,lam(Q2,lam(V,lam(Q1,lam(F,app(app(V,Q1),lam(E,app(Q3,lam(Z,merge(B1:drs([],[B1:Index:rel(E,Z,Sym,0)]),
                                                                                 app(Q2,lam(Y,merge(B2:drs([],[B2:[]:rel(E,Y,rel,0)]),
                                                                                                    app(F,E)))))))))))))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[(((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np)))/np,
               (((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))/np,
               (((s:X\np)/(s:X\np))\((s:X\np)/(s:X\np)))/np,
               (((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))\np]), !,
   Sem = lam(Q,lam(AV,lam(VP,lam(NP,lam(F,app(app(app(AV,VP),NP),lam(E,merge(app(Q,lam(Z,B:drs([],[B:Index:rel(E,Z,Sym,0)]))),
                                                                             app(F,E))))))))).

semlex(Cat,_Sym,Index,Att1-Att2,Sem):-
   member(Cat,[(((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np)))/n,
               (((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))/n,
               (((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))\n,
               (((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))/pp]), !,
   rel(in,Att1-Att2,Relation),
   Sem = lam(N,lam(AV,lam(VP,lam(NP,lam(F,app(app(app(AV,VP),NP),lam(E,merge(merge(B:drs([B:[]:Z],[B:Index:rel(E,Z,Relation,0)]),
                                                                                   app(N,Z)),
                                                                             app(F,E))))))))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[(((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np)))/s:dcl,
               (((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))/s:dcl,
               (((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))\s:dcl]), !,
   Sem = lam(S,lam(AV,lam(VP,lam(NP,lam(F,app(app(app(AV,VP),NP),lam(E,merge(app(S,lam(Z,B:drs([],[B:Index:rel(E,Z,Sym,0)]))),
                                                                             app(F,E))))))))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[(((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))/(s:pss\np),
               (((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))/(s:adj\np)]), !,
   Sem = lam(VP1,lam(AV,lam(VP2,lam(NP,lam(F,app(app(app(AV,VP2),NP),lam(E,merge(app(app(VP1,lam(P,merge(B1:drs([B1:[]:Z],[B1:[]:pred(Z,thing,n,12)]),
                                                                                                         app(P,Z)))),lam(Z,B2:drs([],[B2:Index:rel(E,Z,Sym,0)]))),
                                                                                 app(F,E))))))))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   Cat = pp/np, !,
   Sem = lam(Q,lam(X,app(Q,lam(Y,B:drs([],[B:Index:rel(X,Y,Sym,0)]))))).

semlex(Cat,'$',Index,Att-Att,Sem):-
   Cat = pp/n, !,
   Sem = lam(N,lam(X,merge(B:drs([B:[]:Y],[B:Index:pred(Y,dollar,n,1),B:[]:rel(X,Y,rel,0)]),app(N,Y)))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   Cat = pp/n, !,
   Sem = lam(N,lam(X,merge(B:drs([B:[]:Y],[B:Index:rel(X,Y,Sym,0)]),app(N,Y)))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   Cat = (pp\np)/np, !,
   Sem = lam(Q1,lam(Q2,lam(X,merge(app(Q2,lam(Z,B1:drs([],[B1:[]:rel(X,Z,rel,0)]))),
                                   app(Q1,lam(Y,B2:drs([],[B2:Index:rel(X,Y,Sym,0)]))))))).


/* -------------------------------------------------------------------------
   PP complements 
------------------------------------------------------------------------- */

%  ... limited to providing ...
%
semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[pp/(s:ng\np),pp/(s:adj\np),pp/(s:b\np)]), !,
%  closing(CC),
%  Sem = lam(VP,lam(X,app(app(VP,lam(P,merge(B:drs([B:[]:Y],[B:Index:rel(X,Y,Sym,0)]),app(P,Y)))),CC))).
   Sem = lam(VP,lam(X,app(app(VP,lam(P,merge(B:drs([B:[]:Y],[]),app(P,Y)))),lam(E,C:drs([],[C:Index:rel(X,E,Sym,0)]))))).

%   Prep + NP + VP (... results in shareholders receiving ...)
%
semlex((pp/(s:_\np))/np,Sym,Index,Att-Att,Sem):- !,
   Sem = lam(NP,lam(VP,lam(E,app(app(VP,NP),lam(F,B:drs([],[B:Index:rel(E,F,Sym,0)])))))).

%   PP Relative Pronous
%
semlex(Cat,_Sym,_Index,Att-Att,Sem):-
   Cat = pp/(s:_/np), !,
   closing(CC),
   Sem = lam(VP,lam(X,app(app(VP,lam(P,app(P,X))),CC))).


/* -------------------------------------------------------------------------
   Discourse connectors: if
------------------------------------------------------------------------- */

semlex(Cat,if,Index,Att-Att,Sem):-
  member(Cat,[((s:X\np)\(s:X\np))/s:dcl]), !,
  closing(CC),
  Sem = lam(S,lam(V,lam(Q,lam(F,B:drs([],[B:Index:imp(app(S,CC),
                                                      app(app(V,Q),F))]))))).


/* -------------------------------------------------------------------------
   Discourse connectors: where
------------------------------------------------------------------------- */

semlex(Cat,where,Index,Att1-Att2,Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/s:dcl]), !,
   rel(in,Att1-Att2,Relation),
   Sem = lam(S,lam(V,lam(Q,lam(F,merge(merge(B1:drs([B1:[]:T],[B1:Index:pred(T,location,n,1)]),
                                                app(app(V,Q),lam(E,merge(B2:drs([],[B2:[]:rel(E,T,Relation,0)]),app(F,E))))),
                                       app(S,lam(E,B3:drs([],[B3:[]:rel(E,T,Relation,0)])))))))).


/* -------------------------------------------------------------------------
   Discourse connectors: when
------------------------------------------------------------------------- */

semlex(Cat,when,Index,Att-Att,Sem):-
   option('--tense',true),
   option('--theory',drt),
   member(Cat,[((s:X\np)\(s:X\np))/s:dcl]), !,
   Sem = lam(S,lam(V,lam(Q,lam(F,merge(merge(B1:drs([B1:[]:T],[B1:Index:pred(T,time,n,1)]),
                                             app(S,lam(E,B2:drs([],[B2:[]:rel(E,T,temp_included,1)])))),
                                       app(app(V,Q),lam(E,merge(B3:drs([],[B3:[]:rel(E,T,temp_included,1)]),app(F,E))))))))).


/* -------------------------------------------------------------------------
   Discourse connectors: as does NP
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-Att,Sem):-
   option('--theory',drt),
   Cat = ((s:X\np)\(s:X\np))/s:inv, !,
   att(Att,sense,Sense),
   Sem = lam(S,lam(V,lam(Q,lam(F,merge(app(app(V,Q),lam(E,app(F,E))),
                                       app(S,lam(E,B:drs([],[B:Index:pred(E,Sym,r,Sense)])))))))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   option('--theory',sdrt),
   Cat = ((s:X\np)\(s:X\np))/s:inv, !,
   closing(CC),
   Sem = lam(S,lam(V,lam(Q,lam(F,sdrs([sub(lab(K1,B1),lab(K2,B2))],[Index:rel(K1,K2,Sym)]))))),
   B1 = app(app(V,Q),lam(E,app(F,E))),
   B2 = app(S,CC).


/* -------------------------------------------------------------------------
   Discourse connectors (VP modifying)
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-Att,Sem):-
   option('--theory',sdrt),
   member(Cat,[((s:X\np)\(s:X\np))/s:_,
               ((s:X\np)/(s:X\np))/s:_]), !,
   closing(CC),
   Sem = lam(S,lam(V,lam(Q,lam(F,sdrs([sub(lab(K1,B1),lab(K2,B2))],[Index:rel(K1,K2,Sym)]))))),
   B1 = app(app(V,Q),F),
   B2 = app(S,CC).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/s,
               ((s:X\np)/(s:X\np))/s,
               ((s:X\np)\(s:X\np))/s:_,
               ((s:X\np)/(s:X\np))/s:_]), !,
   closing(CC),
   Sem = lam(S,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,merge(B:drs([B:[]:Z],
                                                                [B:Index:rel(E,Z,Sym,0),
                                                                 B:[]:prop(Z,app(S,CC))]),
                                                          app(F,E)))))))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[(((s:X\np)\(s:X\np))\np)/s:dcl ]), !,
   closing(CC),
   Sem = lam(S,lam(NP,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,merge(B:drs([B:[]:Z],
                                                                       [B:Index:rel(E,Z,Sym,0),
                                                                        B:[]:prop(Z,app(S,CC))]),
                                                                 merge(app(NP,lam(U,B2:drs([],[B2:[]:rel(E,U,rel,0)]))),
                                                                       app(F,E)))))))))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[((s:X\np)/s:_)/(s:X\np)]), !,
   closing(CC),
   Sem = lam(V,lam(S,lam(Q,lam(F,app(app(V,Q),lam(E,merge(B:drs([B:[]:Z],
                                                                [B:Index:rel(E,Z,Sym,0),
                                                                 B:[]:prop(Z,app(S,CC))]),
                                                          app(F,E)))))))).


/* -------------------------------------------------------------------------
   Prepositions:  "VP prep VPing"
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-Att,Sem):-
   Sym = without,
   member(Cat,[((s:X\np)\(s:X\np))/(s:ng\np),
               ((s:X\np)/(s:X\np))/(s:ng\np)]), !,
   closing(CC),
   Sem = lam(VA,lam(VM,lam(Q,lam(F,app(Q,
                                       lam(Z,app(app(VM,lam(P,app(P,Z))),
                                                 lam(E,merge(app(F,E),
                                                             B:drs([],[B:Index:not(app(app(VA,lam(P,app(P,Z))),
                                                                                   CC))])))))))))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/(s:ng\np),
               ((s:X\np)/(s:X\np))/(s:ng\np)]), !,
   Sem = lam(VA,lam(VM,lam(Q,lam(F,app(Q,
                                       lam(Z,app(app(VM,lam(P,app(P,Z))),
                                                 lam(E,merge(app(F,E),
                                                             app(app(VA,lam(P,app(P,Z))),
                                                                 lam(G,B:drs([],[B:Index:rel(E,G,Sym,0)])))))))))))).



%%
%% Need to check this: discourse referent is introduced without properties
%% Similar semantics as above case might do.
%%
semlex(Cat,Sym,Index,Att-Att,Sem):-
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

               ((s:b\np)/(s:ng\np))/(s:adj\np), 
               ((s:b\np)/(s:dcl\np))/(s:adj\np), 
               ((s:pt\np)/(s:ng\np))/(s:adj\np), 
               ((s:dcl\np)/(s:b\np))/(s:adj\np), 
               ((s:dcl\np)/(s:adj\np))/(s:adj\np)]), !,
   Sem = lam(VP,lam(V,lam(Q,lam(F,app(app(V,Q),
                                      lam(E,app(app(VP,
                                                    lam(P,merge(B1:drs([B1:[]:Z],[]),app(P,Z)))),
                                                lam(Y,merge(B2:drs([],[B2:Index:rel(E,Y,Sym,0)]),app(F,E)))))))))).


/* -------------------------------------------------------------------------
   Control Prepositions (NP)
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[(((s:X\np)\(s:X\np))/(s:ng\np))/np,
               (((s:X\np)\(s:X\np))/(s:pt\np))/np,
               (((s:X\np)\(s:X\np))/(s:pss\np))/np,
               (((s:X\np)\(s:X\np))/(s:b\np))/np,
               (((s:X\np)\(s:X\np))/(s:to\np))/np]), !,
   Sem =  lam(NP,lam(VP,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,app(app(VP,lam(P,app(NP,lam(Y,merge(B:drs([],[B:Index:rel(E,Y,Sym,0)]),
                                                                                                 app(P,Y)))))),F)))))))).

/* -------------------------------------------------------------------------
   Control Prepositions (N)
   Example: "in", as in: "I had a plan in place to respond."
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[(((s:X\np)\(s:X\np))/(s:ng\np))/n,
               (((s:X\np)\(s:X\np))/(s:pt\np))/n,
               (((s:X\np)\(s:X\np))/(s:pss\np))/n,
               (((s:X\np)\(s:X\np))/(s:b\np))/n,
               (((s:X\np)\(s:X\np))/(s:to\np))/n]), !,
   closing(CC),
   Sem = lam(N,lam(VP,lam(Y,lam(Q,lam(F,app(Q,lam(U,app(app(Y,lam(P,app(P,U))),lam(E,merge(B:drs([B:[]:Z,B:[]:K],
                                                                                                 [B:Index:rel(E,Z,Sym,0),
								                                  B:[]:prop(K,app(app(VP,lam(P,app(P,U))),CC)),
                                                                                                  B:[]:rel(E,K,theme,0)]),
                                                                 merge(app(N,Z),app(F,E)))))))))))).


semlex(Cat,Sym,Index,Att-Att,Sem):-
   Cat = (((s:X\np)\(s:X\np))/pp)/np, !,
   Sem =  lam(NP,lam(PP,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,merge(app(NP,lam(Y,B:drs([],[B:Index:rel(E,Y,Sym,0)]))),
                                                                   merge(app(PP,E),app(F,E)))))))))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[((s:_\s:_)/pp)/np,
               ((s:_/s:_)/pp)/np]), !,   
   Sem = lam(Q,lam(PP,lam(S,lam(F,app(S,lam(E,merge(app(Q,lam(Y,B:drs([],[B:Index:rel(E,Y,Sym,0)]))),
                                                    merge(app(PP,E),app(F,E))))))))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[(s:wq\s:wq)/np,
               (s:X/s:X)\np,
               (s:X/s:X)/np,
               (s:X\s:X)/np]), !,   
   Sem = lam(Q,lam(S,lam(F,app(S,lam(E,merge(app(Q,lam(Y,B:drs([],[B:Index:rel(E,Y,Sym,0)]))),app(F,E))))))).


/* -------------------------------------------------------------------------
   Sentence-initial determiners
------------------------------------------------------------------------- */

semlex(Cat,Lemma,Index,Att1-Att2,Sem):-
   member(Lemma,[the,that,this,those,these]), 
   member(Cat,[(s:X/s:X)/n,(s:X\s:X)/n]), !,
   rel(in,Att1-Att2,Relation),
   Sem = lam(P,lam(S,lam(F,alfa(def,merge(B1:drs([B1:Index:Y],[]),app(P,Y)),
                                    app(S,lam(E,merge(B2:drs([],[B2:[]:rel(E,Y,Relation,0)]),app(F,E)))))))).

semlex(Cat,Lemma,Index,Att1-Att2,Sem):-
   member(Lemma,[all,every,each,any]), 
   member(Cat,[(s:X/s:X)/n,(s:X\s:X)/n]), !,
   rel(for,Att1-Att2,Relation),
   Sem = lam(P,lam(S,lam(F,B1:drs([],[B1:[]:imp(merge(B2:drs([B2:Index:Y],[]),app(P,Y)),
                                          app(S,lam(E,merge(B3:drs([],[B3:[]:rel(E,Y,Relation,0)]),app(F,E)))))])))).

semlex(Cat,_Sym,Index,Att1-Att2,Sem):-
   member(Cat,[(s:X/s:X)/n,
               (s:X\s:X)/n]), !,
   rel(for,Att1-Att2,Relation),
   Sem = lam(P,lam(S,lam(F,merge(merge(B1:drs([B1:Index:Y],[]),app(P,Y)),
                                 app(S,lam(E,merge(B2:drs([],[B2:[]:rel(E,Y,Relation,0)]),app(F,E)))))))).


/* -------------------------------------------------------------------------
   Example: With violence escalating in Kosovo, S
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[((s:X/s:X)/(s:ng\np))/np,
               ((s:X/s:X)/(s:pt\np))/np,
               ((s:X/s:X)/(s:b\np))/np,
               ((s:X/s:X)/(s:adj\np))/np]), !,   
   closing(CC),
   Sem = lam(Q,lam(VP,lam(S,lam(F,app(S,lam(E,app(app(VP,lam(P,app(Q,lam(Y,merge(B:drs([],[B:Index:rel(E,Y,Sym,0)]),merge(app(P,Y),app(F,E))))))),CC))))))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[((s:X/s:X)\np)/np]), !,
   Sem = lam(Q1,lam(Q2,lam(S,lam(F,app(S,lam(E,app(Q2,lam(Y,app(Q1,lam(Z,merge(B:drs([],[B:Index:rel(Y,Z,Sym,0)]),app(F,E)))))))))))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[((s:X/s:X)\np)/s:dcl]), !,
   Sem = lam(S1,lam(Q2,lam(S,lam(F,app(S,lam(E,app(Q2,lam(Y,app(S1,lam(E,merge(B:drs([],[B:Index:rel(E,Y,Sym,0)]),app(F,E)))))))))))).

% Where on the body ...
semlex(Cat,Sym,Index,Att-Att,Sem):-
   Cat = ((s:wq/(s:q/pp))\(s:wq/(s:q/pp)))/np,
   Sem = lam(NP,lam(Q,lam(VP,lam(F,app(app(Q,VP),lam(E,merge(app(NP,lam(X,B:drs([],[B:Index:rel(E,X,Sym,0)]))),
                                                             app(F,E)))))))). 


/* -------------------------------------------------------------------------
   instead (of)
------------------------------------------------------------------------- */

semlex(Cat,instead,Index,Att-Att,Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/pp,
               ((s:X\np)/(s:X\np))/pp,
               ((s:adj\np)\(s:adj\np))/pp]), !,
   Sem = lam(PP,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,B:drs([],[B:Index:not(merge(app(PP,E),app(F,E)))]))))))). 


/* -------------------------------------------------------------------------
   Double prepositions, such as "out of", "together with"
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-Att,Sem):-
   att(Att,pos,Pos),
   member(Pos,['VBG','VBN']),
   member(Cat,[((s:X\np)\(s:X\np))/pp,
               ((s:X\np)/(s:X\np))/pp,
               ((s:adj\np)\(s:adj\np))/pp]), !,
   Sem = lam(PP,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,merge(B:drs([B:[]:Y],[B:Index:pred(Y,Sym,v,0)]),
                                                           merge(app(PP,Y),app(F,E))))))))). 

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/pp,
               ((s:X\np)/(s:X\np))/pp,
               ((s:adj\np)\(s:adj\np))/pp]), !,
   Sem = lam(PP,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,merge(B:drs([],[B:Index:pred(E,Sym,r,0)]),merge(app(PP,E),app(F,E))))))))). 


/* -------------------------------------------------------------------------
   Double prepositions, such as "Cycling in the north of France, ..."
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-Att,Sem):-
   att(Att,pos,Pos),
   member(Pos,['VBG','VBN']),
   member(Cat,[(s:X/s:X)/pp,
               (s:X\s:X)/pp]), !, 
   Sem = lam(PP,lam(S,lam(F,app(S,lam(E,merge(B:drs([B:[]:Y],[B:Index:pred(Y,Sym,v,0)]),
                                              merge(app(PP,Y),
                                                    app(F,E)))))))). 

semlex(Cat,Sym,Index,Att1-Att2,Sem):-
   Sym = nowhere,
   member(Cat,[(s:X/s:X)/pp,
               (s:X\s:X)/pp]), !, 
   rel(in,Att1-Att2,Relation),
   Sem = lam(PP,lam(S,lam(F,app(S,lam(E,B:drs([],[B:Index:not(merge(B:drs([B:Index:X],[B:[]:pred(X,location,n,1),
                                                                                       B:[]:rel(E,X,Relation,0)]),
                                                                merge(app(PP,X),app(F,E))))])))))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[(s:X/s:X)/pp,
               (s:X\s:X)/pp]), !, 
   att(Att,sense,Sense),
   Sem = lam(PP,lam(S,lam(F,app(S,lam(E,merge(B:drs([],[B:Index:pred(E,Sym,r,Sense)]),
                                              merge(app(PP,E),
                                                    app(F,E)))))))). 


/* -------------------------------------------------------------------------
   VP adverb modifier (negation)
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-Att,Sem):-
   notSymbol(Sym),
   option('--semantics',drg),
   member(Cat,[((s:X\np)/(s:X\np))/((s:X\np)/(s:X\np)),
               ((s:X\np)/(s:X\np))\((s:X\np)/(s:X\np)),
               ((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)),
               ((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np))]), !,
   Sem = lam(AV,lam(VP,lam(NP,lam(F,B1:drs([],[B1:[]:not(app(app(app(AV,VP),NP),lam(E,merge(B2:drs([],[B2:Index:pred(E,Sym,s,1)]),app(F,E)))))]))))).


semlex(Cat,Sym,Index,Att-Att,Sem):-
   notSymbol(Sym), 
   member(Cat,[((s:X\np)/(s:X\np))/((s:X\np)/(s:X\np)),
               ((s:X\np)/(s:X\np))\((s:X\np)/(s:X\np)),
               ((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)),
               ((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np))]), !,
   Sem = lam(AV,lam(VP,lam(NP,lam(F,B:drs([],[B:Index:not(app(app(app(AV,VP),NP),lam(E,app(F,E))))]))))).


/* -------------------------------------------------------------------------
   VP adverb modifier (Cardinals that function as modifiers)
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att1-Att2,Sem):-
   att(Att1,pos,'CD'),
   member(Cat,[((s:X\np)/(s:X\np))/((s:X\np)/(s:X\np)),
               ((s:X\np)/(s:X\np))\((s:X\np)/(s:X\np)),
               ((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)),  
               ((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np))]), 
   string2digit(Sym,Digit), !,
   rel(in,Att1-Att2,Relation),
   Sem = lam(AV,lam(VP,lam(NP,lam(F,app(app(app(AV,VP),NP),lam(E,merge(B:drs([B:[]:Y],[B:Index:card(Y,Digit,eq),
                                                                                       B:[]:rel(E,Y,Relation,0)]),
                                                                       app(F,E)))))))).

/* -------------------------------------------------------------------------
   VP adverb modifier (NPs that function as modifiers)
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att1-Att2,Sem):-
   member(Cat,[((s:X\np)/(s:X\np))/((s:X\np)/(s:X\np)),
               ((s:X\np)/(s:X\np))\((s:X\np)/(s:X\np)),
               ((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)),  
               ((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np))]), 
   att(Att1,pos,Pos),
   member(Pos,['NN','NNS','NNP','NNPS']), !,
   att(Att1,sense,Sense),
   rel(in,Att1-Att2,Relation),
   Sem = lam(AV,lam(VP,lam(NP,lam(F,app(app(app(AV,VP),NP),lam(E,merge(B:drs([B:[]:Y],[B:Index:pred(Y,Sym,n,Sense),
                                                                                       B:[]:rel(E,Y,Relation,0)]),
                                                                       app(F,E)))))))).


/* -------------------------------------------------------------------------
   VP adverb modifier (intersective)
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[((s:X\np)/(s:X\np))/((s:X\np)/(s:X\np)),
               ((s:X\np)/(s:X\np))\((s:X\np)/(s:X\np)),
               ((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)),  
               ((s:adj\np)\(s:adj\np))/((s:adj\np)\(s:adj\np)),  
               ((s:adj\np)/(s:adj\np))/((s:adj\np)/(s:adj\np)),
               ((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np))]), !,
   att(Att,sense,Sense),
   Sem = lam(AV,lam(VP,lam(NP,lam(F,app(app(app(AV,VP),NP),lam(E,merge(B:drs([],[B:Index:pred(E,Sym,r,Sense)]),app(F,E)))))))).

% VP adverb modifier (negation)

semlex(Cat,Sym,Index,Att-Att,Sem):-
   option('--semantics',drg),
   member(Cat,[(((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np)))/(((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np))),
               (((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))/(((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np))),
               (((s:X\np)/(s:X\np))/((s:X\np)/(s:X\np)))/(((s:X\np)/(s:X\np))/((s:X\np)/(s:X\np)))]), 
   notSymbol(Sym), !,
   Sem = lam(M,lam(AV,lam(VP,lam(NP,lam(F,B1:drs([],[B1:Index:not(app(app(app(app(M,AV),VP),NP),lam(E,merge(B2:drs([],[B2:Index:pred(E,Sym,s,1)]),app(F,E)))))])))))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[(((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np)))/(((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np))),
               (((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))/(((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np))),
               (((s:X\np)/(s:X\np))/((s:X\np)/(s:X\np)))/(((s:X\np)/(s:X\np))/((s:X\np)/(s:X\np)))]), 
   notSymbol(Sym), !,
   Sem = lam(M,lam(AV,lam(VP,lam(NP,lam(F,B:drs([],[B:Index:not(app(app(app(app(M,AV),VP),NP),lam(E,app(F,E))))])))))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[(((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))/((s:X\np)\(s:X\np))]), !,
   att(Att,sense,Sense),
   Sem = lam(AV1,lam(AV2,lam(VP,lam(NP,lam(F,app(app(app(AV2,app(AV1,VP)),NP),
                                             lam(E,merge(B:drs([],[B:Index:pred(E,Sym,r,Sense)]),app(F,E))))))))).

% VP adverb modifier (intersective)
semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[(((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np)))/(((s:X\np)\(s:X\np))/((s:X\np)\(s:X\np))), 
               (((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))/(((s:X\np)\(s:X\np))\((s:X\np)\(s:X\np)))]), !,
   att(Att,sense,Sense),
   Sem = lam(M,lam(AV,lam(VP,lam(NP,lam(F,app(app(app(app(M,AV),VP),NP),lam(E,merge(B:drs([],[B:Index:pred(E,Sym,r,Sense)]),app(F,E))))))))).


semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[(s:X/s:X)/(s:_\np),
               (s:X\s:X)/(s:_\np)]), !,
   Sem = lam(VP,lam(S,lam(G,app(app(VP,lam(P,merge(B1:drs([B1:[]:Y],[B1:Index:pred(Y,thing,n,12)]),app(P,Y)))),
                                lam(F,merge(app(G,F),app(S,lam(E,B2:drs([],[B2:[]:rel(E,F,Sym,0)]))))))))).


/* -------------------------------------------------------------------------
   Preposition (in front of WH, as in "From where ...")
------------------------------------------------------------------------- */

semlex((s:wq/(s:q/pp))/(s:wq/(s:q/np)),Sym,Index,Att-Att,Sem):-
   Sem = lam(Q,lam(W,lam(F,app(app(Q,V),F)))),
   V = lam(N,lam(E,app(N,lam(X,app(app(W,lam(Y,B:drs([],[B:Index:rel(Y,X,Sym,0)]))),E))))).

semlex((s:wq/(s:q/np))/(s:wq/(s:q/np)),_Sym,_Index,Att-Att,Sem):-
   Sem = lam(X,X).


/* -------------------------------------------------------------------------
   Possessive 
------------------------------------------------------------------------- */

semlex(Cat,_Lemma,Index,Att-Att,Sem):-
   member(Cat,[(np:nb/n)/(n/n),
               (np/n)/(n/n)]), !,
   Sem = lam(S,lam(P,lam(Q,merge(B:drs([B:[]:U],[]),
                                 merge(app(app(S,lam(X,merge(app(P,X),B1:drs([B1:[]:Y],[B1:Index:rel(X,Y,of,0)])))),U),
                                       app(Q,U)))))).



semlex(Cat,_,Index,Att1-Att2,Sem):- 
   member(Cat,[(np/n)\np, 
               (np:nb/n)\np]), !,
   rel(of,Att1-Att2,Relation),
   Sem = lam(NP,lam(N,lam(P,app(NP,lam(Y,alfa(def,merge(B:drs([B:[]:X],[B:Index:rel(X,Y,Relation,0)]),
                                                        app(N,X)),
                                                  app(P,X))))))).

semlex(Cat,_,Index,Att-Att,Sem):- 
   member(Cat,[(np/(n/pp))\np, 
               (np:nb/(n/pp))\np]), !,
   Sem = lam(NP,lam(RN,lam(P,app(NP,lam(Y,alfa(def,merge(B:drs([B:[]:X],[]),
                                                         app(app(RN,lam(Z,B2:drs([],[B2:Index:rel(Z,Y,of,0)]))),X)),
                                                   app(P,X))))))).


semlex(Cat,_,Index,Att-Att,Sem):- 
   member(Cat,[((np:nb/n)/(n/n))\np,
               ((np/n)/(n/n))\np]), !,
   Sem = lam(N,lam(S,lam(P,lam(Q,merge(B1:drs([B1:[]:U],[]),
                                       merge(app(app(S,lam(X,merge(app(P,X),
                                                                   app(N,lam(Y,B2:drs([],[B2:Index:rel(X,Y,of,0)])))))),U),
                                             app(Q,U))))))).

semlex((n/n)\n,_,Index,Att-Att,Sem):- !,
   Sem = lam(N1,lam(N2,lam(X,merge(B1:drs([B1:[]:Y],[]),
                                   merge(app(N1,Y),
                                         merge(app(N2,X),
                                               B2:drs([],[B2:Index:rel(X,Y,of,0)]))))))).
semlex(Cat,_,Index,Att-Att,Sem):- 
   member(Cat,[((s:wq/(s:q/np))/n)\(s:wq/(s:q/np)),
               ((s:wq\(s:dcl/np))/n)\(s:wq\(s:dcl/np)),
               ((s:wq/(s:dcl\np))/n)\(s:wq/(s:dcl\np))]), !,

   XXX = lam(U,lam(E,app(U,lam(Y,alfa(def,merge(B:drs([B:[]:X],[B:Index:rel(X,Y,of,0)]),
                                          app(N,X)),
                                    app(app(V,lam(Q,app(Q,X))),E)))))),
                                  
   Sem = lam(NP,lam(N,lam(V,lam(P,app(app(NP,XXX),P))))).

%  Sem = lam(NP,lam(N,lam(V,app(V,lam(P2,app(NP,lam(V2,app(V2,lam(Y,alfa(def,merge(B:drs([B:Index:X],[B:Index:rel(X,Y,of,0)]),app(N,X)),app(P2,X))))))))))).


/* -------------------------------------------------------------------------
   Emphasising Pronouns
------------------------------------------------------------------------- */

semlex(np\np, himself,Index,Att-Att,Sem):- !,
   Sem = lam(Q,lam(P,app(Q,lam(X,merge(B:drs([],[B:Index:pred(X,male,n,2)]),app(P,X)))))).

semlex(np\np, herself,Index,Att-Att,Sem):- !,
   Sem = lam(Q,lam(P,app(Q,lam(X,merge(B:drs([],[B:Index:pred(X,female,n,2)]),app(P,X)))))).

semlex(np\np, itself,Index,Att-Att,Sem):- !,
   Sem = lam(Q,lam(P,app(Q,lam(X,merge(B:drs([],[B:Index:pred(X,thing,n,12)]),app(P,X)))))).

semlex(np\np, Sym,Index,Att-Att,Sem):-
   member(Sym,[myself,yourself,thyself,ourselves,themselves]), !,
   Sem = lam(Q,lam(P,app(Q,lam(X,merge(B:drs([],[B:Index:pred(X,person,n,1)]),app(P,X)))))).


/* -------------------------------------------------------------------------
   NP modifiers: floating quantifiers 
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-Att,Sem):-
   option('--x',false),
   member(Cat,[np\np, np/np]), 
   member(Sym,[all,each]), !,
   Sem = lam(Q,lam(P,B1:drs([],[B1:[]:imp(merge(B2:drs([B2:Index:X],[]),app(Q,lam(Y,B3:drs([],[B3:[]:eq(X,Y)])))),app(P,X))]))).


/* -------------------------------------------------------------------------
   NP modifiers: only
------------------------------------------------------------------------- */

semlex(Cat,only,Index,Att-Att,Sem):-
   member(Cat,[np\np, np/np]), !,
   Sem = lam(NP,lam(P,alfa(fac,merge(B1:drs([B1:Index:Z],[]),
                                     app(NP,lam(X,merge(app(P,X),
                                                        B2:drs([],[B2:[]:eq(Z,X)]))))),
                               B3:drs([],[B3:[]:imp(merge(B4:drs([B4:[]:Y],[]),app(P,Y)),
                                                          B5:drs([],[B5:[]:eq(Z,Y)]))])))).


/* -------------------------------------------------------------------------
   NP modifiers: negation
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[np\np, np/np]), 
   notSymbol(Sym), !,
   Sem = lam(NP,lam(P,B:drs([],[B:Index:not(app(NP,P))]))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   Cat = (((np\np)/(np\np))/((np\np)/(np\np))),
   notSymbol(Sym), !,
   Sem = lam(A1,lam(A2,lam(NP,lam(P,B:drs([],[B:Index:not(app(app(app(A1,A2),NP),P))]))))).


/* -------------------------------------------------------------------------
   NP modifiers
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att1-Att2,Sem):-
   att(Att1,pos,Pos),
   member(Pos,['NNP','NNPS']),
   att(Att1,namex,NE), neClassType(NE,tim,Type),
   member(Cat,[np\np, np/np]), !,
   rel(on,Att1-Att2,Relation),
   Sem = lam(Q,lam(P,app(Q,lam(X,merge(B:drs([B:[]:Y],[B:Index:named(Y,Sym,tim,Type),B:[]:rel(X,Y,Relation,0)]),
                                       app(P,X)))))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   att(Att,pos,Pos),
   member(Pos,['NNP','NNPS']),
   member(Cat,[np\np, np/np]), !,
   att(Att,namex,Ne), neClassType(Ne,Class,Type),
   Sem = lam(Q,lam(P,app(Q,lam(X,merge(B:drs([],[B:Index:named(X,Sym,Class,Type)]),
                                       app(P,X)))))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   att(Att,pos,Pos),
   member(Pos,['IN','RB','JJ','JJR','RBR']),
   member(Cat,[np\np, np/np]), !,
   att(Att,sense,Sense),
   Sem = lam(Q,lam(P,app(Q,lam(X,merge(B:drs([],[B:Index:pred(X,Sym,a,Sense)]),
                                       app(P,X)))))).


semlex(Cat,Sym,Index,Att1-Att2,Sem):-
   member(Cat,[np\np, np/np]),
   att(Att1,pos,'CD'),
   string2score(Sym,Score), !,
   rel(with,Att1-Att2,Relation),
   Sem = lam(Q,lam(P,app(Q,lam(X,merge(B:drs([B:[]:Y],[B:Index:named(Y,Score,sco,num),
                                                       B:[]:rel(X,Y,Relation,0)]),
                                       app(P,X)))))).

semlex(Cat,Sym,Index,Att1-Att2,Sem):-
   member(Cat,[np\np, np/np]), !,
   att(Att1,sense,Sense),
   rel(in,Att1-Att2,Relation),
   Sem = lam(Q,lam(P,app(Q,lam(X,merge(B:drs([B:[]:Y],[B:Index:pred(Y,Sym,n,Sense),
                                                       B:[]:rel(X,Y,Relation,0)]),
                                       app(P,X)))))).


/* -------------------------------------------------------------------------
   NP modifiers (superlative contruction)
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[d/np]), !,
   Sem = lam(X,lam(Y,B:drs([],[B:Index:rel(Y,X,Sym,0)]))).


/* -------------------------------------------------------------------------
   NP modifier modifiers: deitics

semlex(Cat,Sym,_Index,Att-Att,Sem):-
   member(Cat,[(np\np)/(np\np),
               (np\np)\(np\np),
               (np/np)/(np/np)]), 
   member(Sym,[there,here,ago,such,now]), !,
   Sem = lam(M,lam(Q,lam(P,app(app(M,Q),P)))). 
------------------------------------------------------------------------- */


/* -------------------------------------------------------------------------
   NP modifier modifiers (proper names)
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-Att,Sem):-
   att(Att,pos,Pos),
   member(Pos,['NNP','NNPS']),
   member(Cat,[(np\np)/(np\np),
               (np\np)\(np\np),
               (np/np)/(np/np)]), !, 
   att(Att,namex,Ne), neClassType(Ne,Class,Type),
   Sem = lam(M,lam(Q,lam(P,app(app(M,Q),lam(X,merge(B:drs([],[B:Index:named(X,Sym,Class,Type)]),
                                                    app(P,X))))))).

/* -------------------------------------------------------------------------
   NP modifier modifiers (not)
------------------------------------------------------------------------- */

semlex(Cat,not,Index,Att-Att,Sem):-
   member(Cat,[(np\np)/(np\np),
               (np\np)\(np\np),
               (np/np)/(np/np)]), !, 
%   Sem = lam(M,lam(Q,lam(P,app(app(M,Q),lam(X,B:drs([],[B:Index:not(app(P,X))])))))).
   Sem = lam(M,lam(Q,lam(P,B:drs([],[B:Index:not(app(app(M,Q),lam(X,app(P,X))))])))).


/* -------------------------------------------------------------------------
   NP modifier modifiers
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[(np\np)/(np\np),
               (np\np)\(np\np),
               (np/np)/(np/np)]), !, 
   att(Att,sense,Sense),
   Sem = lam(M,lam(Q,lam(P,app(app(M,Q),lam(X,merge(B:drs([],[B:Index:pred(X,Sym,n,Sense)]),
                                                    app(P,X))))))).



/* -------------------------------------------------------------------------
   NP modifier modifiers, superlative ("most notably")
------------------------------------------------------------------------- */

semlex(Cat,_Sym,Index,Att-Att,Sem):-
   member(Cat,[(np/np)/(d/np)]), !, 
   Sem = lam(R,lam(Q,lam(P,app(Q,lam(X,merge(B1:drs([],[B1:Index:imp(B2:drs([B2:[]:Y],[B2:[]:not(B3:drs([],[B3:[]:eq(X,Y)]))]),
                                                               app(app(R,X),Y))]),
                                             app(P,X))))))).


/* -------------------------------------------------------------------------
   NPs that function as S modifiers
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att1-Att2,Sem):-
   category(smod,Cat,Sym), 
   att(Att1,pos,Pos),
   member(Pos,['NNP','NNPS']), !,
   att(Att1,namex,Ne), neClassType(Ne,Class,Type),
   rel(in,Att1-Att2,Relation),
   Sem = lam(S,lam(F,app(S,lam(E,merge(B:drs([B:[]:X],[B:Index:named(X,Sym,Class,Type),
                                                       B:[]:rel(E,X,Relation,0)]),app(F,E)))))).

semlex(Cat,Sym,Index,Att1-Att2,Sem):-
   category(smod,Cat,Sym), 
   att(Att1,pos,Pos),
   member(Pos,['NN','NNS']), !,
   att(Att1,sense,Sense),
   rel(in,Att1-Att2,Relation),
   Sem = lam(S,lam(F,app(S,lam(E,merge(B:drs([B:[]:X],[B:Index:pred(X,Sym,n,Sense),
                                                       B:[]:rel(E,X,Relation,0)]),app(F,E)))))).

semlex(Cat,Sym,Index,Att1-Att2,Sem):-
   category(smod,Cat,Sym), 
   member(Sym,[nowhere]), !,
   rel(in,Att1-Att2,Relation),
   Sem = lam(S,lam(F,app(S,lam(E,B1:drs([],[B1:Index:not(merge(B2:drs([B2:Index:X],[B2:[]:pred(X,location,n,1),
                                                                                    B2:[]:rel(E,X,Relation,0)]),app(F,E)))]))))).

semlex(Cat,Sym,Index,Att1-Att2,Sem):-
   category(smod,Cat,Sym), 
   member(Sym,[everywhere,anywhere,somewhere]), !,
   rel(in,Att1-Att2,Relation),
   Sem = lam(S,lam(F,app(S,lam(E,merge(B:drs([B:[]:X],[B:Index:pred(X,location,n,1),
                                                       B:[]:rel(E,X,Relation,0)]),app(F,E)))))).



/* -------------------------------------------------------------------------
   S modifiers
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-Att,Sem):-
   notSymbol(Sym),
   category(smod,Cat,Sym), !,
   Sem = lam(S,lam(F,B:drs([],[B:Index:not(app(S,F))]))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   category(smod,Cat,Sym), !,
   att(Att,sense,Sense),
   Sem = lam(S,lam(F,app(S,lam(E,merge(B:drs([],[B:Index:pred(E,Sym,r,Sense)]),app(F,E)))))).


/* -------------------------------------------------------------------------
   S modifier modifiers
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-Att,Sem):- 
   notSymbol(Sym), 
   member(Cat,[(s:X/s:X)/(s:X/s:X),
               (s:X/s:X)\(s:X/s:X),
               (s:X\s:X)/(s:X\s:X),
               (s:X\s:X)\(s:X\s:X)]), !, 
   Sem = lam(M,lam(S,lam(F,B:drs([],[B:Index:not(app(app(M,S),F))])))).

semlex(Cat,Sym,Index,Att-Att,Sem):- 
   member(Cat,[(s:X/s:X)/(s:X/s:X),
               (s:X/s:X)\(s:X/s:X),
               (s:X\s:X)/(s:X\s:X),
               (s:X\s:X)\(s:X\s:X)]), !, 
   att(Att,sense,Sense),
   Sem = lam(M,lam(Q,lam(P,app(app(M,Q),lam(E,merge(B:drs([],[B:Index:pred(E,Sym,r,Sense)]),
                                                    app(P,E))))))).

semlex(Cat,Sym,Index,Att-Att,Sem):- 
   member(Cat,[((s:X/s:X)/(s:X/s:X))/np,
               ((s:X/s:X)\(s:X/s:X))/np,
               ((s:X\s:X)/(s:X\s:X))/np,
               ((s:X\s:X)\(s:X\s:X))/np]), !, 
   Sem = lam(Q,lam(M,lam(S,lam(F,app(app(M,S),lam(E,merge(app(Q,lam(Y,B:drs([],[B:Index:rel(E,Y,Sym,0)]))),app(F,E)))))))).

semlex(Cat,Sym,Index,Att-Att,Sem):- 
   member(Cat,[((s:X/s:X)/(s:X/s:X))/s:dcl,
               ((s:X/s:X)\(s:X/s:X))/s:dcl,
               ((s:X\s:X)/(s:X\s:X))/s:dcl,
               ((s:X\s:X)\(s:X\s:X))/s:dcl]), !, 
   closing(CC),
   Sem = lam(S1,lam(M,lam(S2,lam(F,merge(B1:drs([B1:[]:E,B1:[]:Z,B1:[]:Y],
                                                [B1:[]:prop(E,B2:drs([],[B2:Index:rel(Z,Y,Sym,0)])),
                                                 B1:[]:prop(Z,app(S1,CC)),
                                                 B1:[]:prop(Y,app(app(M,S2),CC))]),
                                   app(F,E)))))).


/* -------------------------------------------------------------------------
   Mostly Temporal modifiers: "every month", "this week", "Nov. 29"
------------------------------------------------------------------------- */

%semlex(Cat,this,Index,Att-Att,Sem):-
%   member(Cat,[((s:X\np)\(s:X\np))/n,
%               ((s:X\np)/(s:X\np))/n]), !,
%   Sem = lam(N,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,alfa(def,merge(B1:drs([B1:[]:Y],
%                                                                          [B1:[]:pred(Y,current,a,1)]),
%                                                                   app(N,Y)),
%                                                             merge(B2:drs([],[B2:Index:rel(E,Y,rel,0)]),
%                                                                   app(F,E))))))))).

semlex(Cat,Sym,Index,Att1-Att2,Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/n,
               ((s:X\np)/(s:X\np))/n]), 
   member(Sym,[the,this,that,these,those]), !,
   rel(in,Att1-Att2,Relation),
   Sem = lam(N,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,alfa(def,merge(B1:drs([B1:[]:Y],[]),
                                                                   app(N,Y)),
                                                             merge(B2:drs([],[B2:Index:rel(E,Y,Relation,0)]),
                                                                   app(F,E))))))))).

semlex(Cat,Sym,Index,Att1-Att2,Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/n,
               ((s:X\np)/(s:X\np))/n]), 
   member(Sym,[every,each,all,any,either]), !,
   rel(in,Att1-Att2,Relation),
   Sem = lam(N,lam(V,lam(Q,lam(F,B1:drs([],[B1:[]:imp(merge(B2:drs([B2:Index:Y],[]),app(N,Y)),
                                                app(app(V,Q),lam(E,merge(B3:drs([],[B3:[]:rel(E,Y,Relation,0)]),
                                                                         app(F,E)))))]))))).

semlex(Cat,Sym,Index,Att1-Att2,Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/n,
               ((s:X\np)/(s:X\np))/n]), 
   member(Sym,[a,an,some]), !,
   rel(in,Att1-Att2,Relation),
   Sem = lam(N,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,merge(B:drs([B:Index:Y],[B:[]:rel(E,Y,Relation,0)]),
                                                          merge(app(N,Y),app(F,E))))))))).

semlex(Cat,Sym,Index,Att1-Att2,Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/n,
               ((s:X\np)/(s:X\np))/n]), 
   member(Sym,[no]), !,
   rel(for,Att1-Att2,Relation),
   Sem = lam(N,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,B1:drs([],[B1:Index:not(merge(B2:drs([B2:Index:Y],[B2:[]:rel(E,Y,Relation,0)]),
                                                                            merge(app(N,Y),app(F,E))))]))))))).

semlex(Cat,Sym,Index,Att1-Att2,Sem):-
   att(Att1,pos,'NNP'),
   att(Att1,namex,NE), neClass(NE,tim),
   member(Cat,[((s:X\np)\(s:X\np))/n, ((s:X\np)/(s:X\np))/n]), 
   month(Sym,MID), !,
   rel(in,Att1-Att2,Relation),
   Sem = lam(P,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,merge(B1:drs([B1:[]:Y],[B1:Index:timex(Y,date([]:'+',[]:'XXXX',Index:MID,[]:'XX')),
                                                                            B1:[]:rel(E,Y,Relation,0)]),
                                                          merge(app(P,Y),app(F,E))))))))).

semlex(Cat,Sym,Index,Att1-Att2,Sem):-
   member(Cat,[((s:X\np)\(s:X\np))/n,
               ((s:X\np)/(s:X\np))/n]), !,
   att(Att1,sense,Sense),
   rel(in,Att1-Att2,Relation),
   Sem = lam(P,lam(V,lam(Q,lam(F,app(app(V,Q),lam(E,merge(B:drs([B:[]:Y],[B:Index:pred(Y,Sym,n,Sense),
                                                                          B:[]:rel(E,Y,Relation,0)]),
                                                          merge(app(P,Y),app(F,E))))))))).


/* -------------------------------------------------------------------------
   Noun subcategorising for sentence
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[n/s:em,n/s:qem,n/s:bem]), !,
   att(Att,sense,Sense),
   Sem = lam(S,lam(X,merge(B1:drs([],
                                  [B1:Index:pred(X,Sym,n,Sense)]),
                           app(S,lam(E,B2:drs([],[B2:[]:rel(X,E,theme,0)])))))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   Cat = n/s:_, !,
   att(Att,sense,Sense),
   closing(CC),
   Sem = lam(S,lam(X,B:drs([B:[]:K],
                           [B:Index:pred(X,Sym,n,Sense),
                            B:[]:rel(X,K,theme,0),
                            B:[]:prop(K,app(S,CC))]))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   Cat = (n/pp)/s:_, !, 
   att(Att,sense,Sense),
   closing(CC),
   Sem = lam(S,lam(P,lam(X,merge(B:drs([B:[]:K],
                                       [B:Index:pred(X,Sym,n,Sense),
                                        B:[]:rel(X,K,theme,0),
                                        B:[]:prop(K,app(S,CC))]),app(P,X))))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[pp/s:_,pp/s]), !, 
   Sem = lam(S,lam(E,app(S,lam(X,B:drs([],[B:Index:rel(E,X,Sym,0)]))))).


/* -------------------------------------------------------------------------
   NP modifying noun
------------------------------------------------------------------------- */

semlex(n/np,Sym,Index,Att-Att,Sem):- !,
   att(Att,sense,Sense),
   Sem = lam(NP,lam(X,merge(B1:drs([],[B1:Index:pred(X,Sym,n,Sense)]),
                            app(NP,lam(Y,B2:drs([],[B2:[]:rel(X,Y,rel,0)])))))).


/* -------------------------------------------------------------------------
   PP modifiers
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-Att,Sem):- 
   option('--semantics',drg),   
   notSymbol(Sym),
   member(Cat,[pp/pp,pp\pp]), !, 
   Sem = lam(P,lam(E,B:drs([],[B:Index:pred(E,Sym,s,1),B:[]:not(app(P,E))]))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   notSymbol(Sym),
   member(Cat,[pp/pp,pp\pp]), !, 
   Sem = lam(P,lam(E,B:drs([],[B:Index:not(app(P,E))]))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[pp/pp,pp\pp]), !, 
   att(Att,sense,Sense),
   Sem = lam(P,lam(E,merge(app(P,E),
                           B:drs([],[B:Index:pred(E,Sym,r,Sense)])))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[(pp\pp)\np,
               (pp\pp)/np,
               (pp/pp)/np]), !, 
   Sem = lam(NP,lam(P,lam(E,merge(app(P,E),
                                  app(NP,lam(X,B:drs([],[B:Index:rel(E,X,Sym,0)]))))))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[(pp\pp)\s:dcl,
               (pp\pp)/s:dcl,
               (pp/pp)/s:dcl]), !, 
   Sem = lam(S,lam(P,lam(E,merge(app(P,E),
                                 app(S,lam(X,B:drs([],[B:Index:rel(E,X,Sym,0)]))))))).

semlex(Cat,Sym,Index,Att-Att,Sem):-  
   notSymbol(Sym),
   member(Cat,[(pp\pp)/(pp\pp),
               (pp/pp)/(pp/pp)]), !,
   Sem = lam(Z,lam(P,lam(Y,app(app(Z,lam(X,B:drs([],[B:Index:not(app(P,X))]))),Y)))).

semlex(Cat,Sym,Index,Att-Att,Sem):-  
   member(Cat,[(pp\pp)/(pp\pp),
               (pp/pp)/(pp/pp)]), !,
   att(Att,sense,Sense),
   Sem = lam(Z,lam(P,lam(Y,app(app(Z,lam(X,merge(B:drs([],[B:Index:pred(X,Sym,r,Sense)]),
                                                 app(P,X)))),Y)))).

/* -------------------------------------------------------------------------
   Preposition: the (as in "the week before")
------------------------------------------------------------------------- */

semlex(Cat,the,Index,Att1-Att2,Sem):-
   member(Cat,[(pp\pp)/n]), !, 
   rel(in,Att1-Att2,Relation),
   Sem = lam(N,lam(P,lam(E,merge(app(P,E),
                                 alfa(def,merge(B1:drs([B1:[]:X],[]),
                                                app(N,X)),
                                          B2:drs([],[B2:Index:rel(E,X,Relation,0)])))))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[(pp\pp)/n]), !, 
   Sem = lam(N,lam(P,lam(E,merge(app(P,E),
                                 alfa(def,merge(B1:drs([B1:[]:X],[]),
                                                app(N,X)),
                                          B2:drs([],[B2:Index:rel(E,X,Sym,0)])))))).


/* -------------------------------------------------------------------------
   Discourse connectors: when
------------------------------------------------------------------------- */

semlex(Cat,when,Index,Att-Att,Sem):-
   option('--tense',true),
   option('--theory',drt),
   member(Cat,[(s:X/s:X)/s:dcl,
               (s:wq/s:wq)/s:dcl]), !, 
   Sem = lam(S1,lam(S2,lam(F,merge(merge(B1:drs([B1:[]:T],[B1:[]:pred(T,time,n,1)]),
                                         app(S1,lam(E,B2:drs([],[B2:Index:rel(E,T,temp_included,1)])))),
                                   app(S2,lam(E,merge(B3:drs([],[B3:[]:rel(E,T,temp_included,1)]),app(F,E)))))))).

semlex(Cat,when,Index,Att-Att,Sem):-
   option('--tense',true),
   option('--theory',drt),
   member(Cat,[(s:X\s:X)/s:dcl,
               (s:wq\s:wq)/s:dcl]), !, 
   Sem = lam(S2,lam(S1,lam(F,merge(merge(B1:drs([B1:[]:T],[[]:pred(T,time,n,1)]),
                                         app(S1,lam(E,B2:drs([],[B2:Index:rel(E,T,temp_included,1)])))),
                                   app(S2,lam(E,merge(B3:drs([],[B3:[]:rel(E,T,temp_included,1)]),app(F,E)))))))).


/* -------------------------------------------------------------------------
   Discourse connectors: if
------------------------------------------------------------------------- */

semlex(Cat,if,Index,Att-Att,Sem):-
   member(Cat,[(s:X/s:X)/s:dcl,
               (s:wq/s:wq)/s:dcl]), !, 
   closing(CC),
   Sem = lam(S1,lam(S2,lam(F,B:drs([],[B:Index:imp(app(S1,CC),app(S2,F))])))).

semlex(Cat,if,Index,Att-Att,Sem):-
   member(Cat,[(s:X\s:X)/s:dcl,
               (s:wq\s:wq)/s:dcl]), !, 
   closing(CC),
   Sem = lam(S2,lam(S1,lam(F,B:drs([],[B:Index:imp(app(S1,CC),app(S2,F))])))).


/* -------------------------------------------------------------------------
   Discourse connectors: if
------------------------------------------------------------------------- */

semlex((s:X\s:X)/s:dcl,and,_Index,Att-Att,Sem):-
   option('--theory',drt), !,
   closing(CC),
   Sem = lam(S2,lam(S1,lam(F,merge(app(S1,CC),app(S2,F))))).


/* -------------------------------------------------------------------------
   Discourse connectors: all others
------------------------------------------------------------------------- */

semlex(Cat,Sym,Index,Att-Att,Sem):-
   option('--theory',sdrt),
   Cat = (s:X/s:X)/s:_, !,
   closing(CC),
%  Sem = lam(S1,lam(S2,lam(F,sdrs([lab(K1,B1),lab(K2,B2)],[Index:rel(K1,K2,Sym)])))),
   Sem = lam(S2,lam(S1,lam(F,sdrs([sub(lab(K1,B1),lab(K2,B2))],[Index:rel(K1,K2,Sym)])))),
   B1 = app(S1,CC),
   B2 = app(S2,F).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   option('--theory',sdrt),
   Cat = (s:X\s:X)/s:_, !,
   closing(CC),
   Sem = lam(S2,lam(S1,lam(F,sdrs([lab(K1,B1),lab(K2,B2)],[Index:rel(K1,K2,Sym)])))),
   B1 = app(S1,CC),
   B2 = app(S2,F).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[(s:X/s:X)/s:dcl,
               (s:X/s:X)/s:inv,
               (s:wq/s:wq)/s:dcl]), !, 
   closing(CC),
   Sem = lam(S1,lam(S2,lam(F,merge(B1:drs([B1:[]:E,B1:[]:Z,B1:[]:Y],
                                          [B1:[]:prop(E,B2:drs([],[B2:Index:rel(Z,Y,Sym,0)])),
                                           B1:[]:prop(Z,app(S1,CC)),
                                           B1:[]:prop(Y,app(S2,CC))]),
                                   app(F,E))))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[(s:X\s:X)/s:dcl,
               (s:wq\s:wq)/s:dcl]), !, 
   closing(CC),
   Sem = lam(S2,lam(S1,lam(F,merge(B1:drs([B1:[]:E,B1:[]:Z,B1:[]:Y],
                                          [B1:[]:prop(E,B2:drs([],[B2:Index:rel(Z,Y,Sym,0)])),
                                           B1:[]:prop(Z,app(S1,CC)),
                                           B1:[]:prop(Y,app(S2,CC))]),
                                   app(F,E))))).


/* -------------------------------------------------------------------------
   Non-Restrictive Relative Pronous
------------------------------------------------------------------------- */

semlex(Cat,_Sym,_,Att-Att,Sem):-
   member(Cat,[(np\np)/(s:_\np),(np\np)/(s:_/np)]),
   option('--elimeq',true), !,
   closing(CC),
   Sem = lam(VP,lam(Q,lam(P,app(Q,lam(X,merge(app(app(VP,
                                                      lam(P,app(P,X))),
                                                  CC),
                                              app(P,X))))))).

semlex(Cat,_Sym,Index,Att-Att,Sem):-
   member(Cat,[(np\np)/(s:_\np), (np\np)/(s:_/np)]),
   option('--elimeq',false), !,
   closing(CC),
   Sem = lam(VP,lam(Q,lam(P,app(Q,lam(X,merge(app(app(VP,
                                                      lam(P,merge(B:drs([B:Index:Y],[B:[]:eq(X,Y)]),app(P,Y)))),
                                                  CC),
                                              app(P,X))))))).


/* -------------------------------------------------------------------------
   Restrictive Relative Pronous
------------------------------------------------------------------------- */

semlex(Cat,_Sym,_Index,Att-Att,Sem):-
   member(Cat,[(n\n)/(s:_\np),
               (n\n)/(s:_/np)]), !,
   closing(CC),
   Sem = lam(VP,lam(N,lam(X,merge(app(N,X),
                                  app(app(VP,
                                          lam(P,app(P,X))),
                                      CC))))).


/* -------------------------------------------------------------------------
   Other kind of relative pronous (pied piping)
------------------------------------------------------------------------- */

semlex(Cat,_Sym,Index,Att-Att,Sem):-
   Cat=((np\np)/(s:dcl\np))\(np/np), !,
   closing(CC),
   Sem = lam(M,lam(VP,lam(NP,lam(P,app(NP,lam(Y,merge(B:drs([B:[]:Z],[B:Index:eq(Y,Z)]),
                                                      merge(app(P,Y),
                                                            app(app(VP,app(M,lam(Q,app(Q,Z)))),CC))))))))).


/* -------------------------------------------------------------------------
   whose
------------------------------------------------------------------------- */

semlex(((np\np)/(s:dcl\np))/n,_,Index,Att1-Att2,Sem):- !,
   closing(CC),
   rel(of,Att1-Att2,Relation),
   Sem = lam(N,lam(VP,lam(Q,lam(P,app(Q,lam(X,merge(app(app(VP,lam(P,merge(B:drs([B:[]:Y],[B:Index:rel(Y,X,Relation,1)]),
                                                                           merge(app(N,Y),app(P,Y))))),CC),
                                                    app(P,X)))))))). 


/* -------------------------------------------------------------------------
   Further relative pronouns
------------------------------------------------------------------------- */

semlex(((n\n)/(s:dcl\np))/n,_,Index,Att1-Att2,Sem):- !,
   closing(CC),
   rel(of,Att1-Att2,Relation),
   Sem = lam(N,lam(VP,lam(P,lam(X,merge(app(P,X),app(app(VP,lam(P,merge(B:drs([B:[]:Y],[B:Index:rel(Y,X,Relation,1)]),
                                                                        merge(app(N,Y),app(P,Y))))),CC)))))).

semlex((np\np)/s:_,Sym,Index,Att-Att,Sem):- !,
   closing(CC),
   Sem = lam(S,lam(Q,lam(P,app(Q,lam(X,merge(B:drs([B:[]:Z],[B:Index:rel(X,Z,Sym,0),
                                                             B:[]:prop(Z,app(S,CC))]),
                                             app(P,X))))))).

semlex((n\n)/s:_, Sym,Index,Att-Att,Sem):- !,
   closing(CC),
   Sem = lam(S,lam(P,lam(X,merge(B:drs([B:[]:Z],[B:Index:rel(X,Z,Sym,0),
                                               B:[]:prop(Z,app(S,CC))]),
                                 app(P,X))))).


semlex(Cat,Sym,Index,Att-Att,Sem):-
   member(Cat,[((np\np)\(np\np))/s:dcl]), !, 
   Sem = lam(S,lam(M,lam(NP,lam(Q,app(app(M,NP),
                                      lam(X,merge(app(S,lam(E,B:drs([],[B:Index:rel(E,X,Sym,0)]))),
                                                  app(Q,X)))))))).


/* -------------------------------------------------------------------------
   Interjections and Sentential Categories
------------------------------------------------------------------------- */

%semlex(Cat,Sym,Index,Att-Att,Sem):-
%   option('--x',true),
%   member(Sym,[no]),
%   category(s,Cat,intj), !,
%   att(Att,sense,Sense),
%   Sem = lam(E,merge(B1:drs([B1:[]:X],[B1:Index:not(B2:drs([],[B2:Index:pred(X,Sym,n,Sense)]))]),app(E,X))).

semlex(Cat,Sym,Index,Att-Att,Sem):-
   category(s,Cat,_), !,
   att(Att,sense,Sense),
   Sem = lam(E,merge(B:drs([B:[]:X],[B:Index:pred(X,Sym,n,Sense)]),app(E,X))).


/* =========================================================================
   Aux Predicates
========================================================================= */

notSymbol( not    ).
notSymbol( 'n\'t' ).
notSymbol( '\'t'  ).
notSymbol( nor    ).
notSymbol( never  ):- option('--x',true).

