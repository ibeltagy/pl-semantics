
:- module(transform,[preprocess/8,
                     topcat/2]).    % +CCG, -Cat

:- use_module(library(lists),[append/3,member/2]).
:- use_module(semlib(options),[option/2]).
:- use_module(boxer(slashes)).


/* =========================================================================
   Transformations of input CCG tree:
   (1) N/N -> D/N  for adjectives modified by most/least
   (2) PP attachments to NP become to PP attachments to N 
   (3) possessives and superlatives
========================================================================= */


/* -------------------------------------------------------------------------
   Set Token-ID
------------------------------------------------------------------------- */

setTokID(_,Start,Start):-
   option('--tokid',global), !.

setTokID(SID,_,Start):-
   option('--tokid',local), !,
   Start is (SID*1000)+1.


/* -------------------------------------------------------------------------
   Pre-Processing
------------------------------------------------------------------------- */

preprocess(SID,X,Y,Words,Pos,Ne,Start,End):-
   setTokID(SID,Start,TokID),
   trans0(X,TokID,Y,End,Words-[],Pos-[],Ne-[]), !.


/* -------------------------------------------------------------------------
   Repair rule to deal with sentence-final punctuation
------------------------------------------------------------------------- */

trans0(rp(s:dcl,X1,Y1),N1,ba(t:ynq,X2,Y2),N3,Wo1-Wo3,Po1-Po3,Ne1-Ne3):- 
   Y1 = t(period,'?',B,C,D,E), !,
   trans(X1,N1,X2,N2,Wo1-Wo2,Po1-Po2,Ne1-Ne2),
   trans(t(t:ynq\s:dcl,'?',B,C,D,E),N2,Y2,N3,Wo2-Wo3,Po2-Po3,Ne2-Ne3).

trans0(rp(s:M,X1,Y1),N1,ba(t:M,X2,Y2),N3,Wo1-Wo3,Po1-Po3,Ne1-Ne3):- 
   Y1 = t(period,A,B,C,D,E), !,
   trans(X1,N1,X2,N2,Wo1-Wo2,Po1-Po2,Ne1-Ne2),
   trans(t(t:M\s:M,A,B,C,D,E),N2,Y2,N3,Wo2-Wo3,Po2-Po3,Ne2-Ne3).

trans0(X,N,Y,End,Words,Pos,Ne):- 
   trans(X,N,Y,End,Words,Pos,Ne).




% trans(A,B,C,D,E,F,G):- write(trans(A,B,C,D,E,F,G)),nl,fail.

/* -------------------------------------------------------------------------
   Funny (occurs with wrongly analysed cases of N coordination)
------------------------------------------------------------------------- */

trans(funny(_,_,X1),N1,X2,N3,Wo,Po,Ne):- !,
   N2 is N1 + 1, %% assuming we skip one word (i.e. 'and')
   trans(X1,N2,X2,N3,Wo,Po,Ne).


/* -------------------------------------------------------------------------
   Apposition
------------------------------------------------------------------------- */

trans(appo(_,X1,Y1),N1,coord(np,X2,t(N1,conj,',',',',',',0,'O'),Y2),N3,Wo1-Wo3,Po1-Po3,Ne1-Ne3):- 
   trans(X1,N1,X2,N2,Wo1-Wo2,Po1-Po2,Ne1-Ne2),
   topcat(X2,NP1),
   member(NP1,[np:nb,np]),
   trans(Y1,N2,Y2,N3,Wo2-Wo3,Po2-Po3,Ne2-Ne3),
   topcat(Y2,NP2),
   member(NP2,[np:nb,np]), !.

trans(appo(_,X1,Y1),N1,fa(s:dcl,tc(s:dcl/s:dcl,s:dcl,X2),Y2),N3,Wo1-Wo3,Po1-Po3,Ne1-Ne3):- 
   trans(X1,N1,X2,N2,Wo1-Wo2,Po1-Po2,Ne1-Ne2),
   topcat(X2,s:dcl),
   trans(Y1,N2,Y2,N3,Wo2-Wo3,Po2-Po3,Ne2-Ne3),
   topcat(Y2,s:dcl), !.

/* -------------------------------------------------------------------------
   Punctuation typechange rules
------------------------------------------------------------------------- */

trans(rtc(C,X1,Pu),N1,Res,N3,Wo1-Wo3,Po1-Po3,Ne1-Ne3):- !,
   trans(X1,N1,X2,N2,Wo1-Wo2,Po1-Po2,Ne1-Ne2),
   topcat(X2,OldCat),
   ( \+ \+ C = OldCat, Res = X2; \+ C = OldCat, Res = tc(C,OldCat,X2) ),
   trans(Pu,N2,_,N3,Wo2-Wo3,Po2-Po3,Ne2-Ne3).

trans(ltc(C,Pu,X1),N1,Res,N3,Wo1-Wo3,Po1-Po3,Ne1-Ne3):- !,
   trans(Pu,N1,_,N2,Wo1-Wo2,Po1-Po2,Ne1-Ne2),
   trans(X1,N2,X2,N3,Wo2-Wo3,Po2-Po3,Ne2-Ne3),
   topcat(X2,OldCat),
   ( \+ \+ C = OldCat, Res = X2; \+ C = OldCat, Res = tc(C,OldCat,X2) ).

/* -------------------------------------------------------------------------
   Punctuation rules
------------------------------------------------------------------------- */

trans(rp(_,X1,Y1),N1,X2,N3,Wo1-Wo3,Po1-Po3,Ne1-Ne3):- !, 
   trans(X1,N1,X2,N2,Wo1-Wo2,Po1-Po2,Ne1-Ne2),
   trans(Y1,N2,_,N3,Wo2-Wo3,Po2-Po3,Ne2-Ne3).

trans(lp(_,Pu,X1),N1,X2,N3,Wo1-Wo3,Po1-Po3,Ne1-Ne3):- !, 
   trans(Pu,N1,_,N2,Wo1-Wo2,Po1-Po2,Ne1-Ne2),
   trans(X1,N2,X2,N3,Wo2-Wo3,Po2-Po3,Ne2-Ne3).


/* -------------------------------------------------------------------------
   Application
------------------------------------------------------------------------- */

trans(fa(_,X1,Y1),  N1,  fa(C1,X2,Y2), N3, Wo1-Wo3,Po1-Po3,Ne1-Ne3):- !, 
   trans(X1,N1,X2,N2,Wo1-Wo2,Po1-Po2,Ne1-Ne2), 
   topcat(X2,C1/C2),
   trans(Y1,N2,Y2,N3,Wo2-Wo3,Po2-Po3,Ne2-Ne3),
   topcat(Y2,C2).

trans(ba(_,X1,Y1),  N1,  ba(C2,X2,Y2), N3, Wo1-Wo3,Po1-Po3,Ne1-Ne3):- !, 
   trans(X1,N1,X2,N2,Wo1-Wo2,Po1-Po2,Ne1-Ne2), 
   topcat(X2,C1),
   trans(Y1,N2,Y2,N3,Wo2-Wo3,Po2-Po3,Ne2-Ne3),
   topcat(Y2,C2\C1).


/* -------------------------------------------------------------------------
   Composition
------------------------------------------------------------------------- */

trans(fc(_,X1,Y1),  N1,  fc(C1/C3,X2,Y2), N3, Wo1-Wo3,Po1-Po3,Ne1-Ne3):- !, 
   trans(X1,N1,X2,N2,Wo1-Wo2,Po1-Po2,Ne1-Ne2), 
   trans(Y1,N2,Y2,N3,Wo2-Wo3,Po2-Po3,Ne2-Ne3),
   topcat(X2,C1/C2),
   topcat(Y2,C2/C3).

trans(bc(_,X1,Y1),  N1,  bc(C3\C1,X2,Y2), N3, Wo1-Wo3,Po1-Po3,Ne1-Ne3):- !, 
   trans(X1,N1,X2,N2,Wo1-Wo2,Po1-Po2,Ne1-Ne2), 
   trans(Y1,N2,Y2,N3,Wo2-Wo3,Po2-Po3,Ne2-Ne3),
   topcat(X2,C2\C1),
   topcat(Y2,C3\C2).


/* -------------------------------------------------------------------------
   Generalised Composition
------------------------------------------------------------------------- */

trans(gfc(C,X1,Y1),  N1,  gfc(C,X2,Y2), N3, Wo1-Wo3,Po1-Po3,Ne1-Ne3):- !, 
   trans(X1,N1,X2,N2,Wo1-Wo2,Po1-Po2,Ne1-Ne2), 
   trans(Y1,N2,Y2,N3,Wo2-Wo3,Po2-Po3,Ne2-Ne3).

trans(gbc(C,X1,Y1),  N1,  gbc(C,X2,Y2), N3, Wo1-Wo3,Po1-Po3,Ne1-Ne3):- !, 
   trans(X1,N1,X2,N2,Wo1-Wo2,Po1-Po2,Ne1-Ne2), 
   trans(Y1,N2,Y2,N3,Wo2-Wo3,Po2-Po3,Ne2-Ne3).

trans(gfc(C,N,X1,Y1),  N1,  gfc(C,N,X2,Y2), N3, Wo1-Wo3,Po1-Po3,Ne1-Ne3):- !, 
   trans(X1,N1,X2,N2,Wo1-Wo2,Po1-Po2,Ne1-Ne2), 
   trans(Y1,N2,Y2,N3,Wo2-Wo3,Po2-Po3,Ne2-Ne3).

trans(gbc(C,N,X1,Y1),  N1,  gbc(C,N,X2,Y2), N3, Wo1-Wo3,Po1-Po3,Ne1-Ne3):- !, 
   trans(X1,N1,X2,N2,Wo1-Wo2,Po1-Po2,Ne1-Ne2), 
   trans(Y1,N2,Y2,N3,Wo2-Wo3,Po2-Po3,Ne2-Ne3).


/* -------------------------------------------------------------------------
   Crossed Composition
------------------------------------------------------------------------- */

trans(bxc(_,X1,Y1),  N1,  bxc(C3/C1,X2,Y2), N3, Wo1-Wo3,Po1-Po3,Ne1-Ne3):- !, 
   trans(X1,N1,X2,N2,Wo1-Wo2,Po1-Po2,Ne1-Ne2), 
   trans(Y1,N2,Y2,N3,Wo2-Wo3,Po2-Po3,Ne2-Ne3),
   topcat(X2,C2/C1),
   topcat(Y2,C3\C2).

trans(fxc(C,X1,Y1), N1, fxc(C,X2,Y2), N3, Wo1-Wo3,Po1-Po3,Ne1-Ne3):- !, 
   trans(X1,N1,X2,N2,Wo1-Wo2,Po1-Po2,Ne1-Ne2), 
   trans(Y1,N2,Y2,N3,Wo2-Wo3,Po2-Po3,Ne2-Ne3).


/* -------------------------------------------------------------------------
   Generalised Crossed Composition
------------------------------------------------------------------------- */

trans(gfxc(C,X1,Y1), N1, gfxc(C,X2,Y2), N3, Wo1-Wo3,Po1-Po3,Ne1-Ne3):- !, 
   trans(X1,N1,X2,N2,Wo1-Wo2,Po1-Po2,Ne1-Ne2), 
   trans(Y1,N2,Y2,N3,Wo2-Wo3,Po2-Po3,Ne2-Ne3).

trans(gbxc(C,X1,Y1), N1, gbxc(C,X2,Y2), N3, Wo1-Wo3,Po1-Po3,Ne1-Ne3):- !, 
   trans(X1,N1,X2,N2,Wo1-Wo2,Po1-Po2,Ne1-Ne2), 
   trans(Y1,N2,Y2,N3,Wo2-Wo3,Po2-Po3,Ne2-Ne3).

trans(gfxc(C,N,X1,Y1), N1, gfxc(C,N,X2,Y2), N3, Wo1-Wo3,Po1-Po3,Ne1-Ne3):- !, 
   trans(X1,N1,X2,N2,Wo1-Wo2,Po1-Po2,Ne1-Ne2), 
   trans(Y1,N2,Y2,N3,Wo2-Wo3,Po2-Po3,Ne2-Ne3).

trans(gbxc(C,N,X1,Y1), N1, gbxc(C,N,X2,Y2), N3, Wo1-Wo3,Po1-Po3,Ne1-Ne3):- !, 
   trans(X1,N1,X2,N2,Wo1-Wo2,Po1-Po2,Ne1-Ne2), 
   trans(Y1,N2,Y2,N3,Wo2-Wo3,Po2-Po3,Ne2-Ne3).


/* -------------------------------------------------------------------------
   Conjuction
------------------------------------------------------------------------- */

%trans(conj(Cat,s:dcl,X1,Y1), N1, conj(Cat,s:dcl,X2,Y2), N3, Wo1-Wo3,Po1-Po3,Ne1-Ne3):- 
%   X1 = t(conj,   C1,C2,C3,C4,C5), !,
%   X3 = t(conj:s, C1,C2,C3,C4,C5), 
%   trans(X3,N1,X2,N2,Wo1-Wo2,Po1-Po2,Ne1-Ne2), 
%   trans(Y1,N2,Y2,N3,Wo2-Wo3,Po2-Po3,Ne2-Ne3).

trans(conj(np\np,np,X1,Y1), N1, conj(np\np,np,X2,Y2), N3, Wo1-Wo3,Po1-Po3,Ne1-Ne3):- 
   X1 = t(comma,    C1,C2,C3,C4,C5), !,
   X3 = t(conj:app, C1,C2,C3,C4,C5), 
   trans(X3,N1,X2,N2,Wo1-Wo2,Po1-Po2,Ne1-Ne2), 
   trans(Y1,N2,Y2,N3,Wo2-Wo3,Po2-Po3,Ne2-Ne3).
 
trans(conj(_C1,_C2,Y1,Z1), N1, conj(C\C,C,Y2,Z2), N3, Wo1-Wo3,Po1-Po3,Ne1-Ne3):- !, 
   trans(Y1,N1,Y2,N2,Wo1-Wo2,Po1-Po2,Ne1-Ne2), 
   trans(Z1,N2,Z2,N3,Wo2-Wo3,Po2-Po3,Ne2-Ne3),
   topcat(Z2,C).

trans(coord(_C,X1,Y1,Z1), N1, coord(C,X2,Y2,Z2), N4, Wo1-Wo4,Po1-Po4,Ne1-Ne4):- !, 
   trans(X1,N1,X2,N2,Wo1-Wo2,Po1-Po2,Ne1-Ne2), 
   trans(Y1,N2,Y2,N3,Wo2-Wo3,Po2-Po3,Ne2-Ne3), 
   trans(Z1,N3,Z2,N4,Wo3-Wo4,Po3-Po4,Ne3-Ne4),
   topcat(Z2,C).


/* -------------------------------------------------------------------------
   Unary Rules; Type Changing, Type Raising
------------------------------------------------------------------------- */

trans(lx(C1,_,X1), N1, tc(C3,C2,X2), N2, Wo,Po,Ne):- !, 
   addFeatureN(C1,C3),
   trans(X1,N1,X2,N2,Wo,Po,Ne),
   topcat(X2,C2).

trans(tc(C1,_,X1), N1, tc(C3,C2,X2), N2, Wo,Po,Ne):- !, 
   addFeatureN(C1,C3),
   trans(X1,N1,X2,N2,Wo,Po,Ne),
   topcat(X2,C2).

trans(tr(C,X1),     N1, tr(C,X2),     N2, Wo,Po,Ne):- !, 
   trans(X1,N1,X2,N2,Wo,Po,Ne).


/* -------------------------------------------------------------------------
   Substitution
------------------------------------------------------------------------- */

trans(fs(C,X1,Y1),  N1,  fs(C,X2,Y2), N3, Wo1-Wo3,Po1-Po3,Ne1-Ne3):- !, 
   trans(X1,N1,X2,N2,Wo1-Wo2,Po1-Po2,Ne1-Ne2), 
   trans(Y1,N2,Y2,N3,Wo2-Wo3,Po2-Po3,Ne2-Ne3).

trans(bs(C,X1,Y1),  N1,  bs(C,X2,Y2), N3, Wo1-Wo3,Po1-Po3,Ne1-Ne3):- !, 
   trans(X1,N1,X2,N2,Wo1-Wo2,Po1-Po2,Ne1-Ne2), 
   trans(Y1,N2,Y2,N3,Wo2-Wo3,Po2-Po3,Ne2-Ne3).

trans(fxs(C,X1,Y1), N1, fxs(C,X2,Y2), N3, Wo1-Wo3,Po1-Po3,Ne1-Ne3):- !, 
   trans(X1,N1,X2,N2,Wo1-Wo2,Po1-Po2,Ne1-Ne2), 
   trans(Y1,N2,Y2,N3,Wo2-Wo3,Po2-Po3,Ne2-Ne3).

trans(bxs(C,X1,Y1), N1, bxs(C,X2,Y2), N3, Wo1-Wo3,Po1-Po3,Ne1-Ne3):- !, 
   trans(X1,N1,X2,N2,Wo1-Wo2,Po1-Po2,Ne1-Ne2),  
   trans(Y1,N2,Y2,N3,Wo2-Wo3,Po2-Po3,Ne2-Ne3).


/* -------------------------------------------------------------------------
   Token (repair rules -- systematically wrong output of C&C parser)
------------------------------------------------------------------------- */

trans(t(Cat,'\'t',_,'VB',S,Ne),N,Tok,M,Ws,Ps,Ns):- !,
   trans(t(Cat,'\'t',not,'RB',S,Ne),N,Tok,M,Ws,Ps,Ns).

trans(t(Cat,Contr,Contr,Pos,S,Ne),N,Tok,M,Ws,Ps,Ns):- 
   member(Contr,['\'s','\'m','\'re']),
   member(Pos,['VBZ','VBP']), !,
   trans(t(Cat,Contr,be,Pos,S,Ne),N,Tok,M,Ws,Ps,Ns).


/* -------------------------------------------------------------------------
   Quotations
------------------------------------------------------------------------- */

trans(t(n,Token,_,_,_,_),N,t(N,n:F,Quoted,Lemma,Pos,0,Ne),M,W1-W2,P1-P2,N1-N2):- 
   atom(Token), atom_chars(Token,['L','Q','O','_'|Rest]), 
   append(QuotedChars,['_','R','Q','O'],Rest), !,
   atom_chars(Quoted,QuotedChars),
   downcase_atom(Quoted,Lemma), Pos = 'NNP', Ne = 'I-QUO',
   words(W2,N,Token,W1), 
   pos(P2,N,Pos,P1), 
   ne(N2,N,Ne,N1), 
   featureN(Pos,F),
   M is N + 1.

trans(t(n/n,Token,_,_,_,_),N,t(N,Cat,Quoted,Lemma,Pos,0,Ne),M,W1-W2,P1-P2,N1-N2):- 
   atom(Token), atom_chars(Token,['L','Q','O','_'|Rest]), 
   append(QuotedChars,['_','R','Q','O'],Rest), !,
   atom_chars(Quoted,QuotedChars),
   downcase_atom(Quoted,Lemma), Pos = 'NNP', Ne = 'I-QUO',
   addFeatureN(n/n,Cat),
   words(W2,N,Token,W1), 
   pos(P2,N,Pos,P1), 
   ne(N2,N,Ne,N1), 
   M is N + 1.
 

/* -------------------------------------------------------------------------
   Token, special case n (add feature)
------------------------------------------------------------------------- */

trans(t(n,Token,Lemma,Pos,S1,Ne),N,t(N,n:F,Token,Lemma,Pos,S2,Ne),M,W1-W2,P1-P2,N1-N2):- !,
   context(S1,S2),
   words(W2,N,Token,W1), 
   pos(P2,N,Pos,P1), 
   ne(N2,N,Ne,N1), 
   featureN(Pos,F),
   M is N + 1.

/* -------------------------------------------------------------------------
   Compound Token (try decomposition) 
------------------------------------------------------------------------- */

trans(t(Cat,Token,_,Pos,_,Ne),N,CCG,M,W1-W2,P1-P2,N1-N2):- 
   decompose(Cat,Token,N,CCG), !,
   words(W2,N,Token,W1), 
   pos(P2,N,Pos,P1), 
   ne(N2,N,Ne,N1), 
   M is N + 1.

/* -------------------------------------------------------------------------
   Token
------------------------------------------------------------------------- */

trans(t(Cat1,Token,Lemma,Pos,S1,Ne),N,t(N,Cat2,Token,Lemma,Pos,S2,Ne),M,W1-W2,P1-P2,N1-N2):-
   context(S1,S2),
   addFeatureN(Cat1,Cat2),
   words(W2,N,Token,W1), 
   pos(P2,N,Pos,P1), 
   ne(N2,N,Ne,N1), 
   M is N + 1.
 

/* =========================================================================
   External Context Information (for now only Word Sense Disambiguation)
========================================================================= */

context(Number,Sense):- number(Number), !, Sense = Number.
context(_,Sense):- Sense = 0.


/* =========================================================================
   Decomposition
========================================================================= */

decompose(Cat,Token,N,CCG):-
   Cat=n/n,
   atomic_list_concat([Prefix,Suffix],'-',Token),
   decomposition(Cat,Suffix,Lemma,Type,Pos,Ne), !,
   CCG = ba(n:F/n:F,
            ba(pp,
               t(N,n:Type,Prefix,Prefix,Pos,0,Ne),
               t(N,pp\n:Type,'-','-','-',0,'O')),
            t(N,(n:F/n:F)\pp,Suffix,Lemma,'VBN',0,'O')).

decompose(Cat,Token,N,CCG):-
   Cat=n/n,
   member(Unit,[year,month,day,week,
                story,meter,foot,centimeter]),
   atomic_list_concat([Prefix,Unit,Suffix],'-',Token),
   decomposition(Cat,Suffix,Lemma,Type,Pos,Ne), !,
   CCG = ba(n:F/n:F,
            ba(pp,
               fa(n:Type,
                  t(N,n:Type/n:Type,Prefix,Prefix,'CD',0,'O'),
                  t(N,n:Type,Unit,Unit,Pos,0,Ne)),
               t(N,pp\n:Type,'-','-','-',0,'O')),
            t(N,(n:F/n:F)\pp,Suffix,Lemma,'JJ',0,'O')).


/* =========================================================================
   Lexical Decomposition
========================================================================= */

decomposition(n/n, based,     base, loc, 'NNP', 'I-LOC').
decomposition(n/n, born,      bear, loc, 'NNP', 'I-LOC').
decomposition(n/n, related, relate, nom, 'NN',  'O').
decomposition(n/n, backed,    back, nom, 'NN',  'O').
decomposition(n/n, owned,      own, nom, 'NN',  'O').
decomposition(n/n, old,        old, num, 'NN',  'O').
decomposition(n/n, long,      long, nom, 'NN',  'O').
decomposition(n/n, tall,      tall, nom, 'NN',  'O').
decomposition(n/n, high,      high, nom, 'NN',  'O').


/* =========================================================================
   Determine Feature on N
========================================================================= */

featureN('NNPS', T):- !, member(T,[nam,num,nom]).
featureN('NNP',  T):- !, member(T,[nam,num,nom]).
featureN('CD',   T):- !, member(T,[num,nam,nom]).
featureN(_,      T):-    member(T,[nom,nam,num]).


/* =========================================================================
   Add feature on N (bugs in C&C parser)
========================================================================= */

addFeatureN(s, s:_):- !.             %%% bug in C&C parser

addFeatureN(s\np, s:dcl\np):- !.     %%% bug in C&C parser

addFeatureN(s/s:X, s:X/s:X).         %%% bug in C&C parser
addFeatureN(s/s:X, s:_/s:X):- !.     %%% bug in C&C parser

addFeatureN((((s:Y\np)\(s:Y\np))\((s\np)\(s\np)))/((s\np)\(s\np)),
            (((s:Y\np)\(s:Y\np))\((s:X\np)\(s:X\np)))/((s:Z\np)\(s:Z\np))):- !.


/* =========================================================================
   Add feature on N
========================================================================= */

%addFeatureN(np:expl, np_exp).

%addFeatureN(np:thr, np_thr).

addFeatureN(np:_, np):- !.

addFeatureN(n, n:F):- !, member(F,[nom,nam,num]).

addFeatureN((n\n)/n, (n:X\n:X)/n:_):- !.

addFeatureN((n/n)/(n/n), (n:X/n:X)/(n:X/n:X)):- !.

addFeatureN((n/n)/(n/n), (n:X/n:X)/(n:X/n:X)):- !.

addFeatureN(n/n:X, n:X/n:X):- !.

addFeatureN(n/s:X, n:F/s:X):- !, member(F,[nom,nam,num]).

addFeatureN(n/pp, n:F/pp):- !, member(F,[nom,nam,num]).

addFeatureN(n/n, n:X/n:X):- !.

addFeatureN(n\n, n:X\n:X):- !.

addFeatureN(F1/A1,F2/A2):- !,
   addFeatureN(F1,F2),
   addFeatureN(A1,A2).

addFeatureN(F1\A1,F2\A2):- !,
   addFeatureN(F1,F2),
   addFeatureN(A1,A2).

addFeatureN(Cat,Cat).


/* =========================================================================
   Adding Info
========================================================================= */

words(W,N,Word,[word(N,Word)|W]).
pos(W,N,Pos,[pos(N,Pos)|W]).
ne(W,_,'O',W):- !. 
ne(W,N,NE,[ne(N,NE)|W]). 


/* =========================================================================
   Top cat
========================================================================= */

topcat(fa(C,_,_),C).
topcat(ba(C,_,_),C).
topcat(fc(C,_,_),C).
topcat(bc(C,_,_),C).
topcat(fxc(C,_,_),C).
topcat(bxc(C,_,_),C).
topcat(fs(C,_,_),C).
topcat(bs(C,_,_),C).
topcat(fxs(C,_,_),C).
topcat(bxs(C,_,_),C).
topcat(gfc(C,_,_),C).
topcat(gbc(C,_,_),C). 
topcat(gfxc(C,_,_),C). 
topcat(gbxc(C,_,_),C). 
topcat(gfc(C,_,_,_),C).
topcat(gbc(C,_,_,_),C). 
topcat(gfxc(C,_,_,_),C). 
topcat(gbxc(C,_,_,_),C). 
topcat(tr(C,_),C).
topcat(tc(C,_,_),C).
topcat(lx(C,_,_),C).
topcat(t(_,C,_,_,_,_,_),C).
topcat(coord(C,_,_,_),C).
topcat(conj(C,_,_,_),C).

