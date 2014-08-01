
:- module(ccg2drs,[ccg2drs/3,base/4,gen/4]).

:- use_module(library(lists),[member/2]).

:- use_module(boxer(slashes)).
:- use_module(boxer(betaConversionDRT),[betaConvert/2]).
:- use_module(boxer(resolveDRT),[resolveDrs/2]).
%:-use_module(boxer(vpe),[resolveVPE/2]).
:- use_module(boxer(transform),[preprocess/6,topcat/2,topatt/2,topsem/2,topstr/2]).
:- use_module(boxer(closure),[closure/3]).
:- use_module(boxer(lexicon),[semlex/5]).
:- use_module(boxer(coordination),[coordMacro/2,argCard/2]).
:- use_module(boxer(typechange),[typechange/5]).
:- use_module(boxer(evaluation),[incCompleted/0,incAttempted/0]).
:- use_module(boxer(input),[preferred/2]).
:- use_module(boxer(sdrt),[mergeSDRS/2]).
:- use_module(boxer(drs2fdrs),[instDrs/1,instDrs/2]).
:- use_module(boxer(tuples),[tuples/4]).
:- use_module(boxer(categories),[att/3]).

:- use_module(semlib(drs2tacitus),[drs2tac/4]).
:- use_module(semlib(pdrs2drs),[pdrs2drs/2]).
:- use_module(semlib(drs2fol),[drs2fol/2]).
:- use_module(semlib(options),[option/2]).
:- use_module(semlib(errors),[warning/2]).



/* =========================================================================
   Main Predicate
========================================================================= */

ccg2drs(L,Ders,_):-  
   option('--semantics',der), !,
   ccg2ders(L,Ders,1). 

ccg2drs([C|L],XDRS,Context):-  
   build(C,DRS,Tags,1,Index,_), 
   resolveDrs(DRS,Tags), !,
   incAttempted, incCompleted,
   ccg2drss(L,Tags,DRS,Context,XDRS,Index). 

ccg2drs([C|L],XDRS,Context):-
   incAttempted,
   noanalysis(C), 
   ccg2drs(L,XDRS,Context).


/* =========================================================================
   Build rest of underspecified Semantic Representations
========================================================================= */

ccg2drss([],Tags-[],PDRS,_,xdrs(Tags,Sem),_):- !,
   semantics(PDRS,Tags,Sem).

ccg2drss([C|L],Tags1-Tags2,PrevDRS,Context,XDRS,Index):-  
   build(C,DRS,Tags2-Tags3,Index,NewIndex,_), 
   insertDRS(PrevDRS,DRS,NewDRS),
   resolveDrs(NewDRS,Tags1-Tags3), !,
   incAttempted, incCompleted,
   ccg2drss(L,Tags1-Tags3,NewDRS,Context,XDRS,NewIndex). 

ccg2drss([C|L],Tags,PDRS,Context,XDRS,Index):-
   incAttempted,
   noanalysis(C), 
   ccg2drss(L,Tags,PDRS,Context,XDRS,Index). 


/* =========================================================================
   Build syntax-semantics derivations
========================================================================= */

ccg2ders([],[],_):- !.

ccg2ders([C|L],[Der|Ders],Index):-
   ccg2der(C,Der,Index,NewIndex), !,
   incAttempted,
   incCompleted,
   ccg2ders(L,Ders,NewIndex). 

ccg2ders([C|L],Ders,Index):-
   incAttempted,
   noanalysis(C), 
   ccg2ders(L,Ders,Index). 


/* =========================================================================
   Build syntax-semantics derivation
========================================================================= */

ccg2der(N,der(N,Der),Start,End):-
   preferred(N,CCG0),
   preprocess(N,CCG0,CCG1,_Tags,Start,End),
   interpretDer(CCG1,Der).

interpretDer(CCG,Copy):- 
   interpret(CCG,Der), 
   copy_term(Der,Copy), !.

interpretDer(CCG,CCG).


/* =========================================================================
   Insert DRS
========================================================================= */

insertDRS(merge(B1,B2),New,merge(B1,B3)):-
   option('--theory',drt), !, 
   insertDRS(B2,New,B3).

insertDRS(Old,New,B):- 
   option('--theory',drt), !, 
   B = merge(Old,New).

insertDRS(Old,New,B):- 
   option('--theory',sdrt), !, 
   Rel = continuation,
%  Rel = elaboration,
   mergeSDRS(smerge(Old,New,Rel,[]),B).


/* =========================================================================
   Build one DRS for derivation N
========================================================================= */

build(N,UDRS,Tags,Start,End,der(N,Der)):-
   preferred(N,CCG0),
   preprocess(N,CCG0,CCG1,Tags,Start,End),
   interpret(CCG1,Der), 
   topsem(Der,Sem),
   topcat(Der,Cat),
%  topstr(Der,Words), write(Words), nl,
   closure(Cat,Sem,Closed),
   betaConvert(Closed,UDRS), !.


/* =========================================================================
   Analysis failed for derivation N
========================================================================= */

noanalysis(N):-
   preferred(N,_), !,
   warning('no semantics for sentence ~p',[N]).
 
noanalysis(N):-
   warning('no syntax for sentence ~p',[N]).


/* =========================================================================
   Produce Semantic Representation
========================================================================= */

semantics(X,_,Y):-
   option('--instantiate',true), option('--semantics',pdrs), !, 
   instDrs(X), Y=X.

semantics(X,_,Y):-
   option('--semantics',pdrs), !, 
   Y=X.

semantics(X,Tags,Y):-
   option('--semantics',drg), !, 
   instDrs(X,N), tuples(Tags,X,N,Y).

semantics(A,_,B):-
   option('--instantiate',true), option('--semantics',drs), !, 
   instDrs(A), pdrs2drs(A,B).

semantics(A,_,B):-
   option('--semantics',drs), !, 
   pdrs2drs(A,B).

semantics(A,_,C):-
   option('--semantics',fol), !, 
   pdrs2drs(A,B), drs2fol(B,C).

semantics(A,Tags,C):-
   option('--semantics',tacitus), !, 
   instDrs(A,N), pdrs2drs(A,B), drs2tac(B,Tags,N,C).


/* -------------------------------------------------------------------------
   Forward Application
------------------------------------------------------------------------- */

interpret(fa(Cat,_,Att,W,F1,A1),fa(Cat,Sem,Att,W,D1,D2)):- !,
   interpret(F1,D1), topsem(D1,F2),
   interpret(A1,D2), topsem(D2,A2),
   Sem=app(F2,A2).      


/* -------------------------------------------------------------------------
   Backward Application
------------------------------------------------------------------------- */

interpret(ba(Cat,_,Att,W,A1,F1),ba(Cat,Sem,Att,W,D1,D2)):- !,
   interpret(A1,D1), topsem(D1,A2),
   interpret(F1,D2), topsem(D2,F2),
   Sem=app(F2,A2).


/* -------------------------------------------------------------------------
   Forward Composition
------------------------------------------------------------------------- */

interpret(fc(Cat,_,Att,W,F1,A1),fc(Cat,Sem,Att,W,D1,D2)):- !,
   interpret(F1,D1), topsem(D1,F2),
   interpret(A1,D2), topsem(D2,A2),
   Sem=lam(X,app(F2,app(A2,X))).


/* -------------------------------------------------------------------------
   Backward Composition
------------------------------------------------------------------------- */

interpret(bc(Cat,_,Att,W,A1,F1),bc(Cat,Sem,Att,W,D1,D2)):- !,
   interpret(A1,D1), topsem(D1,A2),
   interpret(F1,D2), topsem(D2,F2),
   Sem=lam(X,app(F2,app(A2,X))).


/* -------------------------------------------------------------------------
   Forward Cross Composition
------------------------------------------------------------------------- */

interpret(fxc(Cat,_,Att,W,F1,A1),fxc(Cat,Sem,Att,W,D1,D2)):- !,
   interpret(F1,D1), topsem(D1,F2),
   interpret(A1,D2), topsem(D2,A2),
   Sem=lam(X,app(F2,app(A2,X))).


/* -------------------------------------------------------------------------
   Backward Cross Composition
------------------------------------------------------------------------- */

interpret(bxc(Cat,_,Att,W,A1,F1),bxc(Cat,Sem,Att,W,D1,D2)):- !,
   interpret(A1,D1), topsem(D1,A2),
   interpret(F1,D2), topsem(D2,F2),
   Sem=lam(X,app(F2,app(A2,X))).


/* -------------------------------------------------------------------------
   Forward Substitution
------------------------------------------------------------------------- */

interpret(fs(Cat,_,Att,W,F1,A1),fs(Cat,Sem,Att,W,D1,D2)):- !,
   interpret(F1,D1), topsem(D1,F2),
   interpret(A1,D2), topsem(D2,A2),
   Sem=lam(X,app(app(F2,X),app(A2,X))).


/* -------------------------------------------------------------------------
   Backward Substitution
------------------------------------------------------------------------- */

interpret(bs(Cat,_,Att,W,A1,F1),bs(Cat,Sem,Att,W,D1,D2)):- !,
   interpret(A1,D1), topsem(D1,A2),
   interpret(F1,D2), topsem(F2,F2),
   Sem=lam(X,app(app(F2,X),app(A2,X))).


/* -------------------------------------------------------------------------
   Forward Cross Substitution
------------------------------------------------------------------------- */

interpret(fxs(Cat,_,Att,W,F1,A1),fxs(Cat,Sem,Att,W,D1,D2)):- !,
   interpret(F1,D1), topsem(D1,F2),
   interpret(A1,D2), topsem(D2,A2),
   Sem=lam(X,app(app(F2,X),app(A2,X))).


/* -------------------------------------------------------------------------
   Backward Cross Substitution
------------------------------------------------------------------------- */

interpret(bxs(Cat,_,Att,W,A1,F1),bxs(Cat,Sem,Att,W,D1,D2)):- !,
   interpret(A1,D1), topsem(D1,A2),
   interpret(F1,D2), topsem(D2,F2),
   Sem=lam(X,app(app(F2,X),app(A2,X))).


/* -------------------------------------------------------------------------
   Generalised Forward Composition
------------------------------------------------------------------------- */

interpret(gfc(Cat,N,_,Att,W,F1,A1),gfc(Cat,Sem,Att,W,D1,D2)):- !,
   interpret(F1,D1), topsem(D1,F2),
   interpret(A1,D2), topsem(D2,A2),
   gen(N,F2,A2,Sem).

interpret(gfc(Cat,S,A,W,F1,A1),Der):-
   topcat(F1,S1/S2),
   topcat(A1,ACat),
   base(ACat,S2/S3,Dollar,N),
   base(Cat,S1/S3,Dollar,N), !,
   interpret(gfc(Cat,N,S,A,W,F1,A1),Der).   


/* -------------------------------------------------------------------------
   Generalised Backward Composition
------------------------------------------------------------------------- */

interpret(gbc(Cat,N,_,Att,W,A1,F1),gbc(Cat,Sem,Att,W,D1,D2)):- !,
   interpret(A1,D1), topsem(D1,A2),
   interpret(F1,D2), topsem(D2,F2),
   gen(N,F2,A2,Sem).

interpret(gbc(Cat,S,A,A1,W,F1),Der):- 
   topcat(F1,S1\S2),
   topcat(A1,ACat),
   base(ACat,S2\S3,Dollar,N),
   base(Cat,S1\S3,Dollar,N), !,
   interpret(gbc(Cat,N,S,A,A1,W,F1),Der).


/* -------------------------------------------------------------------------
   Generalised Forward Cross Composition
------------------------------------------------------------------------- */

interpret(gfxc(Cat,N,_,Att,W,F1,A1),gfxc(Cat,Sem,Att,W,D1,D2)):- !,
   interpret(F1,D1), topsem(D1,F2),
   interpret(A1,D2), topsem(D2,A2),
   gen(N,F2,A2,Sem).

interpret(gfxc(Cat,S,A,W,F1,A1),Der):-
   topcat(F1,S1/S2),
   topcat(A1,ACat),
   base(ACat,S2\S3,Dollar,N),
   base(Cat,S1\S3,Dollar,N), !,
   interpret(gfxc(Cat,N,S,A,W,F1,A1),Der).


/* -------------------------------------------------------------------------
   Generalised Backward Cross Composition
------------------------------------------------------------------------- */

interpret(gbxc(Cat,N,_,Att,W,A1,F1),gbxc(Cat,Sem,Att,W,D1,D2)):- !,
   interpret(A1,D1), topsem(D1,A2),
   interpret(F1,D2), topsem(D2,F2),
   gen(N,F2,A2,Sem).

interpret(gbxc(Cat,S,A,W,A1,F1),Der):- 
   topcat(F1,S1\S2),
   topcat(A1,ACat),
   base(ACat,S2/S3,Dollar,N),
   base(Cat,S1/S3,Dollar,N), !,
   interp(gbxc(Cat,N,S,A,W,A1,F1),Der).

/* -------------------------------------------------------------------------
   Token
------------------------------------------------------------------------- */

%interp(t(Cat,_Word,_,Att,I),Sem,nil):-
%   \+ option('--semantics',der),
%   att(Att,lemma,Lemma),
%   downcase_atom(Lemma,Symbol),
%   semlex(Cat,Symbol,[I],Att-_,Sem), !.

interpret(t(Cat,Word,_,Att1,I),t(Cat,Word,Sem,Att2,I)):-
%  option('--semantics',der), 
   att(Att1,lemma,Lemma),
   downcase_atom(Lemma,Symbol),
   semlex(Cat,Symbol,[I],Att1-Att2,Sem), !.


/* -------------------------------------------------------------------------
   Type Changing Rules
------------------------------------------------------------------------- */

interpret(tc(NewCat,OldCat,_,Att,W,A1),tc(NewCat,OldCat,A3,Att,W,D)):- !,
   interpret(A1,D), topsem(D,A2),
   typechange(OldCat,A2,Att,NewCat,A3).


/* -------------------------------------------------------------------------
   Type Raising
------------------------------------------------------------------------- */

interpret(ftr(NewCat,OldCat,_,Att,W,A1),ftr(NewCat,OldCat,Sem,Att,W,D)):- !,
   interpret(A1,D), topsem(D,A2),
   Sem = lam(X,app(X,A2)).

interpret(btr(NewCat,OldCat,_,Att,W,A1),btr(NewCat,OldCat,Sem,Att,W,D)):- !,
   interpret(A1,D), topsem(D,A2),
   Sem = lam(X,app(X,A2)).


/* -------------------------------------------------------------------------
   Coordination (a la Steedman)
------------------------------------------------------------------------- */

interpret(coord(Cat,_,Att,W,L1,C1,R1),coord(Cat,Sem,Att,W,D1,D2,D3)):- !,
   argCard(Cat,N),
   coordMacro(N,Coord),
   interpret(L1,D1), topsem(D1,L2),
   interpret(C1,D2), topsem(D2,C2),
   interpret(R1,D3), topsem(D3,R2),
   Sem = app(app(app(Coord,C2),R2),L2).


/* -------------------------------------------------------------------------
   Apposition (a la Hockenmaier)
------------------------------------------------------------------------- */

interpret(conj(Cat,np,_,Att,W,C1,R1),conj(Cat,np,Sem,Att,W,D1,D2)):-
   topcat(C1,conj:app), 
   topcat(R1,np), 
   interpret(C1,D1), topsem(D1,C2),
   interpret(R1,D2), !, topsem(D2,R2),
   Sem = app(C2,R2).


/* -------------------------------------------------------------------------
   Dedicated coordination
------------------------------------------------------------------------- */

interpret(conj(Cat,CCat,_,Att,W,C1,R1),conj(Cat,CCat,Sem,Att,W,D1,D2)):-  
   topcat(C1,conj:CCat), 
   topcat(R1,CCat), 
   interpret(C1,D1), topsem(D1,C2),
   interpret(R1,D2), !, topsem(D2,R2),
   Sem = app(C2,R2).


/* -------------------------------------------------------------------------
   Coordination (a la Hockenmaier)
------------------------------------------------------------------------- */

interpret(conj(Cat,CCat,_,Att,W,C1,R1),conj(Cat,CCat,Sem,Att,W,D1,D2)):- !,
   argCard(CCat,N),
   coordMacro(N,Coord),
   interpret(C1,D1), topsem(D1,C2),            % conjunctor
   interpret(R1,D2), topsem(D2,R2),            % right conjunct
   Sem = app(app(Coord,C2),R2).


/* -------------------------------------------------------------------------
   Warning Messages
------------------------------------------------------------------------- */

interpret(Input,_,_):-
   Input = t(Cat,Word,_,Att,Index),
   error('no lexical semantics for cat ~p (token: ~p), (attributes: ~p) (index: ~p)',[Cat,Word,Att,Index]), 
   !, fail.

interpret(Input,_,_):-
   error('no interpretation rule for ~p',[Input]), 
   !, fail.


/* =========================================================================
   Semantics for Generalised Rules
========================================================================= */

gen(1,F,A,lam(X1,app(F,app(A,X1)))).
gen(2,F,A,lam(X1,lam(X2,app(F,app(app(A,X1),X2))))).
gen(3,F,A,lam(X1,lam(X2,lam(X3,app(F,app(app(app(A,X1),X2),X3)))))).
gen(4,F,A,lam(X1,lam(X2,lam(X3,lam(X4,app(F,app(app(app(app(A,X1),X2),X3),X4))))))).

    
/*=============================================================
   Base Categories
=============================================================*/

base(Cat,Cat,[],1):- !.

base(Cat/Riht,Base,[riht(Riht)|A],N):- !, 
   base(Cat,Base,A,M), N is M + 1.

base(Cat\Left,Base,[left(Left)|A],N):- !, 
   base(Cat,Base,A,M), N is M + 1.


/* =========================================================================
   Wrapper to choose lemma or word

semlex(Cat,Word,_Lemma,Index,Att1-Att2,Sem):-
   att(Att1,pos,Pos), member(Pos,['NNP','NNPS']), !,
   downcase_atom(Word,Sym),
   semlex(Cat,Sym,Index,Att1-Att2,Sem).

semlex(Cat,_Word,Lemma,Index,Att1-Att2,Sem):-
   downcase_atom(Lemma,Sym),
   semlex(Cat,Sym,Index,Att1-Att2,Sem).
========================================================================= */
