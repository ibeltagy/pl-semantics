
:- module(ccg2drs,[ccg2drs/3,base/4]).

:- use_module(library(lists),[member/2]).

:- use_module(boxer(slashes)).
:- use_module(boxer(betaConversionDRT),[betaConvert/2]).
:- use_module(boxer(presupDRT),[resolveDrs/2]).
:- use_module(boxer(resolveDRT),[projectDrs/2]).
%:- use_module(boxer(vpe),[resolveVPE/2]).
:- use_module(boxer(transform),[preprocess/6,topcat/2,topatt/2]).
:- use_module(boxer(closure),[closure/3]).
:- use_module(boxer(lexicon),[semlex/5]).
:- use_module(boxer(coordination),[coordMacro/2,argCard/2]).
:- use_module(boxer(typechange),[typechange/5]).
:- use_module(boxer(evaluation),[incCompleted/0,incAttempted/0]).
:- use_module(boxer(input),[preferred/2]).
:- use_module(boxer(sdrt),[insertDRS/4]).
:- use_module(boxer(drs2fdrs),[instDrs/2]).
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
   projectDrs(DRS,NewDRS), !,
   incAttempted, incCompleted,
   ccg2drss(L,Tags,NewDRS,Context,XDRS,Index). 

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
   insertDRS(PrevDRS,DRS,TempDRS),
   projectDrs(TempDRS,NewDRS), !,
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

ccg2der(N,der(N,Der),Start,End):-
   preferred(N,CCG0),
   preprocess(N,CCG0,CCG1,_Tags,Start,End),
   interpretDer(CCG1,Der).

interpretDer(CCG,Copy):- interpret(CCG,_,Der), copy_term(Der,Copy), !.
interpretDer(CCG,CCG).


/* =========================================================================
   Insert DRS
========================================================================= */

insertDRS(smerge(B1,B2),New,smerge(B1,B3)):-
   option('--theory',drt), !, 
   insertDRS(B2,New,B3).

insertDRS(Old,New,B):- 
   option('--theory',drt), !, 
   B = smerge(Old,New).

insertDRS(Old,New,B):- 
   option('--theory',sdrt), !, 
   Rel = continuation,
   insertDRS(Rel,Old,New,B).


/* =========================================================================
   Build one DRS for derivation N
========================================================================= */

build(N,UDRS,Tags,Start,End,der(N,Der)):-
   preferred(N,CCG0),
   preprocess(N,CCG0,CCG1,Tags,Start,End),
   interpret(CCG1,Sem,Der),
   topcat(CCG1,Cat),
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
   option('--semantics',pdrs), !, Y=X.

semantics(X,Tags,Y):-
   option('--semantics',drg), !, instDrs(X,N), tuples(Tags,X,N,Y).

semantics(A,_,B):-
   option('--resolve',false),
   option('--semantics',drs), !, 
   pdrs2drs(A,B).

semantics(A,_,C):-
   option('--resolve',true),
   option('--semantics',drs), !, 
   pdrs2drs(A,B), resolveDrs(B,C).

semantics(A,_,C):-
   option('--resolve',false),
   option('--semantics',fol), !, 
   pdrs2drs(A,B), drs2fol(B,C).

semantics(A,_,D):-
   option('--resolve',true),
   option('--semantics',fol), !, 
   pdrs2drs(A,B), resolveDrs(B,C), drs2fol(C,D).

semantics(A,Tags,C):-
   option('--resolve',false),
   option('--semantics',tacitus), !, 
   pdrs2drs(A,B), instDrs(B,N), drs2tac(B,Tags,N,C).

semantics(A,Tags,D):-
   option('--resolve',true),
   option('--semantics',tacitus), !, 
   pdrs2drs(A,B), resolveDrs(B,C), instDrs(C,N), drs2tac(C,Tags,N,D).


/* =========================================================================
   Interpret the CCG tree for semantic composition
========================================================================= */

%interpretN(Node,Sem,nil):- 
%   \+ option('--semantics',der), !, 
%   interpret(Node,Sem,_).

%interpretN(Node,Sem,Copy):- 
%   option('--semantics',der), !, 
%   interpret(Node,Sem,Der),
%   copy_term(Der,Copy).

interpret(CCG,Sem,Der):-
   interp(CCG,Sem,Der), !.


/* -------------------------------------------------------------------------
   Forward Application
------------------------------------------------------------------------- */

interp(fa(Cat,_,Att,F1,A1),Sem,fa(Cat,Sem,Att,D1,D2)):- !,
   interpret(F1,F2,D1),
   interpret(A1,A2,D2),
   Sem=app(F2,A2).


/* -------------------------------------------------------------------------
   Backward Application
------------------------------------------------------------------------- */

interp(ba(Cat,_,Att,A1,F1),Sem,ba(Cat,Sem,Att,D1,D2)):- !,
   interpret(A1,A2,D1),
   interpret(F1,F2,D2),
   Sem=app(F2,A2).


/* -------------------------------------------------------------------------
   Forward Composition
------------------------------------------------------------------------- */

interp(fc(Cat,_,Att,F1,A1),Sem,fc(Cat,Sem,Att,D1,D2)):- !,
   interpret(F1,F2,D1),
   interpret(A1,A2,D2), 
   Sem=lam(X,app(F2,app(A2,X))).


/* -------------------------------------------------------------------------
   Backward Composition
------------------------------------------------------------------------- */

interp(bc(Cat,_,Att,A1,F1),Sem,bc(Cat,Sem,Att,D1,D2)):- !,
   interpret(A1,A2,D1), 
   interpret(F1,F2,D2),
   Sem=lam(X,app(F2,app(A2,X))).


/* -------------------------------------------------------------------------
   Forward Cross Composition
------------------------------------------------------------------------- */

interp(fxc(Cat,_,Att,F1,A1),Sem,fxc(Cat,Sem,Att,D1,D2)):- !,
   interpret(F1,F2,D1),
   interpret(A1,A2,D2),  
   Sem=lam(X,app(F2,app(A2,X))).


/* -------------------------------------------------------------------------
   Backward Cross Composition
------------------------------------------------------------------------- */

interp(bxc(Cat,_,Att,A1,F1),Sem,bxc(Cat,Sem,Att,D1,D2)):- !,
   interpret(A1,A2,D1),  
   interpret(F1,F2,D2),
   Sem=lam(X,app(F2,app(A2,X))).


/* -------------------------------------------------------------------------
   Forward Substitution
------------------------------------------------------------------------- */

interp(fs(Cat,_,Att,F1,A1),Sem,fs(Cat,Sem,Att,D1,D2)):- !,
   interpret(F1,F2,D1),
   interpret(A1,A2,D2), 
   Sem=lam(X,app(app(F2,X),app(A2,X))).


/* -------------------------------------------------------------------------
   Backward Substitution
------------------------------------------------------------------------- */

interp(bs(Cat,_,Att,A1,F1),Sem,bs(Cat,Sem,Att,D1,D2)):- !,
   interpret(A1,A2,D1),
   interpret(F1,F2,D2), 
   Sem=lam(X,app(app(F2,X),app(A2,X))).


/* -------------------------------------------------------------------------
   Forward Cross Substitution
------------------------------------------------------------------------- */

interp(fxs(Cat,_,Att,F1,A1),Sem,fxs(Cat,Sem,Att,D1,D2)):- !,
   interpret(F1,F2,D1),
   interpret(A1,A2,D2), 
   Sem=lam(X,app(app(F2,X),app(A2,X))).


/* -------------------------------------------------------------------------
   Backward Cross Substitution
------------------------------------------------------------------------- */

interp(bxs(Cat,_,Att,A1,F1),Sem,bxs(Cat,Sem,Att,D1,D2)):- !,
   interpret(A1,A2,D1),
   interpret(F1,F2,D2), 
   Sem=lam(X,app(app(F2,X),app(A2,X))).


/* -------------------------------------------------------------------------
   Generalised Forward Composition
------------------------------------------------------------------------- */

interp(gfc(Cat,N,_,Att,F1,A1),Sem,gfc(Cat,Sem,Att,D1,D2)):- !,
   interpret(F1,F2,D1),
   interpret(A1,A2,D2), 
   gen(N,F2,A2,Sem).

interp(gfc(Cat,S,A,F1,A1),Sem,Der):-
   topcat(F1,S1/S2),
   topcat(A1,ACat),
   base(ACat,S2/S3,Dollar,N),
   base(Cat,S1/S3,Dollar,N), !,
   interp(gfc(Cat,N,S,A,F1,A1),Sem,Der).   


/* -------------------------------------------------------------------------
   Generalised Backward Composition
------------------------------------------------------------------------- */

interp(gbc(Cat,N,_,Att,A1,F1),Sem,gbc(Cat,Sem,Att,D1,D2)):- !,
   interpret(A1,A2,D1), 
   interpret(F1,F2,D2),
   gen(N,F2,A2,Sem).

interp(gbc(Cat,S,A,A1,F1),Sem,Der):- 
   topcat(F1,S1\S2),
   topcat(A1,ACat),
   base(ACat,S2\S3,Dollar,N),
   base(Cat,S1\S3,Dollar,N), !,
   interp(gbc(Cat,N,S,A,A1,F1),Sem,Der).


/* -------------------------------------------------------------------------
   Generalised Forward Cross Composition
------------------------------------------------------------------------- */

interp(gfxc(Cat,N,_,Att,F1,A1),Sem,gfxc(Cat,Sem,Att,D1,D2)):- !,
   interpret(F1,F2,D1),
   interpret(A1,A2,D2), 
   gen(N,F2,A2,Sem).

interp(gfxc(Cat,S,A,F1,A1),Sem,Der):-
   topcat(F1,S1/S2),
   topcat(A1,ACat),
   base(ACat,S2\S3,Dollar,N),
   base(Cat,S1\S3,Dollar,N), !,
   interp(gfxc(Cat,N,S,A,F1,A1),Sem,Der).


/* -------------------------------------------------------------------------
   Generalised Backward Cross Composition
------------------------------------------------------------------------- */

interp(gbxc(Cat,N,_,Att,A1,F1),Sem,gbxc(Cat,Sem,Att,D1,D2)):- !,
   interpret(A1,A2,D1), 
   interpret(F1,F2,D2),
   gen(N,F2,A2,Sem).

interp(gbxc(Cat,S,A,A1,F1),Sem,Der):- 
   topcat(F1,S1\S2),
   topcat(A1,ACat),
   base(ACat,S2/S3,Dollar,N),
   base(Cat,S1/S3,Dollar,N), !,
   interp(gbxc(Cat,N,S,A,A1,F1),Sem,Der).

/* -------------------------------------------------------------------------
   Token
------------------------------------------------------------------------- */

interp(t(Cat,_Word,_,Att,I),Sem,nil):-
   \+ option('--semantics',der),
   att(Att,lemma,Lemma),
   downcase_atom(Lemma,Symbol),
   semlex(Cat,Symbol,[I],Att-_,Sem), !.

interp(t(Cat,Word,_,Att1,I),Sem,t(Cat,Word,Sem,Att2,I)):-
   option('--semantics',der), 
   att(Att1,lemma,Lemma),
   downcase_atom(Lemma,Symbol),
   semlex(Cat,Symbol,[I],Att1-Att2,Sem), !.


/* -------------------------------------------------------------------------
   Type Changing Rules
------------------------------------------------------------------------- */

interp(tc(NewCat,OldCat,_,Att,A1),A3,tc(NewCat,OldCat,A3,Att,D)):- !,
   interpret(A1,A2,D),
   typechange(OldCat,A2,Att,NewCat,A3).


/* -------------------------------------------------------------------------
   Type Raising
------------------------------------------------------------------------- */

interp(ftr(NewCat,OldCat,_,Att,A1),Sem,ftr(NewCat,OldCat,Sem,Att,D)):- !,
   interpret(A1,A2,D),
   Sem = lam(X,app(X,A2)).

interp(btr(NewCat,OldCat,_,Att,A1),Sem,btr(NewCat,OldCat,Sem,Att,D)):- !,
   interpret(A1,A2,D),
   Sem = lam(X,app(X,A2)).


/* -------------------------------------------------------------------------
   Coordination (a la Steedman)
------------------------------------------------------------------------- */

interp(coord(Cat,_,Att,L1,C1,R1),Sem,coord(Cat,Sem,Att,D1,D2,D3)):- !,
   argCard(Cat,N),
   coordMacro(N,Coord),
   interpret(L1,L2,D1),
   interpret(C1,C2,D2),
   interpret(R1,R2,D3),
   Sem = app(app(app(Coord,C2),R2),L2).


/* -------------------------------------------------------------------------
   Apposition (a la Hockenmaier)
------------------------------------------------------------------------- */

interp(conj(Cat,np,_,Att,C1,R1),Sem,conj(Cat,np,Sem,Att,D1,D2)):-
   topcat(C1,conj:app), 
   topcat(R1,np), 
   interpret(C1,C2,D1),
   interpret(R1,R2,D2), !,
   Sem = app(C2,R2).


/* -------------------------------------------------------------------------
   Dedicated coordination
------------------------------------------------------------------------- */

interp(conj(Cat,CCat,_,Att,C1,R1),Sem,conj(Cat,CCat,Sem,Att,D1,D2)):-  
   topcat(C1,conj:CCat), 
   topcat(R1,CCat), 
   interpret(C1,C2,D1),
   interpret(R1,R2,D2), !,
   Sem = app(C2,R2).


/* -------------------------------------------------------------------------
   Coordination (a la Hockenmaier)
------------------------------------------------------------------------- */

interp(conj(Cat,CCat,_,Att,C1,R1),Sem,conj(Cat,CCat,Sem,Att,D1,D2)):- !,
   argCard(CCat,N),
   coordMacro(N,Coord),
   interpret(C1,C2,D1),            % conjunctor
   interpret(R1,R2,D2),            % right conjunct
   Sem = app(app(Coord,C2),R2).


/* -------------------------------------------------------------------------
   Warning Messages
------------------------------------------------------------------------- */

interp(Input,_,_):-
   Input = t(Cat,Word,_,Att,Index),
   error('no lexical semantics for cat ~p (token: ~p), (attributes: ~p) (index: ~p)',[Cat,Word,Att,Index]), 
   !, fail.

interp(Input,_,_):-
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
