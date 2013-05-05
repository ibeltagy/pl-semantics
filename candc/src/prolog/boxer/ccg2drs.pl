
:- module(ccg2drs,[ccg2drs/3,udrs2drs/2]).

:- use_module(library(lists),[append/3,member/2]).

:- use_module(boxer(slashes)).
:- use_module(boxer(betaConversionDRT),[betaConvert/2]).
:- use_module(boxer(presupDRT),[resolveDrs/3]).
:- use_module(boxer(vpe),[resolveVPE/2]).
:- use_module(boxer(noncomp),[noncomp/2]).
:- use_module(boxer(transform),[preprocess/8,topcat/2]).
:- use_module(boxer(closure),[closure/3]).
:- use_module(boxer(lexicon),[semlex/5]).
:- use_module(boxer(coordination),[coordMacro/2]).
:- use_module(boxer(typechange),[typechange/5]).
:- use_module(boxer(evaluation),[incCompleted/0,incAttempted/0]).
:- use_module(boxer(input),[preferred/2]).
:- use_module(boxer(sdrt),[insertDRS/4]).
:- use_module(boxer(thematicRoles),[labelRolesDrs/2]).
%:- use_module(boxer(ppDrs),[ppDrs/3]).
%:- use_module(boxer(printDrs),[printDrs/1]).

:- use_module(semlib(options),[option/2]).
:- use_module(semlib(errors),[warning/2]).



/* =========================================================================
   Resolving UDRS (main)
========================================================================= */

udrs2drs(I,XDRS):-
   input:sem(I,Words,Pos,NE,P),
   resolveUDRS(P,U), 
   XDRS=xdrs(Words,Pos,NE,U).


/* =========================================================================
   Resolving UDRS
========================================================================= */

resolveUDRS(smerge(B1,B2),Final):- !,
   resolve(B1,New,_),
   resolveUDRS(B2,New,Final).

resolveUDRS(B,Final):-
   resolve(B,Final,_).

resolveUDRS(smerge(B1,B2),PrevDRS,FinalDRS):- !,
   insertDRS(PrevDRS,B1,TempDRS),
   resolve(TempDRS,NewDRS,_),
   resolveUDRS(B2,NewDRS,FinalDRS).

resolveUDRS(B,PrevDRS,FinalDRS):-
   insertDRS(PrevDRS,B,TempDRS),
   resolve(TempDRS,FinalDRS,_).


/* =========================================================================
   Main Predicate
========================================================================= */

ccg2drs(L,Ders,_):-  
   option('--semantics',der), !,
   ccg2ders(L,Ders,1). 

ccg2drs([C|L],XDRS,Context):-  
   build(C,UDRS,Words1,Pos1,NE1,1,Index,_), 
%   ppDrs(xdrs(Words1,Pos1,NE1,UDRS),Context,xdrs(Words2,Pos2,NE2,PUDRS)),   
   noncomp(xdrs(Words1,Pos1,NE1,UDRS),xdrs(Words2,Pos2,NE2,PUDRS)),
   labelRolesDrs(PUDRS,RPUDRS),
   resolve(RPUDRS,NewDRS,_Links), !,
   incAttempted, incCompleted,
   ccg2drss(L,Words2,Pos2,NE2,NewDRS,Context,XDRS,Index). 

ccg2drs([C|L],XDRS,Context):-
   incAttempted,
   noanalysis(C), 
   ccg2drs(L,XDRS,Context).


/* =========================================================================
   Build rest of underspecified Semantic Representations
========================================================================= */

ccg2drss([],PW,PP,PN,smerge(drs([],[]),PDRS),_,xdrs(PW,PP,PN,PDRS),_):- !.

ccg2drss([],PW,PP,PN,PDRS,_,xdrs(PW,PP,PN,PDRS),_):- !.

ccg2drss([C|L],PrevWords,PrevPos,PrevNE,PrevDRS,Context,XDRS,Index):-  
   build(C,UDRS,Words1,Pos1,NE1,Index,NewIndex,_), 
%   ppDrs(xdrs(Words1,Pos1,NE1,UDRS),Context,xdrs(Words2,Pos2,NE2,PUDRS)),   
   noncomp(xdrs(Words1,Pos1,NE1,UDRS),xdrs(Words2,Pos2,NE2,PUDRS)),
   labelRolesDrs(PUDRS,RPUDRS),   
   insertDRS(PrevDRS,RPUDRS,TempDRS),
   resolve(TempDRS,NewDRS,_Links), !,
   incAttempted,
   incCompleted,
   append(PrevWords,Words2,Words),
   append(PrevPos,Pos2,Pos),
   append(PrevNE,NE2,NE),
   ccg2drss(L,Words,Pos,NE,NewDRS,Context,XDRS,NewIndex). 

ccg2drss([C|L],PW,PP,PN,PDRS,Context,XDRS,Index):-
   incAttempted,
   noanalysis(C), 
   ccg2drss(L,PW,PP,PN,PDRS,Context,XDRS,Index). 


/* =========================================================================
   Build rest of derivations
========================================================================= */

ccg2ders([],[],_).

ccg2ders([C|L],[Der|Ders],Index):-  
   build(C,_,_,_,_,Index,NewIndex,Der), !,
   incAttempted,
   incCompleted,
   ccg2ders(L,Ders,NewIndex). 

ccg2ders([C|L],Ders,Index):-
   incAttempted,
   noanalysis(C), 
   ccg2ders(L,Ders,Index). 


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
   Build one underspecified UDRS for derivation N
========================================================================= */

build(N,UDRS,Words,POS,NE,Start,End,der(N,Der)):-
   preferred(N,CCG0),
   preprocess(N,CCG0,CCG1,Words,POS,NE,Start,End),
   interpretN(CCG1,Sem,Der),
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
   Resolve Semantic Representation
========================================================================= */

resolve(X,Z,Links):-
   option('--vpe',true), 
   option('--resolve',true), !,
   resolveDrs(X,Y,Links),
   resolveVPE(Y,Z).

resolve(X,Z,Links):-
   option('--vpe',false), 
   option('--resolve',true), !,
   resolveDrs(X,Z,Links).

resolve(X,X,[]).


/* =========================================================================
   Interpret the CCG tree for semantic composition
========================================================================= */

interpretN(Node,Sem,nil):- 
   \+ option('--semantics',der), !, 
   interpret(Node,Sem,_,_).

interpretN(Node,Sem,Copy):- 
   option('--semantics',der), !, 
   interpret(Node,Sem,Der,_),
   copy_term(Der,Copy).

interpret(CCG,Sem,Der,Ind):-
   interp(CCG,Sem,Der,Ind), !.


/* -------------------------------------------------------------------------
   Forward Application
------------------------------------------------------------------------- */

interp(fa(Cat,F1,A1),Sem,fa(Cat,Sem,D1,D2),I1-I3):- !,
   interpret(F1,F2,D1,I1-I2),
   interpret(A1,A2,D2,I2-I3),
   Sem=app(F2,A2).


/* -------------------------------------------------------------------------
   Backward Application
------------------------------------------------------------------------- */

interp(ba(Cat,A1,F1),Sem,ba(Cat,Sem,D1,D2),I1-I3):- !,
   interpret(A1,A2,D1,I1-I2),
   interpret(F1,F2,D2,I2-I3),
   Sem=app(F2,A2).


/* -------------------------------------------------------------------------
   Forward Composition
------------------------------------------------------------------------- */

interp(fc(Cat,F1,A1),Sem,fc(Cat,Sem,D1,D2),I1-I3):- !,
   interpret(F1,F2,D1,I1-I2),
   interpret(A1,A2,D2,I2-I3), 
   Sem=lam(X,app(F2,app(A2,X))).


/* -------------------------------------------------------------------------
   Backward Composition
------------------------------------------------------------------------- */

interp(bc(Cat,A1,F1),Sem,bc(Cat,Sem,D1,D2),I1-I3):- !,
   interpret(A1,A2,D1,I1-I2), 
   interpret(F1,F2,D2,I2-I3),
   Sem=lam(X,app(F2,app(A2,X))).


/* -------------------------------------------------------------------------
   Forward Cross Composition
------------------------------------------------------------------------- */

interp(fxc(Cat,F1,A1),Sem,fxc(Cat,Sem,D1,D2),I1-I3):- !,
   interpret(F1,F2,D1,I1-I2),
   interpret(A1,A2,D2,I2-I3),  
   Sem=lam(X,app(F2,app(A2,X))).


/* -------------------------------------------------------------------------
   Backward Cross Composition
------------------------------------------------------------------------- */

interp(bxc(Cat,A1,F1),Sem,bxc(Cat,Sem,D1,D2),I1-I3):- !,
   interpret(A1,A2,D1,I1-I2),  
   interpret(F1,F2,D2,I2-I3),
   Sem=lam(X,app(F2,app(A2,X))).


/* -------------------------------------------------------------------------
   Forward Substitution
------------------------------------------------------------------------- */

interp(fs(Cat,F1,A1),Sem,fs(Cat,Sem,D1,D2),I1-I3):- !,
   interpret(F1,F2,D1,I1-I2),
   interpret(A1,A2,D2,I2-I3), 
   Sem=lam(X,app(app(F2,X),app(A2,X))).


/* -------------------------------------------------------------------------
   Backward Substitution
------------------------------------------------------------------------- */

interp(bs(Cat,A1,F1),Sem,bs(Cat,Sem,D1,D2),I1-I3):- !,
   interpret(A1,A2,D1,I1-I2),
   interpret(F1,F2,D2,I2-I3), 
   Sem=lam(X,app(app(F2,X),app(A2,X))).


/* -------------------------------------------------------------------------
   Forward Cross Substitution
------------------------------------------------------------------------- */

interp(fxs(Cat,F1,A1),Sem,fxs(Cat,Sem,D1,D2),I1-I3):- !,
   interpret(F1,F2,D1,I1-I2),
   interpret(A1,A2,D2,I2-I3), 
   Sem=lam(X,app(app(F2,X),app(A2,X))).


/* -------------------------------------------------------------------------
   Backward Cross Substitution
------------------------------------------------------------------------- */

interp(bxs(Cat,A1,F1),Sem,bxs(Cat,Sem,D1,D2),I1-I3):- !,
   interpret(A1,A2,D1,I1-I2),
   interpret(F1,F2,D2,I2-I3), 
   Sem=lam(X,app(app(F2,X),app(A2,X))).


/* -------------------------------------------------------------------------
   Generalised Forward Composition
------------------------------------------------------------------------- */

interp(gfc(Cat,F1,A1),Sem,Der,I):-
   topcat(F1,S1/S2),
   topcat(A1,ACat),
   base(ACat,S2/S3,Dollar,N),
   base(Cat,S1/S3,Dollar,N), !,
   interp(gfc(Cat,N,F1,A1),Sem,Der,I).   

interp(gfc(Cat,N,F1,A1),Sem,gfc(Cat,Sem,D1,D2),I1-I3):- !,
   interpret(F1,F2,D1,I1-I2),
   interpret(A1,A2,D2,I2-I3), 
   gen(N,F2,A2,Sem).


/* -------------------------------------------------------------------------
   Generalised Backward Composition
------------------------------------------------------------------------- */

interp(gbc(Cat,A1,F1),Sem,Der,I):-
   topcat(F1,S1\S2),
   topcat(A1,ACat),
   base(ACat,S2\S3,Dollar,N),
   base(Cat,S1\S3,Dollar,N), !,
   interp(gbc(Cat,N,A1,F1),Sem,Der,I).

interp(gbc(Cat,N,A1,F1),Sem,gbc(Cat,Sem,D1,D2),I1-I3):- !,
   interpret(A1,A2,D1,I1-I2), 
   interpret(F1,F2,D2,I2-I3),
   gen(N,F2,A2,Sem).


/* -------------------------------------------------------------------------
   Generalised Forward Cross Composition
------------------------------------------------------------------------- */

interp(gfxc(Cat,F1,A1),Sem,Der,I):-
   topcat(F1,S1/S2),
   topcat(A1,ACat),
   base(ACat,S2\S3,Dollar,N),
   base(Cat,S1\S3,Dollar,N), !,
   interp(gfxc(Cat,N,F1,A1),Sem,Der,I).

interp(gfxc(Cat,N,F1,A1),Sem,gfxc(Cat,Sem,D1,D2),I1-I3):- !,
   interpret(F1,F2,D1,I1-I2),
   interpret(A1,A2,D2,I2-I3), 
   gen(N,F2,A2,Sem).


/* -------------------------------------------------------------------------
   Generalised Backward Cross Composition
------------------------------------------------------------------------- */

interp(gbxc(Cat,A1,F1),Sem,Der,I):- 
   topcat(F1,S1\S2),
   topcat(A1,ACat),
   base(ACat,S2/S3,Dollar,N),
   base(Cat,S1/S3,Dollar,N), !,
   interp(gbxc(Cat,N,A1,F1),Sem,Der,I).

interp(gbxc(Cat,N,A1,F1),Sem,gbxc(Cat,Sem,D1,D2),I1-I3):- !,
   interpret(A1,A2,D1,I1-I2), 
   interpret(F1,F2,D2,I2-I3),
   gen(N,F2,A2,Sem).


/* -------------------------------------------------------------------------
   Token
------------------------------------------------------------------------- */

interp(t(I,Cat,Word,Lemma,Pos,Sense,Ne),Sem,nil,Ind):-
   \+ option('--semantics',der), 
   semlex(Cat,Word,Lemma,Pos,Ne,Sense,[I],Sem), !,
   Ind=[I|L]-L.

interp(t(I,Cat,Word,Lemma,Pos,Sense,Ne),Sem,t(Sem,Cat,Word,Pos,Ne),Ind):-
   option('--semantics',der), 
   semlex(Cat,Word,Lemma,Pos,Ne,Sense,[I],Sem), !,
   Ind=[I|L]-L.


/* -------------------------------------------------------------------------
   Type Changing Rules
------------------------------------------------------------------------- */

interp(tc(New,Old,A1),A3,tc(New,A3,D),L-L):- !,
   interpret(A1,A2,D,Ind-[]),
   typechange(Old,A2,New,A3,Ind).


/* -------------------------------------------------------------------------
   Type Raising
------------------------------------------------------------------------- */

interp(tr(Cat,A1),Sem,tr(Cat,Sem,D),Ind):- !,
   interpret(A1,A2,D,Ind),
   Sem = lam(X,app(X,A2)).


/* -------------------------------------------------------------------------
   Coordination (a la Steedman)
------------------------------------------------------------------------- */

interp(coord(Cat,L1,C1,R1),Sem,coord(Cat,Sem,D1,D2,D3),I1-I4):- !,
   argCard(Cat,N),
   coordMacro(N,Coord),
   interpret(L1,L2,D1,I1-I2),
   interpret(C1,C2,D2,I2-I3),
   interpret(R1,R2,D3,I3-I4),
   Sem = app(app(app(Coord,C2),R2),L2).


/* -------------------------------------------------------------------------
   Apposition (a la Hockenmaier)
------------------------------------------------------------------------- */

interp(conj(Cat,np,C1,R1),Sem,conj(Cat,Sem,D1,D2),I2-I4):-
   topcat(C1,conj:app), 
   topcat(R1,np), 
   interpret(C1,C2,D1,I2-I3),
   interpret(R1,R2,D2,I3-I4), !,
   Sem = app(C2,R2).


/* -------------------------------------------------------------------------
   Coordination (a la Hockenmaier)
------------------------------------------------------------------------- */

interp(conj(Cat,CCat,C1,R1),Sem,conj(Cat,Sem,D1,D2),I2-I4):- !,
   argCard(CCat,N),
   coordMacro(N,Coord),
   interpret(C1,C2,D1,I2-I3),
   interpret(R1,R2,D2,I3-I4),
   Sem = app(app(Coord,C2),R2).


/* -------------------------------------------------------------------------
   Warning Messages
------------------------------------------------------------------------- */

interp(Input,_,_,_):-
   Input = t(_Index,Cat,Word,_Lemma,Pos,_Ne), !,
   warning('no lexical semantics for cat ~p (token: ~p), (POS: ~p)',[Cat,Word,Pos]), 
   !, fail.

interp(Input,_,_,_):-
   error('no interpretation rule for ~p',[Input]), 
   fail.


/* =========================================================================
   Argument Cardinality
========================================================================= */

argCard(_:_,C):- !, C = 0.
argCard(X/_,C):- !, argCard(X,N), C is N + 1.
argCard(X\_,C):- !, argCard(X,N), C is N + 1.
argCard(_,0).


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
========================================================================= */

semlex(Cat,Word,_Lemma,Pos,Nam,Sense,Index,Sem):-
   member(Pos,['NNP','NNPS']), !,
   downcase_atom(Word,Sym),
   semlex(Cat,Sym,Pos:Nam:Sense,Index,Sem).

semlex(Cat,_Word,Lemma,Pos,Nam,Sense,Index,Sem):-
   downcase_atom(Lemma,Sym),
   semlex(Cat,Sym,Pos:Nam:Sense,Index,Sem).
