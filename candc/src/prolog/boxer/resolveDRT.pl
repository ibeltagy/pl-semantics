
:- module(resolveDRT,[resolveDrs/2,goldAntecedent/2]).

:- use_module(boxer(bindingViolation),[noBindingViolationDrs/1]).
:- use_module(boxer(freeVarCheck),[boundVarCheckContext/2,drsCondition/2]).
:- use_module(library(lists),[member/2,append/3,select/3]).
:- use_module(semlib(options),[option/2]).
:- use_module(semlib(errors),[warning/2,gold/2]).
:- use_module(boxer(categories),[att/3]).
:- use_module(knowledge(antecedent),[score/4]).


/* ========================================================================
   Dynamic Predicate
======================================================================== */

:- dynamic antecedent/2.


/* ========================================================================
   Managing Gold Standard Antecedents
======================================================================== */

goldAntecedent(Index,Att):-
   att(Att,antecedent,Antecedent), 
   number(Antecedent), !,
%  write(antecedent(Index,Antecedent)),nl,
   assert(antecedent(Index,Antecedent)).

goldAntecedent(_,_).


/* ========================================================================
   Main predicate: resolveDrs/1
======================================================================== */

resolveDrs(B,Tags):- 
   option('--resolve',true), !, 
   copy_term(Tags,L-[]), 
   setof(X,T^member(X:T,L),IDs),
   resolveDRS(B,[]-_,[]-_,IDs).

resolveDrs(_,_).


/* ========================================================================
   Main predicate: resolveDRS/4 (DRS)

   Context is a difference list of pointed DRSs (i.e. a projection
   path), ordered on recency (closest first).
======================================================================== */

resolveDRS(sdrs([],_),C-C,P-P,_):- !.

resolveDRS(sdrs([lab(_,B)|L],C),C1-C3,P1-P3,IDs):- !,
   resolveDRS(B,C1-C2,P1-P2,IDs),
   resolveDRS(sdrs(L,C),C2-C3,P2-P3,IDs).

resolveDRS(sdrs([sub(B1,B2)|L],C),C1-C3,P1-P4,IDs):- !,
   resolveDRS(B1,C1-C2,P1-P2,IDs),
   resolveDRS(B2,C2-_,P2-P3,IDs),
   resolveDRS(sdrs(L,C),C2-C3,P3-P4,IDs).

resolveDRS(merge(B1,B2),C1-C3,P1-P3,IDs):- !,
   resolveDRS(B1,C1-C2,P1-P2,IDs),
   resolveDRS(B2,C2-C3,P2-P3,IDs).

resolveDRS(lab(_,B),Context,P,IDs):- !,
   resolveDRS(B,Context,P,IDs).

resolveDRS(K:B,C1-C2,P1-P3,IDs):-
   anaphoric(K:B,ADRS,C1,P1), !,                                  %%% if there is a free pointer
   project([K:B|C1],ADRS,P1,P1-P2,[K:B|C1],[],IDs),                %%% then resolve it
   resolveDRS(K:B,C1-C2,P2-P3,IDs).

resolveDRS(K:drs(D,C),C1-[K:drs(D,C)|C1],P,IDs):- !,
   resolveConds(C,[K:drs(D,C)|C1],P,IDs).

resolveDRS(U,C-C,P-P,_):- 
   warning('unknown DRS in resolveDRS/3: ~p',[U]).


/* ========================================================================
   Resolve Conditions
======================================================================== */

resolveConds([],_,P-P,_):- !.

resolveConds([_:C|L],Context,P,IDs):- !, 
   resolveConds([C|L],Context,P,IDs).

resolveConds([not(B)|C],Context,P1-P3,IDs):- !,
   resolveDRS(B,Context-_,P1-P2,IDs),
   resolveConds(C,Context,P2-P3,IDs).

resolveConds([nec(B)|C],Context,P,IDs):- !,
   resolveConds([not(B)|C],Context,P,IDs).

resolveConds([pos(B)|C],Context,P,IDs):- !,
   resolveConds([not(B)|C],Context,P,IDs).

resolveConds([prop(_,B)|C],Context,P,IDs):- !,
   resolveConds([not(B)|C],Context,P,IDs).

resolveConds([imp(B1,B2)|C],C1,P1-P4,IDs):- !,
   resolveDRS(B1,C1-C2,P1-P2,IDs),
   resolveDRS(B2,C2-_,P2-P3,IDs),
   resolveConds(C,C1,P3-P4,IDs).

resolveConds([duplex(_,B1,_,B2)|C],Context,P,IDs):- !,
   resolveConds([imp(B1,B2)|C],Context,P,IDs).

resolveConds([or(B1,B2)|C],C1,P1-P4,IDs):- !,
   resolveDRS(B1,C1-_,P1-P2,IDs),
   resolveDRS(B2,C1-_,P2-P3,IDs),
   resolveConds(C,C1,P3-P4,IDs).

resolveConds([_|C],Context,P,IDs):- !,
   resolveConds(C,Context,P,IDs).


/* ========================================================================
   Identify Anaphoric Material (free pointers)

   K1 = K2:X K3:Y
        K2:dog(X,Y)
        K3:male(Y)
        K1:walks(X)
======================================================================== */

anaphoric(P:drs(PDom,PCon),F:drs(FDom,FCon),Context,Presups):-
   member(F:_:_,PDom), \+ P==F,       % pick a free pointer (of DRS domain) 
   \+ (member(K:_,Context), K==F),    % should not be in context (that would mean it's resolved already)
   \+ (member(K:_,Presups), K==F),    % should not be in presuppositions (would mean it's resolved already)
   anaphoricSet(PDom,F,FDom),
   anaphoricSet(PCon,F,FCon),
   noFreeVars(FCon,P,PDom), !.


/* ========================================================================
   Check for bound variable
======================================================================== */

boundVar(X,P1,Dom):-
   member(P2:_:Y,Dom), 
   X==Y, !, \+ P1==P2.
  
boundVar(_,_,_).


/* ========================================================================
   Check if there are no free variables
======================================================================== */

noFreeVars([],_,_).

noFreeVars([F:_:rel(X,Y,_,_)|L],P,Dom):- !,
   (boundVar(X,P,Dom);boundVar(X,F,Dom)),
   (boundVar(Y,P,Dom);boundVar(Y,F,Dom)),
   noFreeVars(L,P,Dom).

noFreeVars([_|L],P,Dom):- !,
  noFreeVars(L,P,Dom).


/* ========================================================================
   Compute Anaphoric Material
======================================================================== */

anaphoricSet([],_,[]).
anaphoricSet([P:E|L1],F,[P:E|L2]):- P==F, !, anaphoricSet(L1,F,L2).
anaphoricSet([_|L1],F,L2):- anaphoricSet(L1,F,L2).


/* ========================================================================
   Projection -- try to bind, else accommodate

   project(+List of Context DRSs (Possible antecedents),
           +Anaphoric DRS,
           +List of presuppositions seen so far (could act as antecedents),
           +Pair of Ingoing and Output List of Presuppositions
           +List of DRSs (local DRS + context DRS, to check for binding violations)
           -Accumulator of solution/4,
           -List of IDs to compute proximity)
======================================================================== */

% No further context DRSs, no presupposed DRSs, but earlier binding
% solutions; so pick most probable solution
%
project([],B,[],P1-P2,Bs,Solutions,_):-                        % Tried all possibilities
   sort([solution(0.94,_,_,free)|Solutions],Sorted),           % Sort on score
   best(Sorted,Bs,B,P1-P2), !.                                 % Add global accommodation as possibility (free)

% No further context DRSs, try a presupposed DRS as antecedent
%
project([],K2:B2,[K1:drs([K0:_:X|D],C)|P],P1-P2,Bs,Solutions,IDs):-
   K1==K0,                                 % Antecedent DRS from context
   match(K1,C,X,B2,IDs,Y,Score,Ant), !,    % Match antecedent with anaphoric DRS
   project([],K2:B2,[K1:drs(D,C)|P],P1-P2,Bs,[solution(Score,K1:X,K2:Y,Ant)|Solutions],IDs).

% No further context DRSs, try accommodation in presupposition
%
project([],K2:B2,[K1:drs([],_)|P],P1-P2,Bs,Solutions,IDs):- !,
   project([],K2:B2,P,P1-P2,Bs,[solution(0.91,K1:_,K2:_,global)|Solutions],IDs).

% Try next presupposed DRS
%
project([],K,[_|P],P1-P2,Bs,Solutions,IDs):- !,
   project([],K,P,P1-P2,Bs,Solutions,IDs).

% Match antecedent with anaphoric DRS
% Look in same DRS for other antecedent
% 
project([K1:drs([K0:_:X|D],C)|Context],K2:B2,P,P1-P2,Bs,Solutions,IDs):-      
   K1==K0,
   match(K1,C,X,B2,IDs,Y,Score,Source), !,
   project([K1:drs(D,C)|Context],K2:B2,P,P1-P2,Bs,[solution(Score,K1:X,K2:Y,Source)|Solutions],IDs).

% Try next discourse referent
%
project([K1:drs([_|D],C)|Context],A,P,P1-P2,Bs,Solutions,IDs):- !,
   project([K1:drs(D,C)|Context],A,P,P1-P2,Bs,Solutions,IDs).

% Tried all discourse referents, accommodate (non-global)
% and go on with next context DRS
%
project([K1:drs([],_)|Context],K2:B2,P,P1-P2,Bs,Solutions,IDs):- !,
   length(Context,Levels), Prob is 0.05/(Levels + 1), Score is 1-Prob,
   project(Context,K2:B2,P,P1-P2,Bs,[solution(Score,K1:_,K2:_,local)|Solutions],IDs).

% Try next context DRS (all other cases)
%
project([_|Context],A,P,P1-P2,Bs,Solutions,IDs):- !,  % first argument can be an SDRS?
   project(Context,A,P,P1-P2,Bs,Solutions,IDs).


/* ========================================================================
   Best (sorted on score, the lower the better!)
======================================================================== */   

best([Solution|_],Bs,ADRS,P-[ADRS|P]):-         % DRS with free pointer
   Solution = solution(_Score,_,_,free),        % hence add to list of presuppositions
   append(Bs,[ADRS|P],Context),
   boundVarCheckContext(Context,ADRS), !.

best([Solution|_],Bs,ADRS,P-P):- 
   Solution = solution(_Score,X,Y,Reason),
   member(Reason,[local,global]),
   append(Bs,P,Context),
   \+ \+ (X=Y, boundVarCheckContext(Context,ADRS)), !, 
   X=Y.

best([Solution|_],Bs,ADRS,P1-P2):- 
   Solution = solution(_Score,X,Y,Reason),
   \+ member(Reason,[local,global,free]),
   append(Bs,P1,Context),
   \+ \+ (X=Y,                                  % if unifying X with Y doesn't
          boundVarCheckContext(Context,ADRS),   % given any free variables
          noBindingViolationDrs(Bs)), !,        % or binding violations
   X=Y,                                         % then do so
   updatePresups(P1,ADRS,P2).

best([_|L],Bs,ADRS,P):- best(L,Bs,ADRS,P).


updatePresups([],_,[]).
updatePresups([K:drs(D1,C1)|L],P:drs(D2,C2),[K:drs(D3,C3)|L]):- P==K, !, append(D1,D2,D3), append(C1,C2,C3).
updatePresups([B|L1],P,[B|L2]):- updatePresups(L1,P,L2).


/* ========================================================================
   Match antecedent with presupposition

   match(+Label of Antecedent DRS,
         +Conditions of Antecedent DRS,
         +Referent of Antecedent DRS,
         +Unlabeled Anaphoric DRS,
         +List of Token IDs
         -Referent of Anaphoric DRS,
         -Matching Score,
         -Matching Type)

======================================================================== */   

match(K1,C1,X,drs([_:_:Y|_],C2),IDs,Y,0,bow):-
   antecedent(I2,AntInd),            % there is a gold antecedent
   member( _:I2:_,C2),               % for the current anaphoric expression
   member(K2:I1:Ant,C1), K1==K2,     % and the antecedent is part of the 
   member(AntInd,I1), !,             % DRS under consideration
   drsCondition(Z,Ant),   Z==X,      % make sure it really is an antecedent condition
   antecedentConditions(X,C1,[]-Conds),
   proximity(I1,I2,IDs,Prox),
   gold('ana_ant(~q,~q,~p).',[C2,Conds,Prox]).

match(K1,C1,X,drs(_,C2),IDs,Y,NewScore,new):-
   option('--x',true),
   member( _:I2:Ana,C2),             % get anaphor condition
   drsCondition(Y,Ana),
   member(K2:I1:Ant,C1), K1==K2,     % get antecedent condition
   drsCondition(Z,Ant),   Z==X,      % make sure it really is an antecedent condition
   antecedentConditions(X,C1,[]-Conds),
   proximity(I1,I2,IDs,Prox), Prox > 0,
   score(C2,Conds,Prox,Score),
   noConflicts(Y,C2,X,C1), !,
   NewScore is 1-Score.

match(K1,C1,X,drs(_,C2),_IDs,Y,NewScore,P):-
   member( _:_:Ana,C2),
   member(K2:_:Ant,C1),          K1==K2, 
   matching(Y^Ana,Z^Ant,Score,P), Z==X,
   noConflicts(Y,C2,X,C1), !,
   NewScore is 1-Score.              % inverse score for sorting purposes


/* ========================================================================
   Calculate Proximity
======================================================================== */   

proximity([X],[Y],IDs,P):- number(X), number(Y), X<Y, !, from(IDs,X,Y,P).
proximity(_  ,_  ,_  ,0).

from([],_,_,0).
from([X|L],X,Y,D):- !, to(L,Y,0,D).
from([_|L],X,Y,D):- from(L,X,Y,D).

to([X|_],X,D1,D2):- !, D2 is D1 + 1.
to([_|L],X,D1,D2):- D is D1 + 1, to(L,X,D,D2).


/* ========================================================================
   Get Conditions of Antecedent
======================================================================== */   

antecedentConditions(X,C1,L1-L2):-
   select(_:_:C,C1,C2), 
   member(C,[pred(Z,_,_,_),named(Z,_,_,_),role(_,Z,_,1),role(Z,_,_,-1)]), Z==X, !,
   antecedentConditions(X,C2,[C|L1]-L2).

antecedentConditions(X,C1,L1-L3):-
   select(_:_:eq(Z,Y),C1,C2), Z==X, !,
   antecedentConditions(X,C2,L1-L2),
   antecedentConditions(Y,C2,L2-L3).
    
antecedentConditions(_,_,L-L).


/* ========================================================================
   Check for Conflicts
======================================================================== */   

noConflicts(X,_C1,Y,C2):-                          %%% resolving should
    \+ \+ ( X=Y,                                   %%% not result in X=X
            \+ ( member(_:_:not(_:drs(_,C0)),C2),  %%% in a negated DRS
                 member(_:_:eq(A,B),C0),
                 A==X, B==X ),
            \+ ( member(_:_:pred(A,male,_,_),C2),
                 member(_:_:pred(B,female,_,_),C2),
                 A==X, B==X ) ).


/* ========================================================================
   Matching (anaphor, antecedent)
======================================================================== */   

% time
matching(Y^pred(Y,now,a,1),Z^pred(Z,now,a,1),0.99,a:now).

% he
matching(Y^pred(Y,male,n,2),Z^named(Z,S,per,_),0.9,per:S).
matching(Y^pred(Y,male,n,2),Z^named(Z,S,_,_),0.1,per:S).
matching(Y^pred(Y,male,n,2),Z^pred(Z,male,n,2),0.99,n:male).
matching(Y^pred(Y,male,n,2),Z^pred(Z,S,n,_),0.5,n:S):-  option('--x',false).
matching(Y^pred(Y,male,n,2),Z^card(Z,_,_),0.1,card):- option('--x',false).

% she
matching(Y^pred(Y,female,n,2),Z^named(Z,S,per,_),0.9,per:S).
matching(Y^pred(Y,female,n,2),Z^named(Z,S,_,_),0.1,per:S).
matching(Y^pred(Y,female,n,2),Z^pred(Z,female,n,2),0.99,n:female).
matching(Y^pred(Y,female,n,2),Z^pred(Z,S,n,_),0.5,n:S):-  option('--x',false).
matching(Y^pred(Y,female,n,2),Z^card(Z,_,_),0.1,card):- option('--x',false).

% it
matching(Y^pred(Y,neuter,a,_),Z^named(Z,S,per,_),0.1,per:S).
matching(Y^pred(Y,neuter,a,_),Z^named(Z,S,_,_),0.8,per:S).
matching(Y^pred(Y,neuter,a,_),Z^pred(Z,neuter,a,_),0.99,a:neuter).
matching(Y^pred(Y,neuter,a,_),Z^pred(Z,S,n,_),0.5,n:S).

% they, them, theirs, this, that, those, these
matching(Y^pred(Y,thing,n,12),Z^pred(Z,S,n,_),0.5,n:S):-  option('--x',false).
matching(Y^pred(Y,thing,n,12),Z^named(Z,S,_,_),0.1,per:S):- option('--x',false).

% I, me, mine, you, yours, we, us, ours, myself, yourself, ourselves
matching(Y^pred(Y,person,n,1),Z^pred(Z,S,n,_),0.1,n:S):-    option('--x',false).
matching(Y^pred(Y,person,n,1),Z^named(Z,S,per,_),0.8,per:S):- option('--x',false).
matching(Y^pred(Y,person,n,1),Z^named(Z,S,_,_),0.5,per:S):-   option('--x',false).

% the
matching(Y^pred(Y,S,n,_),Z^pred(Z,S,n,_),0.9,n:S).

% names
matching(Y^named(Y,S,T,_),Z^named(Z,S,T,_),0.9,per:S).
matching(Y^named(Y,S,_,_),Z^named(Z,S,_,_),0.7,per:S).

% timex
matching(Y^timex(Y,date(_:D1,_:D2,_:D3,_:D4)),Z^timex(Z,date(_:D1,_:D2,_:D3,_:D4)),0.9,timex).
