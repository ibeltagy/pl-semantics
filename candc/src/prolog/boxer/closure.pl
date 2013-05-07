
:- module(closure,[closure/3]).

:- use_module(boxer(slashes)).
:- use_module(library(lists),[member/2]).
:- use_module(boxer(lexicon),[semlex/5]).
:- use_module(semlib(errors),[warning/2]).


/* -------------------------------------------------------------------------
   Closure
------------------------------------------------------------------------- */

closure(Cat,Sem,Closed):-
   member(Cat,[t:_, t]), !, 
   Closed = Sem.

closure(s:_,Sem,Closed):- !,
   Closed = app(Sem,lam(_,_:drs([],[]))).

closure(Cat,Sem,Closed):-
   member(Cat,[s:_\np, s:_/np]), !,
   Closed = app(app(Sem,lam(P,merge(B:drs([B:[]:X],[B:[]:pred(X,thing,n,12)]),app(P,X)))),lam(_,_:drs([],[]))).

closure(Cat,Sem,Closed):-
   member(Cat,[s:_/pp, s:_\pp]), !,
   Closed = app(app(Sem,lam(X,B:drs([],[B:[]:pred(X,thing,n,12)]))),lam(_,_:drs([],[]))).

closure(Cat,Sem,Closed):-
   member(Cat,[s:_/s:_, s:_\s:_]), !,
   Closed = app(app(Sem,
                    lam(P,merge(B:drs([B:[]:X],[B:[]:pred(X,event,n,1)]),app(P,X)))),
                lam(_,_:drs([],[]))).

closure(Cat,Sem,Closed):-
   member(Cat,[(s:_/s:_)/(s:_/s:_), (s:_/s:_)\(s:_/s:_),
               (s:_\s:_)/(s:_\s:_), (s:_\s:_)\(s:_\s:_)]), !,
   Closed = app(app(app(Sem,lam(S,lam(F,app(S,lam(E,app(F,E)))))),
                    lam(P,merge(B:drs([B:[]:X],[]),app(P,X)))),
                lam(_,_:drs([],[]))).

closure(Cat,Sem,Closed):-
   member(Cat,[np, np:_]), !,
   Closed = app(Sem,lam(X,B:drs([],[B:[]:pred(X,topic,a,1)]))).

closure(Cat,Sem,Closed):-
   member(Cat,[np:nb/n, np/n]), !, 
   Closed = app(app(Sem,lam(_,_:drs([],[]))),lam(X,B:drs([],[B:[]:pred(X,topic,a,1)]))).

closure(Cat,Sem,Closed):-
   member(Cat,[np\np, np/np]), !, 
   Closed = app(app(Sem,lam(P,merge(B1:drs([B1:[]:X],[B1:[]:pred(X,thing,n,12)]),app(P,X)))),
                lam(X,B2:drs([],[B2:[]:pred(X,thing,n,12)]))).

closure(Cat,Sem,Closed):-
   member(Cat,[n, pp]), !,
   Closed = merge(B:drs([B:[]:X],[B:[]:pred(X,topic,a,1)]),app(Sem,X)).

closure(Cat,Sem,Closed):-
   member(Cat,[(s:_\np)\((s:_\np)/np)]), !,
   semlex((s:dcl\np)/np,event,_,[]-_,TV),
   closure(s:dcl\np,app(Sem,TV),Closed).

closure(Cat,Sem,Closed):-
   member(Cat,[(s:_\np)\((s:_\np)/pp)]), !,
   semlex((s:dcl\np)/pp,event,_,[]-_,TV),
   closure(s:dcl\np,app(Sem,TV),Closed).

closure(Cat,Sem,Closed):-
   member(Cat,[(s:_\np)\(((s:_\np)/pp)/np)]), !,
   semlex(((s:dcl\np)/pp)/np,event,_,[]-_,DTV),
   closure(s:dcl\np,app(Sem,DTV),Closed).

closure(Cat,Sem,Closed):-
   member(Cat,[(s:dcl\np)/(s:b\np),
               (s:to\np)\(s:to\np),
               (s:b\np)\(s:b\np)]), !,
   semlex(s:b\np,event,_,[]-_,IV),
   closure(s:dcl\np,app(Sem,IV),Closed).

closure(Cat,Sem,Closed):-
   member(Cat,[(s:_\np)\(s:_\np),
               (s:_\np)/(s:_\np),
               (s:dcl/np)\(s:dcl/np),
               (s:dcl\np)\(s:dcl\np)]), !,
   semlex(s:dcl\np,event,_,[]-_,IV),
   closure(s:dcl\np,app(Sem,IV),Closed).

closure(Cat,Sem,Closed):-
   member(Cat,[s:dcl/(s:adj\np)]), !,
   semlex(s:adj\np,event,_,[]-_,IV),
   closure(s:dcl,app(Sem,IV),Closed).

closure(Cat,Sem,Closed):-
   member(Cat,[s:_/(s:_\np)]), !,
   semlex(s:dcl\np,event,_,[]-_,IV),
   closure(s:dcl,app(Sem,IV),Closed).

closure(Cat,Sem,Closed):-
   member(Cat,[s:wq/(s:q/np), s:wq/(s:adj\np)]), !,
   semlex(s:dcl\np,event,_,[]-_,IV),
   closure(s:wq,app(Sem,IV),Closed).

closure(Cat,Sem,Closed):-
   member(Cat,[s:wq/(s:pss\np), s:q/(s:pss\np)]), !,
   semlex(s:pss\np,event,_,[]-_,IV),
   closure(s:wq,app(Sem,IV),Closed).

closure(Cat,Sem,Closed):-
   member(Cat,[s:wq/(s:b\np),
               s:q/(s:b\np),
               s:q/(s:ng\np),
               s:qem/(s:dcl/np)]), !,
   semlex(s:dcl\np, event,_,[]-_,IV),
   closure(s:wq, app(Sem,IV),Closed).

closure(X,_,_):-
   warning('no closure operation for ~p',[X]), 
   fail.
