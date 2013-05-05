
:- module(typechange,[typechange/5  % +OldCat,+OldSem,+NewCat,-NewSem,+Index
                     ]).

:- use_module(boxer(slashes)).
:- use_module(boxer(categories),[category_type/5]).
:- use_module(semlib(errors),[warning/2]).
:- use_module(semlib(options),[option/2]).
:- use_module(library(lists),[member/2]).


/* -------------------------------------------------------------------------

   This file contains the rules for type changing. These are called
   "lexical rules" in CCGbank. They are unary type changing rules that
   change the type of the category, or derived from binary type changing
   rules represented as unary type chaning rule.  As they are not regular
   like type shifting rules, it is impossible to give a general semantic
   pattern for them. Instead each pair of categories (consisting of the
   old and the new type of the category, will get its own semantic
   interpretation.

------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------
   Type Change 
------------------------------------------------------------------------- */

typechange(np,Old, (np/np),New, Index):- !,
   change(np,appo,Old,Index,New).

typechange(np,Old, (s:X/s:X),New, Index):- !,
   change(np,smod,Old,Index,New).

typechange(np,Old, (s:X/s:X),New, Index):- !,
   change(np,smod,Old,Index,New).

typechange(np,Old, ((s:X\np)\(s:X\np)),New, Index):- !,
   change(np,vpmod,Old,Index,New).

typechange(np,Old, ((s:X\np)\(s:X\np)),New, Index):- !,
   change(np,vpmod,Old,Index,New).

typechange((s:dcl/s:dcl),Old, ((s:X\np)\(s:X\np)),New,Index):- !,
   change(smod,vpmod,Old,Index,New).

typechange((s:dcl/s:dcl),Old, ((s:X\np)/(s:X\np)),New,Index):- !,
   change(smod,vpmod,Old,Index,New).

typechange((s:dcl/s:dcl),Sem, (s:X/s:X),Sem,_):- !.
typechange((s:dcl\s:dcl),Sem, (s:X/s:X),Sem,_):- !.
typechange((s:dcl/s:dcl),Sem, (s:X\s:X),Sem,_):- !.
typechange((s:dcl\s:dcl),Sem, (s:X\s:X),Sem,_):- !.

typechange((n:F),Old,np,New,Index):- 
   member(F,[per,nam,loc,org]), !,
   change(n,pn,Old,Index,New).

typechange((n:_),Old,np,New,Index):-
   change(n,np,Old,Index,New), !.

typechange(np,Old, (np/(np\np)),New, Index):-
   change(np,npnpmod,Old,Index,New), !.

typechange(IV,Old,Mod,New,Index):- 
   category_type(IV,_,npV,_,_),
   member(Mod,[n:X/n:X, n:X\n:X]), !, 
   change(iv,adj,Old,Index,New).

typechange(s:dcl\np_exp,Old,Mod,New,Index):- 
   member(Mod,[np/np, np\np]), !,
   change(iv,npmod,Old,Index,New).

typechange(s:dcl\np_thr,Old,Mod,New,Index):- 
   member(Mod,[np/np, np\np]), !,
   change(iv,npmod,Old,Index,New).

typechange(IV,Old,Mod,New,Index):- 
   category_type(IV,_,npV,_,_),
   member(Mod,[np/np, np\np]), !,
   change(iv,npmod,Old,Index,New).

typechange(IV,Old,Mod,New,Index):- 
   category_type(IV,_,npV,_,_),
   member(Mod,[(s:X\np)\(s:X\np), (s:X\np)/(s:X\np)]), !,
   change(iv,vpmod,Old,Index,New).

typechange(IV,Old,Mod,New,Index):- 
   category_type(IV,_,npV,_,_),
   member(Mod,[(s:X\s:X),(s:X/s:X)]), !,
   change(iv,smod,Old,Index,New).

typechange(TV,Old,Mod,New,Index):- 
   category_type(TV,_,npVnp,_,_),
   member(Mod,[np\np, np/np]), !,
   change(tv,npmod,Old,Index,New).

typechange((s:dcl),Old, Mod,New, Index):- 
   member(Mod,[n:X/n:X, n:X\n:X]), !, 
   change(s,adj,Old,Index,New).

typechange((s:dcl),Old, Mod,New, Index):- 
   member(Mod,[np/np ,np\np]), !,
   change(s,npmod,Old,Index,New).

typechange((s:dcl),Old, Mod,New, Index):- 
   member(Mod,[(s:X\s:X),(s:X/s:X),(s:dcl/s:dcl)]), !,
   change(s,smod,Old,Index,New).

typechange((s:ng\np),Old, np,New, Index):- !,
   change(iv,np,Old,Index,New).

typechange(Cat1,_,Cat2,_,_):-
   option('--warnings',true), !,
   warning('no type changing rule for ~p --> ~p',[Cat1,Cat2]),
   fail.
    

/* -------------------------------------------------------------------------
   Semantics
------------------------------------------------------------------------- */

change(s,      adj, Old, _, lam(P,lam(X,merge(app(P,X),app(Old,lam(E,drs([],[[]:rel(E,X,rel,0)]))))))).
change(s,    npmod, Old, _, lam(Q,lam(P,app(Q,lam(X,merge(app(P,X),app(Old,lam(E,drs([],[[]:rel(E,X,rel,0)]))))))))).
change(s,     smod, Old, _, lam(S,lam(E,merge(app(Old,lam(_,drs([],[]))),app(S,E))))).

change(iv,     adj, Old, _, lam(P,lam(X,app(app(Old,lam(Q,merge(app(Q,X),app(P,X)))),lam(_,drs([],[])))))).
change(iv,   npmod, Old, _, lam(Q,lam(P,app(Q,lam(X,merge(app(app(Old,lam(R,app(R,X))),lam(_,drs([],[]))),app(P,X))))))).
change(iv,   vpmod, Old, _, lam(VP,lam(Q,lam(E,app(Q,lam(X,merge(app(app(VP,lam(P,app(P,X))),E),app(app(Old,lam(P,app(P,X))),lam(_,drs([],[])))))))))).
change(iv,    smod, Old, I, lam(S,lam(E,merge(app(app(Old,lam(P,merge(drs([I:X],[]),app(P,X)))),lam(_,drs([],[]))),app(S,E))))).
change(iv,      np, Old, _, lam(F,app(app(Old,lam(P,merge(drs([[]:X],[[]:pred(X,thing,n,12)]),app(P,X)))),lam(E,app(F,E))))).

change(n,       np, Old, I, lam(P,merge(merge(drs([I:X],[]),app(Old,X)),app(P,X)))).
change(n,       pn, Old, I, lam(P,alfa(nam,merge(drs([I:X],[]),app(Old,X)),app(P,X)))).

change(np,    smod, Old, _, lam(S,lam(E,app(S,lam(X,merge(app(Old,lam(Y,drs([],[[]:rel(X,Y,rel,0)]))),app(E,X))))))).
change(np,   vpmod, Old, _, lam(V,lam(N,lam(E,app(app(V,N),lam(X,merge(app(Old,lam(Y,drs([],[[]:rel(X,Y,rel,0)]))),app(E,X)))))))).

change(np, npnpmod, Old, _, lam(M,app(M,Old))).

change(np,    appo, Old, _, lam(M,lam(P,app(Old,lam(X,app(M,lam(Y,merge(drs([],[[]:eq(X,Y)]),app(P,X))))))))).

change(smod, vpmod, Old, _, lam(V,lam(N,app(Old,app(V,N))))).

change(tv,     Cat, Old, I, New):- change(iv,Cat,app(Old,lam(P,merge(drs([[]:X],[[]:pred(X,thing,n,12)]),app(P,X)))),I,New).



