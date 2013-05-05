
:- module(typechange,[typechange/5  % +OldCat,+OldSem,+Att,+NewCat,-NewSem
                     ]).

:- use_module(boxer(slashes)).
:- use_module(boxer(categories),[category_type/5,att/3]).
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

typechange(np,Old,_, (np/np),app(New,Old)):- !,
   change(np,appo,New).

typechange(np,Old,_, (s:X/s:X),app(New,Old)):- !,
   change(np,smod,New).

typechange(np,Old,_, (s:X/s:X),app(New,Old)):- !,
   change(np,smod,New).

typechange(np,Old,_, ((s:X\np)\(s:X\np)),app(New,Old)):- !,
   change(np,vpmod,New).

typechange(np,Old,_, ((s:X\np)\(s:X\np)),app(New,Old)):- !,
   change(np,vpmod,New).

typechange((s:dcl/s:dcl),Old,_, ((s:X\np)\(s:X\np)),app(New,Old)):- !,
   change(smod,vpmod,New).

typechange((s:dcl/s:dcl),Old,_, ((s:X\np)/(s:X\np)),app(New,Old)):- !,
   change(smod,vpmod,New).

typechange((s:dcl/s:dcl),Sem,_, (s:X/s:X),Sem):- !.
typechange((s:dcl\s:dcl),Sem,_, (s:X/s:X),Sem):- !.
typechange((s:dcl/s:dcl),Sem,_, (s:X\s:X),Sem):- !.
typechange((s:dcl\s:dcl),Sem,_, (s:X\s:X),Sem):- !.

typechange(n,Old,Att,np,app(New,Old)):- 
   att(Att,pos,POS), member(POS,['NNP','NNPS']), !,
   change(n,pn,New).

typechange(n,Old,_,np,app(New,Old)):-
   change(n,np,New), !.

typechange(np,Old,_, (np/(np\np)),app(New,Old)):-
   change(np,npnpmod,New), !.

typechange(IV,Old,_,Mod,app(New,Old)):- 
   category_type(IV,_,npV,_,_),
   member(Mod,[n/n, n\n]), !, 
   change(vp,adj,New).

typechange(s:dcl\np_exp,Old,_,Mod,app(New,Old)):- 
   member(Mod,[np/np, np\np]), !,
   change(vp,npmod,New).

typechange(s:dcl\np_thr,Old,_,Mod,app(New,Old)):- 
   member(Mod,[np/np, np\np]), !,
   change(vp,npmod,New).

typechange(IV,Old,_,Mod,app(New,Old)):- 
   category_type(IV,_,npV,_,_),
   member(Mod,[np/np, np\np]), !,
   change(vp,npmod,New).

typechange(IV,Old,_,Mod,app(New,Old)):- 
   category_type(IV,_,npV,_,_),
   member(Mod,[(s:X\np)\(s:X\np), (s:X\np)/(s:X\np)]), !,
   change(vp,vpmod,New).

typechange(IV,Old,_,Mod,app(New,Old)):- 
   category_type(IV,_,npV,_,_),
   member(Mod,[(s:X\s:X),(s:X/s:X)]), !,
   change(vp,smod,New).

typechange(Cat,TV,_,Mod,app(New,app(TV,NP))):- 
   category_type(Cat,_,npVnp,_,_),
   member(Mod,[np\np, np/np]), !,
   NP = lam(P,merge(B:drs([B:[]:X],[B:[]:pred(X,thing,n,12)]),app(P,X))),
   change(vp,npmod,New).

typechange((s:dcl),Old,_, Mod,app(New,Old)):- 
   member(Mod,[n/n, n\n]), !, 
   change(s,adj,New).

typechange((s:dcl),Old,_, Mod,app(New,Old)):- 
   member(Mod,[np/np ,np\np]), !,
   change(s,npmod,New).

typechange((s:dcl),Old,_, Mod,app(New,Old)):- 
   member(Mod,[(s:X\s:X),(s:X/s:X),(s:dcl/s:dcl)]), !,
   change(s,smod,New).

typechange((s:ng\np),Old,_, np,app(New,Old)):- !,
   change(vp,np,New).

typechange(Cat1,_,_,Cat2,_):-
   option('--warnings',true), !,
   warning('no type changing rule for ~p --> ~p',[Cat1,Cat2]),
   fail.
    

/* -------------------------------------------------------------------------
   Semantics
------------------------------------------------------------------------- */

change(s,      adj, lam(Old,lam(P,lam(X,merge(app(P,X),app(Old,lam(E,B:drs([],[B:[]:rel(E,X,rel,0)])))))))).
change(s,    npmod, lam(Old,lam(Q,lam(P,app(Q,lam(X,merge(app(P,X),app(Old,lam(E,B:drs([],[B:[]:rel(E,X,rel,0)])))))))))).
change(s,     smod, lam(Old,lam(S,lam(E,merge(app(Old,lam(_,_:drs([],[]))),app(S,E)))))).

change(vp,     adj, lam(Old,lam(P,lam(X,app(app(Old,lam(Q,merge(app(Q,X),app(P,X)))),lam(_,_:drs([],[]))))))).
change(vp,   npmod, lam(Old,lam(Q,lam(P,app(Q,lam(X,merge(app(app(Old,lam(R,app(R,X))),lam(_,_:drs([],[]))),app(P,X)))))))).
change(vp,   vpmod, lam(Old,lam(VP,lam(Q,lam(E,app(Q,lam(X,merge(app(app(VP,lam(P,app(P,X))),E),app(app(Old,lam(P,app(P,X))),lam(_,_:drs([],[]))))))))))).
change(vp,    smod, lam(Old,lam(S,lam(E,merge(app(app(Old,lam(P,merge(B:drs([B:[]:X],[]),app(P,X)))),lam(_,_:drs([],[]))),app(S,E)))))).
change(vp,      np, lam(Old,lam(F,app(app(Old,lam(P,merge(B:drs([B:[]:X],[B:[]:pred(X,thing,n,12)]),app(P,X)))),lam(E,app(F,E)))))).

change(n,       np, lam(Old,lam(P,merge(merge(B:drs([B:[]:X],[]),app(Old,X)),app(P,X))))).

change(n,       pn, lam(Old,lam(P,alfa(nam,merge(B:drs([B:[]:X],[]),app(Old,X)),app(P,X))))).

change(np,    smod, lam(Old,lam(S,lam(E,app(S,lam(X,merge(app(Old,lam(Y,B:drs([],[B:[]:rel(X,Y,rel,0)]))),app(E,X)))))))).
change(np,   vpmod, lam(Old,lam(V,lam(N,lam(E,app(app(V,N),lam(X,merge(app(Old,lam(Y,B:drs([],[B:[]:rel(X,Y,rel,0)]))),app(E,X))))))))).

change(np, npnpmod, lam(Old,lam(M,app(M,Old)))).

change(np,    appo, lam(Old,lam(M,lam(P,app(Old,lam(X,app(M,lam(Y,merge(B:drs([],[B:[]:eq(X,Y)]),app(P,X)))))))))).

change(smod, vpmod, lam(Old,lam(V,lam(N,app(Old,app(V,N)))))).





