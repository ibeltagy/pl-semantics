
:- module(categories,
          [category/3,       %  +Type, +Cat, -Mood
           category/4,       %  +Type, +Cat, -Roles, -Mood
           category_type/5,  %  +Cat, +Sym, -ArgStruc, -Roles, -Mood
           roles/4,
           sense/4,
           att/3,
           rel/3
          ]).

:- use_module(boxer(slashes)).
:- use_module(semlib(options)).
:- use_module(semlib(errors),[warning/2]).
:- use_module(lex(verbnet),[verbnet/3]).


/* -------------------------------------------------------------------------
   Accessing Attributes
------------------------------------------------------------------------- */

att([],sense,0):- !.
att([],namex,'O'):- !.
att([],verbnet,[]):- !.
att([],_,unknown):- !.
att([F:V|_],F,V):- !.
att([_|L],F,V):- att(L,F,V).

/* -------------------------------------------------------------------------
   Word Senses
------------------------------------------------------------------------- */

sense(_Sym,_Cat,Sense,A-A):- att(A,sense,Sense), \+ Sense = 0, !.
sense(_Sym,_Cat,Sense,A-[sense:Sense|A]):- Sense = 1.


/* -------------------------------------------------------------------------
   Relations
------------------------------------------------------------------------- */

rel(_,Att-Att,Rel):- att(Att,relation,Rel), \+ Rel=unknown, !.
rel(Rel,Att-[relation:Rel|Att],Rel).


/* -------------------------------------------------------------------------
   Thematic Roles introduced by PPs
------------------------------------------------------------------------- */

roles(_,((s:X\np)\(s:X\np))/np,Roles,A-A):- option('--roles',verbnet), att(A,verbnet,Roles), \+ Roles=[], !.
roles(by,((s:X\np)\(s:X\np))/np,Roles,A-[verbnet:Roles|A]):- option('--roles',verbnet), !, Roles = ['Agent'].
roles(_,((s:X\np)\(s:X\np))/np,Roles,A-[verbnet:Roles|A]):- option('--roles',verbnet), !, Roles = ['Instrument'].
roles(_,((s:X\np)\(s:X\np))/np,Roles,A-A):- option('--roles',proto), !, Roles = [agent].


/* -------------------------------------------------------------------------
   Thematic Roles: passive
------------------------------------------------------------------------- */

roles(Verb,(s:pss\np)/np,[Role1,Role2],A):- roles(Verb,((s:dcl\np)/np)/np,[Role1,Role2,_],A), !.
roles(Verb,(s:pss\np)/s:M,[Role1,Role2],A):- roles(Verb,((s:dcl\np)/s:M)/np,[Role1,Role2,_],A), !.
roles(Verb,(s:pss\np)/pp,[Role],A):- roles(Verb,((s:dcl\np)/pp)/np,[Role,_],A), !.
roles(Verb,((s:pss\np)/np)/pp,[Role1,Role2],A):- roles(Verb,(((s:dcl\np)/np)/pp)/np,[Role1,Role2,_],A), !.
roles(Verb,s:pss\np,[Role],A):- roles(Verb,(s:dcl\np)/np,[Role,_],A), !.


/* -------------------------------------------------------------------------
   Thematic Roles: standard case 
------------------------------------------------------------------------- */

roles(_,_,Roles,A-A):- option('--roles',verbnet), att(A,verbnet,Roles), \+ Roles=[], !.
roles(Verb,Cat,Roles,A-[verbnet:Roles|A]):- option('--roles',verbnet), verbnet(Verb,Cat,Roles), !.
roles(Verb,Cat,Roles,A-A):- option('--roles',proto), proto(Verb,Cat,Roles), !.


/* -------------------------------------------------------------------------
   Thematic Roles: fall-back rules
------------------------------------------------------------------------- */

roles(Verb,(s:M\np)\np,Roles,A):- !, roles(Verb,(s:M\np)/np,Roles,A).
roles(Verb,s:inv/np,Roles,A):- !, roles(Verb,s:dcl\np,Roles,A).
roles(Verb,(s:q/np)/np,Roles,A):- !, roles(Verb,(s:dcl\np)/np,Roles,A).
roles(Verb,(s:M\pp)/np,Roles,A):- !, roles(Verb,s:M\np,Roles,A).
roles(Verb,(s:M/np)/np,Roles,A):- !, roles(Verb,(s:M\np)/np,Roles,A).
roles(Verb,s:M/np,Roles,A):- !, roles(Verb,s:M\np,Roles,A).
roles(Verb,C/pp,Roles,A):- !, roles(Verb,C,Roles,A).
roles(Verb,(C/pp)/np,Roles,A):- !, roles(Verb,C/np,Roles,A).
roles(Verb,(C/pp)/s:X,Roles,A):- !, roles(Verb,C/s:X,Roles,A).
roles(Verb,(s:M\np)/(s:X\np),Roles,A):- !, roles(Verb,(s:M\np)/s:X,Roles,A).


/* -------------------------------------------------------------------------
   Thematic Roles: no roles could be assigned
------------------------------------------------------------------------- */

roles(Verb,Cat,Roles,A-A):- 
   warning('role assignment failure for ~p with category ~p',[Verb,Cat]),
   Roles = [].


/* -------------------------------------------------------------------------
   Proto (roles are listed in the order of arguments, not surface order!)
------------------------------------------------------------------------- */

proto(_, s:adj\np,           [topic]):- !.
proto(_, (s:adj\np)\np,      [theme,topic]):- !.
proto(_, s:_\np,             [agent]):- !.
proto(_, (s:_\np)/np,        [patient,agent]):- !.
proto(_, (s:_\np)/s:_,       [theme,agent]):- !.
proto(_, ((s:_\np)/np)/np,   [theme,recipient,agent]):- !.
proto(_, ((s:_\np)/s:_)/np,  [recipient,theme,agent]):- !.



%%%%%%%%%%%%%%%% OLD STUFF %%%%%%%%%%%%%%%%%%

roles2(npV,       _, Roles):- !, Roles = [agent].
roles2(npVprep,   _, Roles):- !, Roles = [agent].

roles2(npVnp,     _, Roles):- !, Roles = [agent,patient].
roles2(npVpp,     _, Roles):- !, Roles = [agent,rel].
roles2(npVs,      _, Roles):- !, Roles = [agent,theme].
roles2(npVpps,    _, Roles):- !, Roles = [agent,rel,theme].
roles2(sVnp,      _, Roles):- !, Roles = [theme,agent].
roles2(vpVnp,     _, Roles):- !, Roles = [theme,agent].
roles2(ppVnp,     _, Roles):- !, Roles = [rel,agent].
roles2(npVnpnp,   _, Roles):- !, Roles = [agent,recipient,theme].
roles2(npVnppp,   _, Roles):- !, Roles = [agent,patient,rel].
roles2(npVppnp,   _, Roles):- !, Roles = [agent,rel,patient].
roles2(npVpppp,   _, Roles):- !, Roles = [agent,rel,rel].
roles2(ppVnpnp,   _, Roles):- !, Roles = [rel,recipient,theme].
roles2(ppVnppp,   _, Roles):- !, Roles = [rel,patient,rel].
roles2(snpVnp,    _, Roles):- !, Roles = [theme,agent,recipient].
roles2(npVnps,    _, Roles):- !, Roles = [agent,recipient,theme].
roles2(npVnpq,    _, Roles):- !, Roles = [agent,recipient,theme].
roles2(npVvpnp,   _, Roles):- !, Roles = [agent,patient].
roles2(npVvppp,   _, Roles):- !, Roles = [agent].
roles2(npVppvp,   _, Roles):- !, Roles = [agent].

roles2(npVnppppp, _, Roles):- !, Roles = [agent,patient,rel,rel].
roles2(npVppnppp, _, Roles):- !, Roles = [agent,rel,patient,rel].


/* -------------------------------------------------------------------------
   Category Types: np V (intransitive verbs)
------------------------------------------------------------------------- */

category_type(s:adj\np,_Verb,npV,[],adj):- !.
category_type(s:pss\np,Verb,npV,Roles,pss):- !, roles2(npVnp,Verb,[_|Roles]).
category_type(s:M\np,Verb,Cat,Roles,M):- !, Cat = npV, roles2(Cat,Verb,Roles).
category_type(s:M/np,Verb,Cat,Roles,M):- !, Cat = npV, roles2(Cat,Verb,Roles).

/* -------------------------------------------------------------------------
   Category Types: np V prep (phrasal verbs)
------------------------------------------------------------------------- */

category_type((s:pss\np)/(pp/np),_,npVprep,[],pss):- !.


/* -------------------------------------------------------------------------
   Category Types: np V np (transitive verbs)
------------------------------------------------------------------------- */

category_type((s:adj\np)/np,_,npVnp,[],adj):- !.
category_type((s:adj\np)\np,_,npVnp,[],adj):- !.
category_type((s:asup\np)/np,_,npVnp,[],asup):- !.
category_type((s:asup\np)\np,_,npVnp,[],asup):- !.
category_type((s:adj\np)\np,_,npVnp,[theme,attribute],adj):- !.
category_type((s:pss\np)/np,Verb,npVnp,Roles,pss):- !, roles2(npVnpnp,Verb,[_|Roles]).
category_type((s:q/np)/np,Verb,Cat,[R1,R2|Roles],q):- !, Cat = npVnp, roles2(Cat,Verb,[R2,R1|Roles]).
category_type((s:M\np)/np,Verb,Cat,Roles,M):- !, Cat = npVnp, roles2(Cat,Verb,Roles).
category_type((s:M/np)/np,Verb,Cat,Roles,M):- !, Cat = npVnp, roles2(Cat,Verb,Roles).


/* -------------------------------------------------------------------------
   Category Types: np V pp
------------------------------------------------------------------------- */

category_type((s:adj\np)/pp,_,npVpp,[theme],adj):- !.
category_type((s:pss\np)/pp,Verb,npVpp,[Role],pss):- !, roles2(npVnppp,Verb,[_,Role|_]).
category_type((s:M\np)/pp,Verb,Cat,[Role],M):- !, Cat = npVpp, roles2(Cat,Verb,[Role|_]).
category_type((s:M/np)/pp,Verb,Cat,[Role],M):- !, Cat = npVpp, roles2(Cat,Verb,[Role|_]).


/* -------------------------------------------------------------------------
   Category Types: np V s
------------------------------------------------------------------------- */

category_type((s:M\np)/s:_,Verb,Cat,Roles,M):- !, Cat = npVs, roles2(Cat,Verb,Roles).
category_type(((s:M\np)/s:_)/pp,Verb,Cat,[R1,R2],M):- !, Cat = npVpps, roles2(Cat,Verb,[R1,_,R2]).

/* -------------------------------------------------------------------------
   Category Types: s V np
------------------------------------------------------------------------- */

category_type((s:M\s:_)\np,Verb,Cat,Roles,M):- !, Cat = sVnp, roles2(Cat,Verb,Roles).
category_type((s:M\s:_)/np,Verb,Cat,Roles,M):- !, Cat = sVnp, roles2(Cat,Verb,Roles).
category_type((s:M/s:_)\np,Verb,Cat,Roles,M):- !, Cat = sVnp, roles2(Cat,Verb,Roles).
category_type((s:M/s:_)/np,Verb,Cat,Roles,M):- !, Cat = sVnp, roles2(Cat,Verb,Roles).

/* -------------------------------------------------------------------------
   Category Types: vp V np
------------------------------------------------------------------------- */

category_type((s:dcl\(s:_\np))/np,Verb,Cat,Roles,dcl):- !, Cat = vpVnp, roles2(Cat,Verb,Roles).
category_type((s:dcl\(s:_\np))\np,Verb,Cat,Roles,dcl):- !, Cat = vpVnp, roles2(Cat,Verb,Roles).
category_type(  (s:q/(s:_\np))\np,Verb,Cat,Roles,  q):- !, Cat = vpVnp, roles2(Cat,Verb,Roles).

/* -------------------------------------------------------------------------
   Category Types: pp V np
------------------------------------------------------------------------- */

category_type((s:pss\pp)/np,Verb,ppVnp,[Role],pss):- !, roles2(npVppnp,Verb,[_,_,Role]).
category_type((s:M\pp)/np,Verb,Cat,[Role],M):- !, Cat = ppVnp, roles2(Cat,Verb,[_,Role]).
category_type((s:M/pp)/np,Verb,Cat,[Role],M):- !, Cat = ppVnp, roles2(Cat,Verb,[_,Role]).

/* -------------------------------------------------------------------------
   Category Types: np V np np (ditransitive verbs)
------------------------------------------------------------------------- */

category_type(((s:q/np)/np)/np,Verb,Cat,[R2,R1|Roles],q):- !, Cat = npVnpnp, roles2(Cat,Verb,[R1,R2|Roles]).
category_type(((s:M\np)/np)/np,Verb,Cat,Roles,M):- !, Cat = npVnpnp, roles2(Cat,Verb,Roles).
category_type(((s:M/np)/np)/np,Verb,Cat,Roles,M):- !, Cat = npVnpnp, roles2(Cat,Verb,Roles).

/* -------------------------------------------------------------------------
   Category Types: np V np pp
------------------------------------------------------------------------- */

category_type(((s:adj\np)/pp)/np,_,npVnppp,[],adj):- !.
category_type(((s:q/np)/pp)/np,Verb,Cat,[Role2,Role1],q):- !, Cat = npVnppp, roles2(Cat,Verb,[Role1,Role2|_]).
category_type(((s:M\np)/pp)/np,Verb,Cat,[Role1,Role2],M):- !, Cat = npVnppp, roles2(Cat,Verb,[Role1,Role2|_]).
category_type(((s:M/np)/pp)/np,Verb,Cat,[Role1,Role2],M):- !, Cat = npVnppp, roles2(Cat,Verb,[Role1,Role2|_]).

/* -------------------------------------------------------------------------
   Category Types: np V pp np
------------------------------------------------------------------------- */

category_type(((s:adj\np)/np)/pp,_,npVppnp,[],adj):- !.
category_type(((s:adj\np)\np)/pp,_,npVppnp,[],adj):- !.
category_type(((s:q/np)/np)/pp,Verb,Cat,[Role2,Role1],q):- !, Cat = npVppnp, roles2(Cat,Verb,[Role1,_,Role2]).
category_type(((s:M\np)/np)/pp,Verb,Cat,[Role1,Role2],M):- !, Cat = npVppnp, roles2(Cat,Verb,[Role1,_,Role2]).
category_type(((s:M/np)/np)/pp,Verb,Cat,[Role1,Role2],M):- !, Cat = npVppnp, roles2(Cat,Verb,[Role1,_,Role2]).

/* -------------------------------------------------------------------------
   Category Types: np V pp pp
------------------------------------------------------------------------- */

category_type(((s:adj\np)/pp)/pp,_,npVpppp,[],adj):- !.
category_type(((s:M\np)/pp)/pp,Verb,Cat,[Role],M):- !, Cat = npVpppp, roles2(Cat,Verb,[Role|_]).
category_type(((s:M/np)/pp)/pp,Verb,Cat,[Role],M):- !, Cat = npVpppp, roles2(Cat,Verb,[Role|_]).

/* -------------------------------------------------------------------------
   Category Types: pp V np np
------------------------------------------------------------------------- */

category_type(((s:M\pp)/np)/np,Verb,Cat,[Role1,Role2],M):- !, Cat = ppVnpnp, roles2(Cat,Verb,[_,Role1,Role2]).
category_type(((s:M/pp)/np)/np,Verb,Cat,[Role1,Role2],M):- !, Cat = ppVnpnp, roles2(Cat,Verb,[_,Role1,Role2]).

/* -------------------------------------------------------------------------
   Category Types: pp V np pp
------------------------------------------------------------------------- */

category_type(((s:M\pp)/pp)/np,Verb,Cat,[Role],M):- !, Cat = ppVnppp, roles2(Cat,Verb,[_,Role,_]).
category_type(((s:M/pp)/pp)/np,Verb,Cat,[Role],M):- !, Cat = ppVnppp, roles2(Cat,Verb,[_,Role,_]).

/* -------------------------------------------------------------------------
   Category Types: s np V np
------------------------------------------------------------------------- */

category_type(((s:M\s:_)\np)/np,Verb,Cat,Roles,M):- !, Cat = snpVnp, roles2(Cat,Verb,Roles).

/* -------------------------------------------------------------------------
   Category Types: np V np q/s
------------------------------------------------------------------------- */

category_type(((s:M\np)/s:qem)/np,Verb,Cat,Roles,M):- !, Cat = npVnpq, roles2(Cat,Verb,Roles).
category_type(((s:M\np)/s:_NQ)/np,Verb,Cat,Roles,M):- !, Cat = npVnps, roles2(Cat,Verb,Roles).

/* -------------------------------------------------------------------------
   Category Types: np V vp np
------------------------------------------------------------------------- */

category_type(((s:M\np)/np)/(s:_\np),Verb,Cat,Roles,M):- !, Cat = npVvpnp, roles2(Cat,Verb,Roles).

/* -------------------------------------------------------------------------
   Category Types: np V vp pp
------------------------------------------------------------------------- */

category_type(((s:M\np)/pp)/(s:adj\np),Verb,Cat,Roles,M):- !, Cat = npVvppp, roles2(Cat,Verb,Roles).

/* -------------------------------------------------------------------------
   Category Types: np V pp vp
------------------------------------------------------------------------- */

category_type(((s:M\np)/(s:adj\np))/pp,Verb,Cat,Roles,M):- !, Cat = npVppvp, roles2(Cat,Verb,Roles).

/* -------------------------------------------------------------------------
   Category Types: np V np pp pp
------------------------------------------------------------------------- */

category_type((((s:M\np)/pp)/pp)/np,Verb,Cat,[Role1,Role2],M):- !, Cat = npVnppppp, roles2(Cat,Verb,[Role1,Role2|_]).
category_type((((s:M\np)/pp)/np)/pp,Verb,Cat,[Role1,Role2],M):- !, Cat = npVppnppp, roles2(Cat,Verb,[Role1,Role2|_]).
category_type((((s:M/np)/pp)/pp)/np,Verb,Cat,[Role1,Role2],M):- !, Cat = npVnppppp, roles2(Cat,Verb,[Role1,Role2|_]).


/* -------------------------------------------------------------------------
   Sentence
------------------------------------------------------------------------- */

category(s, s:X,    X).   


/* -------------------------------------------------------------------------
    Adjectives
------------------------------------------------------------------------- */

category(adj,    n/n,     _).      
category(adj,    n\n,     _).       


/* -------------------------------------------------------------------------
   VP adverbials
------------------------------------------------------------------------- */

category(vpadv, (s:X\np)\(s:X\np),     _).
category(vpadv, (s:X\np)/(s:X\np),     _).


/* -------------------------------------------------------------------------
   S modifiers
------------------------------------------------------------------------- */

category(smod, s:X/s:X,    _).
category(smod, s:X\s:X,    _).
category(smod, s:_/s:dcl,  _).
category(smod, s:dcl/s:inv, _).


/* -------------------------------------------------------------------------
   Complementisers
------------------------------------------------------------------------- */

category(comp, s:poss/s:dcl, _).

category(comp, s:qem/s:dcl, _).
category(comp, s:bem/s:b, _).
category(comp, s:em/s:dcl, _).
category(comp, s:em/s:b, _).
               

/* -------------------------------------------------------------------------
   Subject or Object Control Verbs
------------------------------------------------------------------------- */

%category(socv, ((s:dcl\np)/(s:to\np))/np,    [agent,patient], dcl).
%category(socv, ((s:ng\np)/(s:to\np))/np,     [agent,patient],  ng). 
%category(socv, ((s:b\np)/(s:to\np))/np,      [agent,patient],   b). 
%category(socv, ((s:pt\np)/(s:to\np))/np,     [agent,patient],  pt). 

category(socv, ((s:dcl\np)/(s:b\np))/np,     [agent,patient], dcl). 
category(socv, ((s:ng\np)/(s:b\np))/np,      [agent,patient],  ng). 
category(socv, ((s:b\np)/(s:b\np))/np,       [agent,patient],   b). 
category(socv, ((s:pt\np)/(s:b\np))/np,      [agent,patient],  pt). 

category(socv, ((s:dcl\np)/(s:ng\np))/np,    [agent,patient], dcl). 
category(socv, ((s:b\np)/(s:ng\np))/np,      [agent,patient],   b). 
category(socv, ((s:ng\np)/(s:ng\np))/np,     [agent,patient],  ng). 
category(socv, ((s:pt\np)/(s:ng\np))/np,     [agent,patient],  pt).

category(socv, ((s:dcl\np)/(s:adj\np))/np,   [agent,patient], dcl). 
category(socv, ((s:b\np)/(s:adj\np))/np,     [agent,patient],   b).  
category(socv, ((s:ng\np)/(s:adj\np))/np,    [agent,patient],  ng). 
category(socv, ((s:pt\np)/(s:adj\np))/np,    [agent,patient],  pt). 

category(socv, ((s:dcl\np)/(s:pss\np))/np,   [agent,patient], dcl). 
category(socv, ((s:b\np)/(s:pss\np))/np,     [agent,patient],   b). 
category(socv, ((s:ng\np)/(s:pss\np))/np,    [agent,patient],  ng). 

category(socv, ((s:dcl\np)/(s:pt\np))/np,    [agent,patient], dcl). 
category(socv, ((s:b\np)/(s:pt\np))/np,      [agent,patient],   b). 
category(socv, ((s:ng\np)/(s:pt\np))/np,     [agent,patient],  ng). 




/* -------------------------------------------------------------------------
   Control Verbs
------------------------------------------------------------------------- */

%category(cv, (s:adj\np)/(s:to\np),        [agent,theme], _,        adj).
%category(cv, (s:adj\np)/(s:ng\np),        [agent,theme], _,        adj).

%category(cv, (s:dcl\np)/(s:to\np),        [agent,theme], _,        dcl).
%category(cv, (s:pt\np)/(s:to\np),         [agent,theme], _,         pt).
%category(cv, (s:pss\np)/(s:to\np),        [patient,theme], _,      pss).
%category(cv, (s:ng\np)/(s:to\np),         [agent,theme], _,         ng).
%category(cv, (s:b\np)/(s:to\np),          [agent,theme], _,          b).

%category(cv, (s:pt\np)/(s:ng\np),         [agent,theme], _,        pt).

%category(cv, (s:b\np)/(s:ng\np),          [agent,theme], _,          b).
%category(cv, (s:pss\np)/(s:ng\np),        [patient,theme], _,      pss).

%category(cv, (s:ng\np)/(s:dcl\np),        [agent,theme], _,         ng).
%category(cv, (s:pss\np)/(s:dcl\np),       [patient,theme], _,      pss).

%category(cv, (s:pss\np)/(s:b\np),         [patient,theme], _,      pss).
%category(cv, (s:ng\np)/(s:b\np),          [agent,theme], _,         ng).
%category(cv, (s:pt\np)/(s:b\np),          [agent,theme], _,         pt).

%category(cv, (s:ng\np)/(s:ng\np),         [agent,theme], 'VBG',     ng).
%category(cv, (s:b\np)/(s:b\np),           [agent,theme], 'VB',       b).  % e.g. will help draw

%category(cv, (s:ng\np)/(s:adj\np),        [agent,theme], _,         ng).
%category(cv, (s:pt\np)/(s:adj\np),        [agent,theme], _,         pt).

%category(cv, (s:pss\np)/(s:pt\np),        [patient,theme], _,      pss).
%category(cv, (s:pss\np)/(s:adj\np),        [patient,theme], _,      pss).
%category(cv, (s:pss\np)/(s:pss\np),       [patient,theme], 'VBN',  pss).
%category(cv, (s:pss\np)/(s:pss\np),       [patient,theme], 'VBD',  pss).

