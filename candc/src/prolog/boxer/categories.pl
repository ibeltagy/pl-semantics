
:- module(categories,
          [category/3,       %  +Type, +Cat, -Mood
           category/4,       %  +Type, +Cat, -Roles, -Mood
           category/5,       %  +Type, +Cat, -Roles, +PoS, -Mood
           category_type/5   %  +Cat, +Sym, -ArgStruc, -Roles, -Mood
          ]).

:- use_module(boxer(slashes)).
:- use_module(semlib(options)).


/* -------------------------------------------------------------------------
   Thematic Roles (basic -- default thematic roles)
------------------------------------------------------------------------- */

roles(npV,       _, Roles):- !, Roles = [agent].
roles(npVprep,   _, Roles):- !, Roles = [agent].

roles(npVnp,     _, Roles):- !, Roles = [agent,patient].
roles(npVpp,     _, Roles):- !, Roles = [agent,rel].
roles(npVs,      _, Roles):- !, Roles = [agent,theme].
roles(npVpps,    _, Roles):- !, Roles = [agent,rel,theme].
roles(sVnp,      _, Roles):- !, Roles = [theme,agent].
roles(vpVnp,     _, Roles):- !, Roles = [theme,agent].
roles(ppVnp,     _, Roles):- !, Roles = [rel,agent].
roles(npVnpnp,   _, Roles):- !, Roles = [agent,recipient,theme].
roles(npVnppp,   _, Roles):- !, Roles = [agent,patient,rel].
roles(npVppnp,   _, Roles):- !, Roles = [agent,rel,patient].
roles(npVpppp,   _, Roles):- !, Roles = [agent,rel,rel].
roles(ppVnpnp,   _, Roles):- !, Roles = [rel,recipient,theme].
roles(ppVnppp,   _, Roles):- !, Roles = [rel,patient,rel].
roles(snpVnp,    _, Roles):- !, Roles = [theme,agent,recipient].
roles(npVnps,    _, Roles):- !, Roles = [agent,recipient,theme].
roles(npVnpq,    _, Roles):- !, Roles = [agent,recipient,theme].
roles(npVvpnp,   _, Roles):- !, Roles = [agent,patient].
roles(npVvppp,   _, Roles):- !, Roles = [agent].
roles(npVppvp,   _, Roles):- !, Roles = [agent].

roles(npVnppppp, _, Roles):- !, Roles = [agent,patient,rel,rel].
roles(npVppnppp, _, Roles):- !, Roles = [agent,rel,patient,rel].


/* -------------------------------------------------------------------------
   Category Types: np V (intransitive verbs)
------------------------------------------------------------------------- */

category_type(s:adj\np,_Verb,npV,[],adj):- !.
category_type(s:pss\np,Verb,npV,Roles,pss):- !, roles(npVnp,Verb,[_|Roles]).
category_type(s:M\np,Verb,Cat,Roles,M):- !, Cat = npV, roles(Cat,Verb,Roles).
category_type(s:M/np,Verb,Cat,Roles,M):- !, Cat = npV, roles(Cat,Verb,Roles).

/* -------------------------------------------------------------------------
   Category Types: np V prep (phrasal verbs)
------------------------------------------------------------------------- */

category_type((s:pss\np)/(pp/np),_,npVprep,[],pss):- !.

/* -------------------------------------------------------------------------
   Category Types: np V np (transitive verbs)
------------------------------------------------------------------------- */

category_type((s:adj\np)/np,_,npVnp,[],adj):- !.
category_type((s:adj\np)\np,_,npVnp,[],adj):- !.
%category_type((s:adj\np)\np,_,npVnp,[theme,attribute],adj):- !.
category_type((s:pss\np)/np,Verb,npVnp,Roles,pss):- !, roles(npVnpnp,Verb,[_|Roles]).
category_type((s:q/np)/np,Verb,Cat,[R1,R2|Roles],q):- !, Cat = npVnp, roles(Cat,Verb,[R2,R1|Roles]).
category_type((s:M\np)/np,Verb,Cat,Roles,M):- !, Cat = npVnp, roles(Cat,Verb,Roles).
category_type((s:M/np)/np,Verb,Cat,Roles,M):- !, Cat = npVnp, roles(Cat,Verb,Roles).
category_type((s:asup\np)/np,_,npVnp,[],asup):- !.
category_type((s:asup\np)\np,_,npVnp,[],asup):- !.

/* -------------------------------------------------------------------------
   Category Types: np V pp
------------------------------------------------------------------------- */

category_type((s:adj\np)/pp,_,npVpp,[theme],adj):- !.
category_type((s:pss\np)/pp,Verb,npVpp,[Role],pss):- !, roles(npVnppp,Verb,[_,Role|_]).
category_type((s:M\np)/pp,Verb,Cat,[Role],M):- !, Cat = npVpp, roles(Cat,Verb,[Role|_]).
category_type((s:M/np)/pp,Verb,Cat,[Role],M):- !, Cat = npVpp, roles(Cat,Verb,[Role|_]).


/* -------------------------------------------------------------------------
   Category Types: np V s
------------------------------------------------------------------------- */

category_type((s:M\np)/s:_,Verb,Cat,Roles,M):- !, Cat = npVs, roles(Cat,Verb,Roles).
category_type(((s:M\np)/s:_)/pp,Verb,Cat,[R1,R2],M):- !, Cat = npVpps, roles(Cat,Verb,[R1,_,R2]).

/* -------------------------------------------------------------------------
   Category Types: s V np
------------------------------------------------------------------------- */

category_type((s:M\s:_)\np,Verb,Cat,Roles,M):- !, Cat = sVnp, roles(Cat,Verb,Roles).
category_type((s:M\s:_)/np,Verb,Cat,Roles,M):- !, Cat = sVnp, roles(Cat,Verb,Roles).
category_type((s:M/s:_)\np,Verb,Cat,Roles,M):- !, Cat = sVnp, roles(Cat,Verb,Roles).
category_type((s:M/s:_)/np,Verb,Cat,Roles,M):- !, Cat = sVnp, roles(Cat,Verb,Roles).

/* -------------------------------------------------------------------------
   Category Types: vp V np
------------------------------------------------------------------------- */

category_type((s:dcl\(s:_\np))/np,Verb,Cat,Roles,dcl):- !, Cat = vpVnp, roles(Cat,Verb,Roles).
category_type((s:dcl\(s:_\np))\np,Verb,Cat,Roles,dcl):- !, Cat = vpVnp, roles(Cat,Verb,Roles).
category_type(  (s:q/(s:_\np))\np,Verb,Cat,Roles,  q):- !, Cat = vpVnp, roles(Cat,Verb,Roles).

/* -------------------------------------------------------------------------
   Category Types: pp V np
------------------------------------------------------------------------- */

category_type((s:pss\pp)/np,Verb,ppVnp,[Role],pss):- !, roles(npVppnp,Verb,[_,_,Role]).
category_type((s:M\pp)/np,Verb,Cat,[Role],M):- !, Cat = ppVnp, roles(Cat,Verb,[_,Role]).
category_type((s:M/pp)/np,Verb,Cat,[Role],M):- !, Cat = ppVnp, roles(Cat,Verb,[_,Role]).

/* -------------------------------------------------------------------------
   Category Types: np V np np (ditransitive verbs)
------------------------------------------------------------------------- */

category_type(((s:q/np)/np)/np,Verb,Cat,[R2,R1|Roles],q):- !, Cat = npVnpnp, roles(Cat,Verb,[R1,R2|Roles]).
category_type(((s:M\np)/np)/np,Verb,Cat,Roles,M):- !, Cat = npVnpnp, roles(Cat,Verb,Roles).
category_type(((s:M/np)/np)/np,Verb,Cat,Roles,M):- !, Cat = npVnpnp, roles(Cat,Verb,Roles).

/* -------------------------------------------------------------------------
   Category Types: np V np pp
------------------------------------------------------------------------- */

category_type(((s:adj\np)/pp)/np,_,npVnppp,[],adj):- !.
category_type(((s:q/np)/pp)/np,Verb,Cat,[Role2,Role1],q):- !, Cat = npVnppp, roles(Cat,Verb,[Role1,Role2|_]).
category_type(((s:M\np)/pp)/np,Verb,Cat,[Role1,Role2],M):- !, Cat = npVnppp, roles(Cat,Verb,[Role1,Role2|_]).
category_type(((s:M/np)/pp)/np,Verb,Cat,[Role1,Role2],M):- !, Cat = npVnppp, roles(Cat,Verb,[Role1,Role2|_]).

/* -------------------------------------------------------------------------
   Category Types: np V pp np
------------------------------------------------------------------------- */

category_type(((s:adj\np)/np)/pp,_,npVppnp,[],adj):- !.
category_type(((s:adj\np)\np)/pp,_,npVppnp,[],adj):- !.
category_type(((s:q/np)/np)/pp,Verb,Cat,[Role2,Role1],q):- !, Cat = npVppnp, roles(Cat,Verb,[Role1,_,Role2]).
category_type(((s:M\np)/np)/pp,Verb,Cat,[Role1,Role2],M):- !, Cat = npVppnp, roles(Cat,Verb,[Role1,_,Role2]).
category_type(((s:M/np)/np)/pp,Verb,Cat,[Role1,Role2],M):- !, Cat = npVppnp, roles(Cat,Verb,[Role1,_,Role2]).

/* -------------------------------------------------------------------------
   Category Types: np V pp pp
------------------------------------------------------------------------- */

category_type(((s:adj\np)/pp)/pp,_,npVpppp,[],adj):- !.
category_type(((s:M\np)/pp)/pp,Verb,Cat,[Role],M):- !, Cat = npVpppp, roles(Cat,Verb,[Role|_]).
category_type(((s:M/np)/pp)/pp,Verb,Cat,[Role],M):- !, Cat = npVpppp, roles(Cat,Verb,[Role|_]).

/* -------------------------------------------------------------------------
   Category Types: pp V np np
------------------------------------------------------------------------- */

category_type(((s:M\pp)/np)/np,Verb,Cat,[Role1,Role2],M):- !, Cat = ppVnpnp, roles(Cat,Verb,[_,Role1,Role2]).
category_type(((s:M/pp)/np)/np,Verb,Cat,[Role1,Role2],M):- !, Cat = ppVnpnp, roles(Cat,Verb,[_,Role1,Role2]).

/* -------------------------------------------------------------------------
   Category Types: pp V np pp
------------------------------------------------------------------------- */

category_type(((s:M\pp)/pp)/np,Verb,Cat,[Role],M):- !, Cat = ppVnppp, roles(Cat,Verb,[_,Role,_]).
category_type(((s:M/pp)/pp)/np,Verb,Cat,[Role],M):- !, Cat = ppVnppp, roles(Cat,Verb,[_,Role,_]).

/* -------------------------------------------------------------------------
   Category Types: s np V np
------------------------------------------------------------------------- */

category_type(((s:M\s:_)\np)/np,Verb,Cat,Roles,M):- !, Cat = snpVnp, roles(Cat,Verb,Roles).

/* -------------------------------------------------------------------------
   Category Types: np V np q/s
------------------------------------------------------------------------- */

category_type(((s:M\np)/s:qem)/np,Verb,Cat,Roles,M):- !, Cat = npVnpq, roles(Cat,Verb,Roles).
category_type(((s:M\np)/s:_NQ)/np,Verb,Cat,Roles,M):- !, Cat = npVnps, roles(Cat,Verb,Roles).

/* -------------------------------------------------------------------------
   Category Types: np V vp np
------------------------------------------------------------------------- */

category_type(((s:M\np)/np)/(s:_\np),Verb,Cat,Roles,M):- !, Cat = npVvpnp, roles(Cat,Verb,Roles).

/* -------------------------------------------------------------------------
   Category Types: np V vp pp
------------------------------------------------------------------------- */

category_type(((s:M\np)/pp)/(s:adj\np),Verb,Cat,Roles,M):- !, Cat = npVvppp, roles(Cat,Verb,Roles).

/* -------------------------------------------------------------------------
   Category Types: np V pp vp
------------------------------------------------------------------------- */

category_type(((s:M\np)/(s:adj\np))/pp,Verb,Cat,Roles,M):- !, Cat = npVppvp, roles(Cat,Verb,Roles).

/* -------------------------------------------------------------------------
   Category Types: np V np pp pp
------------------------------------------------------------------------- */

category_type((((s:M\np)/pp)/pp)/np,Verb,Cat,[Role1,Role2],M):- !, Cat = npVnppppp, roles(Cat,Verb,[Role1,Role2|_]).
category_type((((s:M\np)/pp)/np)/pp,Verb,Cat,[Role1,Role2],M):- !, Cat = npVppnppp, roles(Cat,Verb,[Role1,Role2|_]).
category_type((((s:M/np)/pp)/pp)/np,Verb,Cat,[Role1,Role2],M):- !, Cat = npVnppppp, roles(Cat,Verb,[Role1,Role2|_]).


/* -------------------------------------------------------------------------
   Sentence
------------------------------------------------------------------------- */

category(s, s:X,    X).   


/* -------------------------------------------------------------------------
    Adjectives
------------------------------------------------------------------------- */

category(adj,    n:X/n:X,     X).      
category(adj,    n:X\n:X,     X).       


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

/* -------------------------------------------------------------------------
   Complementisers
------------------------------------------------------------------------- */

category(comp, s:poss/s:dcl, _).
category(comp, s:qem/s:dcl, _).
category(comp, s:dcl/s:inv, _).
category(comp, s:bem/s:b, _).
category(comp, s:em/s:dcl, _).
category(comp, s:em/s:b, _).
               

/* -------------------------------------------------------------------------
   Punctuation
------------------------------------------------------------------------- */

category(punctuation, s:wq\s:wq,   '?').
category(punctuation, s:wq\s:wq,   '.').
category(punctuation, s:wq\s:wq,   '!').
category(punctuation, s:dcl\s:dcl, '?').
category(punctuation, s:dcl\s:dcl, '.').
category(punctuation, s:dcl\s:dcl, '!').
category(punctuation, s:X\s:X,     '?').
category(punctuation, s:X\s:X,     '.').
category(punctuation, s:X\s:X,     '!').


/* -------------------------------------------------------------------------
   PP Control Verbs
------------------------------------------------------------------------- */

category(pscv, ((s:dcl\np)/(s:to\np))/pp, [agent,theme],   dcl).
category(pscv, ((s:ng\np)/(s:to\np))/pp,  [agent,theme],    ng).
category(pscv, ((s:pss\np)/(s:to\np))/pp, [patient,theme], pss).
category(pscv, ((s:b\np)/(s:to\np))/pp,   [agent,theme],     b).
category(pscv, ((s:pt\np)/(s:to\np))/pp,  [agent,theme],    pt).


/* -------------------------------------------------------------------------
   Subject or Object Control Verbs
------------------------------------------------------------------------- */

category(socv, ((s:dcl\np)/(s:to\np))/np,    [agent,patient], dcl).
category(socv, ((s:ng\np)/(s:to\np))/np,     [agent,patient],  ng). 
category(socv, ((s:b\np)/(s:to\np))/np,      [agent,patient],   b). 
category(socv, ((s:pt\np)/(s:to\np))/np,     [agent,patient],  pt). 
category(socv, ((s:dcl\np)/(s:b\np))/np,     [agent,patient], dcl). 
category(socv, ((s:ng\np)/(s:b\np))/np,      [agent,patient],  ng). 
category(socv, ((s:b\np)/(s:b\np))/np,       [agent,patient],   b). 
category(socv, ((s:dcl\np)/(s:ng\np))/np,    [agent,patient], dcl). 
category(socv, ((s:b\np)/(s:ng\np))/np,      [agent,patient],   b). 
category(socv, ((s:ng\np)/(s:ng\np))/np,     [agent,patient],  ng). 
category(socv, ((s:dcl\np)/(s:adj\np))/np,   [agent,patient], dcl). 
category(socv, ((s:b\np)/(s:adj\np))/np,     [agent,patient],   b).  
category(socv, ((s:ng\np)/(s:adj\np))/np,    [agent,patient],  ng). 
category(socv, ((s:pt\np)/(s:adj\np))/np,    [agent,patient],  pt). 
category(socv, ((s:dcl\np)/(s:pss\np))/np,   [agent,patient], dcl). 
category(socv, ((s:b\np)/(s:pss\np))/np,     [agent,patient],   b). 
category(socv, ((s:dcl\np)/(s:pt\np))/np,    [agent,patient], dcl). 
category(socv, ((s:b\np)/(s:pt\np))/np,      [agent,patient],   b). 
category(socv, ((s:ng\np)/(s:pss\np))/np,    [agent,patient],  ng). 
category(socv, ((s:ng\np)/(s:pt\np))/np,     [agent,patient],  ng). 
category(socv, ((s:pt\np)/(s:b\np))/np,      [agent,patient],  pt). 
category(socv, ((s:pt\np)/(s:ng\np))/np,     [agent,patient],  pt).


/* -------------------------------------------------------------------------
   Control Verbs
------------------------------------------------------------------------- */

category(cv, (s:adj\np)/(s:to\np),        [agent,theme], _,        adj).
category(cv, (s:adj\np)/(s:ng\np),        [agent,theme], _,        adj).

category(cv, (s:dcl\np)/(s:to\np),        [agent,theme], _,        dcl).
category(cv, (s:pt\np)/(s:to\np),         [agent,theme], _,         pt).
category(cv, (s:pss\np)/(s:to\np),        [patient,theme], _,      pss).
category(cv, (s:ng\np)/(s:to\np),         [agent,theme], _,         ng).
category(cv, (s:b\np)/(s:to\np),          [agent,theme], _,          b).

category(cv, (s:ng\np)/(s:ng\np),         [agent,theme], 'VBG',     ng).
%category(cv, (s:pt\np)/(s:ng\np),         [agent,theme], _,        pt).
category(cv, (s:b\np)/(s:ng\np),          [agent,theme], _,          b).
category(cv, (s:pss\np)/(s:ng\np),        [patient,theme], _,      pss).

category(cv, (s:ng\np)/(s:dcl\np),        [agent,theme], _,         ng).
category(cv, (s:pss\np)/(s:dcl\np),       [patient,theme], _,      pss).

category(cv, (s:pss\np)/(s:b\np),         [patient,theme], _,      pss).
category(cv, (s:ng\np)/(s:b\np),          [agent,theme], _,         ng).
category(cv, (s:pt\np)/(s:b\np),          [agent,theme], _,         pt).
category(cv, (s:b\np)/(s:b\np),           [agent,theme], 'VB',       b).

category(cv, (s:ng\np)\(s:adj\np),        [agent,theme], _,         ng).

category(cv, (s:pss\np)/(s:pt\np),        [patient,theme], _,      pss).
category(cv, (s:pss\np)/(s:pss\np),       [patient,theme], 'VBN',  pss).
category(cv, (s:pss\np)/(s:pss\np),       [patient,theme], 'VBD',  pss).

