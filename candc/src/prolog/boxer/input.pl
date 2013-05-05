
:- module(input,[openInput/0,
                 preferred/2,     % +ID, -CCG
                 identifyIDs/1]).

:- use_module(boxer(slashes)).
:- use_module(boxer(transform),[topcat/2]).
:- use_module(semlib(options),[option/2]).
:- use_module(semlib(errors),[error/2,warning/2]).


/*========================================================================
    Declare Dynamic Predicates
========================================================================*/

:- multifile ccg/2, id/2, sem/5, coref/2.
:- discontiguous ccg/2, id/2, sem/5, coref/2.
:- dynamic inputtype/1.


/*------------------------------------------------------------------------
   Open Input File
------------------------------------------------------------------------*/

openInput:-
   option('--input',user_input), 
   option('--stdin',do), !,
   prompt(_,''),
   catch(load_files([],[autoload(true),encoding(utf8),stream(user_input)]),_,fail),
   checkInputType.

openInput:-
   option('--stdin',dont), 
   option('--input',File), 
   \+ File = user_input, !,
   checkInput(File).


/*========================================================================
   Check Input File
========================================================================*/

checkInput(File):-
   \+ atom(File), !,
   error('file name format of ~p not allowed',[File]),
   fail.

checkInput(File):-
   \+ exists_file(File), !,
   error('file ~p does not exist',[File]),
   fail.

checkInput(File):-
   \+ access_file(File,read), !,
   error('file ~p not readable',[File]),
   fail.

checkInput(File):-
   catch(load_files([File],[autoload(true),encoding(utf8)]),_,fail), !,
   checkInputType.

checkInput(File):-
   error('file ~p not Prolog readable',[File]),
   !, fail.


/*========================================================================
   Check Input Type
========================================================================*/

checkInputType:-
   input:ccg(_,_), !,
   retractall(input:inputtype(_)),
   assert(input:inputtype(ccg)).

checkInputType:-
   input:sem(_,_,_,_,_), !,
   retractall(input:inputtype(_)),
   assert(input:inputtype(drs)).

checkInputType:-
   error('unknown type of input file',[]).
  

/*------------------------------------------------------------------------
   Identify IDs in the input file
------------------------------------------------------------------------*/

identifyIDs(List):-
   findall(id(Id,Numbers),id(Id,Numbers),List),
   \+ List=[], !.

identifyIDs(List):-
   ccg(_,_),
   option('--window','2'), 
   findall(X,ccg(X,_),Sems),
   slidingWindow(Sems,List), !.

identifyIDs(List):-
   ccg(_,_),
   setof(id(Id,[Id]),X^ccg(Id,X),List), !.

identifyIDs([]):-
   \+ id(_,_), \+ ccg(_,_), !,
   error('input file contains no ccg/2 terms',[]), 
   fail.

identifyIDs([]):-
   error('processing input file',[]), 
   fail.


/*------------------------------------------------------------------------
   Generate IDs using a sliding window
------------------------------------------------------------------------*/

slidingWindow([A],[id(A,[A])]):- !.

slidingWindow([A,B],[id(A,[A,B])]):- !.

slidingWindow([A,B|L1],[id(A,[A,B])|L2]):- !,
   slidingWindow([B|L1],L2).


/*------------------------------------------------------------------------
   Preferred CCG analysis
------------------------------------------------------------------------*/
   
preferred(N,CCG):-
   preferred([t:_,s:_,np],N,CCG).

preferred([],N,CCG):-
   ccg(N,CCG), !.

preferred([Cat|_],N,CCG):-
   ccg(N,CCG), 
   topcat(CCG,Cat), !.

preferred([_|L],N,CCG):-
   preferred(L,N,CCG).
