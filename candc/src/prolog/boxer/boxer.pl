
% boxer.pl, by Johan Bos

/*========================================================================
   File Search Paths
========================================================================*/

file_search_path(semlib,     'src/prolog/lib').
file_search_path(boxer,      'src/prolog/boxer').
file_search_path(knowledge,  'src/prolog/boxer/knowledge').
file_search_path(lex,        'src/prolog/boxer/lex').


/*========================================================================
   Load other libraries
========================================================================*/

:- use_module(library(lists),[member/2,select/3]).

:- use_module(boxer(ccg2drs),[ccg2drs/3]).
:- use_module(boxer(input),[openInput/0,identifyIDs/1,preferred/2]).
:- use_module(boxer(evaluation),[initEval/0,reportEval/0]).
:- use_module(boxer(version),[version/1]).
:- use_module(boxer(printCCG),[printCCG/2]).
:- use_module(boxer(transform),[preprocess/6]).
:- use_module(boxer(drs2fdrs),[eqDrs/2]).
:- use_module(boxer(output),[printHeader/4,printFooter/1,printSem/4]).

:- use_module(semlib(errors),[error/2,warning/2]).
:- use_module(semlib(options),[option/2,parseOptions/2,setOption/3,
                               showOptions/1,setDefaultOptions/1]).


/*========================================================================
   Load Knowledge

loadRelations:- 
   option('--nn',true), !, 
   use_module(knowledge(relations),[nn/3]).

loadRelations.

loadKnowledge:- 
   loadRelations.
========================================================================*/


/*========================================================================
   Main
========================================================================*/

box(_,_):-
   option(Option,do), 
   member(Option,['--version','--help']), !, 
   version,
   help.

box(Command,Options):-
%  loadKnowledge,
   openInput,
   openOutput(Stream),
   version(Version),
   printHeader(Stream,Version,Command,Options),
   initEval,
   box(Stream), !,
   printFooter(Stream),
   close(Stream), !,
   reportEval.
   
box(_,_):-
   setOption(boxer,'--help',do), !,
   help.


/*------------------------------------------------------------------------
   Perform depending on input type
------------------------------------------------------------------------*/

box(Stream):-
   input:inputtype(ccg), !,
   identifyIDs(List),
   buildList(List,1,Stream).

box(_):-
   input:inputtype(unknown).


/*------------------------------------------------------------------------
   Open Output File
------------------------------------------------------------------------*/

openOutput(Stream):-
   option('--output',Output),
   atomic(Output), 
   \+ Output=user_output, 
   ( access_file(Output,write), !,
     open(Output,write,Stream,[encoding(utf8)])
   ; error('cannot write to specified file ~p',[Output]),
     Stream=user_output ), !.

openOutput(user_output).


/*------------------------------------------------------------------------
   Context Parameters
------------------------------------------------------------------------*/

contextParameters([],_,[]):- !.

contextParameters(L1,Old,L3):- 
   select(poss(Pos),L1,L2), !,
   contextParameters(L2,[poss(Pos)|Old],L3).

contextParameters(['DOCID':DOCID|L1],Pos,[year:Year,month:Month,day:Day|L2]):- 
   atom_chars(DOCID,Chars),
   ( Chars = [_,_,_,'_','E','N','G','_',Y1,Y2,Y3,Y4,M1,M2,D1,D2,'.'|_]
   ; Chars = ['d','i','r','_',Y1,Y2,Y3,Y4,M1,M2,D1,D2|_]
   ; Chars = ['A','P','W',Y1,Y2,Y3,Y4,M1,M2,D1,D2|_]
   ; Chars = ['N','Y','T',Y1,Y2,Y3,Y4,M1,M2,D1,D2|_]
   ; Chars = ['X','I','E',Y1,Y2,Y3,Y4,M1,M2,D1,D2|_] ), !,
   atom_chars(Year,[Y1,Y2,Y3,Y4]), 
   atom_chars(Month,[M1,M2]), 
   atom_chars(Day,[D1,D2]), !,
   contextParameters(L1,Pos,L2).

contextParameters([role(A,B1,C1)|L1],Pos,[role(A,B2,C2)|L2]):- !,
   correct(Pos,B1,B2),
   correct(Pos,C1,C2),
   contextParameters(L1,Pos,L2).

contextParameters([target(A,B1,C1)|L1],Pos,[target(A,B2,C2)|L2]):- !,
   correct(Pos,B1,B2),
   correct(Pos,C1,C2),
   contextParameters(L1,Pos,L2).

contextParameters([_|L1],Pos,L2):- !,
   contextParameters(L1,Pos,L2).

contextParameters(_,_,[]).


correct([],N,N).

correct([poss(X)|L],N1,N3):-
   (X < N1, !, N2 is N1 + 1; N2 = N1),
   correct(L,N2,N3).



/*------------------------------------------------------------------------
   Print CCG derivations
------------------------------------------------------------------------*/

printCCGs([],_).

printCCGs([N|L],Stream):-  
   preferred(N,CCG0),
   preprocess(N,CCG0,CCG1,_,1,_), !,
   printCCG(CCG1,Stream), 
   printCCGs(L,Stream).

printCCGs([N|L],Stream):-  
   preferred(N,_), !,
   warning('cannot produce derivation for ~p',[N]),
   printCCGs(L,Stream).

printCCGs([N|L],Stream):-  
   warning('no syntactic analysis for ~p',[N]),
   printCCGs(L,Stream).


/*------------------------------------------------------------------------
   Build a DRS from a list of identifiers 
------------------------------------------------------------------------*/

buildList([id(_,Numbers)|L],Index,Stream):- 
   option('--ccg',true), !,
   sort(Numbers,Sorted),
   printCCGs(Sorted,Stream),
   buildList(L,Index,Stream).

buildList([id(Id,Numbers)|L],Index,Stream):- 
   sort(Numbers,Sorted),
   contextParameters(Id,[],Context),
   ccg2drs(Sorted,XDRS,Context),
   outputSem(Stream,Id,Index,XDRS), !,
   NewIndex is Index + 1,
   buildList(L,NewIndex,Stream).

buildList([_|L],Index,Stream):- !,
   buildList(L,Index,Stream).

buildList([],_,_).


/* =======================================================================
   Output Semantic Representation
========================================================================*/

outputSem(Stream,Id,Index,XDRS0):-
%   eqDrs(XDRS0,XDRS1),
   XDRS0=XDRS1,
   printSem(Stream,Id,Index,XDRS1), !.
%   nl(Stream).


/* =======================================================================
   Version
========================================================================*/

version:-
   option('--version',do), !,
   version(V),
   format(user_error,'~p~n',[V]).

version.


/* =======================================================================
   Help
========================================================================*/

help:-
   option('--help',do), !,
   format(user_error,'usage: boxer [options]~n~n',[]),
   showOptions(boxer).

help:-
   option('--help',dont), !.


/* =======================================================================
   Definition of start
========================================================================*/

start:-
   current_prolog_flag(argv,[Comm|Args]),
%  set_prolog_flag(float_format,'%.20g'),
   setDefaultOptions(boxer), 
   parseOptions(boxer,Args),
   box(Comm,Args), !,
   halt.

start:- 
   error('boxer failed',[]), 
   halt.


