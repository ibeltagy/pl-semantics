
% nutcracker.pl, by Johan Bos

/*========================================================================
   File Search Paths
========================================================================*/

file_search_path(semlib,     'src/prolog/lib').
file_search_path(nutcracker, 'src/prolog/nutcracker').
file_search_path(knowledge,  'src/prolog/boxer/knowledge').


/*========================================================================
   Load other libraries
========================================================================*/

:- use_module(library(lists),[member/2,append/3]).
:- use_module(library(ordsets),[list_to_ord_set/2,ord_intersection/3]).
:- use_module(library(readutil),[read_line_to_codes/2]).

:- use_module(semlib(drs2fol),[drs2fol/2]).
:- use_module(semlib(errors),[error/2,warning/2,inform/2]).
:- use_module(semlib(options),[option/2,parseOptions/2,setOption/3,
                               showOptions/1,setDefaultOptions/1]).

:- use_module(nutcracker(version),[version/1]).
:- use_module(nutcracker(input),[openInput/1,inputDRS/2,lenDRS/2,openModel/3]).
:- use_module(nutcracker(callInference),[callMBbis/7,callTPandMB/8]).
:- use_module(nutcracker(miniFrameNet),[axiomsFN/2]).
:- use_module(nutcracker(counting),[countingAxioms/2]).
:- use_module(nutcracker(miniWordNet),[compConcepts/1,compISA/0,
                                       clearMWN/0,cutDownMWN/0,
                                       addTopMWN/0,graphMWN/2,sizeMWN/1,
                                       outputMWN/2,axiomsWN/1]).


/*========================================================================
   Main
========================================================================*/

main:-
   option(Option,do), 
   member(Option,['--version','--help']), !, 
   version,
   help.

main:-
   checkDir(Dirs), !,
   main(Dirs).

main:-
   setOption(nutcracker,'--help',do), !,
   help.


/*========================================================================
   Main (traverse directories)
========================================================================*/

main([]).

main([X|Dirs]):-
   checkFiles(X), !,
   pipeline(X),
   main(Dirs).

main([X|Dirs]):-
   atom_concat(X,'/*',Wild),
   subdirs(Wild,SubDirs), \+ SubDirs = [], !,
   main(SubDirs),
   main(Dirs).

main([_|Dirs]):-
   main(Dirs).


/*------------------------------------------------------------------------
   Pipeline
------------------------------------------------------------------------*/

pipeline(Dir):-
   atomic_list_concat([Dir,'/','prediction.txt'],Prediction),
   atomic_list_concat([Dir,'/','t'],T),
   atomic_list_concat([Dir,'/','h'],H),
   time_check(T,Prediction),
   time_check(H,Prediction),
   open(Prediction,read,Stream),
   read_line_to_codes(Stream,Codes),
   close(Stream),
   atom_codes(Atom,Codes),
   \+ Atom = 'error', 
   \+ Atom = 'informative (wordnet novelty)', 
   \+ Atom = 'entailed (wordnet novelty)', !,
   inform('nothing to do for ~p (~p)',[Dir,Atom]).

pipeline(X):-
   option('--inference',yes),
   tokenise(X,Overlap), 
   meta(X), parse(X), wsd(X), box(X), 
   mwn(X,A1,A2,A3,Novelty),
   nc(X,A1,A2,A3), !,
   prediction(X,Novelty,Overlap).

pipeline(X):-
   option('--inference',no),
   tokenise(X,Overlap), 
   meta(X), parse(X), wsd(X), box(X), 
   mwn(X,_,_,_,Novelty), !,
   prediction(X,Novelty,Overlap).

pipeline(X):-
   option('--inference',only),
   box(X),
   tokenise(X,Overlap), 
   mwn(X,A1,A2,A3,Novelty), 
   nc(X,A1,A2,A3), !,
   prediction(X,Novelty,Overlap).

pipeline(X):-
   tokenise(X,Overlap), !,
   prediction(X,-1,Overlap).

pipeline(X):-
   outputPrediction(X,'error',-99,-99,-99,-99,-99,-99).


/*------------------------------------------------------------------------
   Check Dir
------------------------------------------------------------------------*/

checkDir(Dirs):-
   checkDir1(Dir),        % remove slash at end (if there is one)
   checkDir2(Dir,Dirs).   % check permissions

checkDir1(NewDir):-
   option('--dir',Dir),
   atom_chars(Dir,Chars),
   append(NewChars,['/'],Chars), !,
   atom_chars(NewDir,NewChars),
   setOption(nutcracker,'--dir',NewDir).

checkDir1(Dir):-
   option('--dir',Dir).

checkDir2(Dir,[Dir]):-
   exists_directory(Dir), 
   access_file(Dir,write), !.

checkDir2(Dir,List):- 
   subdirs(Dir,List), !.

checkDir2(Dir,[]):-
   error('cannot access directory ~p',[Dir]).


/*------------------------------------------------------------------------
   Sub Dirs
------------------------------------------------------------------------*/

subdirs(Wild,Dirs):-
   expand_file_name(Wild,List),
   findall(D,( member(D,List),
               exists_directory(D),
               access_file(D,write) ),Dirs), !.


/*------------------------------------------------------------------------
   Check presence of files t and h
------------------------------------------------------------------------*/

checkFiles(Dir):-
   atomic_list_concat([Dir,'/','t'],TFile),
   atomic_list_concat([Dir,'/','h'],HFile),   
   access_file(TFile,read),
   access_file(HFile,read), !,
   printTH(Dir,TFile,HFile).

checkFiles(Dir):-
   warning('directory ~p does not contain files named t and h',[Dir]), 
   !, fail.   


/*------------------------------------------------------------------------
   Print t and h file   
------------------------------------------------------------------------*/

printTH(_,_,_):-
   option('--info',false), !.

printTH(Dir,TFile,HFile):-
   option('--info',true), 
   inform('[=====> ~p <=====]',[Dir]),
   inform('Text:',[]),
   atomic_list_concat(['cat',TFile],' ',Shell1),
   shell(Shell1,Return1), Return1 = 0,
   inform('Hypothesis:',[]),
   atomic_list_concat(['cat',HFile],' ',Shell2),
   shell(Shell2,Return2), Return2 = 0, !.

printTH(_,_,_):-
   error('failed to access t and h file',[]).


/*------------------------------------------------------------------------
   Tokenise (init)
------------------------------------------------------------------------*/

tokenise(Dir,Overlap):-
   atomic_list_concat([Dir,'/','t'],TFile),
   atomic_list_concat([Dir,'/','h'],HFile),   
   atomic_list_concat([Dir,'/','t.tok'],TFileTOK),
   atomic_list_concat([Dir,'/','h.tok'],HFileTOK),
   tokeniseFile(TFile,TFileTOK),
   tokeniseFile(HFile,HFileTOK),
   bagofwords(TFileTOK,TWords),
   bagofwords(HFileTOK,HWords),
   overlap(TWords,HWords,Overlap).


/*------------------------------------------------------------------------
   Tokenise
------------------------------------------------------------------------*/

tokeniseFile(In,Out):-
   time_check(In,Out), !.

tokeniseFile(In,Out):-
   atomic_list_concat(['bin/tokkie',
                '--quotes',delete,
                '--input',In,
                '--output',Out],' ',Shell),
   write(Shell), nl,
   shell(Shell,Return), 
   Return = 0, !.

tokeniseFile(In,_):-
   error('problem tokenising ~p',[In]), 
   !, fail.


/*------------------------------------------------------------------------
   Bag of words overlap
------------------------------------------------------------------------*/

bagofwords(File,Bag):-
   open(File,read,Stream),
   read_line_to_codes(Stream,Codes),
   close(Stream),
   atom_codes(Atom,Codes),
   atomic_list_concat(Bag,' ',Atom).

overlap(T,H,Overlap):-
   list_to_ord_set(T,TO),
   list_to_ord_set(H,HO),
   ord_intersection(TO,HO,Intersection),
   length(Intersection,CardTandH),
   length(HO,CardH), CardH > 0,
   Overlap is CardTandH/CardH.


/*------------------------------------------------------------------------
   File preparation (adding META markup)
------------------------------------------------------------------------*/

meta(Dir):-
   atomic_list_concat([Dir,'/','t.tok'],TFile),
   atomic_list_concat([Dir,'/','h.tok'],HFile),
   access_file(TFile,read),
   access_file(HFile,read), !,
   atomic_list_concat([Dir,'/', 'i.met'], IFileMET),
   atomic_list_concat([Dir,'/', 't.met'], TFileMET),
   atomic_list_concat([Dir,'/', 'h.met'], HFileMET),
   atomic_list_concat([Dir,'/','th.met'],THFileMET),
   meta(IFileMET,'"<META>rte"'),
   meta(IFileMET,TFile,TFileMET),
   meta(IFileMET,HFile,HFileMET),
   meta(IFileMET,TFile,HFile,THFileMET).

meta(Dir):-
   error('directory ~p does not contain files named t.tok and h.tok',[Dir]), 
   !, fail.   


meta(Header,_):-
   access_file(Header,read), !.

meta(Header,Name):-
   atomic_list_concat([echo,Name,'>',Header],' ',Shell), 
   shell(Shell,Return),
   Return = 0, !.

meta(Header,Name):-
   error('cannot make header file ~p with name ~p',[Header,Name]), 
   !, fail.   


meta(_Header,Tok,Met):-
   time_check(Tok,Met), !.

meta(Header,Tok,Met):-
   atomic_list_concat([cat,Header,Tok,'>',Met],' ',Shell), 
   shell(Shell,Return),
   Return = 0, !.

meta(_,_,Met):-
   error('cannot make meta file ~p',[Met]), 
   !, fail.   


meta(_Header,Tok1,Tok2,Met):-
   time_check(Tok1,Met), 
   time_check(Tok2,Met), !.

meta(Header,Tok1,Tok2,Met):-
   atomic_list_concat([cat,Header,Tok1,Tok2,'>',Met],' ',Shell), 
   write(Shell), nl,
   shell(Shell,Return),
   Return = 0, !.

meta(_,_,_,Met):-
   error('cannot make meta file ~p',[Met]), 
   !, fail.   


/*------------------------------------------------------------------------
   Parse (init)
------------------------------------------------------------------------*/

parse(Dir):-
   atomic_list_concat([Dir,'/', 't.met'], TFileMET),
   atomic_list_concat([Dir,'/', 'h.met'], HFileMET),   
   atomic_list_concat([Dir,'/','th.met'],THFileMET),   
   access_file( TFileMET,read),
   access_file( HFileMET,read),
   access_file(THFileMET,read), !,
   atomic_list_concat([Dir,'/', 't.ccg'], TFileCCG),
   atomic_list_concat([Dir,'/', 'h.ccg'], HFileCCG),
   atomic_list_concat([Dir,'/','th.ccg'],THFileCCG),
   parse( TFileMET, TFileCCG),
   parse( HFileMET, HFileCCG),
   parse(THFileMET,THFileCCG).

parse(Dir):-
   error('directory ~p does not contain files named t.met and h.met',[Dir]), 
   !, fail.   


/*------------------------------------------------------------------------
   Parse
------------------------------------------------------------------------*/

parse(In,Out):-
   time_check(In,Out), !.

parse(In,Out):-
   atomic_list_concat(['bin/candc',
                '--input',In,
                '--output',Out,
                '--models models/boxer',
                '--candc-printer boxer'],' ',Shell),
   write(Shell), nl,
   shell(Shell,Return),
   Return = 0, !.

parse(In,_):-
   error('cannot parse ~p',[In]), 
   !, fail.   


/*------------------------------------------------------------------------
   Boxer (init)
------------------------------------------------------------------------*/

box(Dir):-
   ( option('--wsd',true), !, Ext = 'ccg.wsd'; Ext = 'ccg' ),
   atomic_list_concat([Dir,'/', 't.',Ext], TFileCCG),
   atomic_list_concat([Dir,'/', 'h.',Ext], HFileCCG),
   atomic_list_concat([Dir,'/','th.',Ext],THFileCCG),
   access_file( TFileCCG,read),
   access_file( HFileCCG,read),
   access_file(THFileCCG,read), !,
%   atomic_list_concat([Dir,'/', 'ut.drs'], UTFileDRS),
%   atomic_list_concat([Dir,'/', 'uh.drs'], UHFileDRS),
%   atomic_list_concat([Dir,'/','uth.drs'],UTHFileDRS),
   atomic_list_concat([Dir,'/', 't.drs'], TFileDRS),
   atomic_list_concat([Dir,'/', 'h.drs'], HFileDRS),
   atomic_list_concat([Dir,'/','th.drs'],THFileDRS),
%   box( TFileCCG,false,  UTFileDRS),
%   box( HFileCCG,false,  UHFileDRS),
%   box(THFileCCG,false, UTHFileDRS),
   box( TFileCCG,true,  TFileDRS),
   box( HFileCCG,true,  HFileDRS),
   box(THFileCCG,true, THFileDRS).

box(Dir):-
   error('directory ~p does not contain files named t.ccg and h.ccg',[Dir]), 
   !, fail.   


/*------------------------------------------------------------------------
   Boxer 
------------------------------------------------------------------------*/

box(In,_,Out):- time_check(In,Out), !.

box(In,Resolve,Out):-
   option('--plural',PluralOpt), 
   option('--modal',ModalOpt), 
   option('--vpe',VpeOpt), 
   option('--warnings',WarOpt), 
   option('--roles',RolesOpt), 
   atomic_list_concat(['bin/boxer',
                '--input',In,
                '--output',Out,
                '--resolve',Resolve,
                '--plural',PluralOpt,
                '--modal',ModalOpt,
                '--vpe',VpeOpt,
                '--roles',RolesOpt,
                '--copula',false,
                '--elimeq',true,
                '--warnings',WarOpt,
                '--box'],' ',Shell),
   write(Shell), nl,
   shell(Shell,Return),
   Return = 0, !.

box(In,_,_):-
   error('cannot box ~p',[In]), 
   !, fail.   


/*------------------------------------------------------------------------
   WSD (init)
------------------------------------------------------------------------*/

wsd(Dir):-
   option('--wsd',true), 
   atomic_list_concat([Dir,'/','t.ccg'],TFileCCG),
   atomic_list_concat([Dir,'/','h.ccg'],HFileCCG),
   atomic_list_concat([Dir,'/','th.ccg'],THFileCCG),
   access_file(TFileCCG,read),
   access_file(HFileCCG,read), !,
   access_file(THFileCCG,read), !,
   atomic_list_concat([Dir,'/','t.ccg.wsd'],TFileWSD),
   atomic_list_concat([Dir,'/','h.ccg.wsd'],HFileWSD),
   atomic_list_concat([Dir,'/','th.ccg.wsd'],THFileWSD),
   wsd(TFileCCG,TFileWSD),
   wsd(HFileCCG,HFileWSD),
   wsd(THFileCCG,THFileWSD).

wsd(Dir):-
   option('--wsd',true), 
   error('directory ~p does not contain files named t.ccg and h.ccg',[Dir]), 
   !, fail.   

wsd(_):- 
   option('--wsd',false).


/*------------------------------------------------------------------------
   WSD (external) 
------------------------------------------------------------------------*/

wsd(In,Out):- time_check(In,Out), !.

wsd(CCG,WSD):-
   atomic_list_concat(['ext/wsd.pl',
                '--input',CCG,
                '--output',WSD,
                '--slearner','ext/senselearner/'],' ',Shell),
   write(Shell), nl,
   shell(Shell,Return),
   Return = 0, !.

wsd(_,In):-
   error('cannot wsd ~p',[In]), 
   !, fail.   


/*------------------------------------------------------------------------
   Time Check (succeeds if File 1 is older than File 2)
------------------------------------------------------------------------*/

time_check(_,_):- 
   option('--force',true), 
   !, fail.

time_check(In,Out):-
   option('--force',false),
   access_file(In,read),
   access_file(Out,read),
   time_file(In,T1),
   time_file(Out,T2),
   T1 < T2.


/* =======================================================================
   Textual Entailment (logical inference)
========================================================================*/

nc(Dir,KT,KH,KTH):-
   openInput(Dir),

   inputDRS(t,TDRS), 
%  inputDRS(tfate,TDRS), 

   inputDRS(h,HDRS), 
%  inputDRS(hfate,HDRS), 

   inputDRS(th,THDRS), 
%  inputDRS(thfate,THDRS), 

   negDRS(THDRS,TNHDRS), 

   countingAxioms([],Axioms),

   inf(  TDRS,Axioms,Dir,  t,1,ModT), domSize(ModT,DomT),
   inf(  HDRS,Axioms,Dir,  h,1,   _),   
   inf( THDRS,Axioms,Dir, th,DomT,_),
   inf(TNHDRS,Axioms,Dir,tnh,1,   _),

   inf(  TDRS, KT,Dir,  kt,1,ModKT), domSize(ModKT,DomKT),
   inf(  HDRS, KH,Dir,  kh,1,    _), 
   inf( THDRS,KTH,Dir, kth,DomKT,_),
   inf(TNHDRS,KTH,Dir,ktnh,1,    _).

%   notinf( HDRS, KH,Dir, notkh,1,_),
%   notinf( TDRS, KT,Dir, notkt,1,_),
%   notinf(THDRS,KTH,Dir,notkth,1,_).


/* =======================================================================
   Textual Entailment (WordNet)
========================================================================*/

mwn(Dir,AxiomsKT,AxiomsKH,AxiomsKTH,Novelty):-
   
   openInput(Dir),

   inputDRS(t,TDRS), computeMWN(TDRS,Dir,kt,DomT),  
%   inputDRS(tfate,TDRS), computeMWN(TDRS,Dir,kt,DomT),  

   axiomsWN(WNAxiomsKT), axiomsFN(TDRS,FNAxiomsKT), 
   append(WNAxiomsKT,FNAxiomsKT,AxiomsKT0),
   countingAxioms(AxiomsKT0,AxiomsKT),

   inputDRS(h,HDRS), computeMWN(HDRS,Dir,kh,DomH), 
%   inputDRS(hfate,HDRS), computeMWN(HDRS,Dir,kh,_), 

   axiomsWN(WNAxiomsKH), axiomsFN(HDRS,FNAxiomsKH),
   append(WNAxiomsKH,FNAxiomsKH,AxiomsKH0),
   countingAxioms(AxiomsKH0,AxiomsKH),

   inputDRS(th,THDRS), computeMWN(THDRS,Dir,kth,DomTH), 
%   inputDRS(thfate,THDRS), computeMWN(THDRS,Dir,kth, DomTH), 

   axiomsWN(WNAxiomsKTH), axiomsFN(THDRS,FNAxiomsKTH),
   append(WNAxiomsKTH,FNAxiomsKTH,AxiomsKTH0),
   countingAxioms(AxiomsKTH0,AxiomsKTH),
   computeNovelty(DomT,DomH,DomTH,Novelty).


/* =======================================================================
   Inference
========================================================================*/

inf(_,_,Dir,Name,DomSize,Model):- 
   DomSize = 0, !, 
   Model = model([],[]),
   outputModel(Model,Name,Dir,DomSize),
   inform('previously inconsistent, no inference for ~p',[Name]).

inf(B,BK,Dir,Name,MinDom,Model):-
   drs2fol(B,F),
   option('--domsize',MaxDom),
   callTPandMB(Dir,BK,not(F),F,MinDom,MaxDom,TmpModel,TmpEngine),
   ( member(Name,[kt,kth]), !, callMBbis(Dir,BK,F,TmpModel,Model,TmpEngine,Engine)
   ; TmpModel = Model, TmpEngine = Engine ),
   outputModel(Model,Name,Dir,DomSize),
   ( DomSize > 0, Result = 'consistent'
   ; DomSize = 0, Result = 'inconsistent'
   ; DomSize < 0, Result = 'unknown' ),
   inform('~p found result for ~p (~p, domain size: ~p)',[Engine,Name,Result,DomSize]).

/* WORK IN PROGRESS
notinf(B,BK,Dir,Name,MinDom,Model):-
   drs2fol(B,F),
   option('--domsize',MaxDom),
   callTPandMB(Dir,BK,F,not(F),MinDom,MaxDom,TmpModel,TmpEngine),
   ( member(Name,[kt,kth]), !, callMBbis(Dir,BK,not(F),TmpModel,Model,TmpEngine,Engine)
   ; TmpModel = Model, TmpEngine = Engine ),
   outputModel(Model,Name,Dir,DomSize),
   ( DomSize > 0, Result = 'consistent'
   ; DomSize = 0, Result = 'inconsistent'
   ; DomSize < 0, Result = 'unknown' ),
   inform('~p found result for ~p (~p, domain size: ~p)',[Engine,Name,Result,DomSize]).
*/

/* =======================================================================
   Negate DRS (the h part, for checking informativity of h wrt t)
========================================================================*/

negDRS(B,NB):- insert(Position), negDRS(B,Position,NB), !.
negDRS(B,B).

negDRS(alfa(B1,Type,B2),N,alfa(B1,Type,B3)):- !, negDRS(B2,N,B3).
negDRS(smerge(B1,B2),_,smerge(B1,drs([],[[]:not(B2)]))):- !.
%negDRS(smerge(B1,B2),N1,smerge(B1,B3)):- N2 is N1-1, negDRS(B2,N2,B3).

insert(Pos):- inputDRS(t,UT), lenDRS(UT,Pos).


/* =======================================================================
   Prediction (try inference first, else back off to WordNet)
========================================================================*/
 
prediction(Dir,WNNovelty,Overlap):-

   openModel(Dir,t,ModT),     openModel(Dir,h,ModH),
   openModel(Dir,th,ModTH),   openModel(Dir,tnh,ModTNH),
   openModel(Dir,kt,ModKT),   openModel(Dir,kh,ModKH),
   openModel(Dir,kth,ModKTH), openModel(Dir,ktnh,ModKTNH),

   domSize(ModT,DomT),        domSize(ModH,DomH),
   domSize(ModTH,DomTH),      domSize(ModTNH,DomTNH),
   domSize(ModKT,DomKT),      domSize(ModKH,DomKH),
   domSize(ModKTH,DomKTH),    domSize(ModKTNH,DomKTNH),

   relSize(ModKT,RelKT),   
   relSize(ModKH,RelKH),
   relSize(ModKTH,RelKTH),

   computeNovelty(DomKT,DomKH,DomKTH,DomNovelty),
   computeNovelty(RelKT,RelKH,RelKTH,RelNovelty),

   prediction(DomT,DomH,DomTH,DomTNH,DomKT,DomKH,DomKTH,DomKTNH,
              DomNovelty,RelNovelty,WNNovelty,Overlap,Prediction), !,

   outputPrediction(Dir,Prediction,DomKTNH,DomKTH,
                    DomNovelty,RelNovelty,WNNovelty,Overlap).

/* WORK IN PROGRESS
   openModel(Dir,notkh,ModNotKH),
   openModel(Dir,notkt,ModNotKT),
   openModel(Dir,notkth,ModNotKTH),
   domSize(ModNotKT,DomNotKT),   
   domSize(ModNotKH,DomNotKH),
   domSize(ModNotKTH,DomNotKTH), 
   relSize(ModNotKT,RelNotKT),   
   relSize(ModNotKH,RelNotKH),
   relSize(ModNotKTH,RelNotKTH),

   Dom1 is DomKT+DomNotKH ),
   Dom2 is DomKH+DomNotKT ),
   Dom3 is DomKTH+DomNotKTH),
   Rel1 is RelKT+RelNotKH),
   Rel2 is RelKH+RelNotKT),
   Rel3 is RelKTH+RelNotKTH),

   computeNovelty(Dom1,Dom2,Dom3,DomNovelty),
   computeNovelty(Rel1,Rel2,Rel3,RelNovelty),
   ...
*/
  
/* =======================================================================
   Prediction (values based on RTE-3 development)
========================================================================*/

prediction(T,H,0,_,_,_,_,_,_,_,_,_,Prediction):-
   T > 0, H > 0, !,
   Prediction = 'informative (simple proof)'.

prediction(T,H,TH,0,_,_,_,_,_,_,_,_,Prediction):-
   T > 0, H > 0, TH > 0, !,
   Prediction = 'entailed (simple proof)'.

prediction(_,_,_,_,KT,KH,0,_,_,_,_,_,Prediction):-
   KT > 0, KH > 0, !,
   Prediction = 'informative (complex proof)'.

prediction(_,_,_,_,KT,KH,KTH,0,_,_,_,_,Prediction):-
   KT > 0, KH > 0, KTH > 0, !,
   Prediction = 'entailed (complex proof)'.

prediction(_,_,_,_,_,_,_,_,_,_,WNNovelty,Overlap,Prediction):-
   WNNovelty < 0, !, % No DRS could be computed
   ( Overlap > 0.692308, !
   , Prediction = 'entailed (word overlap)'
   ; Prediction = 'informative (word overlap)' ).

prediction(_,_,_,_,_,_,_,_,DomNovelty,_,WNNovelty,_,Prediction):-
   DomNovelty < 0, !, % No model could be computed
   ( WNNovelty =< 0.380952, !
   , Prediction = 'entailed (wordnet novelty)'
   ; Prediction = 'informative (wordnet novelty)' ).
   
prediction(_,_,_,_,_,_,_,_,DomNovelty,_,_,_,Prediction):-
   ( DomNovelty =< 0.6, !
   , Prediction = 'entailed (model novelty)'
   ; Prediction = 'informative (model novelty)' ).


/* =======================================================================
   Output Model
========================================================================*/

outputModel(Model,Name,Dir,Size):-
   atomic_list_concat([Dir,'/',Name,'.mod'],File),
   open(File,write,Stream),
   printModel(Model,Stream), 
   write(Stream,'.'), nl(Stream),
   close(Stream),
   domSize(Model,Size).


/* =======================================================================
   Print Model
========================================================================*/

printModel(model(D,[]),Stream):- !, format(Stream,'model(~p, [])',[D]).

printModel(model(D,[F]),Stream):- !, format(Stream,'model(~p,~n  [~p])',[D,F]).

printModel(model(D,[X,Y|F]),Stream):- !,
   setof(M,Sym^Ext^(member(M,[X,Y|F]),\+ M=f(0,Sym,Ext)),[First|Sorted]),
   format(Stream,'model(~p,~n  [~p,~n',[D,First]),
   printModel(Sorted,Stream).

printModel([Last],Stream):- !, format(Stream,'   ~p])',[Last]).

printModel([X|L],Stream):- !, 
   format(Stream,'   ~p,~n',[X]), 
   printModel(L,Stream).

printModel(Model,Stream):- write(Stream,Model).


/* =======================================================================
   Determine Model Size (Domain)
========================================================================*/

domSize(Model,Size):-
   Model = model(Dom,_), !,
   length(Dom,Size).

domSize(_,-1).


/* =======================================================================
   Determine Model Size (Relations)
========================================================================*/

relSize(Model,Size):-
   Model = model(_,F), !,
%  findall(R,(member(f(2,_,E),F),member(R,E)),Rs),
   findall(R,(member(f(_,_,E),F),member(R,E)),Rs),
   length(Rs,Size).

relSize(_,-1).


/* =======================================================================
   Output Prediction
========================================================================*/

outputPrediction(Dir,Prediction,Proof,Contra,DomNovelty,RelNovelty,WNNovelty,Overlap):-
   atomic_list_concat([Dir,'/','prediction.txt'],File),
   open(File,write,Stream),
   write(Stream,Prediction), nl(Stream),
   close(Stream),
   inform('prediction: ~p',[Prediction]),
   outputDomSizeDif(Dir,Proof,Contra,DomNovelty,RelNovelty,WNNovelty,Overlap).


/* =======================================================================
   Output Domain Size Difference
========================================================================*/

outputDomSizeDif(Dir,Proof,Contradiction,Dom,Rel,WordNet,Overlap):-
   atomic_list_concat([Dir,'/','modsizedif.txt'],File),
   open(File,write,Stream),
   ( Contradiction=0, !, Prover=contradiction
   ; Proof=0, !, Prover=proof
   ; Prover=unknown ),
   format(Stream,'~p.   % prover output    ~n',[Prover]),
   format(Stream,'~p.   % domain novelty   ~n',[Dom]),
   format(Stream,'~p.   % relation novelty ~n',[Rel]),
   format(Stream,'~p.   % wordnet novelty  ~n',[WordNet]),
   format(Stream,'~p.   % word overlap     ~n',[Overlap]),
   close(Stream).


/* =======================================================================
   Compute Novelty of H given T
========================================================================*/

computeNovelty(SizeT,SizeH,SizeTH,Novelty):-
   SizeTH > 0, SizeH > 0, SizeTH > 0, !,
   Novelty is ((SizeTH-SizeT)/SizeH).

computeNovelty(_,_,_,-1).


/* =======================================================================
   MiniWordNet
========================================================================*/

computeMWN(DRS,Dir,File,Size):-   
   option('--wordnet',true), !,
   clearMWN,
   compConcepts(DRS),
   compISA,
   addTopMWN,                   %%% this can cause inconsistencies!
%  cutDownMWN,
   sizeMWN(Size),
   outputMWN(Dir,File),
   graphMWN(Dir,File).

computeMWN(_,_,_,0).


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
   format(user_error,'usage: nc [options]~n~n',[]),
   showOptions(nutcracker).

help:-
   option('--help',dont), !.


/* =======================================================================
   Definition of start
========================================================================*/

start:-
   current_prolog_flag(argv,[_Comm|Args]), 
   \+ Args = [],
   set_prolog_flag(float_format,'%.20g'),
   setDefaultOptions(nutcracker), 
   parseOptions(nutcracker,Args),
   shell('chmod 755 src/prolog/nutcracker/startTPandMB.pl', Return),
   Return = 0,
   main, !,
   halt.

start:- 
   setDefaultOptions(nutcracker), 
   setOption(nutcracker,'--help',do), !,
   help,
   halt.
