
:- module(xdrs2xml,[drs2xml/2,
                    der2xml/2,
                    xfdrs2xml/2,
                    xdrs2xml/2]).

:- use_module(semlib(options),[option/2]).
:- use_module(semlib(errors),[warning/2]).
:- use_module(library(lists),[member/2,append/3]).
:- use_module(boxer(betaConversionDRT),[betaConvert/2]).
:- use_module(boxer(drs2fdrs),[instDrs/1]).



/*========================================================================
   Converting DRSs to XML
========================================================================*/

drs2xml(DRS,Stream):- drs2xml(DRS,Stream,1,[]).

der2xml(Der,Stream):- der2xml(Der,Stream,1,_).

xdrs2xml(XDRS,Stream):-
   XDRS=xdrs(Words,POStags,NEtags,DRS),
   write(Stream,' <words>'), nl(Stream),
   words2xml(Words,Stream),
   write(Stream,' </words>'), nl(Stream),
   write(Stream,' <postags>'), nl(Stream),
   tags2xml(POStags,Stream),
   write(Stream,' </postags>'), nl(Stream),
   write(Stream,' <netags>'), nl(Stream),
   tags2xml(NEtags,Stream),
   write(Stream,' </netags>'), nl(Stream),
   drs2xml(DRS,Stream,1,Words).

xfdrs2xml(XDRS,Stream):-
   XDRS=xdrs(Words,POStags,NEtags,Cons),
   write(Stream,' <words>'), nl(Stream),
   words2xml(Words,Stream),
   write(Stream,' </words>'), nl(Stream),
   write(Stream,' <postags>'), nl(Stream),
   tags2xml(POStags,Stream),
   write(Stream,' </postags>'), nl(Stream),
   write(Stream,' <netags>'), nl(Stream),
   tags2xml(NEtags,Stream),
   write(Stream,' </netags>'), nl(Stream),
   write(Stream,' <cons>'), nl(Stream),
   cons2xml(Cons,Stream),
   write(Stream,' </cons>'), nl(Stream).


/*========================================================================
   Converting CCG derivation to XML (with tab insertion)
========================================================================*/

der2xml(t(Sem,Cat,Token,Pos,Ne),Stream,Tab,[Token]):- !,
   tab(Stream,Tab), format(Stream,'<lex>~n',[]),
   NewTab is Tab + 2,
   tab(Stream,Tab), format(Stream,' <token>~p</token>~n',[Token]),  
   tab(Stream,Tab), format(Stream,' <tag type="pos">~p</tag>~n',[Pos]),  
   tab(Stream,Tab), format(Stream,' <tag type="ne">~p</tag>~n',[Ne]),  
   tab(Stream,Tab), format(Stream,' <cat>~n',[]),  
   cat2xml(Cat,Stream,NewTab),
   tab(Stream,Tab), format(Stream,' </cat>~n',[]),  
   tab(Stream,Tab), format(Stream,' <sem>~n',[]),  
   betaConvert(Sem,Red),
   instDrs(Red),
   drs2xml(Red,Stream,NewTab,[]),
   tab(Stream,Tab), format(Stream,' </sem>~n',[]),  
   tab(Stream,Tab), format(Stream,'</lex>~n',[]).

der2xml(Der,Stream,Tab,Tokens):- 
   Der =.. [Rule,Cat,Sem,Under], !,
   NewTab is Tab + 1,
   NextTab is Tab + 1,
   tab(Stream,Tab), format(Stream,'<unaryrule type="~p">~n',[Rule]),
   tab(Stream,Tab), format(Stream,' <cat>~n',[]),  
   cat2xml(Cat,Stream,NewTab),
   tab(Stream,Tab), format(Stream,' </cat>~n',[]),  
   tab(Stream,Tab), format(Stream,' <sem>~n',[]),  
   betaConvert(Sem,Red),
   instDrs(Red),
   drs2xml(Red,Stream,NewTab,[]),
   tab(Stream,Tab), format(Stream,' </sem>~n',[]),  
   der2xml(Under,Stream,NewTab,Tokens),
   tab(Stream,NewTab), format(Stream,'<tokens>~n',[]),  
   tokens2xml(Tokens,NextTab,Stream),
   tab(Stream,NewTab), format(Stream,'</tokens>~n',[]),  
   tab(Stream,Tab), format(Stream,'</unaryrule>~n',[]).

der2xml(Der,Stream,Tab,Tokens):- 
   Der =.. [Rule,Cat,Sem,Left,Right], !,
   NewTab is Tab + 1,
   NextTab is Tab + 2,
   tab(Stream,Tab), format(Stream,'<binaryrule type="~p">~n',[Rule]),
   tab(Stream,NewTab), format(Stream,'<cat>~n',[]),  
   cat2xml(Cat,Stream,NextTab),
   tab(Stream,NewTab), format(Stream,'</cat>~n',[]),  
   tab(Stream,NewTab), format(Stream,'<sem>~n',[]),  
   betaConvert(Sem,Red),
   instDrs(Red),
   drs2xml(Red,Stream,NewTab,[]),
   tab(Stream,NewTab), format(Stream,'</sem>~n',[]),  
   der2xml(Left,Stream,NewTab,TokensLeft),   
   der2xml(Right,Stream,NewTab,TokensRight),
   append(TokensLeft,TokensRight,Tokens),
   tab(Stream,NewTab), format(Stream,'<tokens>~n',[]),  
   tokens2xml(Tokens,NextTab,Stream),
   tab(Stream,NewTab), format(Stream,'</tokens>~n',[]),  
   tab(Stream,Tab),  format(Stream,'</binaryrule>~n',[]).


/*========================================================================
   Converting CCG categories to XML (with tab insertion)
========================================================================*/

cat2xml('/'(L,R),Stream,Tab):- !,
   tab(Stream,Tab), format(Stream,'<forward>~n',[]),
   NewTab is Tab + 1,
   cat2xml(L,Stream,NewTab),
   cat2xml(R,Stream,NewTab),
   tab(Stream,Tab), format(Stream,'</forward>~n',[]).

cat2xml('\\'(L,R),Stream,Tab):- !,
   tab(Stream,Tab), format(Stream,'<backward>~n',[]),
   NewTab is Tab + 1,
   cat2xml(L,Stream,NewTab),
   cat2xml(R,Stream,NewTab),
   tab(Stream,Tab), format(Stream,'</backward>~n',[]).

cat2xml(Cat:Feature,Stream,Tab):-
   atom(Cat), !,
   upcase_atom(Cat,Up),
   tab(Stream,Tab),
   format(Stream,' <atomic feature="~p">~p</atomic>~n',[Feature,Up]).

cat2xml(Cat,Stream,Tab):- 
   atom(Cat), !,
   upcase_atom(Cat,Up),
   tab(Stream,Tab),
   format(Stream,' <atomic>~p</atomic>~n',[Up]).

cat2xml(Cat,Stream,Tab):- !,
   tab(Stream,Tab),
   format(Stream,' <atomic>~p</atomic>~n',[Cat]).


/*========================================================================
   Guess Sentence ID (a bit of a hack, obviously!)
========================================================================*/

guessSentenceID(drs(Dom,_),I):- member([I]:_,Dom), !.

guessSentenceID(drs(_,Conds),I):- member([I]:_,Conds), !.

guessSentenceID(_,0).


/*========================================================================
   Converting DRSs to XML (with tab insertion)
========================================================================*/

drs2xml(Var,Stream,Tab,_):- 
   var(Var), !,
   tab(Stream,Tab),   
   format(Stream,'<var>~p</var>~n',Var).

drs2xml(Var,Stream,Tab,_):- 
   atom(Var), !,
   tab(Stream,Tab),   
   format(Stream,'<var>~p</var>~n',Var).

drs2xml(Var,Stream,Tab,_):- 
   Var =.. ['$VAR',_], !,
   tab(Stream,Tab),   
   format(Stream,'<var>~p</var>~n',Var).

drs2xml(drs(D,C),Stream,Tab,Tokens):- !,
   NewTab is Tab + 1,
   NextTab is Tab + 2,
   ( Tokens = [], !,
     tab(Stream,Tab),    format(Stream,'<drs type="normal">~n',[])
   ; Tokens = [presup], !,
     tab(Stream,Tab),    format(Stream,'<drs type="presup">~n',[])
   ; tab(Stream,Tab),    format(Stream,'<drs type="sentence">~n',[]),
     tab(Stream,NewTab), format(Stream,'<tokens>~n',[]),
     guessSentenceID(drs(D,C),S),
     tokens2xml(Tokens,S,NextTab,Stream),
     tab(Stream,NewTab), format(Stream,'</tokens>~n',[])
   ),
   tab(Stream,NewTab), format(Stream,'<dom>~n',[]),
   dom2xml(D,Stream,NextTab),
   tab(Stream,NewTab), format(Stream,'</dom>~n',[]),
   tab(Stream,NewTab), format(Stream,'<con>~n',[]),
   conds2xml(C,Stream,NextTab),
   tab(Stream,NewTab), format(Stream,'</con>~n',[]),
   tab(Stream,Tab),    format(Stream,'</drs>~n',[]).

drs2xml(alfa(Type,B1,B2),Stream,Tab,Tokens):- 
   Type = top, !,
   tab(Stream,Tab),   
   format(Stream,'<alfa type="~p">~n',[Type]),
   NewTab is Tab + 1,
   drs2xml(B1,Stream,NewTab,[presup]),
   drs2xml(B2,Stream,NewTab,Tokens),
   tab(Stream,Tab),   
   format(Stream,'</alfa>~n',[]).

drs2xml(alfa(Type,B1,B2),Stream,Tab,_):- !,
   tab(Stream,Tab),   
   format(Stream,'<alfa type="~p">~n',[Type]),
   NewTab is Tab + 1,
   drs2xml(B1,Stream,NewTab,[]),
   drs2xml(B2,Stream,NewTab,[]),
   tab(Stream,Tab),   
   format(Stream,'</alfa>~n',[]).

drs2xml(lam(X,B),Stream,Tab,_):- !,
   tab(Stream,Tab),   
   format(Stream,'<lam>~n',[]),
   NewTab is Tab + 1,
   tab(Stream,Tab),   
   format(Stream,' <var>~p</var>~n',X),
   drs2xml(B,Stream,NewTab,[]),
   tab(Stream,Tab),   
   format(Stream,'</lam>~n',[]).

drs2xml(app(B1,B2),Stream,Tab,_):- !,
   tab(Stream,Tab),   
   format(Stream,'<app>~n',[]),
   NewTab is Tab + 1,
   drs2xml(B1,Stream,NewTab,[]),
   drs2xml(B2,Stream,NewTab,[]),
   tab(Stream,Tab),   
   format(Stream,'</app>~n',[]).

drs2xml(merge(B1,B2),Stream,Tab,_):- !,
   tab(Stream,Tab),   
   format(Stream,'<merge>~n',[]),
   NewTab is Tab + 1,
   drs2xml(B1,Stream,NewTab,[]),
   drs2xml(B2,Stream,NewTab,[]),
   tab(Stream,Tab),   
   format(Stream,'</merge>~n',[]).

drs2xml(smerge(B1,B2),Stream,Tab,Tokens):- !,
   tab(Stream,Tab),   
   format(Stream,'<smerge>~n',[]),
   NewTab is Tab + 1,
   drs2xml(B1,Stream,NewTab,Tokens),
   drs2xml(B2,Stream,NewTab,Tokens),
   tab(Stream,Tab),   
   format(Stream,'</smerge>~n',[]).

drs2xml(sdrs(Labs,Rels),Stream,Tab,Tokens):- !,
   tab(Stream,Tab),   
   format(Stream,'<sdrs>~n',[]),
   NewTab is Tab + 1,
   tab(Stream,NewTab), format(Stream,'<constituents>~n',[]),
   sdrs2xml(Labs,Stream,NewTab,Tokens,Rels),
   tab(Stream,Tab),   
   format(Stream,'</sdrs>~n',[]).

drs2xml(Error,_,_,_):- !,
   write(error:Error),nl.


/*========================================================================
   Converting SDRS to XML
========================================================================*/

sdrs2xml([],Stream,Tab,_,Rels):-
   tab(Stream,Tab), format(Stream,'</constituents>~n',[]),
   tab(Stream,Tab), format(Stream,'<relations>~n',[]),
   relations2xml(Rels,Stream,Tab),
   tab(Stream,Tab), format(Stream,'</relations>~n',[]).

sdrs2xml([lab(K,B)|L],Stream,Tab,Tokens,Rel):-
   label2xml(K,B,Stream,Tab,Tokens),
   sdrs2xml(L,Stream,Tab,Tokens,Rel).   

sdrs2xml([sub(lab(K1,B1),lab(K2,B2))|L],Stream,Tab,Tokens,Rel):-
   member(rel(K,_,presupposition),Rel), K==K1, !,
   tab(Stream,Tab), format(Stream,'<sub>~n',[]),
   label2xml(K1,B1,Stream,Tab,[presup]),
   label2xml(K2,B2,Stream,Tab,Tokens),
   tab(Stream,Tab), format(Stream,'</sub>~n',[]),
   sdrs2xml(L,Stream,Tab,Tokens,Rel).   

sdrs2xml([sub(lab(K1,B1),lab(K2,B2))|L],Stream,Tab,Tokens,Rel):-
   tab(Stream,Tab), format(Stream,'<sub>~n',[]),
   label2xml(K1,B1,Stream,Tab,Tokens),
   label2xml(K2,B2,Stream,Tab,Tokens),
   tab(Stream,Tab), format(Stream,'</sub>~n',[]),
   sdrs2xml(L,Stream,Tab,Tokens,Rel).   


/*========================================================================
   Converting SDRS constituent to XML
========================================================================*/

label2xml(K,B,Stream,Tab,Tokens):-
   tab(Stream,Tab), format(Stream,'<constituent label="~p">~n',[K]),
   drs2xml(B,Stream,Tab,Tokens),
   tab(Stream,Tab), format(Stream,'</constituent>~n',[]).


/*========================================================================
   Converting SDRS relations to XML
========================================================================*/

relations2xml([],_,_).

relations2xml([Index:rel(K1,K2,Rel)|L],Stream,Tab):-
   tab(Stream,Tab), format(Stream,'<drel arg1="~p" arg2="~p" sym="~p">~n',[K1,K2,Rel]),
   index2xml(Index,Stream,Tab),
   tab(Stream,Tab), format(Stream,'</drel>~n',[]),
   relations2xml(L,Stream,Tab).   


/*========================================================================
   Converting DRS-domains to XML (with tab insertion)
========================================================================*/

dom2xml([],_,_).

dom2xml([Index:X|L],Stream,Tab):- !,
   tab(Stream,Tab),   
   format(Stream,'<dr name="~p">~n',[X]),
   index2xml(Index,Stream,Tab),
   tab(Stream,Tab),
   format(Stream,'</dr>~n',[]),
   dom2xml(L,Stream,Tab).

dom2xml([X|L],Stream,Tab):-
   option('--warnings',true), !,
   warning('cannot print DR ~p',[X]),
   dom2xml(L,Stream,Tab).

dom2xml([_|L],Stream,Tab):-
   dom2xml(L,Stream,Tab).


/*========================================================================
   Converting DRS-conditions to XML (with tab insertion)
========================================================================*/

conds2xml([],_,_).

conds2xml([Index:not(B)|L],Stream,Tab):- !,
   tab(Stream,Tab),   
   format(Stream,'<not>~n',[]),
   index2xml(Index,Stream,Tab),
   NewTab is Tab + 1,
   drs2xml(B,Stream,NewTab,[]),
   tab(Stream,Tab),   
   format(Stream,'</not>~n',[]),
   conds2xml(L,Stream,Tab).

conds2xml([Index:nec(B)|L],Stream,Tab):- !,
   tab(Stream,Tab),   
   format(Stream,'<nec>~n',[]),
   index2xml(Index,Stream,Tab),
   NewTab is Tab + 1,
   drs2xml(B,Stream,NewTab,[]),
   tab(Stream,Tab),   
   format(Stream,'</nec>~n',[]),
   conds2xml(L,Stream,Tab).

conds2xml([Index:pos(B)|L],Stream,Tab):- !,
   tab(Stream,Tab),   
   format(Stream,'<pos>~n',[]),
   index2xml(Index,Stream,Tab),
   NewTab is Tab + 1,
   drs2xml(B,Stream,NewTab,[]),
   tab(Stream,Tab),   
   format(Stream,'</pos>~n',[]),
   conds2xml(L,Stream,Tab).

conds2xml([Index:prop(X,B)|L],Stream,Tab):- !,
   tab(Stream,Tab),   
   format(Stream,'<prop argument="~p">~n',[X]),
   index2xml(Index,Stream,Tab),
   NewTab is Tab + 1,
   drs2xml(B,Stream,NewTab,[]),
   tab(Stream,Tab),   
   format(Stream,'</prop>~n',[]),
   conds2xml(L,Stream,Tab).

conds2xml([Index:or(B1,B2)|L],Stream,Tab):- !,
   tab(Stream,Tab),   
   format(Stream,'<or>~n',[]),
   index2xml(Index,Stream,Tab),
   NewTab is Tab + 1,
   drs2xml(B1,Stream,NewTab,[]),
   drs2xml(B2,Stream,NewTab,[]),
   tab(Stream,Tab),   
   format(Stream,'</or>~n',[]),
   conds2xml(L,Stream,Tab).

conds2xml([Index:imp(B1,B2)|L],Stream,Tab):- !,
   tab(Stream,Tab),   
   format(Stream,'<imp>~n',[]),
   index2xml(Index,Stream,Tab),
   NewTab is Tab + 1,
   drs2xml(B1,Stream,NewTab,[]),
   drs2xml(B2,Stream,NewTab,[]),
   tab(Stream,Tab),   
   format(Stream,'</imp>~n',[]),
   conds2xml(L,Stream,Tab).

conds2xml([Index:whq(_,B1,_,B2)|L],Stream,Tab):- !,
   conds2xml([Index:whq(B1,B2)|L],Stream,Tab).

conds2xml([Index:whq(B1,B2)|L],Stream,Tab):- !,
   tab(Stream,Tab),   
   format(Stream,'<whq>~n',[]),
   index2xml(Index,Stream,Tab),
   NewTab is Tab + 1,
   drs2xml(B1,Stream,NewTab,[]),
   drs2xml(B2,Stream,NewTab,[]),
   tab(Stream,Tab),   
   format(Stream,'</whq>~n',[]),
   conds2xml(L,Stream,Tab).

conds2xml([Index:pred(Arg,X,Type,Sense)|L],Stream,Tab):- !,
   tab(Stream,Tab),   
   symbol(X,Y),
   format(Stream,'<pred arg="~p" symbol="~w" type="~p" sense="~p">~n',[Arg,Y,Type,Sense]),
   index2xml(Index,Stream,Tab),
   tab(Stream,Tab),
   format(Stream,'</pred>~n',[]),
   conds2xml(L,Stream,Tab).

conds2xml([Index:rel(Arg1,Arg2,X,Sense)|L],Stream,Tab):- !,
   tab(Stream,Tab),   
   symbol(X,Y),
   format(Stream,'<rel arg1="~p" arg2="~p" symbol="~w" sense="~p">~n',[Arg1,Arg2,Y,Sense]),
   index2xml(Index,Stream,Tab),
   tab(Stream,Tab),   
   format(Stream,'</rel>~n',[]),   
   conds2xml(L,Stream,Tab).

conds2xml([Index:named(Arg,X,Type,_)|L],Stream,Tab):- !,
   tab(Stream,Tab),   
   symbol(X,Y),
   format(Stream,'<named arg="~p" symbol="~w" type="~p">~n',[Arg,Y,Type]),
   index2xml(Index,Stream,Tab),
   tab(Stream,Tab),
   format(Stream,'</named>~n',[]),
   conds2xml(L,Stream,Tab).

conds2xml([Index:card(X,Y,Type)|L],Stream,Tab):- !,
   tab(Stream,Tab),   
   format(Stream,'<card arg="~p" value="~p" type="~p">~n',[X,Y,Type]),
   index2xml(Index,Stream,Tab),
   tab(Stream,Tab),   
   format(Stream,'</card>~n',[]),
   conds2xml(L,Stream,Tab).

conds2xml([Index:timex(X,Y)|L],Stream,Tab):- !,
   tab(Stream,Tab),   
   format(Stream,'<timex arg="~p">~n',[X]),
   index2xml(Index,Stream,Tab),
   timex2xml(Y,Stream,Tab),
   tab(Stream,Tab),   
   format(Stream,'</timex>~n',[]),
   conds2xml(L,Stream,Tab).

conds2xml([Index:eq(X,Y)|L],Stream,Tab):- !,
   tab(Stream,Tab),   
   format(Stream,'<eq arg1="~p" arg2="~p">~n',[X,Y]),
   index2xml(Index,Stream,Tab),
   tab(Stream,Tab),   
   format(Stream,'</eq>~n',[]),  
   conds2xml(L,Stream,Tab).

conds2xml([X|L],Stream,Tab):-
   option('--warnings',true), !,
   warning('cannot print DR-Condition ~p',[X]),
   conds2xml(L,Stream,Tab).

conds2xml([_|L],Stream,Tab):-
   conds2xml(L,Stream,Tab).


/*========================================================================
   Timex
========================================================================*/

timex2xml(date(_:A,_:B,_:C),Stream,Tab):- !,
   tab(Stream,Tab),
   format(Stream,'<date>~w~w~w</date>~n',[A,B,C]).

timex2xml(date(_:Z,_:A,_:B,_:C),Stream,Tab):- !,
   tab(Stream,Tab),
   format(Stream,'<date>~w~w~w~w</date>~n',[Z,A,B,C]).

timex2xml(time(_:A,_:B,_:C),Stream,Tab):- !,
   tab(Stream,Tab),
   format(Stream,'<time>~w~w~w</time>~n',[A,B,C]).

timex2xml(X,Stream,Tab):- !,
   tab(Stream,Tab),
   format(Stream,'<unknown>~p</unknown>~n',[X]).


/*========================================================================
   Words
========================================================================*/

words2xml([],_).

words2xml([word(Index,Word)|L],Stream):-
   symbol(Word,Word1),
   format(Stream,'  <word xml:id="i~p">~w</word>~n',[Index,Word1]),
   words2xml(L,Stream).


/*========================================================================
   Tokens
========================================================================*/

tokens2xml([],_,_).

tokens2xml([T|L],Tab,Stream):-
   symbol(T,T1),
   tab(Stream,Tab), format(Stream,'<token>~w</token>~n',[T1]),
   tokens2xml(L,Tab,Stream).

tokens2xml([],_,_,_).

tokens2xml([word(Index,Word)|L],Sentence,Tab,Stream):-
   wordInSentence(Index,Sentence), !,
   symbol(Word,Word1),
   tab(Stream,Tab), format(Stream,'<token>~w</token>~n',[Word1]),
   tokens2xml(L,Sentence,Tab,Stream).

tokens2xml([_|L],Sentence,Tab,Stream):-
   tokens2xml(L,Sentence,Tab,Stream).


/*========================================================================
   Check whether word is part of sentence
========================================================================*/

wordInSentence(N1,N2):- 
   X is (N1-(mod(N1,1000)))/1000,
   X is (N2-(mod(N2,1000)))/1000.


/*========================================================================
   POS tags
========================================================================*/

tags2xml([],_).

tags2xml([pos(Index,POS)|L],Stream):-
   format(Stream,'  <tag type="pos" index="i~p">~w</tag>~n',[Index,POS]),
   tags2xml(L,Stream).

tags2xml([ne(Index,NE)|L],Stream):-
   format(Stream,'  <tag type="ne" index="i~p">~w</tag>~n',[Index,NE]),
   tags2xml(L,Stream).


/*========================================================================
   Flat DRSs
========================================================================*/

cons2xml([],_).

cons2xml([Label:alfa(Type,L1,L2)|Cons],Stream):- !,
   format(Stream,'  <alfa label="~p" type="~p"><label>~p</label><label>~p</label></alfa>~n',[Label,Type,L1,L2]),
   cons2xml(Cons,Stream).

cons2xml([Label:merge(L1,L2)|Cons],Stream):- !,
   format(Stream,'  <merge label="~p"><label>~p</label><label>~p</label></merge>~n',[Label,L1,L2]),
   cons2xml(Cons,Stream).

cons2xml([Label:smerge(L1,L2)|Cons],Stream):- !,
   format(Stream,'  <smerge label="~p"><label>~p</label><label>~p</label></smerge>~n',[Label,L1,L2]),
   cons2xml(Cons,Stream).

cons2xml([Label:drs(D,Labels)|Cons],Stream):- !,
   format(Stream,'  <drs label="~p">~n',[Label]),
   dom2xml(D,Stream,3),
   labels2xml(Labels,Stream),
   format(Stream,'  </drs>~n',[]),
   cons2xml(Cons,Stream).

cons2xml([Label:Index:named(Arg,X,Type,_)|Cons],Stream):- !,
   symbol(X,Y),
   format(Stream,'  <named label="~p" arg="~p" symbol="~w" type="~p">~n',[Label,Arg,Y,Type]),
   index2xml(Index,Stream,2),
   format(Stream,'  </named>~n',[]),
   cons2xml(Cons,Stream).

cons2xml([Label:Index:pred(Arg,X,Type,Sense)|Cons],Stream):- !,
   symbol(X,Y),
   format(Stream,'  <pred label="~p" arg="~p" symbol="~w" type="~p" sense="~p">~n',[Label,Arg,Y,Type,Sense]),
   index2xml(Index,Stream,2),
   format(Stream,'  </pred>~n',[]), 
   cons2xml(Cons,Stream).

cons2xml([Label:Index:rel(Arg1,Arg2,X,Sense)|Cons],Stream):- !,
   symbol(X,Y),
   format(Stream,'  <rel label="~p" arg1="~p" arg2="~p" symbol="~w" sense="~p">~n',[Label,Arg1,Arg2,Y,Sense]),
   index2xml(Index,Stream,2),
   format(Stream,'  </rel>~n',[]), 
   cons2xml(Cons,Stream).

cons2xml([Label:Index:card(X,Y,Type)|Cons],Stream):- !,
   format(Stream,'  <card label="~p" arg="~p" value="~p" type="~p">~n',[Label,X,Y,Type]),
   index2xml(Index,Stream,2),
   format(Stream,'  </card>~n',[]), 
   cons2xml(Cons,Stream).

cons2xml([Label:Index:timex(X,Y)|Cons],Stream):- !,
   format(Stream,'  <timex label="~p" arg="~p">~n',[Label,X]),
   timex2xml(Y,Stream,2),
   index2xml(Index,Stream,2),
   format(Stream,'  </timex>~n',[]),
   cons2xml(Cons,Stream).

cons2xml([Label:Index:eq(X,Y)|Cons],Stream):- !,
   format(Stream,'  <eq label="~p" arg1="~p" arg2="~p">~n',[Label,X,Y]),
   index2xml(Index,Stream,2),
   format(Stream,'  </eq>~n',[]),
   cons2xml(Cons,Stream).

cons2xml([Label:Index:not(L)|Cons],Stream):- !,
   format(Stream,'  <not label="~p">~n',[Label]),
   format(Stream,'  <label>~p</label>~n',[L]),
   index2xml(Index,Stream,2),
   format(Stream,'  </not>~n',[]),
   cons2xml(Cons,Stream).

cons2xml([Label:Index:nec(L)|Cons],Stream):- !,
   format(Stream,'  <nec label="~p">~n',[Label]),
   format(Stream,'  <label>~p</label>~n',[L]),
   index2xml(Index,Stream,2),
   format(Stream,'  </nec>~n',[]),
   cons2xml(Cons,Stream).

cons2xml([Label:Index:pos(L)|Cons],Stream):- !,
   format(Stream,'  <pos label="~p">~n',[Label]),
   format(Stream,'  <label>~p</label>~n',[L]),
   index2xml(Index,Stream,2),
   format(Stream,'  </pos>~n',[]),
   cons2xml(Cons,Stream).

cons2xml([Label:Index:prop(X,L)|Cons],Stream):- !,
   format(Stream,'  <prop label="~p" argument="~p">~n',[Label,X]),
   format(Stream,'  <label>~p</label>~n',[L]),
   index2xml(Index,Stream,2),
   format(Stream,'  </prop>~n',[]),
   cons2xml(Cons,Stream).

cons2xml([Label:Index:or(L1,L2)|Cons],Stream):- !,
   format(Stream,'  <or label="~p">~n',[Label]),
   format(Stream,'  <label>~p</label>~n  <label>~p</label>~n',[L1,L2]),
   index2xml(Index,Stream,2),
   format(Stream,'  </or>~n',[]),
   cons2xml(Cons,Stream).

cons2xml([Label:Index:imp(L1,L2)|Cons],Stream):- !,
   format(Stream,'  <imp label="~p">~n',[Label]),
   format(Stream,'  <label>~p</label>~n  <label>~p</label>~n',[L1,L2]),
   index2xml(Index,Stream,2),
   format(Stream,'  </imp>~n',[]),
   cons2xml(Cons,Stream).

cons2xml([Label:Index:whq(_,L1,_,L2)|Cons],Stream):- !,
   cons2xml([Label:Index:whq(L1,L2)|Cons],Stream).

cons2xml([Label:Index:whq(L1,L2)|Cons],Stream):- !,
   format(Stream,'  <whq label="~p">~n',[Label]),
   format(Stream,'  <label>~p</label>~n  <label>~p</label>~n',[L1,L2]),
   index2xml(Index,Stream,2),
   format(Stream,'  </whq>~n',[]),
   cons2xml(Cons,Stream).


/*========================================================================
   Labels
========================================================================*/

labels2xml([],_).

labels2xml([Label|L],Stream):-
   format(Stream,'   <label>~w</label>~n',[Label]),
   labels2xml(L,Stream).


/*========================================================================
   Indexes
========================================================================*/

index2xml(I,Stream,Tab):-
   tab(Stream,Tab), format(Stream,'<indexlist>~n',[]),
   index2xml2(I,Stream,Tab),
   tab(Stream,Tab), format(Stream,'</indexlist>~n',[]).

index2xml2([],_,_):- !.

index2xml2([X|L],Stream,Tab):-
   number(X), !,
   Pos is mod(X,1000),
   tab(Stream,Tab), format(Stream,'<index pos="~p">i~p</index>~n',[Pos,X]),
   index2xml2(L,Stream,Tab).

index2xml2([_|L],Stream,Tab):-
   index2xml2(L,Stream,Tab).


/*========================================================================
   Deal with special symbols
========================================================================*/

symbol(S1,S2):-
   name(S1,C1),
   check(C1,C2),
   name(S2,C2).

check([],[]).

%%% Special character &
%%%
check([38|L1],[38,97,109,112,59|L2]):- !,
   check(L1,L2).

%%% Special character <
%%%
check([60|L1],[38,108,116,59|L2]):- !,
   check(L1,L2).

%%% Special character >
%%%
check([62|L1],[38,103,116,59|L2]):- !,
   check(L1,L2).

%%% Special character '
%%%
check([62|L1],[38,97,112,111,115,59|L2]):- !,
   check(L1,L2).

%%% Special character "
%%%
check([34|L1],[38,113,117,111,116,59|L2]):- !,
   check(L1,L2).

check([X|L1],[X|L2]):-
   check(L1,L2).

   

