
:- module(tuples,[tuples/4,write_tuples/2,
                  drs2owl/2,write_rdf/2,dot_rdf/2,
                  drs2rdf/2,
                  trees/2,write_trees/2]).

:- use_module(semlib(drs2tacitus),[label/4]).
:- use_module(semlib(errors),[warning/2]).
:- use_module(library(lists),[member/2,append/3,select/3]).


/* =======================================================================
   Main: OWL format
======================================================================= */

drs2owl(DRS,RDF):-
   numbervars(DRS,100,_),
   tree(DRS,Tree,[],Trees),
   tree2rdf([x0:Tree|Trees],RDF).


/* =======================================================================
   Main: trees
======================================================================= */

trees(DRS,[x0:Tree|Trees]):-
   numbervars(DRS,1,_),
   tree(DRS,Tree,[],Trees).


/* =======================================================================
   Convert DRSs into trees
======================================================================= */

tree(merge(B1,B2),Tree,P1,P3):- !, 
   tree(B1,T1,P1,P2), tree(B2,T2,P2,P3), 
   combine_trees(merge,T1,T2,Tree).

tree(smerge(B1,B2),Tree,P1,P3):- !, 
   tree(B1,T1,P1,P2), tree(B2,T2,P2,P3),
   combine_trees(smerge,T1,T2,Tree).

tree(alfa(_,B1,B2),Tree,P1,P3):- !, 
   tree(B1,T1,P1,P2), tree(B2,T2,P2,P3),
   combine_trees(alfa,T1,T2,Tree).

tree(drs(_,[C1]),T1,P1,P2):- !, 
   tree(C1,T1,P1,P2).

tree(drs(D,[C1,C2|L]),Tree,P1,P3):- !, 
   tree(C1,T1,P1,P2),tree(drs(D,[C2|L]),T2,P2,P3),
   combine_trees(and,T1,T2,Tree).


/* =======================================================================
   Convert DRS-conditions into trees
======================================================================= */

tree(_:rel(A,B,Sym,_Sense),t(rel,[Sym,A,B]),P,P):- !.
tree(_:eq(A,B),t(rel,[eq,A,B]),P,P):- !.
tree(_:named(X,Sym,T,_),nil,P1,P2):- !, extend_tree(X,t(named,[Sym,T]),P1,P2).
tree(_:pred(X,Sym,T,_),nil,P1,P2):- !, extend_tree(X,t(pred,[Sym,T]),P1,P2).
tree(_:timex(X,T),nil,P1,P2):- !, extend_tree(X,t(pred,[timex,T]),P1,P2).
tree(_:card(X,C,T),nil,P1,P2):- number(C), !, extend_tree(X,t(card,[C,T]),P1,P2).
tree(_:card(X,C,eq),t(rel,[card,X,C]),P1,P2):- !, extend_tree(C,t(pred,[number,n]),P1,P2).
tree(_:prop(X,B),t(prop,[X,T]),P1,P2):- !, tree(B,T,P1,P2).
tree(_:not(B),t(not,[T]),P1,P2):- !, tree(B,T,P1,P2).
tree(_:nec(B),t(nec,[T]),P1,P2):- !, tree(B,T,P1,P2).
tree(_:pos(B),t(pos,[T]),P1,P2):- !, tree(B,T,P1,P2).
tree(_:imp(B1,B2),t(imp,[T1,T2]),P1,P2):- !, tree(B1,T1,P1,P3), tree(B2,T2,P3,P2).
tree(_:or(B1,B2),t(or,[T1,T2]),P1,P2):- !, tree(B1,T1,P1,P3), tree(B2,T2,P3,P2).
tree(_:whq(_,B1,X,B2),t(whq,[T1,X,T2]),P1,P2):- !, tree(B1,T1,P1,P3), tree(B2,T2,P3,P2).
tree(_:_,nil,P,P).


/* =======================================================================
   Combine two trees
======================================================================= */

combine_trees(_,nil,T,T):- !.
combine_trees(_,T,nil,T):- !.
combine_trees(Lab,T1,T2,t(Lab,[T1,T2])).


/* =======================================================================
   Extend a tree
======================================================================= */

extend_tree(Name,Tree,P1,P3):-
   select(Name:Old,P1,P2), !,
   P3 = [Name:t(and,[Old,Tree])|P2].

extend_tree(X,Tree,P,[X:Tree|P]).


/* =======================================================================
   Output a tree
======================================================================= */

write_trees([],Stream):- nl(Stream).

write_trees([X:Tree|L],Stream):- 
   write_tree(X,1,Stream), 
   write(Stream,' = '), nl(Stream),
   write_tree(Tree,6,Stream), 
   write(Stream,'.'), nl(Stream),
   write_trees(L,Stream).

write_tree(t(Label,Branches),Tab,Stream):- !,
   tab(Stream,Tab), write(Stream,Label), 
   write(Stream,'('), nl(Stream),
   name(Label,Chars), length(Chars,Len),
   NewTab is Tab + Len + 2,
   write_branches(Branches,NewTab,Stream).

write_tree(Var,Tab,Stream):-
   functor(Var,'$VAR',1), !,
   arg(1,Var,N), 
   tab(Stream,Tab), 
   write(Stream,x), 
   write(Stream,N).

write_tree(Tree,Tab,Stream):-
   tab(Stream,Tab), 
   write(Stream,Tree).

write_branches([],_,_).

write_branches([T],Tab,Stream):-
   write_tree(T,Tab,Stream), 
   write(Stream,' )').

write_branches([T|Branches],Tab,Stream):-
   write_tree(T,Tab,Stream), 
   write(Stream,','), nl(Stream),
   write_branches(Branches,Tab,Stream).


/* =======================================================================
   Translate a tree in RDF format
======================================================================= */

tree2rdf(T,rdf(Nodes,Edges,Out)):- 
   tree2nodes(T,200,[]-Nodes1,[]-Edges1,[]-Out),
   checkNodes(Nodes1,Edges1,Nodes2),
   elimEquality(Edges1,Nodes2,Edges,Nodes).

tree2nodes([],_,N-N,E-E,O-O).

tree2nodes([I:Tree|L],I1,N1-[node(I,'#')|N3],E1-E3,O1-O3):-
   tree2node(Tree,I,I1-I2,N1-N2,E1-E2,O1-O2),
   tree2nodes(L,I2,N2-N3,E2-E3,O2-O3).


/* =======================================================================
   Check validity of nodes (remove dangline nodes)
======================================================================= */

checkNodes([],_,[]).

checkNodes([node(I,V)|L1],Edges,[node(I,V)|L2]):-
   member(edge(_,_,I,_), Edges), !,
   checkNodes(L1,Edges,L2).

checkNodes([node(I,V)|L1],Edges,[node(I,V)|L2]):-
   member(edge(_,_,_,I), Edges), !,
   checkNodes(L1,Edges,L2).

checkNodes([_|L1],Edges,L2):-
   checkNodes(L1,Edges,L2).


elimEquality(Edges1,Nodes1,Edges4,Nodes3):-
   select(edge(_,eq,I1,I2),Edges1,Edges2),
   select(node(I1,'#'),Nodes1,Nodes2),
   member(node(I2,'#'),Nodes2), !,
   subEdges(Edges2,[I1=I2],Edges3),
   elimEquality(Edges3,Nodes2,Edges4,Nodes3).

elimEquality(Edges1,Nodes1,Edges3,Nodes3):-
   select(node(I1,Lab),Nodes1,Nodes2), \+ Lab = '#',
   member(node(I2,Lab),Nodes2), !,
   subEdges(Edges1,[I1=I2],Edges2),
   elimEquality(Edges2,Nodes2,Edges3,Nodes3).

elimEquality(Edges,Nodes,Edges,Nodes).



subEdges([],_,[]).

subEdges([edge(Id,Lab,I1,I2)|L1],I,[edge(Id,Lab,I3,I2)|L2]):-
   member(I1=I3,I), !,
   subEdges(L1,I,L2).

subEdges([edge(Id,Lab,I1,I2)|L1],I,[edge(Id,Lab,I1,I3)|L2]):-
   member(I2=I3,I), !,
   subEdges(L1,I,L2).

subEdges([edge(Id,Lab,I1,I2)|L1],I,[edge(Id,Lab,I1,I2)|L2]):-
   subEdges(L1,I,L2).


/* =======================================================================
   Translate a single node in RDF format
======================================================================= */

tree2node(t(pred,[event,n]),_,I-I,N-N,E-E,O-O):- !.

tree2node(t(pred,[Pred,n]),I,I1-I3,N-[node(I2,Pred)|N],E-[edge(I3,isa,I,I2)|E],O-O):- !, I2 is I1 + 1, I3 is I2 + 1.

tree2node(t(pred,[Pred,a]),I,I1-I3,N-[node(I2,Pred)|N],E-[edge(I3,att,I,I2)|E],O-O):- !, I2 is I1 + 1, I3 is I2 + 1.

tree2node(t(pred,[Pred,v]),I,I0-I4,N-[node(I1,Pred),node(I2,event)|N],E-[edge(I3,isa,I,I1),edge(I4,isa,I1,I2)|E],O-O):- !, I1 is I0 + 1, I2 is I1 + 1, I3 is I2 + 1, I4 is I3 + 1.

tree2node(t(card,[Pred,Num]),I,I0-I4,N-[node(I1,Num),node(I2,Pred)|N],E-[edge(I3,cardinality,I,I2),edge(I4,number,I2,I1)|E],O-O):- !, I1 is I0 + 1, I2 is I1 + 1, I3 is I2 + 1, I4 is I3 + 1.

tree2node(t(named,[Name,NE]),I,I0-I4,N-[node(I1,Name),node(I2,Label)|N],E-[edge(I3,isa,I,I2),edge(I4,name,I,I1)|E],O-O):- !, I1 is I0 + 1, I2 is I1 + 1, I3 is I2 + 1, I4 is I3 + 1, ne2label(NE,Label).

tree2node(t(rel,[Rel,A1,A2]),_,I1-I2,N-N,E-[edge(I2,Rel,A1,A2)|E],O-O):- !, I2 is I1 + 1.

tree2node(t(not,[A]),I,I1-I2,N,E,O):- !, tree2node(A,I,I1-I2,N,E,O).

tree2node(t(prop,[_,A]),I,I1-I2,N,E,O):- !, tree2node(A,I,I1-I2,N,E,O).

tree2node(t(Op,[A1,A2]),I,I1-I3,N1-N3,E1-E3,O1-O3):- 
   member(Op,[and,imp,or,smerge,merge,alfa]), !,
   tree2node(A1,I,I1-I2,N1-N2,E1-E2,O1-O2),
   tree2node(A2,I,I2-I3,N2-N3,E2-E3,O2-O3).

tree2node(t(whq,[A1,X,A2]),I,I0-I4,N1-[node(I1,'#')|N3],E1-[edge(I2,answer,X,I1)|E3],O1-[I1|O3]):- !,
   I1 is I0 + 1, I2 is I1 + 1,
   tree2node(A1,I,I2-I3,N1-N2,E1-E2,O1-O2),
   tree2node(A2,I,I3-I4,N2-N3,E2-E3,O2-O3).

tree2node(nil,_,I-I,N-N,E-E,O-O).


/* =======================================================================
   Translating NE to label
======================================================================= */

ne2label(loc,location).
ne2label(per,person).
ne2label(ttl,title).
ne2label(nam,entity).
ne2label(org,organisation).
ne2label(quo,quotation).


/* =======================================================================
   Output a tree in RDF format
======================================================================= */

write_rdf(rdf(Nodes,Edges,Output),Stream):- 
   write(Stream,'   <nodes>'),nl(Stream),
   write_nodes(Nodes,Stream),
   write(Stream,'   <edges>'),nl(Stream),
   write_edges(Edges,Stream),
   write(Stream,'   <output>'),nl(Stream),
   write_output(Output,Stream).


/* =======================================================================
   Write nodes in RDF
======================================================================= */

write_nodes([],Stream):-
   write(Stream,'   </nodes>'),nl(Stream).

write_nodes([node(Id,Val)|L],Stream):- 
   write_node(Stream,Id,Val),
   write_nodes(L,Stream).


/* =======================================================================
   Write edges in RDF
======================================================================= */

write_edges([],Stream):- 
   write(Stream,'   </edges>'), nl(Stream).

write_edges([edge(Id,Lab,N1,N2)|L],Stream):-
   format(Stream,'      <edge_element>~n',[]),
   format(Stream,'         <id>@~p</id>~n',[Id]),
   format(Stream,'         <node_start>',[]),
   write_id(N1,Stream),
   format(Stream,'</node_start>~n',[N1]),
   format(Stream,'         <node_end>',[]),
   write_id(N2,Stream),
   format(Stream,'</node_end>~n',[N2]),
   format(Stream,'         <label>~p</label>~n',[Lab]),
   format(Stream,'      </edge_element>~n',[]),
   write_edges(L,Stream).

/* =======================================================================
   Write output in RDF
======================================================================= */

write_output([],Stream):- 
   write(Stream,'   </output>'), nl(Stream).

write_output([Node|L],Stream):-
   format(Stream,'         <id>',[]),
   write_id(Node,Stream),
   format(Stream,'</id>~n',[Node]), 
   write_output(L,Stream).


/* =======================================================================
   Write a single node in RDF
======================================================================= */

write_node(Stream,Node,Label):-
   format(Stream,'      <node_element>~n',[]),
   format(Stream,'         <id>',[]),
   write_id(Node,Stream),
   format(Stream,'</id>~n',[]), 
   format(Stream,'         <label>~p</label>~n',[Label]),
   format(Stream,'         <operator>=</operator>~n',[]),
   format(Stream,'      </node_element>~n',[]).


/* =======================================================================
   Write identifier in RDF
======================================================================= */

write_id(Var,Stream):-
   functor(Var,'$VAR',1), !,
   arg(1,Var,N), 
   write(Stream,'#'), 
   write(Stream,N).

write_id(Var,Stream):-
   write(Stream,'$'), 
   write(Stream,Var).


/* =======================================================================
   Output RDF in DOT format
======================================================================= */

dot_rdf(rdf(Nodes,Edges,Output),Stream):- 
   dot_nodes(Nodes,Output,Stream),
   dot_edges(Edges,Stream),
   write(Stream,'}'), nl(Stream),
   nl(Stream).


/* =======================================================================
   Dot nodes in RDF
======================================================================= */

dot_nodes([],_,Stream):- nl(Stream).

dot_nodes([node(Id,Val)|L],Output,Stream):- 
   dot_node(Stream,Id,Val,Output),
   dot_nodes(L,Output,Stream).


/* =======================================================================
   Dot edges in RDF
======================================================================= */

dot_edges([],Stream):- nl(Stream).

dot_edges([edge(N1,Label,N2)|L],Stream):- !,
   dot_edges([edge(_,Label,N1,N2)|L],Stream).

dot_edges([edge(_Id,Label,N1,N2)|L],Stream):-
   tab(Stream,5), 
   dot_id(N1,Stream),
   write(Stream,' -> '),
   dot_id(N2,Stream),
   format(Stream,' [label="~p"];~n',[Label]),
   dot_edges(L,Stream).


/* =======================================================================
   Dot a single node in RDF
======================================================================= */

dot_node(Stream,Node,Label,Output):-
   member(Node,Output), !,
   tab(Stream,5), 
   dot_id(Node,Stream),
   format(Stream,' [label="~p", peripheries=2];~n',[Label]).

dot_node(Stream,Node,Label,_):-
   tab(Stream,5), 
   dot_id(Node,Stream),
   format(Stream,' [label="~p"];~n',[Label]).


/* =======================================================================
   Dot identifier in RDF
======================================================================= */

dot_id(Var,Stream):-
   functor(Var,'$VAR',1), !,
   arg(1,Var,N), 
   write(Stream,N).

dot_id(Var,Stream):-
   write(Stream,Var).


/* =======================================================================
   Main: tuples
======================================================================= */

tuples(Words,DRS,N1,Tuples):- label(N1,k,K0,N2) ,tuples(DRS,K0,Words,[]-Tuples,N2-_).

tuples(sdrs(D,R),K,W,T1-T3,N1-N3):- !, tuples(D,K,W,T1-T2,N1-N2), tuples(R,K,W,T2-T3,N2-N3).
tuples(smerge(B1,B2),K,W,T1-T3,N1-N3):- !, tuples(B1,K,W,T1-T2,N1-N2), tuples(B2,K,W,T2-T3,N2-N3).
tuples(merge(B1,B2),K,W,T1-T3,N1-N3):- !, tuples(B1,K,W,T1-T2,N1-N2), tuples(B2,K,W,T2-T3,N2-N3).
tuples(alfa(_,B1,B2),K,W,T1-T3,N1-N3):- !, tuples(B1,K,W,T1-T2,N1-N2), tuples(B2,K,W,T2-T3,N2-N3).

tuples(lab(K,B),_,W,T1-T2,N1-N2):- !, tuples(B,K,W,T1-T2,N1-N2).
tuples(sub(B1,B2),K,W,T1-T3,N1-N3):- !, tuples(B1,K,W,T1-T2,N1-N2), tuples(B2,K,W,T2-T3,N2-N3).
tuples(drs([],C),K,W,T1-T2,N1-N2):- !, tuples(C,K,W,T1-T2,N1-N2).

tuples(drs([I:R|L],C),K,W,T1-[T|T2],N1-N2):- !,
    word(I,W,Word,NI), !,
    T = tuple(NI,s0,R,referent,K,0,Word),
    tuples(drs(L,C),K,W,T1-T2,N1-N2).

tuples([lab(A,B)|L],K,W,T1-T3,N1-N3):- !, tuples(lab(A,B),K,W,T1-T2,N1-N2),tuples(L,K,W,T2-T3,N2-N3).

tuples([sub(A,B)|L],K,W,T1-T3,N1-N3):- !, tuples(sub(A,B),K,W,T1-T2,N1-N2),tuples(L,K,W,T2-T3,N2-N3).

tuples([I:pred(X,Sym,n,Sense)|L],K,W,T1-[T|T2],N1-N2):-
    word(I,W,Word,NI), !,
    T = tuple(NI,K,X,concept,Sym,Sense,Word),
    tuples(L,K,W,T1-T2,N1-N2).

tuples([I:pred(X,Sym,v,Sense)|L],K,W,T1-[T|T2],N1-N2):- 
    word(I,W,Word,NI), !,
    T = tuple(NI,K,X,event,Sym,Sense,Word),
    tuples(L,K,W,T1-T2,N1-N2).

tuples([I:pred(X,Sym,a,Sense)|L],K,W,T1-[T|T2],N1-N2):- 
    word(I,W,Word,NI), !,
    T = tuple(NI,K,X,attribute,Sym,Sense,Word),
    tuples(L,K,W,T1-T2,N1-N2).

tuples([I:eq(X,Y)|L],K,W,T1-[T|T2],N1-N2):- 
    word(I,W,Word,NI), !,
    T = tuple(NI,K,X,equality,Y,1,Word),
    tuples(L,K,W,T1-T2,N1-N2).

tuples([I:named(X,Sym,Type,_)|L],K,W,T1-[T|T2],N1-N2):- 
    word(I,W,Word,NI), !,
    T = tuple(NI,K,X,named,Sym,Type,Word),
    tuples(L,K,W,T1-T2,N1-N2).

tuples([_:timex(X,date(_,I2:Y,I3:M,I4:D))|L],K,W,T,N):- !,
    tuples([I2:year(X,Y),I3:month(X,M),I4:day(X,D)|L],K,W,T,N).

tuples([I:year(X,Sym)|L],K,W,T1-[T|T2],N1-N2):- 
    word(I,W,Word,NI), !, Sense = 1,
    T = tuple(NI,K,X,year,Sym,Sense,Word),
    tuples(L,K,W,T1-T2,N1-N2).

tuples([I:month(X,Sym)|L],K,W,T1-[T|T2],N1-N2):- 
    word(I,W,Word,NI), !, Sense = 1,
    T = tuple(NI,K,X,month,Sym,Sense,Word),
    tuples(L,K,W,T1-T2,N1-N2).

tuples([I:day(X,Sym)|L],K,W,T1-[T|T2],N1-N2):- 
    word(I,W,Word,NI), !, Sense = 1,
    T = tuple(NI,K,X,day,Sym,Sense,Word),
    tuples(L,K,W,T1-T2,N1-N2).

tuples([I:rel(X,Y,Sym,Sense)|L],K,W,T1-[T|T2],N1-N2):-
    word(I,W,Word,NI), !,
    T = tuple(NI,K,X,Sym,Y,Sense,Word),
    tuples(L,K,W,T1-T2,N1-N2).

tuples([I:rel(X,Y,Sym)|L],K,W,T1-[T|T2],N1-N2):-
    word(I,W,Word,NI), !, Sense = 1,
    T = tuple(NI,K,X,Sym,Y,Sense,Word),
    tuples(L,K,W,T1-T2,N1-N2).

tuples([I:card(X,Y,Type)|L],K,W,T1-[T|T2],N1-N2):-
    word(I,W,Word,NI), !, 
    T = tuple(NI,K,X,card,Y,Type,Word),
    tuples(L,K,W,T1-T2,N1-N2).

tuples([I:prop(X,B)|L],K,W,T1-[T|T3],N1-N3):-
    word(I,W,Word,NI), !,
    T = tuple(NI,s0,K,subordinates,X,proposition,Word),
    tuples(B,X,W,T1-T2,N1-N2),
    tuples(L,K,W,T2-T3,N2-N3).

tuples([I:not(B)|L],K1,W,T1-[T|T3],N1-N4):-
    word(I,W,Word,NI), !,
    T = tuple(NI,s0,K1,subordinates,K2,negation,Word),
    label(N1,k,K2,N2),
    tuples(B,K2,W,T1-T2,N2-N3),
    tuples(L,K1,W,T2-T3,N3-N4).

tuples([I:pos(B)|L],K1,W,T1-[T|T3],N1-N4):-
    word(I,W,Word,NI), !,
    T = tuple(NI,s0,K1,subordinates,K2,possibility,Word),
    label(N1,k,K2,N2),
    tuples(B,K2,W,T1-T2,N2-N3),
    tuples(L,K1,W,T2-T3,N3-N4).

tuples([I:nec(B)|L],K1,W,T1-[T|T3],N1-N4):-
    word(I,W,Word,NI), !,
    T = tuple(NI,s0,K1,subordinates,K2,necessity,Word),
    label(N1,k,K2,N2),
    tuples(B,K2,W,T1-T2,N2-N3),
    tuples(L,K1,W,T2-T3,N3-N4).

tuples([I:imp(B1,B2)|L],K1,W,T1-[Tu1,Tu2|T4],N1-N6):-
    word(I,W,Word,NI), !,
    Tu1 = tuple(NI,s0,K1,subordinates,K2,conditional,Word),
    label(N1,k,K2,N2),
    tuples(B1,K2,W,T1-T2,N2-N3),
    Tu2 = tuple(NI,s0,K2,subordinates,K3,implication,Word),
    label(N3,k,K3,N4),
    tuples(B2,K3,W,T2-T3,N4-N5),
    tuples(L,K1,W,T3-T4,N5-N6).

tuples([I:whq(_,B1,_,B2)|L],K1,W,T1-[Tu1,Tu2|T4],N1-N6):-
    word(I,W,Word,NI), !,
    Tu1 = tuple(NI,s0,K1,subordinates,K2,question,Word),
    label(N1,k,K2,N2),
    tuples(B1,K2,W,T1-T2,N2-N3),
    Tu2 = tuple(NI,s0,K2,subordinates,K3,background,Word),
    label(N3,k,K3,N4),
    tuples(B2,K3,W,T2-T3,N4-N5),
    tuples(L,K1,W,T3-T4,N5-N6).

tuples([I:or(B1,B2)|L],K1,W,T1-[Tu1,Tu2|T4],N1-N6):-
    word(I,W,Word,NI), !,
    Tu1 = tuple(NI,s0,K1,subordinates,K2,disjunction,Word),
    label(N1,k,K2,N2),
    tuples(B1,K2,W,T1-T2,N2-N3),
    Tu2 = tuple(NI,s0,K2,coordinates,K3,disjunction,Word),
    label(N3,k,K3,N4),
    tuples(B2,K3,W,T2-T3,N4-N5),
    tuples(L,K1,W,T3-T4,N5-N6).

tuples([Err|L],K,W,T1-T2,N1-N2):- !, 
     warning('unknown tuple: ~p',[Err]),
     tuples(L,K,W,T1-T2,N1-N2).

tuples([],_,_,T-T,N-N).


/* =======================================================================
   Convert words in tuple format
======================================================================= */

word([],_,[],0):- !.
word([Index],W,[Word],Index):- member(word(Index,Word),W), !.
word(Index,_,[],0):- warning('unknown index: ~p',[Index]).


/* =======================================================================
   Output tuples to stream
======================================================================= */

write_tuples(Tuples,Stream):-
   sort(Tuples,Sorted),
   writeTuples(Sorted,Stream).

writeTuples([],_).

writeTuples([tuple(Order,Box,Ref,Edge,Node,Sense,Word)|L],Stream):- !,
   format(Stream,'~w ~w ~w ~w ~w ~w ~w~n',[Box,Ref,Edge,Node,Sense,Order,Word]),
   writeTuples(L,Stream).

writeTuples([T|L],Stream):-
   write(err:T),nl,
   writeTuples(L,Stream).



/* =======================================================================
   DRS in RDF format (work in progress, eventually replaces --flat option)
======================================================================= */

drs2rdf(DRS,rdf(Nodes,Sorted,[])):-
   drs2rdf(DRS,_,[]-E),
   setof(node(Var,'#'),Node^Rel^(member(edge(Var,Rel,Node),E),var(Var)),Nodes),
   sort(E,Sorted),
   numbervars(rdf(Nodes,Sorted),100,_).

drs2rdf(smerge(B1,B2),K,E1-[edge(K,instance,'MergedDrs')|E4]):-
   subClass('MergedDrs','Drs',E1-E2),
   drs2rdf(B1,K1,[edge(K,fstArg,K1)|E2]-E3),
   drs2rdf(B2,K2,[edge(K,scdArg,K2)|E3]-E4).

drs2rdf(drs(Dom,Conds),K,E1-E9):-
   subClass('Domain','FiniteSet',E1-E2),
   subClass('Conditions','FiniteSet',E2-E3),
   subClass('FiniteSet','Set',E3-E4),
   subClass('Set','Thing',E4-E5),
   subClass('BasicDrs','Drs',E5-E6),
   subClass('Drs','Thing',E6-E7),
   dom2rdf(Dom,D,[edge(K,hasDomain,D),edge(K,instance,'BasicDrs')|E7]-E8),
   conds2rdf(Conds,C,[edge(K,hasConditions,C),edge(C,instance,'Conditions')|E8]-E9).
   
dom2rdf([],K,E-[edge(K,instance,'Domain')|E]).

dom2rdf([_:R|Dom],K,E1-[edge(R,instance,'DiscourseReferent'),edge(R,setMember,K)|E3]):-
   subClass('DiscourseReferent','Thing',E1-E2),
   dom2rdf(Dom,K,E2-E3).

conds2rdf([],_,E-E).

conds2rdf([_:Cond|Conds],K,E1-[edge(C,setMember,K)|E4]):-
   subClass('DrsCondition','Thing',E1-E2),
   cond2rdf(Cond,C,E2-E3),
   conds2rdf(Conds,K,E3-E4).

cond2rdf(pred(X,Sym,Pos,Sense),C,E1-[T1,T2,T3,T4,T5|E8]):- !,
   T1 = edge(C,instance,'Predicate'),
   T2 = edge(C,hasSymbol,Sym),
   T3 = edge(C,hasArg,X),
   T4 = edge(C,hasPos,Pos),
   T5 = edge(C,hasSense,Sense),
   subClass(Sym,'Symbol',E1-E2),
   subClass('Symbol','Thing',E2-E3),
   subClass(Sense,'Integer',E3-E4),
   subClass('Integer','Thing',E4-E5),
   subClass(Pos,'PartOfSpeech',E5-E6),
   subClass('PartOfSpeech','Thing',E6-E7),
   subClass('Predicate','DrsCondition',E7-E8).

cond2rdf(named(X,Sym,_,_),C,E1-[T1,T2,T3|E4]):- !,
   T1 = edge(C,instance,'Named'),
   T2 = edge(C,hasSymbol,Sym),
   T3 = edge(C,hasArg,X),
   subClass(Sym,'Name',E1-E2),
   subClass('Name','Thing',E2-E3),
   subClass('Named','DrsCondition',E3-E4).

cond2rdf(rel(X,Y,Sym,Sense),C,E1-[T1,T2,T3,T4,T5|E6]):- !,
   T1 = edge(C,instance,'Relation'),
   T2 = edge(C,hasSymbol,Sym),
   T3 = edge(C,hasIntArgument,X),
   T4 = edge(C,hasExtArgument,Y),
   T5 = edge(C,hasSense,Sense),
   subClass(Sym,'Symbol',E1-E2),
   subClass('Symbol','Thing',E2-E3),
   subClass(Sense,'Integer',E3-E4),
   subClass('Integer','Thing',E4-E5),
   subClass('Relation','DrsCondition',E5-E6).

cond2rdf(imp(B1,B2),C,E1-[edge(C,instance,'Implication')|E4]):- !,
   subClass('Implication','DrsCondition',E1-E2),
   drs2rdf(B1,K1,[edge(K1,isAntecedent,C)|E2]-E3),
   drs2rdf(B2,K2,[edge(K2,isConsequent,C)|E3]-E4).

cond2rdf(or(B1,B2),C,E1-[edge(C,instance,'Disjunction')|E4]):- !,
   subClass('Disjunction','DrsCondition',E1-E2),
   drs2rdf(B1,K1,[edge(K1,isFstDisjunct,C)|E2]-E3),
   drs2rdf(B2,K2,[edge(K2,isSndDisjunct,C)|E3]-E4).

cond2rdf(_,_,E-E).


subClass(A,B,E-E):- member(edge(A,subClass,B),E), !.
subClass(A,B,E-[edge(A,subClass,B)|E]).
