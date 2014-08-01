
:- module(ne,[neClass/2,neClassType/3]).

neClassType('I-LOC',geo,nam):- !.
neClassType('B-LOC',geo,nam):- !.
neClassType('E-LOC',geo,nam):- !.

neClassType('I-ORG',org,nam):- !.
neClassType('B-ORG',org,nam):- !.
neClassType('E-ORG',org,nam):- !.

neClassType('I-PER',per,nam):- !.
neClassType('B-PER',per,nam):- !.
neClassType('E-PER',per,nam):- !.

neClassType('I-DAT',tim,nam):- !.
neClassType('B-DAT',tim,nam):- !.
neClassType('E-DAT',tim,nam):- !.

neClassType('I-TIM',tim,nam):- !.
neClassType('B-TIM',tim,nam):- !.
neClassType('E-TIM',tim,nam):- !.

neClassType('Person',per,nam):- !.
neClassType('Organization',org,nam):- !.
neClassType('Location',geo,nam):- !.
neClassType('Artifact',art,nam):- !.
neClassType('Event',eve,nam):- !.
neClassType('Natural_Object',nat,nam):- !.
neClassType('Time',tim,nam):- !.
neClassType('GPE',gpe,nam):- !.

neClassType(N,Class,Type):- atom(N), atomic_list_concat([Class,Type],'-',N), !.
neClassType(_,nam,nam).

neClass(N,C):- neClassType(N,C,_).
