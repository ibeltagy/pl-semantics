
:- module(ne,[netype/2]).

netype('I-LOC',loc).
netype('B-LOC',loc).
netype('E-LOC',loc).

netype('I-ORG',org).
netype('B-ORG',org).
netype('E-ORG',org).

netype('I-PER',per).
netype('B-PER',per).
netype('E-PER',per).

netype('I-DAT',tim).
netype('B-DAT',tim).
netype('E-DAT',tim).

netype('I-TIM',tim).
netype('B-TIM',tim).
netype('E-TIM',tim).

netype('Person',per).
netype('Organization',org).
netype('Location',loc).
netype('Artifact',art).
netype('Event',eve).
netype('Natural_Object',nat).
netype('Time',tim).
netype('GPE',gpe).

netype(_,nam).
