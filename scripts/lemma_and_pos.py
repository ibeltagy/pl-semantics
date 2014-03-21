#!/usr/bin/env python

# usage:
# cat   resources/sick/sick-rte.txt| tr '\t' '\n' | candc/bin/candc  --models candc/models/boxer/  --candc-int-betas "0.075 0.03 0.01 0.005 0.001"| python scripts/lemma_and_pos.py

import sys

def filter_lines(lines):
    for line in lines:
        if line.startswith("w("):
            yield line

def parse_w_tuple(wstr):
    # example:
    # w(400, 15, 'gloves', 'glove', 'NNS', 'I-NP', 'O', 'N').
    tup = wstr[2:-1].split(", ")
    idn = int(tup[0])
    lemma = tup[3][1:-1]
    pos = tup[4][1:-1]
    return idn, lemma, pos

def nicepos(pos):
    np = pos[0].lower()
    if np == 'j':
        return 'a'
    else:
        return np

def group_lines(lines):
    grp = []
    gid = None
    for idn, lemma, pos in lines:
        if gid != idn:
            if grp:
                yield grp
                grp = []
            gid = idn
        grp.append(lemma + "-" + nicepos(pos))

    if grp:
        yield grp


lines = (l.strip() for l in sys.stdin)
lines = (l.replace("\\", "") for l in lines)
lines = filter_lines(lines)
lines = (parse_w_tuple(l) for l in lines)
sentences = group_lines(lines)
lemmatized = [" ".join(sentence).lower() for sentence in sentences]

assert len(lemmatized) % 2 == 0

while lemmatized:
    text = lemmatized.pop(0)
    hypo = lemmatized.pop(0)

    print "%s\t%s" % (text, hypo)



