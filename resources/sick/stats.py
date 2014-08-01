#!/usr/bin/env python

import difflib
import random
from collections import Counter

DATA = "rte"
#DATA = "sts"
#print DATA
TEXT = "sick-%s.txt" % DATA
STS_LABELS = "sick-sts.gs"
RTE_LABELS = "sick-rte.gs"

LABEL_BINS = 2

HIST_WIDTH = 50

txtf = open(TEXT)
labf1 = open(RTE_LABELS)
labf2 = open(STS_LABELS)

c = Counter()


def find_difference(left, right):
    intersect = set(left).intersection(set(right))
    sdiffleft = set(left).difference(intersect)
    sdiffright = set(right).difference(intersect)
    sdiff = sdiffleft.union(sdiffright)

    opcodes = difflib.SequenceMatcher(a=left, b=right).get_opcodes()

    left_pieces = []
    right_pieces = []
    for tag, i1, i2, j1, j2 in opcodes:
        if tag == 'equal':
            continue
        elif tag == 'replace':
            left_pieces.append(left[i1:i2])
            right_pieces.append(right[j1:j2])
        elif tag == 'insert':
            left_pieces.append([])
            right_pieces.append(right[j1:j2])
        elif tag == 'delete':
            left_pieces.append(left[i1:i2])
            right_pieces.append([])

    return zip(left_pieces, right_pieces)



def main():
    for sno, (sentences, label1, label2) in enumerate(zip(txtf, labf1, labf2)):
        if random.random() > .05:
            continue
        left, right = [s.split() for s in sentences.lower().rstrip().split("\t")]
        label1 = label1.strip()

        label2 = float(label2)
        # label2 = round(label2 * LABEL_BINS) / LABEL_BINS
        # label2 = "%3.1f" % label2

        print "Sentence #%d" % sno
        print "labels: %s  %.2f" % (label1, label2)
        print "annotations: "
        print " ".join(left)
        print " ".join(right)
        for i, (ldiff, rdiff) in enumerate(find_difference(left, right), 1):
            diffstr = "%3d  %30s    %-30s" % (i, " ".join(ldiff), " ".join(rdiff))
            diffstr = diffstr.rstrip()
            print diffstr

        print

        #c.update([len(find_difference(left, right))])
        #c.update([label])
        #c.update([len(diff)])

    total = float(sum(c.values()))
    for k in sorted(c.keys()):
        percent = c[k] / total
        width = int(HIST_WIDTH * percent)
        print "%-5s   %4d  %4.1f%%  %s" % (k, c[k], 100 * percent, "-" * width)

if __name__ == '__main__':
    main()

