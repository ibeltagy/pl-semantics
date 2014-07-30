#!/usr/bin/env python

import sys
import argparse
import pandas as pd

def read_rte_preds(filename):
    RECORDING = False
    with open(filename) as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            if RECORDING:
                line = line.replace('+', ' ')
                inst, act, pred, foo = line.split()
                yield inst, pred[2:]
            if line.startswith("inst#"):
                RECORDING = True

def read_sts_preds(filename):
    RECORDING = False
    with open(filename) as f:
        for line in f:
            line = line.strip()
            if not line:
                continue
            if RECORDING:
                line = line.replace('+', ' ')
                inst, act, pred, foo = line.split()
                yield inst, pred
            if line.startswith("inst#"):
                RECORDING = True


RTE_TABLE = {
    '0': 'CONTRADICTION',
    '0.5': 'NEUTRAL',
    '1': 'ENTAILMENT'
}


def main():
    parser = argparse.ArgumentParser('Stitches the weka prediction outputs into the right format for the SEMEVAL-14 submission.')
    parser.add_argument('--test', help='Original SICK_test.txt file provided by SEMEVAL folks.')
    parser.add_argument('--rte', help='RTE prediction file.')
    parser.add_argument('--sts', help='STS prediction file.')
    args = parser.parse_args()

    rte_preds = list(read_rte_preds(args.rte))
    sts_preds =  list(read_sts_preds(args.sts))

    assert len(rte_preds) == len(sts_preds)

    table = pd.read_table(args.test)

    for (rinst, rpred), (sinst, spred) in zip(rte_preds, sts_preds):
        assert rinst == sinst

    table['entailment_judgment'] = [RTE_TABLE[rpred] for rinst, rpred in rte_preds]
    table['relatedness_score'] = [spred for sinst, spred in sts_preds]

    del table['sentence_A']
    del table['sentence_B']

    table.to_csv(sys.stdout, index=False, sep="\t")


if __name__ == '__main__':
    main()

