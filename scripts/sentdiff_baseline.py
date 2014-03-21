#!/usr/bin/env python

import argparse
import sys
import difflib
import operator
from util import tokenize
from vector_addn_baseline import norm, cosine, load_vectorspace
from vector_addn_baseline import extract_features as addn_extract_features

def find_difference(left, right):
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

    return left_pieces, right_pieces

def extract_features(left_pieces, right_pieces, vectorspace, combiner):
    left_pieces = reduce(operator.add, left_pieces)
    right_pieces = reduce(operator.add, right_pieces)
    return addn_extract_features(left_pieces, right_pieces, vectorspace, combiner)

def main():
    parser = argparse.ArgumentParser("Creates a result file indicating the sentence-difference baseline.")
    parser.add_argument("--vectorspace", "-v", help="Vector space file.")
    parser.add_argument("--sentences", "-s", help="The appropriate .txt file in resources.")
    parser.add_argument("--vectormaker", choices=("add", "mul"), default="add", help="Use vector addition or multiplication.")
    args = parser.parse_args()

    if args.vectormaker == "add":
        combiner = operator.add
    elif args.vectormaker == "mul":
        combiner = operator.mul
    else:
        sys.stderr.write("Don't know what to do with vectormaker '%s'." % args.vectormaker)
        sys.exit(1)

    sentence_pairs = [l.strip().lower().split("\t") for l in open(args.sentences)]
    tokenized_sentences = [(tokenize(left), tokenize(right)) for left, right in sentence_pairs]

    vectorspace = load_vectorspace(args.vectorspace)

    sys.stdout.write("Errors found: 0\n")
    sys.stdout.write("[")
    for left_sent, right_sent in tokenized_sentences:
        left_pieces, right_pieces = find_difference(left_sent, right_sent)
        features = extract_features(left_pieces, right_pieces, vectorspace, combiner)
        sys.stdout.write(" " + ",".join(map(str, features)))
    sys.stdout.write("]\n")





if __name__ == '__main__':
    main()


