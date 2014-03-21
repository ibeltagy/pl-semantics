#!/usr/bin/env python

import sys
import argparse
import cPickle
import operator
import numpy as np
from util import tokenize

def norm(v):
    return v / np.sqrt(v.dot(v))

def cosine(v1, v2):
    return norm(v1).dot(norm(v2))

def load_vectorspace(filename):
    with open(filename) as f:
        return cPickle.load(f)

def extract_features(left_sent, right_sent, vectorspace, combiner):
    left_vectors = [vectorspace.get_row(w).mat.A[0] for w in left_sent if w in vectorspace.row2id]
    right_vectors = [vectorspace.get_row(w).mat.A[0] for w in right_sent if w in vectorspace.row2id]
    if not left_vectors and not right_vectors:
        return [1]
    elif not left_vectors or not right_vectors:
        return [0]
    left_vector = reduce(combiner, left_vectors)
    right_vector = reduce(combiner, right_vectors)
    return [cosine(left_vector, right_vector)]

def main():
    parser = argparse.ArgumentParser("Creates a result file indicating the vector addition baseline.")
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
        features = extract_features(left_sent, right_sent, vectorspace, combiner)
        sys.stdout.write(" " + ",".join(map(str, features)))
    sys.stdout.write("]\n")





if __name__ == '__main__':
    main()


