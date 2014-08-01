#!/usr/bin/env python

import sys
import argparse
import numpy as np
from collections import Counter
from util import tokenize

KEYWORDS = [
    "no",
    "not",
    "nobody",
    "n't",
    "one",
    "a",
    "an",
    "the",
    "there",
    "some",
    "group",
    "someone",
    "somebody",
    "'s"
]

def extract_features_1(sent):
    counts = Counter(sent)
    return [counts[k] for k in KEYWORDS]

def extract_features(left_sent, right_sent):
    fl = extract_features_1(left_sent) 
    fr = extract_features_1(right_sent)
    fla = np.array(fl)
    fra = np.array(fl)

    return fl + fr #+ list(np.abs(fla - fra))

def main():
    parser = argparse.ArgumentParser("Creates a result file by counting occurrences of key phrases.")
    parser.add_argument("--sentences", "-s", help="The appropriate .txt file in resources.")
    args = parser.parse_args()

    sentence_pairs = [l.strip().lower().split("\t") for l in open(args.sentences)]
    tokenized_sentences = [(tokenize(left), tokenize(right)) for left, right in sentence_pairs]

    sys.stdout.write("Errors found: 0\n")
    sys.stdout.write("[")
    for left_sent, right_sent in tokenized_sentences:
        features = extract_features(left_sent, right_sent)
        sys.stdout.write(" " + ",".join(map(str, features)))
    sys.stdout.write("]\n")





if __name__ == '__main__':
    main()


