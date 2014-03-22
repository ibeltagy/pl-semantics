#!/usr/bin/env python
import sys

def tokenize(sent):
    sent = sent.replace(".", " .")
    sent = sent.replace(",", " ,")
    sent = sent.replace("n't", " n't")
    sent = sent.replace("'s", " 's")
    sent = sent.replace("  ", " ")
    return sent.split(" ")

if __name__ == '__main__':
    for line in sys.stdin:
        print " ".join(tokenize(line.strip()))

