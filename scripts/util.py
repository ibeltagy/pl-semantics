#!/usr/bin/env python

def tokenize(sent):
    sent = sent.replace(".", " .")
    sent = sent.replace(",", " ,")
    sent = sent.replace("n't", " n't")
    sent = sent.replace("'s", " 's")
    sent = sent.replace("  ", " ")
    return sent.split(" ")

