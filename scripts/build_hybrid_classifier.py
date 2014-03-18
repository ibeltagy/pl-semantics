#!/usr/bin/env python

import sys
import argparse

def read_mln_output(directory):
    resultsf = open(directory + "/result")
    for line in resultsf:
        if line.startswith("Errors found: "):
            numErrors = int(line.split(" ")[-1])
            if numErrors > 0:
                sys.stderr.write("Results from directory '%s' has %d errors. You must fix them first.\n" % numErrors)
                sys.exit(1)
        if line.startswith("[ "):
            results = line.strip()[2:-1]
            results = results.split(" ")
            results = [map(float, r.split(",")) for r in results]
            return results

    sys.stderr.write("Couldn't find the results line in '%s/result'. Double check it's a valid directory and you've run collect.\n")
    sys.exit(1)

def output_train_arff(feats, golds, outf):
    outf.write("@relation rte\n")
    for i, f in enumerate(feats[0], 1):
        outf.write("@attribute act%d real\n" % i)
    outf.write("@attribute gt {0,0.5,1}\n")
    outf.write("@data\n")
    for x, y in zip(feats, golds):
        outf.write(",".join(map(str, x)) + "," + y + "\r\n")
    

def output_test_arff(feats, outf):
    pass

def main():
    parser = argparse.ArgumentParser("Takes in a bunch of system outputs and builds an ensemble arff file.")
    parser.add_argument('--mln', '-m', nargs='+', help="Take input from a MLN's system output.")
    parser.add_argument('--gold', '-g', help="Gold labels file.")
    args = parser.parse_args()

    if args.gold:
        golds = [l.strip() for l in open(args.gold)]

    mln_feats = [read_mln_output(m) for m in args.mln]
    num_feats = [len(m) for m in mln_feats]
    if not all(x == num_feats[0] for x in num_feats):
        sys.stderr.write("Not all inputs have the same number of rows. Check the inputs were all trained on the same data.\n")
        sys.exit(1)

    all_feats = [reduce(lambda x, y: x + y, x) for x in zip(*mln_feats)]

    output_train_arff(all_feats, golds, sys.stdout)




if __name__ == '__main__':
    main()


