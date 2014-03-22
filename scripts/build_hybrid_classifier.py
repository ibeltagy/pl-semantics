#!/usr/bin/env python

import os
import sys
import argparse

wekaJarPath="/u/beltagy/workspace/deft/weka/weka.jar"
libsvmPath="/u/beltagy/wekafiles/packages/LibSVM/lib/libsvm.jar:/u/beltagy/wekafiles/packages/LibSVM/LibSVM.jar"
weka_prefix = "/usr/bin/java -Xmx1024m -cp %s:%s " % (wekaJarPath, libsvmPath)

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

    sys.stderr.write("Couldn't find the results line in '%s/result'. Double check it's a valid directory and you've run collect.\n" % directory)
    sys.exit(1)

def output_train_arff(feats, golds, outf, mode="classify"):
    outf.write("@relation rte\n")
    for i, f in enumerate(feats[0], 1):
        outf.write("@attribute act%d real\n" % i)
    if mode == 'classify':
        outf.write("@attribute gt {0,0.5,1}\n")
    elif mode == 'regression':
        outf.write("@attribute gt real\n")
    else:
        raise ValueError("I don't know what to do with mode '%s'" % mode)
    outf.write("@data\n")
    for x, y in zip(feats, golds):
        outf.write(",".join(map(str, x)) + "," + y + "\r\n")

def output_test_arff(feats, outf):
    pass

def main():
    parser = argparse.ArgumentParser("Takes in a bunch of system outputs and builds an ensemble arff file.")
    parser.add_argument('--mln', '-m', nargs='+', help="Take input from a MLN's system output.")
    parser.add_argument('--gold', '-g', help="Gold labels file.")
    parser.add_argument('--out', '-o', type=argparse.FileType('w'), default=sys.stdout, help='Output file. Defaults to stdout.')
    parser.add_argument('--weka', '-w', action='store_true', help='Automatically launch WEKA after generating file.')
    args = parser.parse_args()

    if args.gold:
        golds = [l.strip() for l in open(args.gold)]
        is_classify = 'rte' in args.gold
        is_regress = 'sts' in args.gold
        assert is_classify ^ is_regress, "Must be either a classification or regression task."
        mode = is_classify and 'classify' or 'regression'

    mln_feats = [read_mln_output(m) for m in args.mln]
    num_feats = [len(m) for m in mln_feats]
    if not all(x == num_feats[0] for x in num_feats):
        sys.stderr.write("Not all inputs have the same number of rows. Check the inputs were all trained on the same data.\n")
        sys.exit(1)

    all_feats = [reduce(lambda x, y: x + y, x) for x in zip(*mln_feats)]

    output_train_arff(all_feats, golds, args.out, mode)
    args.out.close()

    if args.weka:
        train_file = args.out.name
        if mode == 'classify':
            launcher = weka_prefix + "weka.Run -no-scan -no-load  weka.classifiers.functions.LibSVM -i -t %s" % train_file
        else:
            launcher = weka_prefix + "weka.classifiers.meta.AdditiveRegression -i -t %s -S 0.95 -I 10 -W weka.classifiers.rules.M5Rules" % train_file
        os.execl(*launcher.split())




if __name__ == '__main__':
    main()


