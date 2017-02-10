#!/usr/bin/env python

# input = [ features1 features2 features3]
# features = comma separated real values

import sys
import argparse

import numpy as np
import sklearn
import sklearn.svm
import sklearn.metrics
import sklearn.cross_validation

NUM_CV = 10
SEED = 1337

FIXED_SIGNALS = [1.0, 0.0, -1.0, -2.0, -3.0, -4.0, -5.0, -6.0]
NUM_BINS = 10
bins = (np.arange(NUM_BINS+1)) / float(NUM_BINS)

def binarize(feats):
    return feats
    output = [np.clip(feats, 0.0, 1.0)]
    for n in FIXED_SIGNALS:
        output.append(feats == n)
    for lower, upper in zip(bins, bins[1:]):
        output.append((feats > lower) & (feats <= upper))
    return np.concatenate(output, axis=1)

def read_features(file):
    return binarize(np.array([map(float, x.strip().split(",")) for x in file]))

def read_gold(file):
    result = []
    for line in file:
        line = line.strip()
        if not line: continue
        if line == "0": result.append(0)
        elif line == "0.5": result.append(1)
        elif line == "1": result.append(2)
        else: raise ValueError("Invalid gold entry '%s'?" % line)
    return np.array(result)

def full_eval(Y, Yhat):
    print "Acc: %.3f" % sklearn.metrics.accuracy_score(Y, Yhat)
    f1 = sklearn.metrics.f1_score
    print "F1: %.3f    %.3f    %.3f" % (
            f1(Y == 0, Yhat == 0), f1(Y == 1, Yhat == 1), f1(Y == 2, Yhat == 2))
    print "Confusion ( true \ pred ):"
    print sklearn.metrics.confusion_matrix(Y, Yhat)


def cv_eval(clf, X, Y):
    clf.fit(X, Y)
    cv_folds = sklearn.cross_validation.KFold(len(X), n_folds=NUM_CV, shuffle=True, random_state=SEED)
    scores = sklearn.cross_validation.cross_val_score(clf, X, Y, cv=cv_folds)
    print "Accuracy (CV): %.3f" % np.mean(scores)
    print str(scores)
    Yhat = sklearn.cross_validation.cross_val_predict(clf, X, Y, cv=cv_folds)
    full_eval(Y, Yhat)

def main():
    parser = argparse.ArgumentParser('Post-hoc classifier')
    parser.add_argument('--train', '-t', type=argparse.FileType('r'), help='Training input')
    parser.add_argument('--gold-train', '-g', type=argparse.FileType('r'), help='Training gold')
    parser.add_argument('--test', '-T', type=argparse.FileType('r'), help='Test input (optional)')
    parser.add_argument('--gold-test', '-G', type=argparse.FileType('r'), help='Test gold')
    parser.add_argument('--arff', action='store_true')
    args = parser.parse_args()

    #print args

    #clf = sklearn.svm.SVC(class_weight='balanced')
    clf = sklearn.svm.SVC()

    Xtrain = read_features(args.train)
    Ytrain = read_gold(args.gold_train)

    if args.arff:
        print "@relation rte"
        for i in xrange(Xtrain.shape[1]):
            print "@ATTRIBUTE feat%d NUMERIC" % (i)
        print "@ATTRIBUTE class {ent,neu,con}"
        print
        print "@DATA"
        lookup = np.array(["con","neu","ent"])
        for i in xrange(Xtrain.shape[0]):
            print "%s,%s" % (",".join(map(str, Xtrain[i])), lookup[Ytrain[i]])
        return

    #print "X:", Xtrain.shape
    #print "Y:", Ytrain.shape

    #print "### TRAINING FIT ###"
    #clf.fit(Xtrain, Ytrain)
    #full_eval(Ytrain, clf.predict(Xtrain))


    print
    print "### TRAINING (%d-fold CV) ###" % NUM_CV
    cv_eval(clf, Xtrain, Ytrain)

    if args.test:
        print
        print "### TESTING ###"
        Xtest = read_features(args.test)
        Ytest = read_gold(args.gold_test)
        clf.fit(Xtrain, Ytrain)
        full_eval(clf.predict(Xtest), Ytest)


if __name__ == '__main__':
    main()

