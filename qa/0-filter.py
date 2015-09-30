#!/bin/python

#Filters the documents that are not in the list of accepted docs

#read line

import sys
import argparse
import pdb

parser = argparse.ArgumentParser()

#parser.add_argument("mode", choices=[TRAIN, TEST, DEV], default=TEST, help="mode: train or test on the test set or test on the dev set")
parser.add_argument("docsFile", help="text file of a list of all docs")
#parser.add_argument("-verbose", help="increase output verbosity", action="store_true")

args = parser.parse_args()

docsFile = open (args.docsFile)
docs = dict ()

for line in docsFile:
    docs[line.strip() ] = True;

#pdb.set_trace()


printFlag = False
for line in sys.stdin:
    #print (line);
    if line.startswith("<doc id="):
        docTitle = line.split ("\"")[5];
        if docs.get(docTitle, False):
            printFlag = True;
        else:
            
            printFlag = False
    if printFlag:
        print line.strip();


#cat  ../pages-simple/filtered | sed '/<doc id=.*>/ {s/.*//; N; s/[\n]/### /g}' | sed  '/<\/doc>/d' | awk '{if ($1 == "###") print $0"."; else print $0}

