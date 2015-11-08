#!/bin/python

#read line

import sys
import argparse
import pdb
import hashlib
import os

#pdb.set_trace()
parser = argparse.ArgumentParser()

#cat /scratch/cluster/beltagy/qa/pagesEn/filtered  | python qa/1-split.py /scratch/cluster/beltagy/qa/pagesEn/splits


#parser.add_argument("mode", choices=[TRAIN, TEST, DEV], default=TEST, help="mode: train or test on the test set or test on the dev set")
#parser.add_argument("docsFile", help="text file of a list of all docs")
parser.add_argument("outDir", help="output directory (will be overwritten)")
#parser.add_argument("-verbose", help="increase output verbosity", action="store_true")

args = parser.parse_args()

#docsFile = open (args.docsFile)
#docs = dict ()

#for line in docsFile:
#    w = line.strip();
#    print str(hashlib.md5( w).hexdigest())[0]
#    docs[w] = True;

#sys.exit()
#pdb.set_trace()


first = False
currFile = None;
cnt = 0
for line in sys.stdin:
    #print (line);
    if line.startswith("<doc id="):
        cnt = cnt + 1
        if cnt % 1000 == 0 :
            print cnt
        docTitle = line.split ("\"")[5];
        h = str(hashlib.md5( docTitle ).hexdigest())
        directory = args.outDir + "/" + h[0] + "/"
        if not os.path.exists (directory):
            os.makedirs(directory);
        filePath = directory + h
        currFile = open(filePath, "w")
        first = True;
    elif first:
        currFile.write(line.strip() + ".")
        first = False;
    elif "</doc>" in line:
        None
    else:
        currFile.write(line)


#cat  ../pages-simple/filtered | sed '/<doc id=.*>/ {s/.*//; N; s/[\n]/### /g}' | sed  '/<\/doc>/d' | awk '{if ($1 == "###") print $0"."; else print $0}

