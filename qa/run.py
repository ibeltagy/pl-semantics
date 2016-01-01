#!/bin/python

#read line

import sys
import argparse
import pdb
import xml.etree.ElementTree as ET
import os
import re
from nltk.tokenize import TreebankWordTokenizer
from nltk.tokenize import sent_tokenize
from sets import Set
import subprocess
import numpy as np
import imp
condorizer = imp.load_source("condorizer","./bin/condorizer.py")

#Util
#------------------------
def parseIntSet(nputstr=""):
  selection = set()
  invalid = set()
  # tokens are comma seperated values
  tokens = [x.strip() for x in nputstr.split(',')]
  for i in tokens:
     try:
        # typically tokens are plain old integers
        selection.add(int(i))
     except:
        # if not, then it might be a range
        try:
           token = [int(k.strip()) for k in i.split('-')]
           if len(token) > 1:
              token.sort()
              # we have items seperated by a dash
              # try to build a valid range
              first = token[0]
              last = token[len(token)-1]
              for x in range(first, last+1):
                 selection.add(x)
        except:
           # not an int and not a range...
           invalid.add(i)
  # Report invalid tokens before returning valid selection
  print "Invalid set: " + str(invalid)
  return selection
# end parseIntSet
#-------------------------
parser = argparse.ArgumentParser()

#parser.add_argument("ds", choices=["wikipedia", "cnn", "dailymail", "samples"], help="QA datasets: wikipedia, cnn, dailymail")
#parser.add_argument("mode", choices=["test", "validation", "training"], help="training, validation, or test")
parser.add_argument("-ds", type=str, help="Directory of questions")
parser.add_argument("algo", choices=["bow", "mln"], help="QA algorithms: bow")
parser.add_argument("-range", type=str, default="", help="set of indecies of questions to run")
parser.add_argument("-anonymous", action='store_true', default=False, help="anonymise the entities or keep them ?")
parser.add_argument("-limitFeat", help="a limited set of features that is more appropriate for the NN [false]")
parser.add_argument("-mlnArgs", default="", help="string containing MLN args")
parser.add_argument("-condor", type=str, default="", help="condor output folder. If empty, run serially")


args = parser.parse_args()
#directory = "qa/resources/" + args.ds + "/" + args.mode
directory = args.ds
args.range = parseIntSet(args.range)

def bow (context, question):
	sentences = context.split(".");
	qBow = Set (question.split(" "))
	bestEntity = ""
	bestSen = ""
	maxOverlab = -1
	for sentence in sentences:
		senBow = Set(sentence.split(" "));
		intersection = senBow & qBow;
		senEntity = None
		for w in senBow:
			if w.startswith("@entity"):
				senEntity = w;
				break;
		if not senEntity == None and len(intersection) > maxOverlab:
			maxOverlab = len(intersection)
			bestEntity = senEntity;
			bestSen = sentence
			
	#print bestSen
	return bestEntity
	
def mln (qFilePath):
	mlnArgs = args.mlnArgs.split( );
	allArgs = ["bin/mlnsem", "qa", qFilePath] + mlnArgs
	if args.condor == "":
	    subprocess.call( allArgs) 
        else:
            condorizer.condorize( ["ARG0"] + allArgs + [args.condor + "/" + str(qIdx)])     
        
		#"-log", "TRACE", "-soap", "localhost:9000", "-diffRules", "false", "-irLvl", "0", "-negativeEvd", "true", "-withNegT", "false"])
	return ""


qIdx = 0
rightAnswersCount = 0
totalAnswersCount = 0
fileList = os.listdir(directory)
for qFileName in sorted (fileList):
	qFilePath = directory + "/" +qFileName
        qIdx = qIdx + 1;
	
	if (len(args.range) > 0 and not (qIdx in args.range) ):
		continue;

	answer = ""
        rightAnswer = ""
	print "#################################"
	print "##Processing Q: " + str(qIdx)
	print "#################################"
	sys.stdout.flush() 
	if args.algo == "bow":
            qFile = open(qFilePath);
            title = qFile.readline().strip();
            qFile.readline();
            context = qFile.readline().strip();
            qFile.readline();
            question = qFile.readline().strip();
            qFile.readline();
            rightAnswer = qFile.readline().strip();
            assert qFile.readline().strip() == "";
            if not args.anonymous: #deanonymize the data
                for l in qFile:
                    splits = l.strip().split(":");
                    context = context.replace (splits[0], splits[1])
                    #print context
	    answer = bow (context, question);
	elif args.algo == "mln":
		answer = mln (qFilePath);
	else:
		raise ("not implemented yet")

	print answer + "  ##  " + rightAnswer
	if (answer == rightAnswer):
		rightAnswersCount = rightAnswersCount + 1
	totalAnswersCount = totalAnswersCount + 1
	
	#print question + " => " +  answer + " => " + context;
print "right answers: " + str(rightAnswersCount) + ", total answers: " + str(totalAnswersCount) + "/" + str(qIdx-1) + ", accuracy = " + str(100.0*rightAnswersCount / max(1, totalAnswersCount)) + "%"
sys.exit()
