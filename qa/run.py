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
from nltk.stem import WordNetLemmatizer
import os.path
import math 
import numpy as np
from gensim.models import Word2Vec
import datetime


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
parser.add_argument("algo", choices=["bow", "mln", "word", "embed"], help="QA algorithms: bow")
parser.add_argument("-range", type=str, default="", help="set of indecies of questions to run")
parser.add_argument("-anonymous", action='store_true', default=True, help="anonymise the entities or keep them ?")
parser.add_argument("-limitFeat", help="a limited set of features that is more appropriate for the NN [false]")
parser.add_argument("-mlnArgs", default="", help="string containing MLN args")
parser.add_argument("-condor", type=str, default="", help="condor output folder. If empty, run serially")
parser.add_argument("-vocab", type=str, default="", help="vocabulary and statistics")
parser.add_argument("-maxDist", type=str, default="5", help="parameter to the 'word' baseline")
parser.add_argument("-vs", type=str, default="", help="path of word2vec model")
parser.add_argument("-log", choices=["debug", "off"], default="off", help="log level")


args = parser.parse_args()
#directory = "qa/resources/" + args.ds + "/" + args.mode
directory = args.ds
args.range = parseIntSet(args.range)

def log(s):
	if args.log != "off":
		print s;

vocabDict = {}
wordCnt = 1
if os.path.isfile(args.vocab) : 
	vocabFile = open(args.vocab);
	for l in vocabFile:
		splits = l.strip().split(" ");
		assert len(splits) == 2, "no two tokens: " + str(splits)
		vocabDict[splits[1]] = splits[0];	
		wordCnt += int ( splits[0])

wnl = WordNetLemmatizer()
stopWords = Set() 

vsModel = None
if args.vs != "":
	print str(datetime.datetime.now().time()) + " Start loading vs ... " 
	vsModel = Word2Vec.load_word2vec_format(args.vs, binary=True)
	print str(datetime.datetime.now().time()) + " Done loading vs"

def embed (context, question, entityList):
	#pdb.set_trace()
	words = np.array(context.split(" "));
	qWords = np.array(question.split(" "));
	minCost = 0
	minCostEntity = "";
	#if vsModel == None:
		#assert (vsModel != None)

	simTable = dict()
	for qw in qWords : 
		innerTable = [None] * len(words)
		for i in range (0, len(words)):
			#replace the few lines below with word2vec similarity
			try:
				innerTable[i] = vsModel.similarity(words[i], qw);
			except :
				if words[i] == qw:
					innerTable[i] = 1
				else : 
					innerTable[i] = 0
		simTable[qw] = innerTable;

	placeholderLoc = np.where (qWords == "@placeholder")[0][0]

	for e in entityList:
		entityLoc = np.where (words == e) [0];
		entityCost = 0;
		#placeholderLoc = np.where (qWords == "@placeholder")[0][0]
		entityDistMapBefore = [None] * len(words)
		entityDistMapAfter = [None] * len(words)
		#print entityLoc
		if len(entityLoc) == 0: 
			continue;
		
		inf = float("inf")
		for i in range (0, len(words)):
			entityDistMapBefore[i] = min([(x if x >= 0 else inf) for x in (entityLoc - (i))])
			if entityDistMapBefore[i] == 0:
				entityDistMapBefore[i] = inf;
			
			entityDistMapAfter[i] = max([(x if x <= 0 else -inf) for x in (entityLoc - (i))])
			if entityDistMapAfter[i] == 0:
				entityDistMapAfter[i] = -inf;
			#entityDistMapAfter[i] = max(entityLoc - i)
			#if entityDistMapAfter[i] == 0:
			#	entityDistMapBefore[i] = inf
			#	entityDistMapAfter[i] = inf
		#pdb.set_trace()


		#pdb.set_trace()
		for i in range (0, len (qWords)):
			qWord = qWords[i]
			simTableQWord = simTable[qWord];
			
			minDistBefore = max(np.divide(simTableQWord, np.add(1, abs(np.add(entityDistMapBefore, i - placeholderLoc)))))
			minDistAfter = max(np.divide(simTableQWord, np.add(1, abs(np.add(entityDistMapAfter, i - placeholderLoc)))))
			minDist = max(minDistBefore, minDistAfter)
			#minDist = max(np.multiply(entityDistMap, simTableQWord))

			entityCost += minDist
			#wordLoc = np.where (words == qWord);
			#minDist = int(args.maxDist)

			#for idx1 in entityLoc:
			#	for idx2 in wordLoc:
					#pdb.set_trace()
					#minDist = min(minDist, max(0, abs(idx1 - idx2) - abs (placeholderLoc - i)))
			#		minDist = min(minDist, abs(idx1 - idx2 - placeholderLoc + i))
			log(" >> " + qWord + " " + str(minDist))
			#pdb.set_trace()
			#entityCost += minDist
		log("## " + e + " " + str(entityCost))
		#pdb.set_trace()
		if entityCost > minCost:
			minCost = entityCost
			minCostEntity = e

	#print "## " + str(minCost)
	#print "## " + minCostEntity
	return minCostEntity

def word (context, question, entityList):
	words = np.array(context.split(" "));
	qWords = np.array(question.split(" "));
	#minCost = sys.maxint
	minCost = 0
	minCostEntity = "";
	for e in entityList:

		entityLoc = np.where (words == e);
		entityCost = 0;
		placeholderLoc = np.where (qWords == "@placeholder")[0][0]
		for i in range (0, len (qWords)):
			qWord = qWords[i]
			#qWordScore = math.log(wordCnt) - math.log( int (vocabDict.get(qWord.lower(), "1")))
			#qWordScore = 1.0/float(vocabDict.get(qWord.lower(), "1")) # <<<<<<<<<<<<<<<?????????????
			wordLoc = np.where (words == qWord);
			#minDist = int(args.maxDist)
			minDist = 0
			#pdb.set_trace()

			for idx1 in entityLoc[0]:
				for idx2 in wordLoc[0]:
					#pdb.set_trace()
					#minDist = min(minDist, max(0, abs(idx1 - idx2) - abs (placeholderLoc - i)))
					#minDist = min(minDist, abs(idx1 - idx2 - placeholderLoc + i))
					#if (idx1 - idx2 - placeholderLoc + i) == 0:
					#	pdb.set_trace()
					minDist = max(minDist, 1.0/(1+abs(idx1 - idx2 - placeholderLoc + i)))
					if ( i == 9):
						log(" >> " + str(minDist) + " " + str(idx1) + " " + str(idx2) + " " + str(placeholderLoc) + " " + str(i))
					
			log(" >> " + qWord + " " + str(minDist))
			entityCost += minDist
			#pdb.set_trace()
		log("## " + e + " " + str(entityCost))
		#if entityCost < minCost:
		if entityCost > minCost:
			minCost = entityCost
			minCostEntity = e

	#print "## " + str(minCost)
	#print "## " + minCostEntity
	return minCostEntity


def bow (context, question):
	#pdb.set_trace()
	sentences = context.split(".");
	#qBow =  Set([wnl.lemmatize(x) for x in Set (question.split(" "))])
	qBow =  Set (question.split(" "))
	bestEntity = ""
	bestSen = ""
	maxOverlab = -sys.maxint - 1
	maxOverlabWords = None;
	for sentence in sentences:
		#senBow = Set([wnl.lemmatize(x) for x in Set (sentence.split(" "))])
		senBow = Set (sentence.split(" "))
		intersection = (senBow & qBow) - stopWords;
		senEntity = None
		for w in senBow:
			if w.startswith("@entity"):
				senEntity = w;
				break;
		#pdb.set_trace()
		#overlabScore = sum ( [ math.log(wordCnt)-math.log(float(vocabDict.get(x.lower(), "1"))) for x in intersection] )
		overlabScore = sum ( [ wordCnt*1.0/float(vocabDict.get(x.lower(), "1"))  for x in intersection] )
		#if not senEntity == None and len(intersection) > maxOverlab:
		if not senEntity == None and len(intersection) >  0 and overlabScore > maxOverlab:
			maxOverlab = overlabScore
			maxOverlabWords = intersection
			bestEntity = senEntity;
			bestSen = sentence
			
	print "## " + bestSen
	print "## " + question
	print "## " + str(maxOverlabWords)
	print "## " + str(maxOverlab)
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
	print qFilePath
	sys.stdout.flush() 

	qFile = open(qFilePath);
	title = qFile.readline().strip();
	print title;
	qFile.readline();
	context = qFile.readline().strip();
	qFile.readline();
	question = qFile.readline().strip();
	qFile.readline();
	rightAnswer = qFile.readline().strip();
	assert qFile.readline().strip() == "";

	entityList = [];
	for l in qFile:
		splits = l.strip().split(":");
		entityList.append(splits[0]);
		if not args.anonymous: #deanonymize the data
			#include a space after entity index to make sure I am replacing a whole token
			context = context.replace (splits[0]+" ", splits[1] + " " )
	#print str(entityList)

	if args.algo == "bow":
		answer = bow (context, question);
	elif args.algo == "word":
		answer = word (context, question, entityList);
	elif args.algo == "embed":
		answer = embed (context, question, entityList);
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
