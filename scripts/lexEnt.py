#!/usr/bin/env python2.7

import argparse
import sys
import difflib
import operator
import commands
from scipy.sparse import csr_matrix, lil_matrix
import scipy.sparse
from sklearn.svm import SVC
from sklearn.linear_model import LogisticRegression
from sklearn.multiclass import OneVsRestClassifier
from sklearn.preprocessing import MultiLabelBinarizer
from sklearn.preprocessing import maxabs_scale
from sklearn.metrics import classification_report
from sklearn.ensemble import AdaBoostClassifier

from sklearn import tree
from sklearn.cross_validation import StratifiedKFold
from sklearn.ensemble import RandomForestClassifier
from sets import Set
import datetime
import numpy as np
from sklearn.externals import joblib
from sklearn.feature_extraction import DictVectorizer
from sklearn.feature_extraction import FeatureHasher
from sklearn import metrics
import sys
import pdb
import Queue
import multiprocessing
from multiprocessing import Process
import pickle
import math
import itertools
import argparse
from os.path import exists, isfile
import os
from sklearn.decomposition import TruncatedSVD
import logging
import climate
from sklearn.preprocessing import StandardScaler
from nltk.stem import WordNetLemmatizer
from nltk.corpus import wordnet
from nltk import pos_tag
from nltk.wsd import lesk
from curses import ascii
from sklearn import cross_validation


#WORKAROUND TO FIX AN UGLY PROBLEM WITH PYTHON ON WINDOWS
#discussion: https://stackoverflow.com/questions/15457786/ctrl-c-crashes-python-after-importing-scipy-stats
try: #try-catch because win32api is not available in some machines
	import imp
	import win32api
	import ctypes
	import thread
	basepath = imp.find_module('numpy')[1]
	ctypes.CDLL(os.path.join(basepath, 'core', 'libmmd.dll'))
	ctypes.CDLL(os.path.join(basepath, 'core', 'libifcoremd.dll'))
	# Now set our handler for CTRL_C_EVENT. Other control event 
	# types will chain to the next handler.
	def handler(dwCtrlType, hook_sigint=thread.interrupt_main):
		if dwCtrlType == 0: # CTRL_C_EVENT
			hook_sigint()
			return 1 # don't chain to the next handler
		return 0 # chain to the next handler
	win32api.SetConsoleCtrlHandler(handler, 1)
except: 
	None
	#print "ERROR: Failed to solve the keyboardInterrupt problem"
#END OF THE WORKAROUND

TRAIN = "train" #constant value for mode
TEST = "test" #constant value for mode
DEV = "dev" #constant value for mode (test on the dev dataset)
MEMM = "memm" # constant value for model
MAXENT = "maxent" # constant value for model
NN = "nn" # constant value for model
RNN = "rnn" # constant value for model
MAX = "max" # constant value for vsFun
ADD = "add" # constant value for vsFun
FULL = "full"
MINI = "mini"
ROOT = "ROOT"  # the label for the root 
THIS = "THIS"  # the label for the root 
THAT = "THAT"  # the label for the root 

delimiter = "_##_"

#########################  CONFIG  ##########################
#nThread = -1 #maximum possible
#trainingSize = 77495
#mode = TEST;
#beamWidth = 25;
#modelName = "memm-x"  #memm, memm-ext, maxent
#brownClsFreqThr = 0
#brownClsFile = 'brown-cls-100.txt'

#dependencies edges
graphs = dict()
graphs[0] = [] #no dependencies
graphs[4] = [(0,1), (1,4), (2,3), (3,5)] #disconnected chain
graphs[5] = [(0,1), (1,4), (0, 2), (2,3), (3,5)] #chain
graphs[6] = [(0,1), (1,4), (0, 2), (2,3), (1,3), (3,5)] #graph
graphs[8] = [(0,1), (1,4), (0, 2), (2,3), (1,3), (3,5), (0, 3), (1,2)] #graph

#constraintEdges edges
constraintEdges = [(0,0), (0,1), (2, 2), (2,3)] #graph

structures = dict()
structures["cfi"] = [0,1,4,2,3,5] #T.CH, T.CH.FN, T.CH.FN.ING, A.CH, A.CH.FN, A.CH.FN.ING
structures["cf"] = [0,1,2,3] #T.CH, T.CH.FN, A.CH, A.CH.FN
structures["c"] = [0,-1, 2] #T.CH, A.CH   (negative value in just a place holder)
structures["ct"] = [0] #T.CH
structures["ca"] = [-1,-1, 2] #A.CH  (negative value in just a place holder)

trainCodeFile = "new/train/code_columns.txt"
trainLangFile = "new/train/lang.txt"

testCodeFile = "new/test/code_columns.txt" #will be changed to DEV based on args.mode
testLangFile = "new/test/lang.txt" #will be changed to DEV based on args.mode

devCodeFile = "new/dev/code_columns.txt" #this is used only to be concatenated to the training data then do k-fold
devLangFile = "new/dev/lang.txt" #this is used only to be concatenated to the training data then do k-fold

testIDsFile = "all_test_ids.txt"
intelligibleIDsFile = "intelligible_ids.txt"

pretrainLangFile = "pretrain.txt"

#############################  Read ARGS ######################
if __name__ == '__main__':
	print  (sys.argv)

parser = argparse.ArgumentParser()

parser.add_argument("mode", choices=[TRAIN, TEST, DEV], default=TEST, help="mode: train or test on the test set or test on the dev set")
parser.add_argument("modelFile", help="model file")
parser.add_argument("inputFile", help="precomputed features file")
parser.add_argument("-verbose", help="increase output verbosity", action="store_true")
parser.add_argument("-nThread", type=int, default=-1, help="Number of threads to use [-1]")
parser.add_argument("-trainCnt", type=int, default=77495, help="Number of training entries to use [77495]")
parser.add_argument("-beam", type=int, default=8, help="Beam size [8]")
parser.add_argument("-browThr", type=int, default=0, help="Brown cluster's word frequency threshold [0]")
parser.add_argument("-brown", default='brown-cluster\\brown-cls-250.txt', help="Brown cluster's file [brown-cls-250.txt]")
parser.add_argument("-graph", type=int, choices=[0, 4, 5, 6, 8], default=8, help="Dependencies graph design [8]")
parser.add_argument("-model", choices=[MEMM, MAXENT, NN, RNN], default="memm", help="model: memm or maxent or nn[memm]")
parser.add_argument("-vs", default="", help="vector space [ ]")
parser.add_argument("-debug", action='store_true', default=False, help="debug: true or false [false]")
parser.add_argument("-intel", action='store_true', default=False, help="filter training data or test on intelligible entries only. -mode should be \"train\" or \"test\" [false]")
parser.add_argument("-vsFun", choices=[MAX, ADD], default=ADD, help="vsFun: vector space sentence aggregation function max or add [add]")
parser.add_argument("-limitFeat", action='store_true', default=False, help="a limited set of features that is more appropriate for the NN [false]")
parser.add_argument("-svd", type=int, default=0, help="reduced number of dimentions using SVD. 0 means no reduction[0]")
parser.add_argument("-log", action='store_true', default=False, help="enable or disable logging to the file modelpath.log [false]")
parser.add_argument("-struct", choices=["cfi", "cf", "c", "ct", "ca"], default="cfi", help="cfi: full tree, cf: channels and functions, c: channels only, ct: trigger channel, ca: action channel [cfi]")
parser.add_argument("-batch", type=int, default=64, help="Batch size for NN [64]")
parser.add_argument("-char3freq", type=int, default=0, help="Drop character tri-grams with frequency less than [0]")
parser.add_argument("-pretrain", action='store_true', default=False, help="pretrain the network. Only works with NN [false]")
parser.add_argument("-pretrainCnt", type=int, default=1000000, help="Number of pre training entries to use [1,000,000]")
#parser.add_argument("-prexThr", type=add_argument, , default=8, help="Perplexity threshold on pretraining data [5.0]")
parser.add_argument("-gpu", default='cpu', help="gpu0, gpu1, ...  [cpu]")
parser.add_argument("-seed", action='store_true', default=False, help="fixed seed [false]")
parser.add_argument("-normalize", action='store_true', default=False, help="normalize input features (mean = 0, variance = 1) [false]")
parser.add_argument("-charSeq", action='store_true', default=False, help="input to RNN is a sequence of characters [false]")
parser.add_argument("-layerwise", action='store_true', default=False, help="pretrain the NN layerwise, then train the full network [false]")
parser.add_argument("-activation", choices=['sigmoid', 'tanh', 'relu'], default='sigmoid', help="activation function used in the NN [sigmoid]")
parser.add_argument("-complexNN", action='store_true', default=False, help="All outputs in a single NN [false]") #not very good naming. #graph should be 0, and struct should be ct
parser.add_argument("-reload", action='store_true', default=False, help="Load an existing NN model [false]")
parser.add_argument("-grammar", choices=['none', 'full', 'mini'], default='none', help="Use productions of a grammar(full or mini), not 6 columns [none]") #graph should be 0, and struct should be ct
parser.add_argument("-noLabelBias", action='store_true', default=False, help="Testing with gold standard labels (to evaluate the label bias problem )[false]")
parser.add_argument("-paraphrase", action='store_true', default=False, help="Enable paraphrasing to increase the training set [false]")
parser.add_argument("-featFreq", type=int, default=0, help="Remove features with frequency less than or equal ... [0]")
parser.add_argument("-foldCnt", type=int, default=0, help="Number of training folds. The default is to keep the training and validation split with no change [0]")
parser.add_argument("-foldIdx", type=int, default=0, help="A 0-based index of the foled to be used [0]") #paraphrase, intel, featFreq do not work with fold
parser.add_argument("-layers", type=int, default=1, help="Number of hidden layers in the NN [1]") 
parser.add_argument("-noBigram", action='store_true', default=False, help="Enable or disable adding bigrams to the features. Default is adding them [false]")



args = parser.parse_args()
if args.model == NN and args.mode == TRAIN : # if NN, clear the command line arguments because the NN libraries are also expecting command line args
	sys.argv = [ sys.argv[0]] 

# process args 

if args.foldCnt > 0: 
	assert not args.paraphrase
	assert not args.intel
	assert args.featFreq == 0
	assert args.model != RNN
	assert args.foldIdx >= 0 and  args.foldIdx < args.foldCnt
	args.modelFile = args.modelFile + "_" + str(args.foldCnt) + "_" + str(args.foldIdx)
	
if args.model == RNN:	#in RNN, 
	args.graph = 0		#assume for now that no dependencies between the edges
	args.limitFeat = True	#we use the charNgram features only
	
deps = graphs[args.graph]

if args.intel == True and (args.mode not in [TEST, TRAIN]):
	 raise Exception("running on intelligible entries is only available on the train and test modes")
if args.mode in [TRAIN, DEV]:  #if dev, then test on the dev set. 
								#if train, then I need the dev set for the NN training
	testCodeFile = "new/dev/code_columns.txt"
	testLangFile = "new/dev/lang.txt"

##The lines below should be commented
#if args.mode in [TRAIN]:  #if train, then I need the dev set for the NN training
#	testCodeFile = "new/test/code_columns.txt"
#	testLangFile = "new/test/lang.txt"

if args.mode == DEV:  #if dev, then change the mode to test 
	args.mode = TEST

traversalOrder = structures[args.struct]

if args.log:
	logging.basicConfig(filename=args.modelFile + ".log", level="DEBUG", format="%(asctime)s:%(name)s:%(levelname)s:%(funcName)s:%(lineno)d - %(message)s")
else:
	climate.enable_default_logging()
	
if args.model in [NN, RNN]:
	if args.gpu != "cpu":
		import theano.sandbox.cuda
		theano.sandbox.cuda.use(args.gpu)	
	import theanets
	import theano	

if not os.path.exists(args.modelFile):
    os.makedirs(args.modelFile)

seeder = np.random.RandomState() #Random
if args.seed: #initilized the random number generators with a fixed seed. This way, we always get the same output for the sake of debugging 
	seeder = np.random.RandomState(42)

def printArgs ():
	logging.info (sys.argv)
	logging.info( "nThread = " + str(args.nThread) )
	logging.info( "trainCnt = " + str(args.trainCnt) )
	logging.info( "beam = " + str(args.beam) )
	logging.info( "mode = " + str(args.mode) )
	logging.info( "browThr = " + str(args.browThr) )
	logging.info( "brown = " + str(args.brown) )
	logging.info( "graph = " + str(args.graph) )
	logging.info( "deps = " + str(deps) )
	logging.info( "model = " + str(args.model) )
	logging.info( "modelFile = " + str(args.modelFile) )
	logging.info( "vs = " + str(args.vs) )
	logging.info( "debug = " + str(args.debug) )
	logging.info( "testFiles = " + testLangFile )
	logging.info( "intel = " + str(args.intel) )
	logging.info( "vsFun = " + str(args.vsFun) )
	logging.info( "limitFeat = " + str(args.limitFeat) )
	logging.info( "svd = " + str(args.svd) )
	logging.info( "log = " + str(args.log) )
	logging.info( "struct = " + str(args.struct) )
	logging.info( "traversalOrder = " + str(traversalOrder) )
	logging.info( "batch = " + str(args.batch) )
	logging.info( "pretrain = " + str(args.pretrain) )
	logging.info( "pretrainCnt = " + str(args.pretrainCnt) )
	logging.info( "gpu = " + str(args.gpu) )
	logging.info( "seed = " + str(args.seed) )
	logging.info( "normalize = " + str(args.normalize) )
	logging.info( "charSeq = " + str(args.charSeq) )
	logging.info( "layerwise = " + str(args.layerwise) )
	logging.info( "activation = " + str(args.activation) )
	logging.info( "complexNN = " + str(args.complexNN) )
	logging.info( "reload = " + str(args.reload) )
	logging.info( "grammar = " + str(args.grammar) )
	logging.info( "noLabelBias = " + str(args.noLabelBias) )
	logging.info( "featFreq = " + str(args.featFreq) )
	logging.info( "foldCnt = " + str(args.foldCnt) )
	logging.info( "foldIdx = " + str(args.foldIdx) )
	logging.info( "layers = " + str(args.layers) )
	logging.info( "noBigram = " + str(args.noBigram) )

	#sys.exit()

######################################################

def lexEnt(): 
	#printArgs();
	#print "Lex Ent";
	
	inputFile = open(args.inputFile)
	featuresDict = []
	labels = []

	positiveStrings = Set()
	if args.mode == TRAIN:
		for line in inputFile:
			splits = line.strip().split(" ", 1); #+/-1 idx:val idx:val  ... 
			label = splits[0]
			features = splits[1]
			if (label == "1.0"):
				positiveStrings.add(features)
		print 'Number of positive entries: ' + str(len(positiveStrings));

		inputFile.seek(0)
	
	deletedNegatives = 0;
	for line in inputFile:
		splits = line.strip().split(" ",1); #+/-1 idx:val idx:val  ... 
		label = splits[0]
		featureString = splits[1]
		if featureString in positiveStrings and label == "-1.0":
			deletedNegatives = deletedNegatives + 1;
			continue;
		features = featureString.split(" ")

		row = dict()
		for f in features:
			idxVal = f.split(":");
			row[idxVal[0]] = float(idxVal[1])
		labels.append(label)
		featuresDict.append(row)
		#print label + " -- " + str(features)
	inputFile.close();
	print len(labels)
	print len(featuresDict)
	print "deletedNegatives: " + str(deletedNegatives)
	
	dictVectorizerModelPath = args.modelFile  + '/DictVectorizer'
	classifierModelPath = args.modelFile  + '/classifier'
	if args.mode == TRAIN:
		v = DictVectorizer(sparse=True)
		X = v.fit_transform(featuresDict)
		joblib.dump(v, dictVectorizerModelPath)
		#maxabs_scale(X, copy=False)
		Y = np.array(labels)
		#classifier = SVC(verbose=True,class_weight='balanced',cache_size=500,max_iter=200)
		#classifier = LogisticRegression(verbose=1,class_weight='balanced',penalty='l2')
		#classifier = tree.DecisionTreeClassifier(class_weight='balanced')
		classifier = RandomForestClassifier(n_estimators=20, class_weight='balanced')
		#classifier = AdaBoostClassifier(n_estimators=100)

		classifier.fit(X, Y);
		joblib.dump(classifier, classifierModelPath)
		logging.info ("training accuracy: " + str(classifier.score(X, Y)))
		predictions = classifier.predict(X);
		logging.info ("\n" + metrics.classification_report (Y, predictions) );

		skf = StratifiedKFold(Y, 4)
		for train, test in skf:
			classifier.fit(X[train], Y[train]);	
			logging.info ("training accuracy: " + str(classifier.score(X[train], Y[train])))
			predictions = classifier.predict(X[test]);
			logging.info ("\n" + metrics.classification_report (Y[test], predictions) );
	elif args.mode == TEST:
		print 'testing'
		v = joblib.load(dictVectorizerModelPath)
		X = v.transform(featuresDict)
		Y = np.array(labels)
		classifier = joblib.load(classifierModelPath)
		predictions = classifier.predict(X);
		predictionProbas = classifier.predict_proba(X);
		for p in predictionProbas:
			#if (p == '1.0'):
			#	print 'Hi: 1'
			#else: 
			#	print 'Hi: 0'
			print ('Hi: ' +  str(p[1]))
		print str(classifier.classes_)
		logging.info ("\n" + metrics.classification_report (Y, predictions) );

	#pdb.set_trace()

lexEnt();
sys.exit();

def break_point(): 
	if args.debug:
		pdb.set_trace()

#beam search
def isValid (partialTree, toColumn, label):

	if args.grammar == MINI:
		assert toColumn in [0, 2]
		if toColumn == 0 and label.startswith ("T.CH_"):
			return True
		if toColumn == 2 and label.startswith ("A.CH_"):
			return True
		return False
	
	
	fromColumns = [x[0] for x in constraintEdges if x[1] == toColumn]
	
	valid = True;

	for fromColumn in fromColumns:
		if fromColumn == toColumn:
			fromColumnLabel = label
		else:
			fromColumnLabel = partialTree[fromColumn]
		constraintTable = constraintsTable[(fromColumn, toColumn)]
		if  not (fromColumnLabel, label) in constraintTable:
			valid = False
			break;

	return valid;

#input data to an RNN batch. 	
def toRnnBatch (b):
	features = b[0]
	addFour = 0
	if args.complexNN and (len(b) == 2 or args.mode == TEST )  : # if complexNN and not pretraining (if input length is 2, or in the test mode, then this is not a pretraining)
		addFour = 4 # increase time steps and number of features

	timeSize = max([x.shape[0] for x in features]) + addFour
	batchSize = len(features)
	featSize = len (charNgramSet) + 1 + addFour
	s = np.zeros((batchSize, timeSize, featSize), dtype=np.float32)

	for idx in range(0, len(features)):
		rowF = features[idx]
		if addFour != 0: # if complexNN and not pretraining 
			outputTimeSteps = lil_matrix((4, featSize), dtype=np.float32)
			outputTimeSteps[0, featSize-4] = 1
			outputTimeSteps[1, featSize-3] = 1
			outputTimeSteps[2, featSize-2] = 1
			outputTimeSteps[3, featSize-1] = 1
			rowF = scipy.sparse.vstack([rowF, outputTimeSteps])
		timeSteps = rowF.shape[0]
		s[idx][:timeSteps] = rowF.toarray()
		#for innerIdx in range (0, len(rowF.data)):
		#	s[idx][rowF.nonzero()[0][innerIdx]][rowF.nonzero()[1][innerIdx]] = rowF.data[innerIdx][0]

	if len(b) == 1: 
		return [s.transpose([1, 0, 2])]
	elif len(b) == 2:
		labels = b[1]
		timeStepsList = [x.shape[0] - 1 for x in features]
		if addFour != 0:
			labelsFlat = np.concatenate((labels[0], labels[1], labels[2], labels[3]))
			rowsIndices = np.concatenate(( range(0, batchSize), range(0, batchSize), range(0, batchSize), range(0, batchSize)) )
			colsIndices = np.concatenate(( np.add(timeStepsList, 1), np.add(timeStepsList, 2), np.add(timeStepsList, 3), np.add(timeStepsList, 4) ) )
			ones = np.ones(batchSize * 4)
		else:
			labelsFlat = labels
			rowsIndices = range(0, batchSize)
			colsIndices = timeStepsList
			ones = np.ones(batchSize)
		t = csr_matrix((labelsFlat, (colsIndices, rowsIndices)), shape=(timeSize, batchSize), dtype=np.int)
		w = csr_matrix((ones, (colsIndices, rowsIndices)), shape=(timeSize, batchSize), dtype=np.float32)
		return [s.transpose([1, 0, 2]), t.toarray(), w.toarray()]
	else:
		raise Exception ("What !! ")

def inference(fromIndex, toIndex, result): #fromInclusive, toExclusive
	#with struct_predictions_lock:
	logging.info( "Thread: " + str(fromIndex) + "-" + str(toIndex) + " started" )
	#pdb.set_trace()	
	for i in range (fromIndex, toIndex): # for all data points in the test set
	#for i in range (1, 2): # for all data points in the test set
		if i%100 == 0:
		#	with struct_predictions_lock:
			logging.info( str(datetime.datetime.now()) + " - step: " + str(i) )
		
		bestTree = []
		bestTreeScore = 0

		if args.model in [MEMM, NN, RNN]:
			#Beam search for directed acyclic graphical model MAP inference. The graph I am using is very simple, and simple beamSearch is expected to do well
			beam = [ (1, [None]*6)]; ## initialize an empty table.  each entry is (score, [found labels]). Table length will always be args.beam except in the first iteration
			#scan the nodes of the MRF following the traversalOrder, and pick the top best labels on each node. 
			#label score = score of previous nodes * score of transition 
			
			for col in traversalOrder:
				if col < 0:
					continue;
				filteredDeps = [x for x in deps if x[1] == col] #find dependencies of the current column
				#condDepCols = [list(set(unzipedTrainingLabels[x[0]])) for x in filteredDeps ] #find all possible labels for each conditional dependency column
				newBeam = [] # each row is a step in time. row width = args.beam
				for prevBeamEntry in beam:

					if args.model == RNN:
						vectorizedTestingFeatures = [rawTestingFeatures[i]]
						if  not (args.complexNN and col > 0) :  #the following step is expensive, so do it only when needed
							vectorizedTestingFeatures = toRnnBatch([vectorizedTestingFeatures]) [0]
					else:
						rowCopy = dict(rawTestingFeatures[i]); #copy features
						if args.grammar == FULL and (col in [0, 2]):
							rowCopy[ROOT] = 1
						elif args.grammar == MINI and col  == 0:
							rowCopy[THIS] = 1
						elif args.grammar == MINI and col  == 2:
							rowCopy[THAT] = 1
							rowCopy[prevBeamEntry[1][0]] = 1

						for j in range (0, len(filteredDeps)):  #for all conditional dependencies
							#add label to the set of features
							#rowCopy["DEP_" + str(filteredDeps[j][0]) + "_" + prevBeamEntry[1][filteredDeps[j][0]] ] = 1 #prevBeamEntry[list of previous labels][ particular column denoted by filteredDeps]
							
							#use gold standard context labels, or use actual predictions 
							#this is just to check the effect of the label bias problem
							if args.noLabelBias:
								prevLabel = rawTestingLabels[i][filteredDeps[j][0]]
							else:
								prevLabel = prevBeamEntry[1][filteredDeps[j][0]]  #prevBeamEntry[list of previous labels][ particular column denoted by filteredDeps]
							rowCopy[prevLabel] = 1 
							

						#Run the classifier for the current combination of conditional dependency labels 
						#dictTestingFeatures = dict(zip (rowCopy, map( lambda y: 1, rowCopy)))
						dictTestingFeatures = rowCopy
						vectorizedTestingFeatures = vectorizers[col].transform(dictTestingFeatures)
						if args.model == NN:
							vectorizedTestingFeatures = vectorizedTestingFeatures.toarray()
						if args.svd > 0:
							#The commented line below is equivalent to vectorizedTestingFeatures = vectorizedTestingFeatures.dot (SVDs[col].components_.T)
							#For no obvious reason, it is terribly slow, while the equivalent line after it is very fast. 
							#vectorizedTestingFeatures = SVDs[col].transform(vectorizedTestingFeatures)
							vectorizedTestingFeatures = np.dot (vectorizedTestingFeatures.toarray(),  SVDs[col].components_.T)
						
					classConf = []
					#break_point()
					if args.model == MEMM:
						classConf.extend( classifiers[col].predict_proba(vectorizedTestingFeatures)) #classifier confidences
					elif args.model in  [NN, RNN]:
						if args.svd > 0:
							vectorizedTestingFeatures = np.array(vectorizedTestingFeatures, dtype=np.float32)
						#pdb.set_trace()
						if not (args.model == RNN and args.complexNN):  #if not complexNN
							proba = classifiers[col].predict_proba(vectorizedTestingFeatures) #classifier confidences
							if args.model == RNN: 
								proba = [proba[-1:][0, 0, :]]  # results of the last time step
						else: #complexNN
							if col == 0: 
								complexNNproba = classifiers[col].predict_proba(vectorizedTestingFeatures) #classifier confidences
								complexNNproba = complexNNproba[-4:][:, 0, :]
							prevLength = sum([len(x) for x in fromIndexToLabelDicts[0:col]])
							proba = [complexNNproba[col][prevLength:prevLength+len(fromIndexToLabelDicts[col])]]
						classConf.extend( proba ) 
					else:
						raise Exception("not reachable");


					allColLabels = []
					if args.model == MEMM:
						allColLabels = classifiers[col].classes_
					elif args.model in  [NN, RNN]: 
						allColLabels = np.array([fromIndexToLabelDicts[col][x] for x in range (0, len(fromIndexToLabelDicts[col]))])
						#topLabels.extend( map(fromIndexToLabelDicts[col].get, topSortedScoresIndex) )
					else:
						raise Exception("not reachable");
					assert(len(allColLabels) == len(classConf[0]))
					labelsScores = np.multiply(classConf[0], prevBeamEntry[0]) #
										
					if not args.noLabelBias:  #no validation, just to make evaluating effect of label bias easier. 
						#if not valid label, set score to zero  (I am using propabilities, not log propabilities)					
						for labelIdx in range(0, len(labelsScores)):
							if not isValid (prevBeamEntry[1], col, allColLabels[labelIdx]):
								labelsScores[labelIdx] = 0
					#pdb.set_trace()
					
					sortedScoresIndex = np.argsort(-labelsScores) #sort descending
					topSortedScoresIndex = sortedScoresIndex[0:args.beam] # use just the top args.beam entries
					topLabels = allColLabels[topSortedScoresIndex]
					
					def addLabelToPartialHypothesis (label):
						y = list(prevBeamEntry[1]) #copy the previous partial hypothesis
						y[col] = label #add the new label
						return y
					
					newPartialHypothesis = map (addLabelToPartialHypothesis, topLabels )
					newProposedBeamEntries = zip(labelsScores[topSortedScoresIndex], newPartialHypothesis)
					newBeam = newProposedBeamEntries +  newBeam
					newBeam = sorted(newBeam, key=lambda e: -e[0])[0:args.beam] #sort on the scores of the partial hypothesis then pick the top args.beam element
					
				#replace the previous beam with the new one with added hypothesis
				beam = newBeam
			bestTree = 	beam[0][1];

		elif args.model == MAXENT:
			q =  Queue.Queue() #queue of partial trees. Each entry in the queue is a pair (partial tree, score)
			#initialize q
			q.put ((0, [None]*6))   # (score, list(labels), non)
			while not q.empty():
				item = q.get(); #(score, list(label))
				#pdb.set_trace()
				if len(filter(None, item[1])) == len(traversalOrder): #full tree
					#pdb.set_trace()
					if item[0] > bestTreeScore: # a better tree found
						bestTree = item[1]
						bestTreeScore = item[0]
				else:
					colIndex = len(filter(None, item[1]))
					col = traversalOrder[colIndex] #which column in rawTestingLabels is being added now
					scores = predictions_proba[col][i] #prediction scores of the data entry i for column col
					sortedScoresIndex = np.argsort(-scores) #sort descending
					labels = classifiers[col].classes_;
					assert len(labels) == len(scores) and len(labels) == len(sortedScoresIndex)
					beam = 0;
					for currPointer in range (0, len(scores)):
						labelToTry =  labels[sortedScoresIndex[currPointer]]
						if isValid (item[1], col, labelToTry): #is it valid to add labelToTry to col given this partial tree ?
							newPartialTree = (item[1])[:]  #clone
							newPartialTree[col] = labelToTry
							#newPartialTree.append(labelToTry)
							q.put ( (item[0] + scores[sortedScoresIndex[currPointer]], newPartialTree))
							beam = beam + 1
							if beam == args.beam: # found enough 
								break;
		else:
			raise "Invalid model: " + args.model

		result.put((i, bestTree))  #>>>>>>>>>>>>>>>>>>>>>>>(1)
	#with struct_predictions_lock:
	logging.info( "Thread: " + str(fromIndex) + "-" + str(toIndex) + " finished" )
	#pdb.set_trace()
	sys.exit(0)
	return
	

def featurize(s):
	#words = s.lower().strip().split(" ")
	#words = s.decode("utf-8").strip().lower().replace(":", "_COLON").replace("=", "_EQUALS_").split(" ")
	cleanedS = s.decode("utf-8").strip().lower()
	words = cleanedS.split(" ")

	
	features = []
	
	#if args.model == RNN:
	#	features = []
	#else:
	#	features = dict()
	
	#def addToFeat (f) # increment count by one
	#	features[f] = features.get(f, 0) + 1
	
	#uni-grams
	if not args.limitFeat:
		features.extend(words)
		#[addToFeat(w) for w in words ]


	if args.model != RNN: # if not RNN
		features = features + [brownClsList.get(w, None) for w in words if brownClsList.get(w, None) is not None ] 
		#[addToFeat(brownClsList.get(w, None)) for w in words if brownClsList.get(w, None) is not None ] 
	
	#bi-grams
	if not args.limitFeat:
		if not args.noBigram:
			for i in range (0, len(words)-1):
				features.append (words[i] + "+" + words[i+1])
				#addToFeat (words[i] + "+" + words[i+1])
	
	#character tri-grams
	for word in words:
		wordChr3Grams = list();
		for end in range (2, max(3, len(word)) + 1):
		#for end in range (2, len(word)):
			begin = max(0, end - 3);
			wordChr3Gram = "char3gram-" + word[begin:end]
			wordChr3Grams.append(wordChr3Gram);
			if not args.charSeq and charNgramSet != None:  #if not in the characterSequence mode, and in the training set, collect char3grams. Do not collect char3gram for test and validate set. 
				charNgramSet[wordChr3Gram] = charNgramSet.get(wordChr3Gram,0)  + 1  #map from char3grams to frequency. Frequencies will be thresholded, then the map will be to build the bit vector 

		if args.model == RNN: # if RNN
			features.append(wordChr3Grams) # keep char3grams of each word separate
		else:
			features.extend(wordChr3Grams)
			
			

	if args.model == RNN: # if RNN
		if args.charSeq: #just a sequence of characters
			features = list(cleanedS)
			#if charNgramSet != None:  #for training set, collect charNgrams, but not for testing set. 
			#	for ch in features:
			#		charNgramSet[ch] = charNgramSet.get(ch,0)  + 1

		return features; #char3grams
	
	featuresDict = dict(zip (features, map( lambda y: 1, features))) #binary features
	
	sentenceVec = np.ones(vsVecLen) * float("-inf")
	changed = False;

	for w in words: 
		if w in vs: 
			changed =True
			if args.vsFun == MAX:
				np.maximum(sentenceVec, vs[w], sentenceVec);
			elif args.vsFun == ADD:
				np.add(sentenceVec, vs[w], sentenceVec);
			else:
				raise Exception("Invalid " + args.vsFun)

	if not changed:
		sentenceVec = np.zeros(vsVecLen)

	realFeatures = dict(zip ( [ "vs_" + str(x) for x in range(0, vsVecLen)],  sentenceVec )) 
	featuresDict.update(realFeatures)
	return featuresDict

def multiclassMultilabelAccuracy (X, Y):
	assert len(X) == len(Y)
	totalCount = len(X)
	correctCount = 0.0
	for i in range(0, len(X)):
		assert len(X[i]) == len(Y[i])
		if X[i] == Y[i]:
			correctCount = correctCount + 1
		#	print "\t".join (X[i]) + " == "  +  "\t".join (Y[i])
		#else:
		#	print "\t".join (X[i]) + " <> " +  "\t".join (Y[i])
	#pdb.set_trace()
	return correctCount/totalCount


brownClsList = dict()
vs = dict()
rawTrainingFeatures = []
rawPreTrainingFeatures = []
rawTrainingLabels = []
rawTestingFeatures = []
rawTestingLabels = []
unzipedTrainingLabels = []
unzipedTestingLabels = []
constraintsTable = dict()
classifiers = []
predictions = []
predictions_proba = []
vectorizers = []
SVDs = []
vsVecLen = 0
testIDs = []
intelligibleTestIDs = []
fromLabelToIndexDicts = [] # array of dictionaries to map from label to its index. used with NN
fromIndexToLabelDicts = [] # array of dictionaries to map from index to its label. used with NN
charNgramSet = dict() #a set of all character tri-grams (or uni-grams) used

def init ():
	global vsVecLen
	global charNgramSet
	global rawTrainingFeatures
	global rawTestingFeatures
	global rawPreTrainingFeatures
	global unzipedTrainingLabels
	global unzipedTestingLabels
	global rawTrainingLabels
	global rawTestingLabels

	printArgs()

	brownCls = open(args.brown)
	vsFile = []
	pretrainLang = []
	if args.vs != "":
		vsFile = open(args.vs)
	if args.pretrain:
		pretrainLang = open(pretrainLangFile)
	trainCode = open(trainCodeFile)
	trainLang = open(trainLangFile)
	devCode = open(devCodeFile)
	devLang = open(devLangFile)
	testCode = open(testCodeFile)
	testLang = open(testLangFile)


	#brownClsList = dict();
	for line in brownCls:
		splits = line.strip().split("\t"); #(clusterID, word, frequncy)
		if (int(splits[2]) > args.browThr):
			brownClsList[splits[1]] = splits[0];

	#vs = dict();
	for line in vsFile:
		splits = line.strip().split(" "); #(clusterID, word, frequncy)
		word = splits[0]
		vec = splits[1:]
		vs[word] = [float(x) for x in vec]
		vsVecLen = len(vec)

	#<<<<<<<<<<<<<<<<<<<<<<<<<<<<
	#trainLang = open('tmp/trainFeatures')
	#testLang = open('tmp/testFeatures')
	#<<<<<<<<<<<<<<<<<<<<<<<<<<<<

	#reading training data
				
	def isIntel(s):
		try:
			asciiString = s.decode('ascii')
		except UnicodeDecodeError:
			return False
		else:
			splits = asciiString.strip().split(" ")
			if len(splits) < 3:
				return False
		return True	
	

	errors = []
	#rawTrainingFeatures = []
	featuresSet = dict()
	allWords = []#dict()
	intelTrain = []
	paraphrases = []
	vocab = []
	wnl = WordNetLemmatizer()
	trainingLines = []
	stopWords = ['a', 'the', 'an', 'it']
	nonIntel = []

	def isAsciiString(s):
		return sum([not ascii.isascii(x) for x in s]) == 0

	def readTrainLangLine (line):
		if args.intel ==  False or isIntel (line):
			intelTrain.append(True)
			
			fList = featurize(line)
			rawTrainingFeatures.append(fList)
			if args.model != RNN: #I do not want to count the features in case of RNN
				for f in fList:
					featuresSet[f] = featuresSet.get(f, 0) + 1;
				
			if args.paraphrase:
				words = line.strip().lower().split(" ")
				vocab.extend([wnl.lemmatize(x.decode("utf-8")) for x in words])
				trainingLines.append((cnt, line)) #I need the cnt to find the labels after reading the code.txt file
		else:
			intelTrain.append(False)
			nonIntel.append(line)
	
	cnt = 0	
	for line in trainLang:			
		readTrainLangLine(line)
		cnt = cnt + 1;
		if cnt == args.trainCnt:
			break;
	if args.foldCnt > 0: #read the development set and add it to the training set
		for line in devLang:
			readTrainLangLine(line)

	#wordnetWords = set()
	#wnl = WordNetLemmatizer()
	#for w in allWords:
	#	wLemma =  wnl.lemmatize(w)
	#	cnt = len(wordnet.synsets(wLemma))
	#	if cnt > 0:
	#		wordnetWords.add((wLemma, cnt))
			#wordnetWords.append((wLemma, cnt))

	def extractIngredientNames (s): 
		args = s.split("#@#")
		cleaned = map(lambda x: (x.split(" ", 2))[0], args)
		cleanedStr = ','.join([str(x) for x in cleaned])
		return cleanedStr

	#rawTrainingLabels = []
	def readTrainCodeLine (line):
		if intelTrain[cnt] == True:
			rawLabels = line.strip().split("\t") #channel, function, channel, function, ing, ing
			rawTrainingLabels.append( ["T.CH_" + rawLabels[0], "T.F_" +rawLabels[1], "A.CH_" +rawLabels[2], "A.F_" +rawLabels[3], "T.I_" + extractIngredientNames(rawLabels[4]), "A.I_" +extractIngredientNames(rawLabels[5]) ])
			#rawTrainingLabels.append( [rawLabels[0], rawLabels[1], rawLabels[2], rawLabels[3], extractIngredientNames(rawLabels[4]), extractIngredientNames(rawLabels[5]) ])
	cnt = 0	
	for line in trainCode:
		readTrainCodeLine(line)
		cnt = cnt + 1;
		if cnt == args.trainCnt:
			break;
	if args.foldCnt > 0: #read the development set and add it to the training set
		for line in devCode:
			readTrainCodeLine(line)

	vocab = set(vocab)
	cnt = 0
	if args.paraphrase:
		for lineTuple in trainingLines:
			line = lineTuple[1]
			idx = lineTuple[0]
			words = line.strip().lower().split(" ")
			#vocab.extend([wnl.lemmatize(x.decode("utf-8")) for x in words])
			#trainingLines.append((cnt, line)) #I need the cnt to find the labels after reading the code.txt file
			newWords = []
			changed = False
			for w in words:
				newWord = w
				if isAsciiString(w): #if ascii word
					allSyn = wordnet.synsets(w);
					allLemmas = [x.lemmas() for x in allSyn]
					allLemmas = set([item for sublist in allLemmas for item in sublist]) #flat set
					allLemmasNames = set([x.name() for x in allLemmas])
					wLemma = wnl.lemmatize(w)
					if wLemma in allLemmasNames:
						allLemmasNames.remove (wLemma);
					candidateLemmas = vocab.intersection(allLemmasNames)
					#if len(candidateLemmas ) == 0 and len(allLemmasNames) > 0:
					#	pdb.set_trace()	
					if len (candidateLemmas) > 0:
						newWord = seeder.choice( list(candidateLemmas) ) # for now, just use any random one 
						changed = True
				newWords.append(newWord)
			if changed:
				try:
					newLine = " ".join(newWords)
					fList = featurize(newLine)

					rawTrainingFeatures.append (fList)
					rawTrainingLabels.append (rawTrainingLabels[idx])
					cnt = cnt + 1
					for f in fList:
						featuresSet[f] = featuresSet.get(f, 0) + 1;
				except :
					errors.append(newWords)
		logging.info ("Paraphrase sentences generated: " + str(cnt))

	#add the paraphrases to the training data
	#for p in paraphrases:
	#	rawTrainingFeatures.append (p[1])
	#	rawTrainingLabels.append (rawTrainingLabels[p[0]])

	#pdb.set_trace()
	#prepare features dictionary
	#featuresDict = dict(zip(featuresSet, range (1, len(featuresSet) + 1)))
	#dictTrainingFeatures = map(lambda x:  dict(zip (x, map( lambda y: featuresDict[y], x))) , rawTrainingFeatures)

	#reading testIDs and intelligibleIDs
	f = open(testIDsFile)
	for line in f:
		testIDs.append(int(line.strip()))
	f.close()
	
	if args.intel:
		f = open(intelligibleIDsFile)
		for line in open(intelligibleIDsFile):
			intelligibleTestIDs.append(int(line.strip()))
		f.close()

	#reading testing data
	#dictTestingFeatures = []

	#for i in range (0, len(testIDs) ):
	#	print str(testIDs[i] in intelligibleTestIDs)
	
	
	#ugly hack(1/2): just set charNgramSet to None telling featurize not to edit it.
	tmp = charNgramSet
	charNgramSet = None
	#rawTestingFeatures = []
	cnt = 0
	for line in testLang:
		if args.intel == False or args.mode == TRAIN or testIDs[cnt] in intelligibleTestIDs: 
			fList = featurize(line)
			#fList = [x.split(" ")[0] for x in line.strip().split(",")] #<<<<<<<<<<<<<<<<<<
			rawTestingFeatures.append(fList)
			#print str(cnt) + " ->" + str(testIDs[cnt]);
			#fListDict = dict(zip (fList, map( lambda y: featuresDict.get(y, 0), fList)))
			#dictTestingFeatures.append(fListDict)
		cnt = cnt + 1;
	
	try:
		cnt = 0
		logging.info("Start reading pretraining data")
		for line in pretrainLang:
			fList = featurize(line.split('\t')[3])
			rawPreTrainingFeatures.append(fList)
			cnt = cnt + 1;
			if cnt%10000 == 0:
				logging.info(str(cnt))
			if cnt == args.pretrainCnt:
				break;
		logging.info("Done reading pretraining data")
	except:
		print "KeyboardInterrupt"
		break_point()
	
	#ugly hack(2/2): the rest of the ugly hack. get the old value of charNgramSet back from tmp.
	charNgramSet = tmp
	
	#rawTestingLabels = []
	cnt = 0
	for line in testCode:
		if args.intel == False or args.mode == TRAIN or testIDs[cnt] in intelligibleTestIDs:
			rawLabels = line.strip().split("\t") #channel, function, channel, function, ing, ing
			rawTestingLabels.append( ["T.CH_" + rawLabels[0], "T.F_" +rawLabels[1], "A.CH_" +rawLabels[2], "A.F_" +rawLabels[3], "T.I_" + extractIngredientNames(rawLabels[4]), "A.I_" +extractIngredientNames(rawLabels[5]) ])
		cnt = cnt + 1;

	#close files
	brownCls.close()
	trainCode.close()
	trainLang.close()
	devCode.close()
	devLang.close()
	testCode.close()
	testLang.close()
	if args.vs != "":
		vsFile.close()
	
	if args.pretrain:
		pretrainLang.close()

	#process the kfolds
	if args.foldCnt > 0 : 
		assert args.foldIdx >= 0 and  args.foldIdx < args.foldCnt
		assert len(rawTrainingFeatures) == len (rawTrainingLabels);
		kf = cross_validation.KFold(len(rawTrainingFeatures), n_folds=args.foldCnt, shuffle=True, random_state=42) #fixed random number generator for reproducibility
		fold = [x for x in kf] [args.foldIdx]
		if args.mode == TRAIN: # If training, use the last for validation. If testing, keep the testing data as is
			rawTestingFeatures = [rawTrainingFeatures[x] for x in fold[1]]
			rawTestingLabels = [rawTrainingLabels[x] for x in fold[1]]
		rawTrainingFeatures = [rawTrainingFeatures[x] for x in fold[0]]
		rawTrainingLabels = [rawTrainingLabels[x] for x in fold[0]]
			
	#prepare labels
	unzipedTrainingLabels.extend(zip(*rawTrainingLabels));
	unzipedTestingLabels.extend(zip(*rawTestingLabels));

	#prepare constraints tables
	#constraintsTable = dict();
	for id in constraintEdges:
		constraintsTable[id] = set(zip(unzipedTrainingLabels[id[0]], unzipedTrainingLabels[id[1]]))
	
	#pdb.set_trace()
	if args.model != RNN:
		for row in rawTrainingFeatures:
			toBeDeleted = [] #can not delete from a dictionary while iterating it. 
			for f in row: 
				if featuresSet.get(f, 0) <= args.featFreq:
					toBeDeleted.append(f)
			for d in toBeDeleted: #delete now.
				del row[d]

	
	
	if args.grammar == FULL: 
		contextTrainingLabels = []
		contextTestingLabels = []
		for toColumn in [0, 1, 2, 3]:
			contextColIdx = [x for x in deps if x[1] == toColumn] #indices of context columns of toColumn
			extraTrainCols = [unzipedTrainingLabels[x[0]] for x in contextColIdx ]
			extraTestCols = [unzipedTestingLabels[x[0]] for x in contextColIdx ]
			if toColumn in [0, 2]: #channels, they have ROOT at parent 
				extraTrainCols.append( tuple([ROOT] * len(unzipedTrainingLabels[0])) )
				extraTestCols.append( tuple([ROOT] * len(unzipedTestingLabels[0])) )
			contextTrainingLabels.extend(zip(*extraTrainCols))
			contextTestingLabels.extend(zip(*extraTestCols))

		#contextTrainingLabels = tuple([ROOT] * len(unzipedTrainingLabels[0])) +  unzipedTrainingLabels[0]  + unzipedTrainingLabels[0] + unzipedTrainingLabels[2]
		unzipedTrainingLabels =  np.ravel(unzipedTrainingLabels[0:4])
		rawTrainingFeatures = rawTrainingFeatures + list(rawTrainingFeatures) + list(rawTrainingFeatures) + list(rawTrainingFeatures)
		assert len(rawTrainingFeatures) == len(contextTrainingLabels)
		assert len(rawTrainingFeatures) == len(unzipedTrainingLabels)
		for i in range (0, len(rawTrainingFeatures)):
			rawTrainingFeatures[i] = dict(rawTrainingFeatures[i]) #copy the dictionary first
			for l in contextTrainingLabels[i]: #then add the context labels to it
				rawTrainingFeatures[i] [l] = 1 
		unzipedTrainingLabels = [unzipedTrainingLabels]  # for backward compatibility with the code below that I wrote assuming unzipedTrainingLabels is an array of arrays

		if args.mode == TRAIN:  # if in the training phase, change the testing data for the sake of validation. 
								#but if in a testing phase, keep the testing data as it is
			#contextTestingLabels = tuple([ROOT] * len(unzipedTestingLabels[0])) +  unzipedTestingLabels[0]  + unzipedTestingLabels[0] + unzipedTestingLabels[2]
			unzipedTestingLabels =  np.ravel(unzipedTestingLabels[0:4])
			rawTestingFeatures = rawTestingFeatures + list(rawTestingFeatures)  + list(rawTestingFeatures)  + list(rawTestingFeatures)
			assert len(rawTestingFeatures) == len(contextTestingLabels)
			assert len(rawTestingFeatures) == len(unzipedTestingLabels)
			for i in range (0, len(rawTestingFeatures)):
				rawTestingFeatures[i] = dict(rawTestingFeatures[i])
				for l in contextTestingLabels[i]: #then add the context labels to it
					rawTestingFeatures[i] [l] = 1 
			unzipedTestingLabels = [unzipedTestingLabels]    #

	elif args.grammar == MINI: 

		unzipedTrainingLabels[0] = [delimiter.join (x) for x in zip(unzipedTrainingLabels[0], unzipedTrainingLabels[1])]
		unzipedTrainingLabels[2] = [delimiter.join (x) for x in zip(unzipedTrainingLabels[2], unzipedTrainingLabels[3])]
		
		unzipedTestingLabels[0] =  [delimiter.join (x) for x in  zip(unzipedTestingLabels[0], unzipedTestingLabels[1])]
		unzipedTestingLabels[2] =  [delimiter.join (x) for x in  zip(unzipedTestingLabels[2], unzipedTestingLabels[3])]
		
		contextTrainingLabels = [[THIS]] * len(unzipedTrainingLabels[0]) +  zip( [THAT] * len(unzipedTrainingLabels[0]),  unzipedTrainingLabels[0])
		contextTestingLabels = [[THIS]] * len(unzipedTestingLabels[0]) +  zip( [THAT] * len(unzipedTestingLabels[0]),  unzipedTestingLabels[0])
		
		#contextTrainingLabels = [[THIS]] * len(unzipedTrainingLabels[0]) + [[THAT]] * len(unzipedTrainingLabels[0])
		#contextTestingLabels = [[THIS]] * len(unzipedTestingLabels[0]) +   [[THAT]] * len(unzipedTestingLabels[0])
				
		unzipedTrainingLabels = unzipedTrainingLabels[0] + unzipedTrainingLabels[2]		
		
		rawTrainingFeatures = rawTrainingFeatures + list(rawTrainingFeatures)

		assert len(rawTrainingFeatures) == len(contextTrainingLabels)
		assert len(rawTrainingFeatures) == len(unzipedTrainingLabels)
		for i in range (0, len(rawTrainingFeatures)):
			rawTrainingFeatures[i] = dict(rawTrainingFeatures[i]) #copy the dictionary first
			for l in contextTrainingLabels[i]: #then add the context labels to it
				rawTrainingFeatures[i] [l] = 1
			#rawTrainingFeatures[i] [contextTrainingLabels[i]] = 1

		unzipedTrainingLabels = [unzipedTrainingLabels]  # for backward compatibility with the code below that I wrote assuming unzipedTrainingLabels is an array of arrays
		if args.mode == TRAIN:  # if in the training phase, change the testing data for the sake of validation. 
								#but if in a testing phase, also change it to combine the columns. Make sure to run test with argument -struct c 
			
			unzipedTestingLabels = unzipedTestingLabels[0] + unzipedTestingLabels[2]		
			rawTestingFeatures = rawTestingFeatures + list(rawTestingFeatures) 
			
			assert len(rawTestingFeatures) == len(contextTestingLabels)
			assert len(rawTestingFeatures) == len(unzipedTestingLabels)
			for i in range (0, len(rawTestingFeatures)):
				rawTestingFeatures[i] = dict(rawTestingFeatures[i])
				for l in contextTestingLabels[i]: #then add the context labels to it
					rawTestingFeatures[i] [l] = 1 
				#rawTestingFeatures[i] [contextTestingLabels[i]] = 1
			unzipedTestingLabels = [unzipedTestingLabels]    #			

	if args.model == RNN:
		if args.charSeq: 
			charStat = np.unique([item for sublist in rawTrainingFeatures for item in sublist], return_counts=True)
			charNgramSet = dict(zip(charStat[0], charStat[1]))
		logging.info ("Number of charNgram before filtering: " + str(len(charNgramSet)))
		charNgramSet = [x[0] for x in charNgramSet.items() if x[1] > args.char3freq] #drop charNgrams with frequency less than a threshold
		logging.info ("Number of charNgram after filtering: " + str(len(charNgramSet)))

		#input: list of list of char3gram
		#output: list of bit vectors as one sparse matrix
		#actually, I used counts, not bits 
		def char3gramsToBitVector(words):
			featSize = len(charNgramSet) + 1
			if args.complexNN:
				featSize = featSize + 4 # add place for extra features that will be used later. 
										#It would have been more readable to do this change later, 
										#but I will have to resize the array. That is why I am doing it here.				
			bitVecs = lil_matrix((len(words), featSize), dtype=np.int32)
			for i in range (0, len (words)):
				for j in range (0, len (words[i])):
					index = charNgramSet.get(words[i][j], 0) #zero is the default if the char3gram is not found
					bitVecs[i, index] = 1 + bitVecs[i, index];			
			return bitVecs

		charNgramSet = dict (zip (charNgramSet, range (1, len(charNgramSet) + 1))) #replace frequencies with index
		#rawTrainingFeatures = [[char3gramsToBitVector(word) for word in row ] for row in rawTrainingFeatures]
		logging.info ("Start char3gramsToBitVector for the training set")
		rawTrainingFeatures = [char3gramsToBitVector(row) for row in rawTrainingFeatures]
		logging.info ("Start char3gramsToBitVector for the testing set")
		rawTestingFeatures = [char3gramsToBitVector(row) for row in rawTestingFeatures]
		logging.info ("Start char3gramsToBitVector for the pretraining set")
		rawPreTrainingFeatures = [char3gramsToBitVector(row) for row in rawPreTrainingFeatures]

if (args.mode == TRAIN and __name__ == '__main__') or (args.mode == TEST ):
	init()

#classifiers = []
#predictions = []
#predictions_proba = []
#vectorizers = []

#for i in range (0, len(unzipedTrainingLabels)):
for i in traversalOrder:

	extraTrainCols = []
	extraTestCols = []
	extendedRawTrainFeat = [] #list(rawTrainingFeatures);
	extendedRawTestFeat = [] #list(rawTrainingFeatures);
	vectorizer = DictVectorizer(dtype=np.float32)

	if i < 0:
		classifiers.append(None)
		predictions.append(None)
		predictions_proba.append(None)
		vectorizers.append(None)
		SVDs.append(None)
		fromIndexToLabelDicts.append(None)
		fromLabelToIndexDicts.append(None)
		continue;
	
	if  (args.mode == TRAIN and __name__ == '__main__') or (args.mode == TEST and __name__ != '__main__') or args.debug:
		
		if i > 0 and (args.complexNN or args.grammar in [FULL, MINI]): #if (complexNN or grammar), all columns are learned in the first iteration
			vectorizers.append(vectorizers[0]) #the vectorizer used for the first column will be used during testing for all columns.
			classifiers.append(classifiers[0]) #the classifier used for the first column will be used during testing for all columns.
			fromIndexToLabelDicts.append(fromIndexToLabelDicts[0]) #same as above
			continue;	
		
		allLabels = []
		#(index, useless) = np.unique (unzipedTrainingLabels[i], return_inverse=True)
		if args.complexNN:
			#collect all labels in one column (think of it as a list of all possible rules in the grammar)
			#Do it for functions and channels, but not for ingredients because we just use the function signature
			#which is unique given the channel and function
			#prevLength = 0
			for labelCol in range (0, 4): #channels and functions only 
				colLabels = sorted(list(set(unzipedTrainingLabels[labelCol])))
				allLabels.extend( colLabels )
				#fromIndexToLabelDicts.append ( dict(zip(range(prevLength, len(colLabels)+prevLength), colLabels))  )
				#prevLength = prevLength + len(colLabels)
				fromIndexToLabelDicts.append ( dict(zip(range(0, len(colLabels)), colLabels))  )
		else:
			allLabels.extend (sorted(list(set(unzipedTrainingLabels[i]))))
			fromIndexToLabelDicts.append ( dict(zip(range(0, len(allLabels)), allLabels))  )
		fromLabelToIndexDicts.append ( dict(zip(allLabels, range(0, len(allLabels))))  )

		#print datetime.datetime.now()
		modelFileName = 'col' + str(i)
		#modelFilePath = "tmp\\models_" + modelName +"\\" + modelFileName + "\\model.pkl"
		modelFilePath = args.modelFile +"\\" + modelFileName
		if not os.path.exists(modelFilePath):
			os.makedirs(modelFilePath)
		modelFilePath = modelFilePath + "\\model.pkl"

		#if i == 1:
		#	pdb.set_trace()
		
		filteredDeps = [x for x in deps if x[1] == i] #find dependencies of the current column
		
		#add features representing dependencies
		if args.model in [MEMM, NN, RNN]:
			extraTrainCols = [unzipedTrainingLabels[x[0]] for x in filteredDeps ]
			extraTestCols = [unzipedTestingLabels[x[0]] for x in filteredDeps ]
		elif args.model == MAXENT:
			extraTrainCols = [] # do nothing
			extraTestCols = [] # do nothing
		else:
			raise "Invalid model: " + args.model
			
		
		#copy features + add dependency features + set pretraining data
		
		#TODO: what did I change that makes the following line break ?? :(  grrrr
		rawPreTrainingFeatures = rawPreTrainingFeatures + rawTrainingFeatures

		seeder.shuffle(rawPreTrainingFeatures)
		#shuffle the training data
		shuffler = range(0, len(rawTrainingFeatures))
		seeder.shuffle(shuffler);
		rawTrainingFeatures = np.array(rawTrainingFeatures)[shuffler]
		unzipedTrainingLabels[i] = np.array(unzipedTrainingLabels[i])[shuffler]
		
		if args.model == RNN:
			vectorizedTrainingFeatures = rawTrainingFeatures
			vectorizedTestingFeatures = rawTestingFeatures
			vectorizedPreTrainingFeatures = rawPreTrainingFeatures
			vectorizers.append(charNgramSet)
		else:
			if args.grammar in [FULL, MINI]:  #if grammar, then no need for the extra columns because they are already added
				extendedRawTrainFeat = rawTrainingFeatures
				extendedRawTestFeat = rawTestingFeatures
			else:
				for j in range(0, len(rawTrainingFeatures)):
					rowCopy = dict(rawTrainingFeatures[j])
					extendedRawTrainFeat.append(rowCopy) 
					for k in range (0, len(extraTrainCols)):
						#rowCopy.append("DEP_" + str(filteredDeps[k][0]) + "_" + extraTrainCols[k][j] )
						#rowCopy["DEP_" + str(filteredDeps[k][0]) + "_" + extraTrainCols[k][j] ] = 1;
						rowCopy[extraTrainCols[k][j] ] = 1;
				#for the dev/test set
				for j in range(0, len(rawTestingFeatures)):
					rowCopy = dict(rawTestingFeatures[j])
					extendedRawTestFeat.append(rowCopy) 
					for k in range (0, len(extraTestCols)):
						#rowCopy.append("DEP_" + str(filteredDeps[k][0]) + "_" + extraTestCols[k][j] )
						rowCopy["DEP_" + str(filteredDeps[k][0]) + "_" + extraTestCols[k][j] ] = 1;
				
			#vectorization for training and testing features
			
			#pdb.set_trace()
			vectorizers.append(vectorizer)
			#dictTrainingFeatures = map(lambda x:  dict(zip (x, map( lambda y: 1, x))) , extendedRawTrainFeat)
			dictTrainingFeatures = extendedRawTrainFeat
			vectorizedTrainingFeatures = vectorizer.fit_transform(dictTrainingFeatures)
			
			logging.info ("Number of features: " + str(len(vectorizer.feature_names_))) 
			#logging.info ("All labels: " + str(len(allLabels))) 
			#[logging.info(x) for x in vectorizer.feature_names_]
			#sys.exit()
			svdFileName = 'svd' + str(i) + '.npy'
			svdFilePath = args.modelFile +"\\" + svdFileName
			#break_point()
			if args.svd > 0: 
				svd = TruncatedSVD(n_components=args.svd, random_state=42)
				SVDs.append(svd)
				if not os.path.isfile(svdFilePath):
					logging.info( "calculate SVD " + str(i))
					vectorizedTrainingFeatures = svd.fit_transform(vectorizedTrainingFeatures)
					np.save(svdFilePath, svd.components_)
				else:
					logging.info( "load SVD " + str(i))
					svd.components_ = np.load(svdFilePath)
					vectorizedTrainingFeatures = svd.transform(vectorizedTrainingFeatures)

			#TOOOODDOOOOOOOOO
			#dictTestingFeatures = map(lambda x:  dict(zip (x, map( lambda y: 1, x))) , rawTestingFeatures)
			dictTestingFeatures = extendedRawTestFeat
			vectorizedTestingFeatures = vectorizer.transform(dictTestingFeatures)
			if args.svd > 0: 
				vectorizedTestingFeatures = SVDs[i].transform(vectorizedTestingFeatures)
			
			vectorizedPreTrainingFeatures = vectorizer.transform(rawPreTrainingFeatures)
		
		if args.normalize:
			normalizer = StandardScaler(with_mean=False) # can not center sparse matrix. Calcualte mean now, then apply it when densify
			vectorizedTrainingFeatures = normalizer.fit_transform(vectorizedTrainingFeatures)
			means = vectorizedTrainingFeatures.mean(0)
			#vectorizedTrainingFeatures = normalizer.transform(vectorizedTrainingFeatures)		
			vectorizedTestingFeatures = normalizer.transform(vectorizedTestingFeatures)
			vectorizedPreTrainingFeatures = normalizer.transform(vectorizedPreTrainingFeatures)		

	#train and store model
	if args.mode == TRAIN and __name__ == '__main__': #if training and in the main process
		logging.info("start train col: " + str(i))
		trainingColumn =  unzipedTrainingLabels[i];
		testingColumn =  unzipedTestingLabels[i];

		if args.model in [ NN, RNN ]:
			
			def trainFun():
				#global lastIndex
				if (isinstance(train[0], type( [] ))): #list
					dataSize = len(train[0])
				else: #np.array or scipy.sparse.csr.csr_matrix
					dataSize = train[0].shape[0]

				lastIndex = seeder.randint( dataSize )
				start = lastIndex;
				lastIndex = lastIndex +  args.batch
				lastIndex = min(lastIndex, dataSize)

				features = train[0][start:lastIndex]

				if len(train) == 1:# if pretraining
					featuresLabels = [features]
				elif len(train) == 2:  #if training
					if args.complexNN:
						labels = train[1][:, start:lastIndex]
					else:
						labels = train[1][start:lastIndex]
					featuresLabels = [features, labels]
				else:
					raise Exception ("What !! ")

				if args.model == RNN:
					featuresLabels = toRnnBatch(featuresLabels)
				else:
					featuresLabels[0] = featuresLabels[0].toarray()
					#pdb.set_trace()
					if args.normalize:
						featuresLabels[0] = featuresLabels[0].toarray() - means;

				#if lastIndex >= len(train[1]):
				#	lastIndex = 0;
				
				return featuresLabels
				
			sparse_input = (args.svd == 0 and args.batch == 0 and not args.model == RNN)  #no svd and no given batch size which means use sparse input with the default batch size. 
			
			# >>>>>>>>>>>>begin NN parameters <<<<<<<<<<<<<<<<<<<<<<<
			# 1) Network architecture 
			assert len(allLabels) ==  len(fromLabelToIndexDicts[i])
			if args.model == RNN:
				weighted = True
				inputSize = len(charNgramSet) + 1 #the " + 1 " is for the OOV char3gram
				if args.complexNN:
					inputSize = inputSize + 4 # "+ 4", one for each output column
				layers = [ inputSize, 
							dict(size=100, nrng=seeder.randint(1000), activation=args.activation),
							dict(size=100, form='lstm', nrng=seeder.randint(1000), activation=args.activation), #bptt_limit=1, 
							#dict(size=100, form='lstm', nrng=seeder.randint(1000), activation=args.activation),
							dict(size=len(allLabels), nrng=seeder.randint(1000))]
				network = theanets.recurrent.Classifier
			else: 
				weighted = False
				#'sigmoid', 'relu', 'tanh'
				layers = [	int(vectorizedTrainingFeatures.shape[1]) ] 
				for l in range (0, args.layers):
					layers.append(dict(size=200, nrng=seeder.randint(1000), activation=args.activation)) 
				layers.append(dict(size=len(allLabels), nrng=seeder.randint(1000)))
				
				network = theanets.Classifier
			
			# 2) Training 			
			#sgd, nag, rprop, rmsprop, adadelta, esgd, adam
			commonKwargs = dict(optimize='rmsprop', nesterov=False, weight_l2=0, input_dropout=0.00, input_noise=0.00, save_progress=modelFilePath, save_every=-5, train_batches=100)
			ptKwargs = dict(learning_rate=1e-2, momentum=0.0, weight_l2=0.00, hidden_dropout=0.5, hidden_noise=0.01, rng=seeder.randint(1000), monitor_gradients=False, patience=1, min_improvement=0.05)
			kwargs =   dict(learning_rate=1e-3, momentum=0.0, weight_l1=0.0, weight_l2=0.005, hidden_dropout=0.5, hidden_noise=0.005, rng=seeder.randint(1000), monitor_gradients=False, patience=3)
			# >>>>>>>>>>>>end NN parameters <<<<<<<<<<<<<<<<<<<<<<<
		
			ptKwargs.update(commonKwargs)
			kwargs.update(commonKwargs)
			if args.reload and os.path.isfile(modelFilePath):
				cls = theanets.Experiment(modelFilePath)
				[l.log() for l in cls.network.layers]
			else:
				cls = theanets.Experiment(network, layers=layers, sparse_input=sparse_input, weighted=weighted)

			if args.pretrain:
				ptLayers = list(layers)
				ptOutLayer = dict(ptLayers[len(ptLayers)-1])
				ptOutLayer["size"] = ptLayers[0];
				ptLayers[len(ptLayers)-1] = ptOutLayer		
				if args.model == RNN:
					ptNetwork = theanets.recurrent.Predictor
					weighted = False
				else:
					ptNetwork = theanets.Autoencoder
				train = [vectorizedPreTrainingFeatures];
				
				if args.model == RNN:
					validate = [vectorizedTestingFeatures]; 
					validate = toRnnBatch(validate)
				else:
					validate = [vectorizedTestingFeatures.toarray()]; 
					if args.normalize:
						validate[0] = validate[0] - means;

				ptCls = theanets.Experiment(ptNetwork, layers=ptLayers, sparse_input=sparse_input, weighted=False)
				#lastIndex = 0 #should be set to zero before calling trainFun
				try:
					#KEEP input_noise=0.00 because changing it messes things up ALOT. It makes everything terribly slow. 
					ptCls.train(trainFun, validate, **ptKwargs)
				except KeyboardInterrupt:
					print "KeyboardInterrupt"

				logging.info("Pretrain Configurations: ")
				if args.model == RNN:
					logging.info("Train size: " + str( len(train[0]) ) + ", Valid size: " + str( validate[0].shape[1])  )
				else: 
					logging.info("Train size: " + str( train[0].shape[0] ) + ", Valid size: " + str( validate[0].shape[0])  )
				logging.info("Layers:" + str(ptLayers))
				logging.info("Train args:" + str(ptKwargs))

				assert len(cls.network.layers) == len(ptCls.network.layers)
				for l in range (1, len(ptCls.network.layers)-1 ): # for all hidden layers (ignore input layer -does not have parameters- and output layer -will be replaced with another layer-)
					param = cls.network.layers[l].params
					ptParam = ptCls.network.layers[l].params
					assert len(param) == len(ptParam)
					for p in range (0, len(param)):
						assert (ptCls.network.layers[l].params[p].name == cls.network.layers[l].params[p].name)
						w = ptCls.network.layers[l].params[p].get_value()
						cls.network.layers[l].params[p].set_value(w)


			if args.complexNN:
				trainingColumn =  unzipedTrainingLabels[0:4];
				testingColumn =  unzipedTestingLabels[0:4];
				trainingColumnLabels = [map(lambda x: fromLabelToIndexDicts[i].get(x, -1), y) for y in trainingColumn]
				testingColumnLabels = [map(lambda x: fromLabelToIndexDicts[i].get(x, -1), y) for y in testingColumn]
			else:
				trainingColumn =  unzipedTrainingLabels[i];
				testingColumn =  unzipedTestingLabels[i];
				trainingColumnLabels = map(lambda x: fromLabelToIndexDicts[i].get(x, -1), trainingColumn)
				testingColumnLabels = map(lambda x: fromLabelToIndexDicts[i].get(x, -1), testingColumn)

			if args.svd > 0:
				vectorizedTrainingFeatures = np.array(vectorizedTrainingFeatures, dtype=np.float32)
				vectorizedTestingFeatures = np.array(vectorizedTestingFeatures, dtype=np.float32)
			train = [vectorizedTrainingFeatures, np.array(trainingColumnLabels, dtype=np.int32)];
			
			trainInput = trainFun  #densified input
			if args.model == RNN:
				validate = [vectorizedTestingFeatures,  np.array(testingColumnLabels, dtype=np.int32)]; 
				validate = toRnnBatch(validate)
				if args.batch <=0 : #densify for RNN
					trainInput = toRnnBatch(train)
			else:
				validate = [vectorizedTestingFeatures.toarray(),  np.array(testingColumnLabels, dtype=np.int32)]; 
				if args.normalize:
					validate[0] = validate[0] - means;
				if args.batch <=0 :  #keep input array sparse
					trainInput = train #sparse input

	
			#lastIndex = 0  #should be set to zero before calling trainFun
			try:
				if args.layerwise:  #layerwise pretraining
					optimize = kwargs['optimize'];
					kwargs['optimize'] = 'layer'
					cls.train(trainInput, validate, **kwargs)
					kwargs['optimize'] = optimize;
				
				logging.info ("training set: " + str( len(trainingColumn)))
				#-----------------
				#pdb.set_trace()
				#lrCls = joblib.load(modelFilePath.replace("nn", "nn-lr"))
				#lrCls = joblib.load("C:\\Users\\t-isbelt\\tmp\\grammar-memm-8\\col0\\model.pkl")
				#cls.network.params[1].set_value(  np.array([x[0] for x in lrCls.intercept_], dtype=np.float32) )
				#cls.network.params[0].set_value( lrCls.coef_.T.astype('float32'))
				#pdb.set_trace()
				#metrics.accuracy_score(cls.network.predict(validate[0]), validate[1])
				#-----------------
				#if args.reload:
				#	pdb.set_trace()
				#	pred0 = cls.network.predict(validate[0][0:5000])
				#	pred1 = cls.network.predict(validate[0][5000:10000])
				#	pred2 = cls.network.predict(validate[0][10000:15000])
				#	pred3 = cls.network.predict(validate[0][15000:20000])
				#	pred4 = cls.network.predict(validate[0][20000:])
				#	preds = np.concatenate( (pred0, pred1, pred2, pred3, pred4))
				#	logging.info ("validation accuracy: " + str( metrics.accuracy_score(preds, validate[1]) ))
				#	pdb.set_trace()
				#	sys.exit()
				cls.train(trainInput, validate, **kwargs)

			except KeyboardInterrupt:
				print "KeyboardInterrupt"

			if args.pretrain:
				logging.info("Pretrain Configurations: ")
				logging.info("Train size: " + str( vectorizedPreTrainingFeatures.shape[0] ) + ", Valid size: " + str( validate[0].shape[0] ))
				logging.info("Layers:" + str(ptLayers))
				logging.info("Train args:" + str(ptKwargs))
			logging.info("Training Configurations: ")
			if args.model == RNN:
				logging.info("Train size: " + str( len(train[0]) ) + ", Valid size: " + str( validate[0].shape[1])  )
			else: 
				logging.info("Train size: " + str( train[0].shape[0] ) + ", Valid size: " + str( validate[0].shape[0])  )
			logging.info("Layers:" + str(layers))
			[l.log() for l in cls.network.layers]
			logging.info("Train args:" + str(kwargs))
			cls.save(modelFilePath);
			break_point()
			#exp.network.predict(X)
		else:

			baseCls = LogisticRegression()
			cls =  OneVsRestClassifier(baseCls)	
			cls.n_jobs = args.nThread
			logging.info ("training set: " + str( len(trainingColumn)))
			if args.reload and os.path.isfile(modelFilePath):
				cls = joblib.load(modelFilePath)
				logging.info("Model loaded")
				#logging.info ("validation accuracy: " + str(cls.score(vectorizedTestingFeatures, testingColumn)))
				#pdb.set_trace()
				#sys.exit()
			else:
				try:
					cls.fit(vectorizedTrainingFeatures, trainingColumn)
				except WindowsError:
					print "WINDOWSERROR exception caught<<<<<<<<<<<<<"
					pass
					
				joblib.dump(cls, modelFilePath)
			logging.info ("validation accuracy: " + str(cls.score(vectorizedTestingFeatures, testingColumn)))
			logging.info ("training accuracy: " + str(cls.score(vectorizedTrainingFeatures, trainingColumn)))

		
		classifiers.append(cls);
		logging.info( "Column: " + str(i) + " done" )

	#load model and test
	if (args.mode == TEST and __name__ != '__main__') or args.debug:  #if testing and in one of the child processes
		#testingColumn =  unzipedTestingLabels[i];	
		#pdb.set_trace()
		if args.model == NN:
			#cls = theanets.feedforward.load(modelFilePath)
			cls = theanets.Experiment(modelFilePath)
			#pdb.set_trace()
			#cls = theanets.Experiment(theanets.Classifier, layers=(231528, 892))
			#lrCls = joblib.load("C:\\Users\\t-isbelt\\tmp\\grammar-memm-8\\col0\\model.pkl")
			#cls.network.params[1].set_value(  np.array([x[0] for x in lrCls.intercept_], dtype=np.float32) )
			#cls.network.params[0].set_value( lrCls.coef_.T.astype('float32'))
			#pdb.set_trace()
			#metrics.accuracy_score(cls.network.predict(validate[0]), validate[1])
			cls = cls.network
		else:
			cls = joblib.load(modelFilePath)
		logging.info( cls );
		#print datetime.datetime.now()

		if args.model == MAXENT:
			res = cls.predict(vectorizedTestingFeatures)
			predictions.append(res);
			res = cls.predict_proba(vectorizedTestingFeatures)
			predictions_proba.append(res);

		#np.savetxt(modelFileName + ".csv", res, delimiter=",")
		#print datetime.datetime.now()
		classifiers.append(cls);
	#store classifier in a list

	#print datetime.datetime.now()
	#sys.exit()

if args.mode == TEST and __name__ == '__main__':
	struct_predictions = None	
	acc_ch = acc_fun = acc_ing = 0;
	if args.model == MAXENT:
		acc_ch = multiclassMultilabelAccuracy(zip(unzipedTestingLabels[0], unzipedTestingLabels[2]), zip(predictions[0],    predictions[2]))
		acc_fun = multiclassMultilabelAccuracy(zip(unzipedTestingLabels[0],  unzipedTestingLabels[1],  unzipedTestingLabels[2],  unzipedTestingLabels[3]), zip(predictions[0],  predictions[1], predictions[2], predictions[3]))
		acc_ing = multiclassMultilabelAccuracy(zip(unzipedTestingLabels[0],  unzipedTestingLabels[1],  unzipedTestingLabels[2],  unzipedTestingLabels[3], unzipedTestingLabels[4], unzipedTestingLabels[5]), zip(predictions[0],  predictions[1], predictions[2], predictions[3], predictions[4], predictions[5]))

		logging.info ("Channel Accuracy: " + str(acc_ch))
		logging.info ("Channel + Function Accuracy: " + str(acc_fun))
		logging.info ("Channel + Function + Ingredients Accuracy: " + str(acc_ing))

		noStructPredFile = open('structPred_no.tsv', 'wb')
		for x in zip(predictions[0],  predictions[1], predictions[2], predictions[3], predictions[4], predictions[5]):
			noStructPredFile.write("\t".join(x) + "\n");
		noStructPredFile.close()

	#sys.exit();

	threadsCount = args.nThread
	totalCount = len(rawTestingLabels)
	if threadsCount < 0:
		threadsCount = multiprocessing.cpu_count()
	rate = math.ceil(totalCount * 1.0 / threadsCount)

	sharedQ = multiprocessing.Queue()
	
	if args.debug:
		inference(3000, 3001, sharedQ)
		pdb.set_trace();
	else:
		for thr in range (0, threadsCount):
			try:
				fromInclusive = thr * rate
				fromInclusive = int(min(fromInclusive, totalCount))
				toExclusive = (thr+1) * rate
				toExclusive = int(min(toExclusive, totalCount))
				logging.info( str(fromInclusive) + " - " + str(toExclusive) );
		
				t = Process(target=inference, args=(fromInclusive, toExclusive, sharedQ, ));
				t.start();
			except:
				print "Error: unable to start thread"
	
	
	consumed = 0;
	results = []
	while consumed < totalCount:
		oneRow = sharedQ.get();
		logging.info ( oneRow )
		results.append(oneRow)
		consumed = consumed + 1
		if consumed%10 == 0: 
			logging.info( "consumed: " + str(consumed) );
	
	#pdb.set_trace()
	
	logging.info( "Done consuming: " + str(len(results)) )
	logging.info( "Sorting .. " )
	
	sortedList = sorted(results, key=lambda e: e[0])
	struct_predictions = map(lambda x: x[1], sortedList)
	
	#pdb.set_trace()
	
	#struct_predictions = []
	#for res in results:
	#	while not res.empty():
	#		struct_predictions.append(res.get())

	
	#struct_predictions = [];  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>3
	#inference(0, len(rawTestingLabels), struct_predictions);
	#inference(0, 5, struct_predictions);
	
	logging.info( "struct_predictions len: " + str(len(struct_predictions)) )
	
	if args.grammar == MINI:
		#unpack the columns of the predictions and the labels
		struct_predictions = [[x[0].split(delimiter)[0], x[0].split(delimiter)[1], x[2].split(delimiter)[0], x[2].split(delimiter)[1], x[4], x[5]]  for x in struct_predictions]
		unzipedTestingLabels[0] = [x.split(delimiter)[0] for x in unzipedTestingLabels[0] ]
		unzipedTestingLabels[2] = [x.split(delimiter)[0] for x in unzipedTestingLabels[2] ]
	
	struct_pred_file = open(struct_pred_file_name, 'wb')
	for x in struct_predictions:
		struct_pred_file.write("\t".join(filter(None, x)) + "\n");
	struct_pred_file.close()
	
	unzip_struct_pred =  zip(*struct_predictions)
		
	acc = []
	acc.append(multiclassMultilabelAccuracy(zip(unzipedTestingLabels[0], unzipedTestingLabels[2]), zip(unzip_struct_pred[0],  unzip_struct_pred[2]))  )
	acc.append(multiclassMultilabelAccuracy(zip(unzipedTestingLabels[0],  unzipedTestingLabels[1],  unzipedTestingLabels[2],  unzipedTestingLabels[3]), zip(unzip_struct_pred[0],  unzip_struct_pred[1], unzip_struct_pred[2], unzip_struct_pred[3])) )
	acc.append (multiclassMultilabelAccuracy(zip(unzipedTestingLabels[0],  unzipedTestingLabels[1],  unzipedTestingLabels[2],  unzipedTestingLabels[3], unzipedTestingLabels[4], unzipedTestingLabels[5]), zip(unzip_struct_pred[0],  unzip_struct_pred[1], unzip_struct_pred[2], unzip_struct_pred[3], unzip_struct_pred[4], unzip_struct_pred[5])) )
	
	for i in [0, 1, 2, 3]:
		acc.append(multiclassMultilabelAccuracy(zip(unzipedTestingLabels[i]), zip(unzip_struct_pred[i])) )
	
	logging.info ("Channel Accuracy (beamSearch): " + str(acc[0]) )
	logging.info ("Channel + Function Accuracy (beamSearch): " + str(acc[1]) )
	logging.info ("Channel + Function + Ingredients Accuracy (beamSearch): " + str(acc[2]) )
	logging.info ( "\t".join( [str(round(100*x, 2))  for x in acc ])) 
	logging.info ("Total number of recipes: " + str(len(unzipedTestingLabels[0])) )
	#pdb.set_trace()
	
#if (args.mode == TRAIN and __name__ == '__main__') or (args.mode == TEST ):
#	pdb.set_trace()