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

parser = argparse.ArgumentParser()

#parser.add_argument("mode", choices=[TRAIN, TEST, DEV], default=TEST, help="mode: train or test on the test set or test on the dev set")
parser.add_argument("q", help="text file of questions to filter")
parser.add_argument("context", help="folder of articles where to search for the answer")
#parser.add_argument("-verbose", help="increase output verbosity", action="store_true")

args = parser.parse_args()
stopR = {
'tell', 
'tells', 
'told',
'ask',
'asks',
'asked',
'have', 
'has',
'had',
'become',
'becomes', 
'became', 
'go',
'goes', 
'went', 
'gone',
'one',
'ones', 
'call',
'calls',
'called',
'take',
'takes', 
'took',
'taken',
'middle',
'see',
'sees',
'saw',
'seen',
'say',
'said', 
'says',
'want',
'wants',
'wanted',
'need',
'needs',
'needed',
'between'}


qFile = open(args.q)
contextFilePath = None
contextFile = None
query = None
counter = 0
for line in qFile:
	if line.startswith("###"):
		
		sent = line.strip().split("###") [1];
		#pdb.set_trace()
		[subj, verb, obj] = [x.strip() for x in sent.split(",")]
		#search for sent in contextFileStr
		#contextFile.seek(0, 0)
		#print "search for "  + sent + " in "  +  args.context + "/"+ contextFilePath.replace(".xml", "") + ", title: " + contextFileTitle
		found = ""
		if subj in contextFileStr:
			found  = "SubjFound"
		if obj in contextFileStr: 
			found  = found + "ObjFound"
                isStopR = False;
                for w in stopR:
                    if w in verb:
                        found  = found + "StopRel"
                        break
                
                fullRel = verb.replace ("#", "");
                if "#" in verb: 
                    verb = verb.split("#")[0].strip() #remove the preposition

                if not (subj + verb + obj).replace(" ", "").isalpha():
                    found  = found + "StopChar"

                m = re.search(subj + "[^\.]+" + verb + "[^\.]+" + obj, contextFileStr)
                if m != None:
                    found = found + "TRIVIAL, " + m.group(0)
                else:
                    subjOR = "(" + subj.replace (" ", "|") + ")"
                    objOR = "(" + obj.replace (" ", "|") + ")"
                    #pdb.set_trace()
                    m = re.search(subjOR + "[^\.]+" + verb + "[^\.]+" + objOR, contextFileStr)
                    if m != None:
                        found = found + "COREFTRIV, " + m.group(0)
                    else:
                        m = re.findall(subj + "[^\.]+"  + obj, contextFileStr)
                        if len(m) > 0 :
                            #pdb.set_trace()
                            found = found + "ONESEN, " + " $$$ ".join(m)
                            #pdb.set_trace()
                    
		found = found.replace("\n", " ")
                print found + ", Q:"  + sent + ", C: " + contextFileTitle
                #pdb.set_trace()
                if found.startswith("SubjFoundObjFoundONESEN,") or found == ("SubjFoundObjFound"):
                    counter = counter + 1;
                    questionFile = open ("qa/questions/" + contextFilePath.replace(".xml", "") + "-" + str(counter), "w");
                    questionFile.write (contextFileTitle + " ## " + found +  "\n" + "\n");
                    sentTok = sent_tokenize (contextFileStr.replace ("\n", " ") )
                    wordTok = " ".join([" ".join(TreebankWordTokenizer().tokenize(x)) for x in sentTok])
                    questionFile.write ( wordTok + "\n" + "\n");
                    if (obj.lower() == contextFileTitle.lower()):
                        questionFile.write ("@placeholder " + fullRel + "  " + obj + "\n\n" + subj + "\n\n" );
                    else:
                        questionFile.write (subj +  "  " + fullRel + " @placeholder" + "\n\n" + obj + "\n\n" );
                    print "FileCreated"

	else:
		if contextFile != None:
			contextFile.close()
		contextFilePath = line.strip().split(" ")[1]
		if not contextFilePath.endswith(".xml"):
			continue;
		contextFile = open (args.context + "/"+ contextFilePath.replace(".xml", ""))
		contextFileTitle = contextFile.readline().strip()[:-1]
		contextFileStr = contextFile.read();
                counter = 0 ;
contextFile.close()
qFile.close()
sys.exit()

