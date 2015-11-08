#!/bin/python

#read line

import sys
import argparse
import pdb
import xml.etree.ElementTree as ET
import os

parser = argparse.ArgumentParser()

#parser.add_argument("mode", choices=[TRAIN, TEST, DEV], default=TEST, help="mode: train or test on the test set or test on the dev set")
parser.add_argument("parsed", help="xml file containing the output from corenlp")
#parser.add_argument("-verbose", help="increase output verbosity", action="store_true")

args = parser.parse_args()

'''
  33779             <NER>DATE</NER>
   3791             <NER>DURATION</NER>
  34375             <NER>LOCATION</NER> <<<<<
  15280             <NER>MISC</NER> XX 
    956             <NER>MONEY</NER>
  19668             <NER>NUMBER</NER>
 823470             <NER>O</NER>
   3374             <NER>ORDINAL</NER>
  19954             <NER>ORGANIZATION</NER> <<<< 
    724             <NER>PERCENT</NER>
  37679             <NER>PERSON</NER> <<<<
    587             <NER>SET</NER>
    467             <NER>TIME</NER>
'''
acceptedNE = {'LOCATION', 'ORGANIZATION', "PERSON"}

cnt = 0 
for f in os.listdir(args.parsed):
    if f.endswith(".xml"):
        cnt = cnt + 1
        print str(cnt) + ": " + f
        tree = ET.parse(args.parsed + "/" + f)
        root = tree.getroot()
        root.getchildren()[0].getchildren()
        sentences = root.getchildren()[0].getchildren()[0]
        assert sentences.tag == "sentences"
        corefs = None
        if len(root.getchildren()[0].getchildren()) > 1 :
            corefs = root.getchildren()[0].getchildren()[1]
            assert corefs.tag == "coreference"
        #pdb.set_trance()
        for sentence in sentences.getchildren():
            tokens = None
            dep = None
            for elem in sentence.getchildren():
                if elem.tag == "tokens":
                    tokens = elem
                if elem.tag == "dependencies" and elem.get("type") == "collapsed-ccprocessed-dependencies":
                    dep = elem
            assert not (tokens == None) and not (dep == None)
            tokensDict = dict()
            for token in tokens:
                tokensDict[token.get("id")] = token
    
            root = None
            rootComp = []
            nsubj = None
            nsubjComp = []
            
            objList = [None, None, None]
            objNames = ["dobj", "iobj", "nmod:"]
            hasObjs = [False, False, False]
            objComp = [[], [], []]
            
            #TODO:
            #1) read the full NE
            #2) exclude light verbs 
            #3) exclude trivial questions
            #4) exclude un-answerable questions
            #5) generate more complex questions 
            
            def isNE (tokenID):
                return tokensDict[tokenID].find("NER").text in acceptedNE 
            for d in dep:
                #if root != None and nsubj != None and dobj != None:
                #    break;
                if d.get("type") == "root" and root == None:
                    rootId = d.getchildren()[1].get("idx");
                    #if not tokensDict[rootId].find("POS").text.startswith("V"): #root is a verb
                    #    break; 
                    root = d;

                if d.get("type") == "nsubj" and nsubj == None and root != None:
                    xId = d.getchildren()[1].get("idx")
                    if (d.getchildren()[0].get("idx") == root.getchildren()[1].get("idx") #same root verb
                        and isNE(xId) ) : #the subject is a NE
                        nsubj = d
                
                
                for objIdx in range (0, len(objNames)) : 
                    if d.get("type").startswith(objNames[objIdx]) and objList[objIdx] == None and root != None:
                        xId = d.getchildren()[1].get("idx")
                        if (d.getchildren()[0].get("idx") == root.getchildren()[1].get("idx")): #same root verb
                            hasObjs[objIdx] = True
                            if (isNE (xId)) : #the object is a NE
                                objList[objIdx] = d
           
            #search for compounds
            for d in dep:
                if d.get("type") in  ["compound", "amod"]:
                    parentID = d.getchildren()[0].get("idx");
                    compID = d.getchildren()[1].get("idx");
                    if root != None and parentID == root.getchildren()[1].get("idx"):
                        rootComp.append(d)
                    elif nsubj != None and parentID == nsubj.getchildren()[1].get("idx"):
                        nsubjComp.append(d)
                    else:
                        for objIdx in range (0, len(objList)):
                            if objList[objIdx] != None and parentID == objList[objIdx].getchildren()[1].get("idx"):
                                objComp[objIdx].append(d)


            #if root != None and nsubj != None and dobj != None:
            #    print u" ".join (("##: ", nsubj.find("dependent").text, root.find("dependent").text, dobj.find("dependent").text)).encode('utf-8')
            
            #pdb.set_trace()
            buff = ""

            if nsubj != None:
                buff = (" ".join( [x.find("dependent").text for x in nsubjComp] + [( nsubj.find("dependent").text)] ) ).replace(",", ".")

            if root != None:
                buff = buff + ", " + (" ".join([x.find("dependent").text for x in rootComp] + [(root.find("dependent").text)] )).replace(",", ".")

            ##TODO: printing 
            objCnt = 0
            oneObj = None
            oneObjComp = None
            for objIdx in range (0, len (objNames)):
                if objList[objIdx] != None:
                    typeName = objList[objIdx].get("type")
                    if typeName.startswith("nmod:"):
                        prep = typeName.split(":")[1];
                        buff = buff + " #" + prep

                    buff = buff + ", " +  (" ".join([x.find("dependent").text for x in objComp[objIdx]] + [(objList[objIdx].find("dependent").text)] )).replace(",", ".")
                    objCnt = objCnt + 1
                    oneObj = objList[objIdx].find("dependent").text
                    oneObj = objComp[objIdx]

                elif hasObjs[objIdx]:
                    objCnt = objCnt + 1

            fullSen = " ".join ([x.find("word").text for x in tokens.getchildren()])
            if nsubj != None and root != None and objCnt == 1 and oneObj != None: 
                #if "dobj" in buff or "iobj" in buff or "pobj" in buff:
                print ("### " + buff + " ### " + fullSen).encode('utf-8')
            #else:
            #    print str(nsubj)  + " " + str(root) + " " + str(dobj)

            #pdb.set_trace()

#parser.add_argument("outDir", help="output directory (will be overwritten)")
#pdb.set_trace()

#parsedFile = open (args.parsed)
#docs = dict ()

#for line in docsFile:
#    docs[line.strip() ] = True;

#pdb.set_trace()

'''
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
'''

#cat  ../pages-simple/filtered | sed '/<doc id=.*>/ {s/.*//; N; s/[\n]/### /g}' | sed  '/<\/doc>/d' | awk '{if ($1 == "###") print $0"."; else print $0}

