#!/usr/bin/env python


import argparse
import sys
import difflib
import operator
import commands

#cat resources/fracas/fracas.txt | tr '\t' '\n' | sed 's/\./\.\n/g' |grep "\." -n | awk -F ':' '{print $1}' > tmp/tmp.fracas.multisentence
#candc/bin/boxer --input resources/fracas/fracas.5.ccg  --box false --resolve true --elimeq true --instantiate true  --semantics drs | grep -E "drs\(|id\("  > tmp/tmp.fracas.5.box
#cat resources/fracas/fracas.gsp


def main():
	print(commands.getoutput('cat resources/fracas/fracas.txt | tr "\\t" "\\n" | sed "s/\\./\\.\\n/g" | grep "\\." -n | awk -F : \'{print $1}\' > tmp/tmp.fracas.multisen  '))
	f1 = open('tmp/tmp.fracas.multisen')
	multisen = f1.readlines()

	print(commands.getoutput('cat resources/fracas/fracas.txt | tr "\\t" "\\n" | tr "\\." "\\n" |  candc/bin/pos -model candc/models/pos | candc/bin/ner -model candc/models/ner -ofmt "%w|%p|%n \\n" | java -jar easyccg/easyccg.jar --model easyccg/model -i POSandNERtagged -o prolog -r S[dcl] --nbest 5  > tmp/tmp.fracas.ccg'))

	print(commands.getoutput('candc/bin/boxer --input tmp/tmp.fracas.ccg  --box false --resolve true --elimeq true --instantiate true  --semantics drs | grep -E "drs\\(|id\\("   > tmp/tmp.fracas.box'))

	f2 = open('tmp/tmp.fracas.box')
	box = f2.readlines()

	f3 = open('resources/fracas/fracas.gsp')
	gsSenIndx = f3.readlines()
	
	currentSen = 0
	lastPrintedId = 0
	lastDrs = ""
#	for line in box:
	for l in range(0, len(box)/2):
		line = box[l*2]
		#currentSen = currentSen + 1
		lineId =  line.split(",")[0].split("(")[1] #id(12305,555).
		currentDrs = box[l*2+1][:-3]
		currentDrsId ="ID(" + lineId+")"+ box[l*2+1][:-1]
		id = int(lineId)/100
		currentSen = int(lineId)%100
		gsSenStr = gsSenIndx[id-1][0:1]
		gsSen = 1
		if (gsSenStr != "x"):
			gsSen = int(gsSenStr)
		#print gsSen
		#print id
		if (id != lastPrintedId):
			lastPrintedId = id
			#currentSen = 1
			#print lastDrs
			#print id
			#currentDrs = box[l*2+1][:-1]
			#print "Some(prs("+box[l*2+1][:-3]+",1))"
		if (currentSen == gsSen):
			if (str(id-1)+"\n") in multisen:
				#print id
				lastDrs =  "smerge(" + lastDrs + "," + currentDrs + ")"
			else: 
				if lastDrs != "" :
					print "Some(prs("+lastDrs+",1))"
				lastDrs = currentDrs
		
	print "Some(prs("+lastDrs+",1))"
	f3.close()
	f2.close()
	f1.close()

main()
#print(commands.getoutput('cat resources/fracas/fracas.txt | tr "\t" "\n" | sed "s/\./\.\n/g" |grep "\." -n | awk -F ":" "{print $1}" '))
#print(commands.getoutput('cat resources/fracas/fracas.txt | tr "\\t" "\\n" | sed "s/\\./\\.\\n/g" | grep "\\." -n | awk -F : \'{print $1}\' > tmp/fracas.multisen  '))
