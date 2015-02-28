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
	print(commands.getoutput('ant -f easyccg/build.xml'))
	print(commands.getoutput('cat '+sys.argv[1]+' | tr "\\t" "\\n" | candc/bin/pos -model candc/models/pos | candc/bin/ner -model candc/models/ner -ofmt "%w|%p|%n \\n" | java -jar easyccg/easyccg.jar --model easyccg/model -i POSandNERtagged -o prolog -r S[dcl] --nbest 1  > ' + sys.argv[1]+".easyccg_ccg"))
	print(commands.getoutput('candc/bin/boxer --input '+sys.argv[1]+".easyccg_ccg"+'  --box false --resolve true --elimeq true --instantiate true  --semantics drs | grep -E "drs\\(|id\\("   > ' + sys.argv[1]+".easyccg_boxer"))


#	print(commands.getoutput('cat '+sys.argv[1]+' | tr "\\t" "\\n" | candc/bin/candc  --models candc/models  --candc-printer boxer  > ' + sys.argv[1]+".candc_ccg"))
#	print(commands.getoutput('candc/bin/boxer --input '+sys.argv[1]+".candc_ccg"+'  --box false --resolve true --elimeq true --instantiate true  --semantics drs | grep -E "drs\\(|id\\("   > ' + sys.argv[1]+".candc_boxer"))

	f2 = open(sys.argv[1]+".easyccg_boxer")
	box = f2.readlines()

#	print(commands.getoutput('mv ' + sys.argv[2] + ' ' + sys.argv[2] + '.candc'))
	f3 = open(sys.argv[2])
	candc_box = f3.readlines()
	
	lastSen = 0
	for l in range(0, len(box)/2):
		line = box[l*2]
		lineId =  line.split(",")[0].split("(")[1] #id(12305,555).
		currentDrs = box[l*2+1][:-3]
		currentDrsId ="ID(" + lineId+")"+ box[l*2+1][:-1]
		id = int(lineId)/100
		for i in range(lastSen+1, id):
		#	print i
		#	print candc_box[i-1],
			print "Some(prs("  + candc_box[i-1][9:-3]  + ":))"
		#print id
		lastSen = id
		print "Some(prs("  + candc_box[id-1][9:-3]  + currentDrs+",1:))"
		
	f2.close()
	f3.close();

main()
#print(commands.getoutput('cat resources/fracas/fracas.txt | tr "\t" "\n" | sed "s/\./\.\n/g" |grep "\." -n | awk -F ":" "{print $1}" '))
#print(commands.getoutput('cat resources/fracas/fracas.txt | tr "\\t" "\\n" | sed "s/\\./\\.\\n/g" | grep "\\." -n | awk -F : \'{print $1}\' > tmp/fracas.multisen  '))
