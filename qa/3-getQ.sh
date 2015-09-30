#!/bin/bash

#read line
base=$1

for d in 0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f
#for d in 6  7  8  9  a  b  c  d  e
do
	#find  $base/splits/$d |  tail -n +2  > tmp/$d
	echo "getQ from  $base/parsed/$d to $base/q/$d "
	#stanford-corenlp-full-2015-04-20/corenlp.sh -annotators tokenize,ssplit,pos,lemma,ner,parse,dcoref  -outputDirectory $base/parsed/$d  -filelist tmp/$d
	#/usr/lib/jvm/java-8-oracle/jre/bin/java   -mx3g -cp "/scratch/cluster/beltagy/qa/stanford-corenlp-full-2015-04-20/*" edu.stanford.nlp.pipeline.StanfordCoreNLP
	#~/workspace/deft/mln-semantics/bin/condorizer.py stanford-corenlp-full-2015-04-20/corenlp.sh -annotators tokenize,ssplit,pos,lemma,ner,parse,dcoref  -outputDirectory $base/parsed/$d  -filelist tmp/$d   tmp/condor.$d
	~/workspace/deft/mln-semantics/bin/condorizer.py  /usr/local/bin/python 3-getQ.py $base/parsed/$d $base/q/$d
done

#while read p; do
#  echo $p
#done 

#while IFS = read -r line; do
    ## some work
#    printf '%s\n' "$line" >> output
#done

#echo $line
