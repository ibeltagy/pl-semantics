#!/bin/bash

#split 
#split -l 52000 -a 1  /scratch/cluster/beltagy/qa/pages-simple/filtered /scratch/cluster/beltagy/qa/pages-simple/chunks/chunk

#read line
base=$1

for d in   a  b  c  d  e  f g h i j k l m n o p
#for d in 6  7  8  9  a  b  c  d  e
do
	#find  $base/splits/$d |  tail -n +2  > tmp/$d
	#echo "getQ from  $base/parsed/$d to $base/q/$d "
	#stanford-corenlp-full-2015-04-20/corenlp.sh -annotators tokenize,ssplit,pos,lemma,ner,parse,dcoref  -outputDirectory $base/parsed/$d  -filelist tmp/$d
	#/usr/lib/jvm/java-8-oracle/jre/bin/java   -mx3g -cp "/scratch/cluster/beltagy/qa/stanford-corenlp-full-2015-04-20/*" edu.stanford.nlp.pipeline.StanfordCoreNLP
	#~/workspace/deft/mln-semantics/bin/condorizer.py stanford-corenlp-full-2015-04-20/corenlp.sh -annotators tokenize,ssplit,pos,lemma,ner,parse,dcoref  -outputDirectory $base/parsed/$d  -filelist tmp/$d   tmp/condor.$d
	#~/workspace/deft/mln-semantics/bin/condorizer.py  /usr/local/bin/python qa/3-getQ.py $base/parsed/$d $base/q/$d
	#python qa/4-filterQA.py  $base/pages-simple/q/$d.out  $base/pagesEn/splits/$d/
	~/workspace/deft/mln-semantics/bin/condorizer.py  /usr/lib/jvm/java-8-oracle/jre/bin/java  -jar /scratch/cluster/beltagy/openie/target/scala-2.10/openie-assembly-4.1.4-SNAPSHOT.jar  /scratch/cluster/beltagy/qa/pages-simple/chunks/chunk.$d  /scratch/cluster/beltagy/qa/pages-simple/openieChunkOut/openieout.$d  -b -s  ~/workspace/deft/mln-semantics/tmp/openie.$d      

done

#while read p; do
#  echo $p
#done 

#while IFS = read -r line; do
    ## some work
#    printf '%s\n' "$line" >> output
#done

#echo $line
