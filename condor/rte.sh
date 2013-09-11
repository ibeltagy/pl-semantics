#!/bin/bash

for i in {1..800}
do
	echo $i
#./condor/condorizer.py bin/mlnsem ==>>$ds<<== $i -kbest 1 -task rte -vectorMaker add -rules $RES/rte/rules -multiOut $RES/sts/multiOut-$rtemode/RTE$version.$rtemode.multiOut ==>>$outputFile<<==

#	condor_submit condor/condor_submit_file_test1_$i
#	condor_submit condor/condor_submit_file_test2_$i
#	condor_submit condor/condor_submit_file_test3_$i
done

#for i in {1..800}
#do
#	echo $i
#	condor_submit condor/condor_submit_file_train2_$i
#	condor_submit condor/condor_submit_file_train3_$i
#done

#for i in {1..567}
#do
#	echo $i
#	condor_submit condor/condor_submit_file_train1_$i
#done


