#!/bin/bash

function run_setting () {
	export MLN=/scratch/cluster/roller/rtesystem/pl-semantics
	export LEX=/scratch/cluster/roller/rtesystem/lexical
	name=$1
	shift

	# clean up old stuff
	rm -f \
		${MLN}/predictions/${name}/train/out.* \
	    ${MLN}/predictions/${name}/train/err.* \
	    ${MLN}/predictions/${name}/train/result \
	    ${MLN}/predictions/${name}/test/out.* \
	    ${MLN}/predictions/${name}/test/err.* \
	    ${MLN}/predictions/${name}/test/result

	# mass farm out the cores
	echo "RUNNING CONDOR FULL (Train)"
	./bin/condor full "${MLN}/predictions/${name}/train" 5 sick-rte \
		-diffRules true \
		-withNegT false \
		-phrases "${LEX}/output/${name}/sick_train.txt.cv"

	echo
	echo
	echo "RUNNING CONDOR FULL (Test)"
	./bin/condor full "${MLN}/predictions/${name}/test" 5 "sick-rte-test" \
		-diffRules true \
		-withNegT false \
		-phrases "${LEX}/output/${name}/sick_test.txt"

	# compute the final classifier setting
	echo
	echo
	echo "RUNNING FINAL CLASSIFICATION"
	python bin/classifier.py \
		--train $MLN/predictions/${name}/train/result \
		--gold-train resources/sick/sick-rte.gs \
		--test $MLN/predictions/${name}/test/result \
		--gold-test resources/sick/sick-test-rte.gs
}

run_setting neu

