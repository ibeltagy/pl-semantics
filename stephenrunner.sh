#!/bin/bash

function run_setting () {
	export MLN=/scratch/cluster/roller/rtesystem/pl-semantics
	export LEX=/scratch/cluster/roller/rtesystem/lexical
	name=$1
	shift
	job=$1
	shift

	echo
	echo
	echo "**************************************************"
	echo "running setting '$name' (job mode '$job')"
	echo "**************************************************"

	case $job in
		all | mln)
			# clean up old stuff
			rm -f \
				${MLN}/predictions/${name}/train/out.* \
				${MLN}/predictions/${name}/train/err.* \
				${MLN}/predictions/${name}/train/result \
				${MLN}/predictions/${name}/test/out.* \
				${MLN}/predictions/${name}/test/err.* \
				${MLN}/predictions/${name}/test/result
			;;
	esac

	case $job in
		all | mln)
			# mass farm out the cores
			echo "RUNNING CONDOR FULL (Train)"
			./bin/condor full "${MLN}/predictions/${name}/train" 5 sick-rte \
				-extendDiffRulesLvl 1  -splitDiffRules true -withFixCWA true \
				-diffRules true \
				-withNegT false \
				-metaW 0.30 \
				-phrases "${LEX}/output/${name}/sick_train.txt.cv"

			#echo
			#echo
			#echo "RUNNING CONDOR FULL (Test)"
			#./bin/condor full "${MLN}/predictions/${name}/test" 5 "sick-rte-test" \
			#	-diffRules true \
			#	-phrases "${LEX}/output/${name}/sick_test.txt"
			;;
	esac

	case $job in
		all | evaluate)
		# compute the final classifier setting
		echo
		echo "LEXICAL RESULTS:"
		cat $LEX/output/log.${name}.out
		cat $LEX/output/log.${name}.err
		echo
		echo "RUNNING FINAL CLASSIFICATION"
		python bin/classifier.py \
			--train $MLN/predictions/${name}/train/result \
			--gold-train resources/sick/sick-rte.gs \
			--test $MLN/predictions/${name}/test/result \
			--gold-test resources/sick/sick-test-rte.gs
		;;
	esac
}

#run_setting neu all
run_setting gold all

