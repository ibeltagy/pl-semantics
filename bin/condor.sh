#This script should be called from the folder: mln-semantic using the command bin/condor
CMD=$1

case $CMD in
	status)
		/lusr/opt/condor/bin/condor_q
		exit 0
		;;
	remove)
      /lusr/opt/condor/bin/condor_rm -all
		exit 0
		;;
esac

expDir=$2
if [ -z "$expDir" ]; then
   echo "Experiment directory can not be empty"
   exit 1
fi

outputDir="$expDir.exp/"

#step=$3
#if [ -z "$step" ] || [ "$step" -le "0" ]; then
#   echo "step can not be zero or empty"
#   exit 1
#fi

#total=$(( 5000+$step-1 ))
#total=$(( $total / $step ))
#echo "Number of jobs $total"
#shift
#shift
#shift

case $CMD in
   submit | print)
	  	step=$3
	  	if [ -z "$step" ] || [ "$step" -le "0" ]; then
		  echo "step can not be zero or empty"
		  exit 1
	  	fi

	  	total=$(( 5000+$step-1 ))
	  	total=$(( $total / $step ))
	  	echo "Number of jobs $total"
	  	shift
		shift
		shift
		args="$@"
		echo "Storing condor output files in : $outputDir. Old content will be overwritten"
		
		if [ "$CMD" == "submit" ]; then
			rm $outputDir -r
			mkdir $outputDir
			echo  $step  > $outputDir/config
      	echo  $total >> $outputDir/config
	      echo "$args" >> $outputDir/config
		fi
		;;
	fix | collect)
		step=$(head -n 1 $outputDir/config)
	   total=$(head -n 2 $outputDir/config| tail -n 1)
	   args=$(tail  -n 1 $outputDir/config)
		echo "step: $step"
		echo "total: $total"
		echo "args: $args"
		;;
	*)
		echo $CMD
		;;
esac


collectBuffer=""
for (( i=1; i<=$total; i++ ))
do
outputFile=$outputDir$i
 case $CMD in
   print)
      echo "bin/mlnsem run-condor $step $i $@ $outputFile"
      ;;
   submit)
      echo "bin/mlnsem run-condor $step $i $@ $outputFile" 
      bin/mlnsem run-condor $step $i $@ $outputFile
      ;;
	*)
      b=$(tail $outputFile.out -n 5 | head -n 1)
      len=$(expr length "$b")
      len=$(expr $len - 2);
      firstChar=$(expr substr "$b" 1 1)
      if [[ "$firstChar" == "[" ]]; then
      	b=$(expr substr "$b" 2 $len)
      else
      	b="ERROR"
      fi
		;;
 esac
 case $CMD in
	collect)
      echo "$outputFile: $b"
      collectBuffer="$collectBuffer $b"
		;;
	fix)
		if [ "$b" == "ERROR" ]; then
			echo "RESUBMIT bin/mlnsem run-condor $step $i $args $outputFile"
         bin/mlnsem run-condor $step $i $args $outputFile
		else
		  	echo "$i is ok"
		fi
		;;
 esac

done	

case $CMD in 
	collect)
   collectBuffer="[$collectBuffer]"
	echo $collectBuffer
	;;
esac


exit 0
