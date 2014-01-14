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

expName=$2
if [ -z "$expName" ]; then
   echo "Experiment name can not be empty"
   exit 1
fi

outputDir="condor/$expName/"

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

case $CMD in
   submit)
		echo "Storing condor output files in : $outputDir. Old content will be overwritten"
		rm $outputDir -r
		mkdir $outputDir
      echo "$outputDir $step $@" >> $outputDir/config
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
			echo "RESUBMIT bin/mlnsem run-condor $step $i $@ $outputFile"
         bin/mlnsem run-condor $step $i $@ $outputFile
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
