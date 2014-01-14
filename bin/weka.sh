read line
wekaJarPath="/u/beltagy/workspace/deft/weka/weka.jar"
act=$line
CMD=$1
gs=$2
columns=$3
#size=$4
size=5000
#ens=$5
count=0

if [ -z "$columns" ] || [ "$columns" -le "0" ]; then
	columns=1
fi

#case $CMD in
#	classify)
#   	let kbestSqr=kbest*kbest
#   ;;
#   regress)
#      let kbestSqr=kbest*kbest*2
#   ;;
#   *)
#   	echo "Invalid command $CMD"
#	   exit 1
#   ;;
#esac

echo "$act" | sed 's/^\[\(.*\)]$/\1/'  | tr ' ' '\n' |  grep -v '^$'  > tmp-act

if [ -n "$ens" ]; then
	cat "$ens" | sed 's/^\[\(.*\)]$/\1/'  | tr ' ' '\n' |  grep -v '^$'  > tmp-ens
fi


#	len=$(expr length "$act")
#	len=$(expr $len - 2);
#	act=$(expr substr "$act" 2 $len)
#  echo $act
#	echo $act |sed  's/ /\n/g' > tmp-act

	echo "@relation sts" > tmp-train.arff
   echo "@relation sts" > tmp-test.arff

	for (( i=1; i<=$columns; i++ ))
	do
	   echo "@attribute act$i real" >> tmp-train.arff
      echo "@attribute act$i real" >> tmp-test.arff
	done

if [ -n "$ens" ]; then
      echo "@attribute ens real" >> tmp-train.arff
      echo "@attribute ens real" >> tmp-test.arff
fi

  	case $CMD in 
		classify)
			echo "@attribute gt {0,0.5,1}" >> tmp-train.arff
         echo "@attribute gt {0,0.5,1}" >> tmp-test.arff
			;;
		regress)
			echo "@attribute gt real" >> tmp-train.arff
         echo "@attribute gt real" >> tmp-test.arff
			;;
		*)
			echo "Invalid command $CMD"
			exit 1
			;;
	esac
	echo "@data" >> tmp-train.arff
   echo "@data" >> tmp-test.arff


if [ -n "$ens" ]; then
	files="tmp-act tmp-ens $gs"
else
   files="tmp-act $gs"
fi

case $size in
  "1500")
	paste -d , $files | head -n 750  >> tmp-train.arff
   paste -d , $files | tail -n 750  >> tmp-test.arff
	testCV=" -t tmp-train.arff -T tmp-test.arff "
	;;
  "5000")
   paste -d , $files | head -n 5000  >> tmp-train.arff
   testCV=" -t tmp-train.arff "
	;;
  *)
	echo "wrong dataset size"
	;;
esac


#	echo "@relation sts" > tmp-test.arff
#   for (( i=1; i<=$kbestSqr; i++ ))
#   do
#      echo "@attribute act$i real" >> tmp-test.arff
#   done
#	echo "@attribute gt {0,0.5,1}" >> tmp-test.arff
#	echo "@data" >> tmp-test.arff
#	paste -d , tmp-act $gs | tail -n 800  >> tmp-test.arff

#	result=$(java -Xmx1024m -cp $wekaJarPath weka.classifiers.meta.AdaBoostM1 -i -t tmp-train.arff -T tmp-test.arff -- )

   case $CMD in
      classify)
         result=$(java -Xmx1024m -cp $wekaJarPath weka.classifiers.meta.AdaBoostM1 -i $testCV  )
         ;;
      regress)  
			result=$(java -Xmx1024m -cp $wekaJarPath weka.classifiers.meta.AdditiveRegression -i $testCV  -S 0.95 -I 10 -W weka.classifiers.rules.M5Rules )
			echo "$result"
         result=$(java -Xmx1024m -cp $wekaJarPath weka.classifiers.functions.MultilayerPerceptron -i $testCV   )
         ;;
   esac
	
	echo "$result"
	echo "----------------------------------------------------------------------------------------------------------"
