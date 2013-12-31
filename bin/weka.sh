read line
wekaJarPath="/u/beltagy/workspace/deft/weka/weka.jar"
act=$line
CMD=$1
gs=$2
kbest=$3
count=0
let kbestSqr=kbest*kbest
	len=$(expr length "$act")
	len=$(expr $len - 2);
	act=$(expr substr "$act" 2 $len)
	echo $act |sed  's/ /\n/g' > tmp-act

	echo "@relation sts" > tmp-train.arff
	for (( i=1; i<=$kbestSqr; i++ ))
	do
	   echo "@attribute act$i real" >> tmp-train.arff
	done
  	case $CMD in 
		classify)
			echo "@attribute gt {0,0.5,1}" >> tmp-train.arff
			;;
		regress)
			echo "@attribute gt real" >> tmp-train.arff
			;;
		*)
			echo "Invalid command $CMD"
			exit 1
			;;
	esac
	echo "@data" >> tmp-train.arff
	paste -d , tmp-act $gs | head -n 5000  >> tmp-train.arff

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
         result=$(java -Xmx1024m -cp $wekaJarPath weka.classifiers.meta.AdaBoostM1 -i -t tmp-train.arff  )
         ;;
      regress)  
			result=$(java -Xmx1024m -cp $wekaJarPath weka.classifiers.meta.AdditiveRegression -i -t tmp-train.arff  -S 0.95 -I 10 -W weka.classifiers.rules.M5Rules )
         ;;
   esac
	
	echo "$result"
	echo "----------------------------------------------------------------------------------------------------------"
