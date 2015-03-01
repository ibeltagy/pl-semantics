rteAct=$1
rteGt=$2
kbest=$3
count=0
let kbestSqr=kbest*kbest
cat $rteAct | while read act
do
	len=$(expr length "$act")
	len=$(expr $len - 2);
	act=$(expr substr "$act" 2 $len)
	echo $act |sed  's/ /\n/g' > tmp-act

	#MLN
	echo "@relation sts" > tmp-train.arff
	for (( i=1; i<=$kbestSqr; i++ ))
	do
	   echo "@attribute act$i real" >> tmp-train.arff
	done	
	echo "@attribute gt {0,1}" >> tmp-train.arff
	echo "@data" >> tmp-train.arff
	paste -d , tmp-act $rteGt | head -n 567  >> tmp-train.arff

	echo "@relation sts" > tmp-test.arff
   for (( i=1; i<=$kbestSqr; i++ ))
   do
      echo "@attribute act$i real" >> tmp-test.arff
   done
	echo "@attribute gt {0,1}" >> tmp-test.arff
	echo "@data" >> tmp-test.arff
	paste -d , tmp-act $rteGt | tail -n 800  >> tmp-test.arff

	mln=$(java -Xmx1024m -cp ../weka/weka.jar weka.classifiers.meta.AdaBoostM1 -i -t tmp-train.arff -T tmp-test.arff -- )
	
	echo "$mln "
	echo "----------------------------------------------------------------------------------------------------------"
done
