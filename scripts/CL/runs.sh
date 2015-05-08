#!/bin/bash
task=$1

echo $task

step=25

# 0.67 0.83 1.16 1.33 1.67

scripts/CL/run.py  $task  rules-12-wordentOnly-multiparse-seenRules-ppdb-0.67 $step "-distWeight 1.25 -distRulesMode  gs -logOddsW true -wordnet false -corefOnIR true  -coref true  -diffRules true       -kbest 2  -rules resources/handwritten:resources/ppdb/ppdb.xl.tag -rulesWeight 0.67 "


scripts/CL/run.py  $task  rules-12-wordentOnly-multiparse-seenRules-ppdb-0.83 $step "-distWeight 1.25 -distRulesMode  gs -logOddsW true -wordnet false -corefOnIR true  -coref true  -diffRules true       -kbest 2  -rules resources/handwritten:resources/ppdb/ppdb.xl.tag -rulesWeight 0.83 "


scripts/CL/run.py  $task  rules-12-wordentOnly-multiparse-seenRules-ppdb-1.16 $step "-distWeight 1.25 -distRulesMode  gs -logOddsW true -wordnet false -corefOnIR true  -coref true  -diffRules true       -kbest 2  -rules resources/handwritten:resources/ppdb/ppdb.xl.tag -rulesWeight 1.16 "


scripts/CL/run.py  $task  rules-12-wordentOnly-multiparse-seenRules-ppdb-1.33 $step "-distWeight 1.25 -distRulesMode  gs -logOddsW true -wordnet false -corefOnIR true  -coref true  -diffRules true       -kbest 2  -rules resources/handwritten:resources/ppdb/ppdb.xl.tag -rulesWeight 1.33 "

scripts/CL/run.py  $task  rules-12-wordentOnly-multiparse-seenRules-ppdb-1.67 $step "-distWeight 1.25 -distRulesMode  gs -logOddsW true -wordnet false -corefOnIR true  -coref true  -diffRules true       -kbest 2  -rules resources/handwritten:resources/ppdb/ppdb.xl.tag -rulesWeight 1.67 "


#scripts/CL/run.py  $task  rules-12-wordentOnly-multiparse-seenRules-ppdb-3 $step "-distWeight 1.25 -distRulesMode  gs -logOddsW true -wordnet false -corefOnIR true  -coref true  -diffRules true       -kbest 2  -rules resources/handwritten:resources/ppdb/ppdb.xl.tag -rulesWeight 3 "


#scripts/CL/run.py  $task  rules-3-fixbugdeclarations $step "-distWeight 1.25 -distRulesMode  gs -logOddsW true -wordnet false -corefOnIR true  -coref true  -diffRules false  -kbest 1  -rules resources/handwritten -phrases"

#-distWeight 8
#-distRulesMode: hard, weight, noNeu
#-logOddsW false
#-wThr 0.2
#-wFixCWA 6

for logOddsW in true false
do
  
	for distRulesMode in hard weight noNeu
	do 

      if [ $logOddsW == true ]; then
			distWeightList="0 0.25 0.50 0.75 1 1.5 2 2.5 3 3.5 4"
			#distWeightList="0.75 0.83 1 1.13 1.25 1.38 1.5 1.63 1.75 1.88 2 5 6"
			#distWeightList="0 0.4 0.75 1 1.05 1.1 1.15 1.2 1.25 1.3 1.35 1.4 1.45 1.5 1.55 1.6 1.65 1.7 1.75 2 3"
		else
			distWeightList="0 0.5 1 2 3 4 5 6 7"
			#distWeightList="9 10 11"
      fi

      if [ $distRulesMode == "hard" ]; then
	      if [ $logOddsW == true ]; then
   	      distWeightList="1"
      	else
         	distWeightList="8"
	      fi
      fi


		for distWeight in $distWeightList
		do 

			exp="logOddsW_$logOddsW-distRulesMode_$distRulesMode-distWeight_$distWeight"
#			echo "EXP: $exp"
#bin/condor collect  condor/sick-sts-test-tunewith-u+-$wThr 100 sick-sts-test -wThr $wThr -vsWithPos true
#scripts/CL/run.py  $task  run-wlearn-full-noMem-2-$exp $step "-distWeight $distWeight -distRulesMode $distRulesMode -logOddsW $logOddsW  -wFixCWA 6 -wThr 0.2 -wordnet false -corefOnIR true  -coref true  -diffRules true  -kbest 2 -rulesWeight 1 -rules resources/handwritten -phrases"
		done
	done
done


#scripts/CL/run.py  $task  run-wlearn-full-ppdb $step "-distWeight 1.25 -distRulesMode  weight  -logOddsW true -wFixCWA 6 -wThr 0.2 -wordnet false -corefOnIR true  -coref true  -diffRules true  -kbest 2 -rulesWeight 0.5 -rules resources/ppdb/ppdb.xl.tag:resources/handwritten -phrases"


#scripts/CL/run.py  $task  run-wlearn-... $step "-distWeight 8 -distRulesMode hard -logOddsW false -wFixCWA 6 -wThr 0.2 -wordnet false -corefOnIR true  -coref true  -diffRules false  -kbest 1 -rulesWeight 1 -rules resources/handwritten -phrases"

#scripts/CL/run.py  $task  run-wlearn-hard-logOddsWfalse-1 $step "-distWeight 8 -distRulesMode hard -logOddsW false -wFixCWA 1 -wThr 0.2 -wordnet false -corefOnIR true  -coref true  -diffRules false  -kbest 1 -rulesWeight 1 -rules resources/handwritten -phrases"

#scripts/CL/run.py  $task  run-wlearn-hard-logOddsWfalse-2 $step "-distWeight 8 -distRulesMode hard -logOddsW false -wFixCWA 2 -wThr 0.2 -wordnet false -corefOnIR true  -coref true  -diffRules false  -kbest 1 -rulesWeight 1 -rules resources/handwritten -phrases"

#scripts/CL/run.py  $task  run-wlearn-hard-logOddsWfalse-3 $step "-distWeight 8 -distRulesMode hard -logOddsW false -wFixCWA 3 -wThr 0.2 -wordnet false -corefOnIR true  -coref true  -diffRules false  -kbest 1 -rulesWeight 1 -rules resources/handwritten -phrases"

#scripts/CL/run.py  $task  run-wlearn-hard-logOddsWfalse-4 $step "-distWeight 8 -distRulesMode hard -logOddsW false -wFixCWA 4 -wThr 0.2 -wordnet false -corefOnIR true  -coref true  -diffRules false  -kbest 1 -rulesWeight 1 -rules resources/handwritten -phrases"

#scripts/CL/run.py  $task  run-wlearn-hard-logOddsWfalse-5 $step "-distWeight 8 -distRulesMode hard -logOddsW false -wFixCWA 5 -wThr 0.2 -wordnet false -corefOnIR true  -coref true  -diffRules false  -kbest 1 -rulesWeight 1 -rules resources/handwritten -phrases"

#scripts/CL/run.py  $task  run-wlearn-hard-logOddsWfalse-6 $step "-distWeight 8 -distRulesMode hard -logOddsW false -wFixCWA 5 -wThr 0.2 -wordnet false -corefOnIR true  -coref true  -diffRules false  -kbest 1 -rulesWeight 1 -rules resources/handwritten -phrases"

#scripts/CL/run.py  $task  run-cwa-coref-dist-ppdb  "-wordnet false   -corefOnIR true  -coref true  -diffRules false  -kbest 1 -rulesWeight 1 -rules resources/empty  -phrases"  



#scripts/CL/run.py  $task  run-cwa-coref-dist-ppdb  "-wordnet false   -corefOnIR true  -coref true  -diffRules false  -kbest 1 -rulesWeight 1 -rules resources/ppdb/ppdb.xl.tag  -withFixCWA true -phrases"  

#scripts/CL/run.py  $task  run-cwa-coref-dist-ppdb-wl  "-wordnet false   -corefOnIR true  -coref true  -diffRules false  -kbest 1 -rulesWeight 3 -rules resources/ppdb/ppdb.xl.tag  -withFixCWA true -phrases"

#scripts/CL/run.py  $task  run-cwa-coref-dist-ppdb-wl-handcoded  "-wordnet false   -corefOnIR true  -coref true  -diffRules false  -kbest 1 -rulesWeight 3 -rules resources/ppdb/ppdb.xl.tag:resources/handwritten  -withFixCWA true -phrases"

#scripts/CL/run.py  $task  run-cwa-coref-dist-ppdb-wl-handcoded-multiparse  "-wordnet false   -corefOnIR true  -coref true  -diffRules false  -kbest 2 -rulesWeight 3 -rules resources/ppdb/ppdb.xl.tag:resources/handwritten  -withFixCWA true -phrases"

#scripts/CL/run.py  $task  run-cwa-coref-dist-ppdb-wl-handcoded-multiparse-enforce  "-wordnet false   -corefOnIR true  -coref true  -diffRules true  -kbest 2 -rulesWeight 3 -rules resources/ppdb/ppdb.xl.tag:resources/handwritten  -withFixCWA true -phrases"

#scripts/CL/run.py  $task  run-cwa-coref-dist-ppdb-wl-handcoded-multiparse-enforce-wn  "-wordnet true   -corefOnIR true  -coref true  -diffRules true  -kbest 2 -rulesWeight 3 -rules resources/ppdb/ppdb.xl.tag:resources/handwritten  -withFixCWA true -phrases"

#scripts/CL/run.py  $task  run-cwa-coref-dist-ppdb-wl-handcoded-multiparse-enforce-wn_noCorefIR  "-wordnet true   -corefOnIR false  -coref true  -diffRules true  -kbest 2 -rulesWeight 3 -rules resources/ppdb/ppdb.xl.tag:resources/handwritten  -withFixCWA true -phrases"


#scripts/CL/run.py  $task  run-cwa-coref-dist-handcoded  "-wordnet false   -corefOnIR true  -coref true  -diffRules false  -kbest 1 -rulesWeight 3 -rules resources/handwritten  -withFixCWA true -phrases"

#scripts/CL/run.py  $task  run-cwa-coref-dist-handcoded-multiparse  "-wordnet false   -corefOnIR true  -coref true  -diffRules false  -kbest 2 -rulesWeight 3 -rules resources/handwritten  -withFixCWA true -phrases"

#scripts/CL/run.py  $task  run-cwa-coref-dist-handcoded-multiparse-enforce-trueRules_all  "-wordnet false   -corefOnIR true  -coref true  -diffRules true  -kbest 2 -rulesWeight 3 -rules resources/handwritten  -withFixCWA true -phrases"

#scripts/CL/run.py  $task  run-cwa-coref-dist-ppdb-wl-0  "-wordnet false   -corefOnIR true  -coref true  -diffRules false  -kbest 1 -rulesWeight 3 -rules resources/ppdb/ppdb.xl.tag  -withFixCWA true -phrases"

#scripts/CL/run.py  $task  run-cwa-coref-dist-ppdb-wl-0.5  "-wordnet false   -corefOnIR true  -coref true  -diffRules false  -kbest 1         -rulesWeight 0.5 -rules resources/ppdb/ppdb.xl.tag  -withFixCWA true -phrases"

#scripts/CL/run.py  $task  run-cwa-coref-dist-ppdb-wl-1  "-wordnet false   -corefOnIR true  -coref true  -diffRules false  -kbest 1         -rulesWeight 1  -rules resources/ppdb/ppdb.xl.tag  -withFixCWA true -phrases"


#scripts/CL/run.py  $task  run-cwa-coref-dist-ppdb-wl-1.5  "-wordnet false   -corefOnIR true  -coref true  -diffRules false  -kbest 1         -rulesWeight 1.5 -rules resources/ppdb/ppdb.xl.tag  -withFixCWA true -phrases"


#scripts/CL/run.py  $task  run-cwa-coref-dist-ppdb-wl-2  "-wordnet false   -corefOnIR true  -coref true  -diffRules false  -kbest 1         -rulesWeight 2 -rules resources/ppdb/ppdb.xl.tag  -withFixCWA true -phrases"


#scripts/CL/run.py  $task  run-cwa-coref-dist-ppdb-wl-2.5  "-wordnet false   -corefOnIR true  -coref true  -diffRules false  -kbest 1         -rulesWeight 2.5 -rules resources/ppdb/ppdb.xl.tag  -withFixCWA true -phrases"


#scripts/CL/run.py  $task  run-cwa-coref-dist-ppdb-wl-3  "-wordnet false   -corefOnIR true  -coref true  -diffRules false  -kbest 1         -rulesWeight 3  -rules resources/ppdb/ppdb.xl.tag  -withFixCWA true -phrases"


#scripts/CL/run.py  $task  run-cwa-coref-dist-ppdb-wl-3.5  "-wordnet false   -corefOnIR true  -coref true  -diffRules false  -kbest 1         -rulesWeight 3.5  -rules resources/ppdb/ppdb.xl.tag  -withFixCWA true -phrases"


#scripts/CL/run.py  $task  run-cwa-coref-dist-ppdb-full2-wl-0  "-wordnet false   -corefOnIR true  -coref true  -diffRules true  -kbest 2 -rulesWeight 0 -rules resources/ppdb/ppdb.xl.tag:resources/handwritten  -withFixCWA true -phrases"

#scripts/CL/run.py  $task  run-cwa-coref-dist-ppdb-full2-wl-0.5  "-wordnet false   -corefOnIR true  -coref true  -diffRules true  -kbest 2 -rulesWeight 0.5 -rules resources/ppdb/ppdb.xl.tag:resources/handwritten  -withFixCWA true -phrases"

#scripts/CL/run.py  $task  run-cwa-coref-dist-ppdb-full2-wl-1  "-wordnet false   -corefOnIR true  -coref true  -diffRules true  -kbest 2 -rulesWeight 1 -rules resources/ppdb/ppdb.xl.tag:resources/handwritten  -withFixCWA true -phrases"

#scripts/CL/run.py  $task  run-cwa-coref-dist-ppdb-full2-wl-1.5  "-wordnet false   -corefOnIR true  -coref true  -diffRules true  -kbest 2 -rulesWeight 1.5 -rules resources/ppdb/ppdb.xl.tag:resources/handwritten  -withFixCWA true -phrases"

#scripts/CL/run.py  $task  run-cwa-coref-dist-ppdb-full2-wl-2  "-wordnet false   -corefOnIR true  -coref true  -diffRules true  -kbest 2 -rulesWeight 2 -rules resources/ppdb/ppdb.xl.tag:resources/handwritten  -withFixCWA true -phrases"

#scripts/CL/run.py  $task  run-cwa-coref-dist-ppdb-full2-wl-2.5  "-wordnet false   -corefOnIR true  -coref true  -diffRules true  -kbest 2 -rulesWeight 2.5 -rules resources/ppdb/ppdb.xl.tag:resources/handwritten  -withFixCWA true -phrases"

#scripts/CL/run.py  $task  run-cwa-coref-dist-ppdb-full2-wl-3  "-wordnet false   -corefOnIR true  -coref true  -diffRules true  -kbest 2 -rulesWeight 3 -rules resources/ppdb/ppdb.xl.tag:resources/handwritten  -withFixCWA true -phrases"

#scripts/CL/run.py  $task  run-cwa-coref-dist-ppdb-full2-wl-3.5  "-wordnet false   -corefOnIR true  -coref true  -diffRules true  -kbest 2 -rulesWeight 3.5 -rules resources/ppdb/ppdb.xl.tag:resources/handwritten  -withFixCWA true -phrases"
