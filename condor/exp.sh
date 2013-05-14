#THis script should be called from the folder: mln-semantic using the command condor/exp.sh

#failed experiments
fails="run-vid-irLvl0-peInffalse-vectorMakeradd-noDupfalse-scaleWtrue-maxProb0.75-wThr0.35-range-.1391-1400 run-vid-irLvl0-peInffalse-vectorMakeradd-noDupfalse-scaleWtrue-maxProb0.93-wThr0.35-range-.1331-1340 run-vid-irLvl0-peInffalse-vectorMakeradd-noDupfalse-scaleWtrue-maxProb0.93-wThr0.35-range-.471-480 run-vid-irLvl0-peInffalse-vectorMakeradd-noDuptrue-scaleWtrue-maxProb0.75-wThr0.35-range-.341-350 run-vid-irLvl0-peInffalse-vectorMakeradd-noDuptrue-scaleWtrue-maxProb0.75-wThr0.35-range-.511-520 run-vid-irLvl0-peInffalse-vectorMakeradd-noDuptrue-scaleWtrue-maxProb0.75-wThr0.35-range-.761-770 run-vid-irLvl0-peInffalse-vectorMakeradd-noDuptrue-scaleWtrue-maxProb0.93-wThr0.35-range-.1131-1140 run-vid-irLvl0-peInffalse-vectorMakermul-noDupfalse-scaleWtrue-maxProb0.75-wThr0.35-range-.351-360 run-vid-irLvl0-peInffalse-vectorMakermul-noDupfalse-scaleWtrue-maxProb0.75-wThr0.35-range-.471-480 run-vid-irLvl0-peInffalse-vectorMakermul-noDupfalse-scaleWtrue-maxProb0.93-wThr0.35-range-.1331-1340 run-vid-irLvl0-peInffalse-vectorMakermul-noDuptrue-scaleWtrue-maxProb0.75-wThr0.35-range-.1131-1140 run-vid-irLvl0-peInffalse-vectorMakermul-noDuptrue-scaleWtrue-maxProb0.93-wThr0.35-range-.581-590 run-vid-irLvl0-peInffalse-vectorMakermul-noDuptrue-scaleWtrue-maxProb0.93-wThr0.35-range-.81-90 run-vid-irLvl0-peInftrue-vectorMakeradd-noDuptrue-scaleWtrue-maxProb0.75-wThr0.35-range-.1331-1340 run-vid-irLvl0-peInftrue-vectorMakeradd-noDuptrue-scaleWtrue-maxProb0.75-wThr0.35-range-.351-360 run-vid-irLvl0-peInftrue-vectorMakeradd-noDuptrue-scaleWtrue-maxProb0.93-wThr0.35-range-.1331-1340 run-vid-irLvl0-peInftrue-vectorMakeradd-noDuptrue-scaleWtrue-maxProb0.93-wThr0.35-range-.1371-1380 run-vid-irLvl0-peInftrue-vectorMakermul-noDupfalse-scaleWtrue-maxProb0.75-wThr0.35-range-.661-670 run-vid-irLvl0-peInftrue-vectorMakermul-noDupfalse-scaleWtrue-maxProb0.93-wThr0.35-range-.1421-1430 run-vid-irLvl1-peInffalse-vectorMakeradd-noDupfalse-scaleWtrue-maxProb0.75-wThr0.35-range-.351-360 run-vid-irLvl1-peInffalse-vectorMakeradd-noDupfalse-scaleWtrue-maxProb0.93-wThr0.35-range-.581-590 run-vid-irLvl1-peInffalse-vectorMakeradd-noDuptrue-scaleWtrue-maxProb0.75-wThr0.35-range-.1331-1340 run-vid-irLvl1-peInffalse-vectorMakeradd-noDuptrue-scaleWtrue-maxProb0.75-wThr0.35-range-.401-410 run-vid-irLvl1-peInffalse-vectorMakeradd-noDuptrue-scaleWtrue-maxProb0.93-wThr0.35-range-.401-410 run-vid-irLvl1-peInffalse-vectorMakermul-noDupfalse-scaleWtrue-maxProb0.75-wThr0.35-range-.1331-1340 run-vid-irLvl1-peInffalse-vectorMakermul-noDupfalse-scaleWtrue-maxProb0.75-wThr0.35-range-.341-350 run-vid-irLvl1-peInffalse-vectorMakermul-noDuptrue-scaleWtrue-maxProb0.75-wThr0.35-range-.401-410 run-vid-irLvl1-peInftrue-vectorMakeradd-noDupfalse-scaleWtrue-maxProb0.75-wThr0.35-range-.1421-1430 run-vid-irLvl1-peInftrue-vectorMakeradd-noDuptrue-scaleWtrue-maxProb0.75-wThr0.35-range-.471-480 run-vid-irLvl1-peInftrue-vectorMakermul-noDupfalse-scaleWtrue-maxProb0.75-wThr0.35-range-.661-670 run-vid-irLvl1-peInftrue-vectorMakermul-noDupfalse-scaleWtrue-maxProb0.75-wThr0.35-range-.761-770 run-vid-irLvl1-peInftrue-vectorMakermul-noDuptrue-scaleWtrue-maxProb0.75-wThr0.35-range-.491-500 run-vid-irLvl1-peInftrue-vectorMakermul-noDuptrue-scaleWtrue-maxProb0.75-wThr0.35-range-.581-590 run-vid-irLvl2-peInffalse-vectorMakeradd-noDupfalse-scaleWtrue-maxProb0.93-wThr0.35-range-.661-670 run-vid-irLvl2-peInffalse-vectorMakeradd-noDuptrue-scaleWtrue-maxProb0.75-wThr0.35-range-.661-670 run-vid-irLvl2-peInffalse-vectorMakeradd-noDuptrue-scaleWtrue-maxProb0.93-wThr0.35-range-.1331-1340 run-vid-irLvl2-peInffalse-vectorMakeradd-noDuptrue-scaleWtrue-maxProb0.93-wThr0.35-range-.511-520 run-vid-irLvl2-peInftrue-vectorMakeradd-noDupfalse-scaleWtrue-maxProb0.93-wThr0.35-range-.1391-1400 run-vid-irLvl2-peInftrue-vectorMakermul-noDupfalse-scaleWtrue-maxProb0.75-wThr0.35-range-.341-350 "

let cnt=0;

#-vsWithPos true, (false)
#-chopLvl (rpr), rp

varBind=false #true

if [[ "$varBind" == "false"* ]]; then
	#without variable binding, system is fast so use wide ranges
   rangeVals="1-150 151-300 301-450 451-600 601-750 751-900 901-1050 1051-1200 1201-1350 1351-1500"
else
   #with variable binding, it is terribly slow, use narrow ranges
   rangeVals="1-10 11-20 21-30 31-40 41-50 51-60 61-70 71-80 81-90 91-100 101-110 111-120 121-130 131-140 141-150 151-160 161-170 171-180 181-190 191-200 201-210 211-220 221-230 231-240 241-250 251-260 261-270 271-280 281-290 291-300 301-310 311-320 321-330 331-340 341-350 351-360 361-370 371-380 381-390 391-400 401-410 411-420 421-430 431-440 441-450 451-460 461-470 471-480 481-490 491-500 501-510 511-520 521-530 531-540 541-550 551-560 561-570 571-580 581-590 591-600 601-610 611-620 621-630 631-640 641-650 651-660 661-670 671-680 681-690 691-700 701-710 711-720 721-730 731-740 741-750 751-760 761-770 771-780 781-790 791-800 801-810 811-820 821-830 831-840 841-850 851-860 861-870 871-880 881-890 891-900 901-910 911-920 921-930 931-940 941-950 951-960 961-970 971-980 981-990 991-1000 1001-1010 1011-1020 1021-1030 1031-1040 1041-1050 1051-1060 1061-1070 1071-1080 1081-1090 1091-1100 1101-1110 1111-1120 1121-1130 1131-1140 1141-1150 1151-1160 1161-1170 1171-1180 1181-1190 1191-1200 1201-1210 1211-1220 1221-1230 1231-1240 1241-1250 1251-1260 1261-1270 1271-1280 1281-1290 1291-1300 1301-1310 1311-1320 1321-1330 1331-1340 1341-1350 1351-1360 1361-1370 1371-1380 1381-1390 1391-1400 1401-1410 1411-1420 1421-1430 1431-1440 1441-1450 1451-1460 1461-1470 1471-1480 1481-1490 1491-1500"
fi

#changing the parameters change the final result. 
#From my previous experiment, usually one of the following 8 combinations has the best score. 
for ds in run-vid
do
for irLvl in  2 # 0 1
 do
  for peInf in true false
  do
   for vectorMaker in add # mul
   do
	for noDup in false true
	do
		for scaleW in true
		do
			for maxProb in 0.93  0.75
			do
			  for wThr in 0.35
			  do
				a="["
				#File name is a concatination of the configuration. 
				outputFileConfig="$ds-irLvl$irLvl-peInf$peInf-vectorMaker$vectorMaker-noDup$noDup-scaleW$scaleW-maxProb$maxProb-wThr$wThr"
#				echo $outputFileConfig
#   				continue

				for range in $rangeVals
				do
					cnt=`expr $cnt + 1`;
					#Add range to file name
					outputFile="$outputFileConfig-range-.$range"

#THe next block is to rerun the failed experiments. Failed experiments are listed in the variable "fails"
#					if [[ "$fails" == *"$outputFile"* ]]; then
#						echo $cnt $outputFile
#					else
#						continue
#					fi
#END of rerun-failed-experiments block

					#make sure to create the folder "out"
					outputFile="condor/out/$outputFile"

#This line is to print the experiments, just to count them before actually scheduling them into condor.      
					echo $cnt $outputFile

#This line schedule the condor job. Make sure what you are going to run is correct before you actually schedule them.
./condor/condorizer.py bin/mlnsem $ds $range -timeout 1000000 -varBind $varBind -vectorMaker $vectorMaker -log OFF -noDup $noDup -scaleW $scaleW -maxProb $maxProb -wThr $wThr -peInf $peInf -irLvl $irLvl $outputFile

#The next block is to collect results of many output files
#					b=$(tail $outputFile.out -n 2 | head -n 1)
#					len=$(expr length "$b")
#					len=$(expr $len - 2);
#					b=$(expr substr "$b" 2 $len)
#					a="$a $b"
#End part 1 of  collecting-results block
				done
#Continue of the collecting-results block
#				a="$a]"
#				echo $a
#End of collecting-results block 
			  done
			done
		done
	 done
   done
  done
 done
done


