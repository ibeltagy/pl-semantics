#THis script should be called from the folder: mln-semantic using the command condor/exp.sh

#failed experiments
fails="run-vid-irLvl0-peInffalse-vectorMakeradd-noDupfalse-scaleWtrue-maxProb0.75-wThr0.35-range-.1391-1400 run-vid-irLvl0-peInffalse-vectorMakeradd-noDupfalse-scaleWtrue-maxProb0.93-wThr0.35-range-.1331-1340 run-vid-irLvl0-peInffalse-vectorMakeradd-noDupfalse-scaleWtrue-maxProb0.93-wThr0.35-range-.471-480 run-vid-irLvl0-peInffalse-vectorMakeradd-noDuptrue-scaleWtrue-maxProb0.75-wThr0.35-range-.341-350 run-vid-irLvl0-peInffalse-vectorMakeradd-noDuptrue-scaleWtrue-maxProb0.75-wThr0.35-range-.511-520 run-vid-irLvl0-peInffalse-vectorMakeradd-noDuptrue-scaleWtrue-maxProb0.75-wThr0.35-range-.761-770 run-vid-irLvl0-peInffalse-vectorMakeradd-noDuptrue-scaleWtrue-maxProb0.93-wThr0.35-range-.1131-1140 run-vid-irLvl0-peInffalse-vectorMakermul-noDupfalse-scaleWtrue-maxProb0.75-wThr0.35-range-.351-360 run-vid-irLvl0-peInffalse-vectorMakermul-noDupfalse-scaleWtrue-maxProb0.75-wThr0.35-range-.471-480 run-vid-irLvl0-peInffalse-vectorMakermul-noDupfalse-scaleWtrue-maxProb0.93-wThr0.35-range-.1331-1340 run-vid-irLvl0-peInffalse-vectorMakermul-noDuptrue-scaleWtrue-maxProb0.75-wThr0.35-range-.1131-1140 run-vid-irLvl0-peInffalse-vectorMakermul-noDuptrue-scaleWtrue-maxProb0.93-wThr0.35-range-.581-590 run-vid-irLvl0-peInffalse-vectorMakermul-noDuptrue-scaleWtrue-maxProb0.93-wThr0.35-range-.81-90 run-vid-irLvl0-peInftrue-vectorMakeradd-noDuptrue-scaleWtrue-maxProb0.75-wThr0.35-range-.1331-1340 run-vid-irLvl0-peInftrue-vectorMakeradd-noDuptrue-scaleWtrue-maxProb0.75-wThr0.35-range-.351-360 run-vid-irLvl0-peInftrue-vectorMakeradd-noDuptrue-scaleWtrue-maxProb0.93-wThr0.35-range-.1331-1340 run-vid-irLvl0-peInftrue-vectorMakeradd-noDuptrue-scaleWtrue-maxProb0.93-wThr0.35-range-.1371-1380 run-vid-irLvl0-peInftrue-vectorMakermul-noDupfalse-scaleWtrue-maxProb0.75-wThr0.35-range-.661-670 run-vid-irLvl0-peInftrue-vectorMakermul-noDupfalse-scaleWtrue-maxProb0.93-wThr0.35-range-.1421-1430 run-vid-irLvl1-peInffalse-vectorMakeradd-noDupfalse-scaleWtrue-maxProb0.75-wThr0.35-range-.351-360 run-vid-irLvl1-peInffalse-vectorMakeradd-noDupfalse-scaleWtrue-maxProb0.93-wThr0.35-range-.581-590 run-vid-irLvl1-peInffalse-vectorMakeradd-noDuptrue-scaleWtrue-maxProb0.75-wThr0.35-range-.1331-1340 run-vid-irLvl1-peInffalse-vectorMakeradd-noDuptrue-scaleWtrue-maxProb0.75-wThr0.35-range-.401-410 run-vid-irLvl1-peInffalse-vectorMakeradd-noDuptrue-scaleWtrue-maxProb0.93-wThr0.35-range-.401-410 run-vid-irLvl1-peInffalse-vectorMakermul-noDupfalse-scaleWtrue-maxProb0.75-wThr0.35-range-.1331-1340 run-vid-irLvl1-peInffalse-vectorMakermul-noDupfalse-scaleWtrue-maxProb0.75-wThr0.35-range-.341-350 run-vid-irLvl1-peInffalse-vectorMakermul-noDuptrue-scaleWtrue-maxProb0.75-wThr0.35-range-.401-410 run-vid-irLvl1-peInftrue-vectorMakeradd-noDupfalse-scaleWtrue-maxProb0.75-wThr0.35-range-.1421-1430 run-vid-irLvl1-peInftrue-vectorMakeradd-noDuptrue-scaleWtrue-maxProb0.75-wThr0.35-range-.471-480 run-vid-irLvl1-peInftrue-vectorMakermul-noDupfalse-scaleWtrue-maxProb0.75-wThr0.35-range-.661-670 run-vid-irLvl1-peInftrue-vectorMakermul-noDupfalse-scaleWtrue-maxProb0.75-wThr0.35-range-.761-770 run-vid-irLvl1-peInftrue-vectorMakermul-noDuptrue-scaleWtrue-maxProb0.75-wThr0.35-range-.491-500 run-vid-irLvl1-peInftrue-vectorMakermul-noDuptrue-scaleWtrue-maxProb0.75-wThr0.35-range-.581-590 run-vid-irLvl2-peInffalse-vectorMakeradd-noDupfalse-scaleWtrue-maxProb0.93-wThr0.35-range-.661-670 run-vid-irLvl2-peInffalse-vectorMakeradd-noDuptrue-scaleWtrue-maxProb0.75-wThr0.35-range-.661-670 run-vid-irLvl2-peInffalse-vectorMakeradd-noDuptrue-scaleWtrue-maxProb0.93-wThr0.35-range-.1331-1340 run-vid-irLvl2-peInffalse-vectorMakeradd-noDuptrue-scaleWtrue-maxProb0.93-wThr0.35-range-.511-520 run-vid-irLvl2-peInftrue-vectorMakeradd-noDupfalse-scaleWtrue-maxProb0.93-wThr0.35-range-.1391-1400 run-vid-irLvl2-peInftrue-vectorMakermul-noDupfalse-scaleWtrue-maxProb0.75-wThr0.35-range-.341-350 "

let cnt=0;

#-vsWithPos true, (false)
#-chopLvl (rpr), rp

varBind=false #true

#if [[ "$varBind" == "false"* ]]; then
	#without variable binding, system is fast so use wide ranges
#   rangeVals="1-150 151-300 301-450 451-600 601-750 751-900 901-1050 1051-1200 1201-1350 1351-1500"
#else
   #with variable binding, it is terribly slow, use narrow ranges
#   rangeVals="1-10 11-20 21-30 31-40 41-50 51-60 61-70 71-80 81-90 91-100 101-110 111-120 121-130 131-140 141-150 151-160 161-170 171-180 181-190 191-200 201-210 211-220 221-230 231-240 241-250 251-260 261-270 271-280 281-290 291-300 301-310 311-320 321-330 331-340 341-350 351-360 361-370 371-380 381-390 391-400 401-410 411-420 421-430 431-440 441-450 451-460 461-470 471-480 481-490 491-500 501-510 511-520 521-530 531-540 541-550 551-560 561-570 571-580 581-590 591-600 601-610 611-620 621-630 631-640 641-650 651-660 661-670 671-680 681-690 691-700 701-710 711-720 721-730 731-740 741-750 751-760 761-770 771-780 781-790 791-800 801-810 811-820 821-830 831-840 841-850 851-860 861-870 871-880 881-890 891-900 901-910 911-920 921-930 931-940 941-950 951-960 961-970 971-980 981-990 991-1000 1001-1010 1011-1020 1021-1030 1031-1040 1041-1050 1051-1060 1061-1070 1071-1080 1081-1090 1091-1100 1101-1110 1111-1120 1121-1130 1131-1140 1141-1150 1151-1160 1161-1170 1171-1180 1181-1190 1191-1200 1201-1210 1211-1220 1221-1230 1231-1240 1241-1250 1251-1260 1261-1270 1271-1280 1281-1290 1291-1300 1301-1310 1311-1320 1321-1330 1331-1340 1341-1350 1351-1360 1361-1370 1371-1380 1381-1390 1391-1400 1401-1410 1411-1420 1421-1430 1431-1440 1441-1450 1451-1460 1461-1470 1471-1480 1481-1490 1491-1500"
#fi
rangeVals="1-5 6-10 11-15 16-20 21-25 26-30 31-35 36-40 41-45 46-50 51-55 56-60 61-65 66-70 71-75 76-80 81-85 86-90 91-95 96-100 101-105 106-110 111-115 116-120 121-125 126-130 131-135 136-140 141-145 146-150 151-155 156-160 161-165 166-170 171-175 176-180 181-185 186-190 191-195 196-200 201-205 206-210 211-215 216-220 221-225 226-230 231-235 236-240 241-245 246-250 251-255 256-260 261-265 266-270 271-275 276-280 281-285 286-290 291-295 296-300 301-305 306-310 311-315 316-320 321-325 326-330 331-335 336-340 341-345 346-350 351-355 356-360 361-365 366-370 371-375 376-380 381-385 386-390 391-395 396-400 401-405 406-410 411-415 416-420 421-425 426-430 431-435 436-440 441-445 446-450 451-455 456-460 461-465 466-470 471-475 476-480 481-485 486-490 491-495 496-500 501-505 506-510 511-515 516-520 521-525 526-530 531-535 536-540 541-545 546-550 551-555 556-560 561-565 566-570 571-575 576-580 581-585 586-590 591-595 596-600 601-605 606-610 611-615 616-620 621-625 626-630 631-635 636-640 641-645 646-650 651-655 656-660 661-665 666-670 671-675 676-680 681-685 686-690 691-695 696-700 701-705 706-710 711-715 716-720 721-725 726-730 731-735 736-740 741-745 746-750 751-755 756-760 761-765 766-770 771-775 776-780 781-785 786-790 791-795 796-800"

#rangeVals="1-3 10-13 799-800"

#changing the parameters change the final result. 
#From my previous experiment, usually one of the following 8 combinations has the best score. 
for ds in rte
do
for rteCnt in  1 #2 3
 do
  for rteMode in  test  train
  do
   for vectorMaker in add # mul
   do
	for noDup in false #true
	do
		for scaleW in true
		do
			for maxProb in 0.93 #  0.75
			do
			  for wThr in 0.2
			  do
				a=""
				#File name is a concatination of the configuration. 
				#outputFileConfig="$ds-irLvl$irLvl-peInf$peInf-vectorMaker$vectorMaker-noDup$noDup-scaleW$scaleW-maxProb$maxProb-wThr$wThr"
				outputFileConfig="rte-$rteCnt-$rteMode-wThr$wThr"
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
					outputFile="condor/rte-$rteCnt-$rteMode/$outputFile"

#This line is to print the experiments, just to count them before actually scheduling them into condor.      
					echo $cnt $outputFile

#This line schedule the condor job. Make sure what you are going to run is correct before you actually schedule them.
#./condor/condorizer.py bin/mlnsem $ds $range -timeout 1000000 -varBind $varBind -vectorMaker $vectorMaker -log OFF -noDup $noDup -scaleW $scaleW -maxProb $maxProb -wThr $wThr -peInf $peInf -irLvl $irLvl $outputFile

	case $1 in 
		submit)
./condor/condorizer.py bin/mlnsem run $ds $rteCnt $rteMode $range -timeout 300000 -wThr $wThr -log OFF -fixDCA true -noHMinus true $outputFile
		;;
		
		collect)
      	#The next block is to collect results of many output files
			b=$(tail $outputFile.out -n 2 | head -n 1)
         len=$(expr length "$b")
         len=$(expr $len - 2);
         b=$(expr substr "$b" 2 $len)
	 echo $b
         a="$a $b"
		;;
	esac

				done

	case $1 in      
      collect)
           echo $a |sed  's/ /\n/g'  > results/RTE$rteCnt.$rteMode.res
           a="[$a]"
           echo $a
      ;;
   esac
			  done
			done
		done
	 done
   done
  done
 done
done


