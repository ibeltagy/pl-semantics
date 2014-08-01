#!/bin/bash

cd $SCRATCH/mln-semantics

export TEAMNAME="UTexas"

# SUBMISSION-1: CLEAN, COSINE MLN/PSL
export RUN=run2
export RTE_INPUTS1="$SCRATCH/runs/train/rte/cosine/*"
export STS_INPUTS1="$SCRATCH/runs/train/sts/cosine/*"
echo "Generating $RUN..."
python scripts/build_hybrid_classifier.py -g resources/sick/sick-rte.gs --out $SCRATCH/runs/test/submissions/$RUN/rte_train.arff -w --mln $RTE_INPUTS1 > $SCRATCH/runs/test/submissions/$RUN/rte_cv.txt
python scripts/build_hybrid_classifier.py -g resources/sick/sick-rte.gs --out $SCRATCH/runs/test/submissions/$RUN/rte_train.arff -w -t --mln $RTE_INPUTS1 > $SCRATCH/runs/test/submissions/$RUN/rte_pred.txt
python scripts/build_hybrid_classifier.py -g resources/sick/sick-sts.gs --out $SCRATCH/runs/test/submissions/$RUN/sts_train.arff -w --mln $STS_INPUTS1 > $SCRATCH/runs/test/submissions/$RUN/sts_cv.txt
python scripts/build_hybrid_classifier.py -g resources/sick/sick-sts.gs --out $SCRATCH/runs/test/submissions/$RUN/sts_train.arff -w -t --mln $STS_INPUTS1 > $SCRATCH/runs/test/submissions/$RUN/sts_pred.txt

python scripts/stitch.py --rte $SCRATCH/runs/test/submissions/$RUN/rte_pred.txt --sts $SCRATCH/runs/test/submissions/$RUN/sts_pred.txt --test $SCRATCH/mln-semantics/scripts/SICK_test.txt > $SCRATCH/runs/test/submissions/$RUN/${TEAMNAME}_${RUN}.txt
python scripts/gen_readme.py --run $RUN > $SCRATCH/runs/test/submissions/$RUN/${TEAMNAME}_${RUN}_README.txt

# SUBMISSION-2: CLEAN, ASYM MLN/PSL

export RUN=run3
export RTE_INPUTS2="$SCRATCH/runs/train/rte/asym/*"
export STS_INPUTS2="$SCRATCH/runs/train/sts/asym/*"
echo "Generating $RUN..."
python scripts/build_hybrid_classifier.py -g resources/sick/sick-rte.gs --out $SCRATCH/runs/test/submissions/$RUN/rte_train.arff -w --mln $RTE_INPUTS2 > $SCRATCH/runs/test/submissions/$RUN/rte_cv.txt
python scripts/build_hybrid_classifier.py -g resources/sick/sick-rte.gs --out $SCRATCH/runs/test/submissions/$RUN/rte_train.arff -w -t --mln $RTE_INPUTS2 > $SCRATCH/runs/test/submissions/$RUN/rte_pred.txt
python scripts/build_hybrid_classifier.py -g resources/sick/sick-sts.gs --out $SCRATCH/runs/test/submissions/$RUN/sts_train.arff -w --mln $STS_INPUTS2 > $SCRATCH/runs/test/submissions/$RUN/sts_cv.txt
python scripts/build_hybrid_classifier.py -g resources/sick/sick-sts.gs --out $SCRATCH/runs/test/submissions/$RUN/sts_train.arff -w -t --mln $STS_INPUTS2 > $SCRATCH/runs/test/submissions/$RUN/sts_pred.txt

python scripts/stitch.py --rte $SCRATCH/runs/test/submissions/$RUN/rte_pred.txt --sts $SCRATCH/runs/test/submissions/$RUN/sts_pred.txt --test $SCRATCH/mln-semantics/scripts/SICK_test.txt > $SCRATCH/runs/test/submissions/$RUN/${TEAMNAME}_${RUN}.txt
python scripts/gen_readme.py --run $RUN > $SCRATCH/runs/test/submissions/$RUN/${TEAMNAME}_${RUN}_README.txt

# SUBMISSION-3: PROPOSED "BASELINES"

export RUN=run4
export RTE_INPUTS3="$SCRATCH/runs/train/baselines/*"
export STS_INPUTS3="$SCRATCH/runs/train/baselines/*"
echo "Generating $RUN..."
python scripts/build_hybrid_classifier.py -g resources/sick/sick-rte.gs --out $SCRATCH/runs/test/submissions/$RUN/rte_train.arff -w --mln $RTE_INPUTS3 > $SCRATCH/runs/test/submissions/$RUN/rte_cv.txt
python scripts/build_hybrid_classifier.py -g resources/sick/sick-rte.gs --out $SCRATCH/runs/test/submissions/$RUN/rte_train.arff -w -t --mln $RTE_INPUTS3 > $SCRATCH/runs/test/submissions/$RUN/rte_pred.txt
python scripts/build_hybrid_classifier.py -g resources/sick/sick-sts.gs --out $SCRATCH/runs/test/submissions/$RUN/sts_train.arff -w --mln $STS_INPUTS3 > $SCRATCH/runs/test/submissions/$RUN/sts_cv.txt
python scripts/build_hybrid_classifier.py -g resources/sick/sick-sts.gs --out $SCRATCH/runs/test/submissions/$RUN/sts_train.arff -w -t --mln $STS_INPUTS3 > $SCRATCH/runs/test/submissions/$RUN/sts_pred.txt

python scripts/stitch.py --rte $SCRATCH/runs/test/submissions/$RUN/rte_pred.txt --sts $SCRATCH/runs/test/submissions/$RUN/sts_pred.txt --test $SCRATCH/mln-semantics/scripts/SICK_test.txt > $SCRATCH/runs/test/submissions/$RUN/${TEAMNAME}_${RUN}.txt
python scripts/gen_readme.py --run $RUN > $SCRATCH/runs/test/submissions/$RUN/${TEAMNAME}_${RUN}_README.txt

# SUBMISSION-4: ENSEMBLE OF SUBMISSIONS 1-3

export RUN=run1primary
export RTE_INPUTS4="$RTE_INPUTS1 $RTE_INPUTS2 $RTE_INPUTS3"
export STS_INPUTS4="$STS_INPUTS1 $STS_INPUTS2 $STS_INPUTS3"
echo "Generating $RUN..."
python scripts/build_hybrid_classifier.py -g resources/sick/sick-rte.gs --out $SCRATCH/runs/test/submissions/$RUN/rte_train.arff -w --mln $RTE_INPUTS4 > $SCRATCH/runs/test/submissions/$RUN/rte_cv.txt
python scripts/build_hybrid_classifier.py -g resources/sick/sick-rte.gs --out $SCRATCH/runs/test/submissions/$RUN/rte_train.arff -w -t --mln $RTE_INPUTS4 > $SCRATCH/runs/test/submissions/$RUN/rte_pred.txt
python scripts/build_hybrid_classifier.py -g resources/sick/sick-sts.gs --out $SCRATCH/runs/test/submissions/$RUN/sts_train.arff -w --mln $STS_INPUTS4 > $SCRATCH/runs/test/submissions/$RUN/sts_cv.txt
python scripts/build_hybrid_classifier.py -g resources/sick/sick-sts.gs --out $SCRATCH/runs/test/submissions/$RUN/sts_train.arff -w -t --mln $STS_INPUTS4 > $SCRATCH/runs/test/submissions/$RUN/sts_pred.txt

python scripts/stitch.py --rte $SCRATCH/runs/test/submissions/$RUN/rte_pred.txt --sts $SCRATCH/runs/test/submissions/$RUN/sts_pred.txt --test $SCRATCH/mln-semantics/scripts/SICK_test.txt > $SCRATCH/runs/test/submissions/$RUN/${TEAMNAME}_${RUN}.txt
python scripts/gen_readme.py --run $RUN > $SCRATCH/runs/test/submissions/$RUN/${TEAMNAME}_${RUN}_README.txt

