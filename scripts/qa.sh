#!/bin/bash
mkdir $1
echo "dir: $1"  #directory 
echo "args: $2"  # "-logic dep  -baseline search"
echo "vectorspace: $3"  # "../../GoogleNews-vectors-negative300.bin"
trainDir="qa/resources/cnn/validation/  1-3924"
testDir="qa/resources/cnn/test/  1-3198"

bin/mlnsem qa $trainDir  -log OFF  -withNegT false   -negativeEvd false   -softLogic psl -diffRules false  -keepUniv   false  -ner candc  -graphRules 2  -timeout 600000 -graphRuleLengthLimit 5  -coref false  -emRandInit false -emTrainOnBest false  -treeDep true  -noMergeEntity false  -vectorSpace $3  $2   > $1/train.qa  &

bin/mlnsem qa  $testDir  -log OFF  -withNegT false   -negativeEvd false  -softLogic psl -diffRules false  -keepUniv   false  -ner candc  -graphRules 2  -timeout 600000 -graphRuleLengthLimit 5  -coref false  -emRandInit false -emTrainOnBest false  -treeDep true  -noMergeEntity false -vectorSpace $3  $2  > $1/test.qa  &

wait


echo "Validation result without lexEnt................................................"
tail -n 5 $1/train.qa
echo "Testing result without lexEnt................................................"
tail -n 5 $1/test.qa


grep  -E "^pos\t|^neg\t" $1/train.qa  > $1/train.rules
grep  -E "^pos\t|^neg\t" $1/test.qa > $1/test.rules

echo "Generating lexEnt features................................................"


target/start utcompling.mlnsemantics.run.Baseline generate $1/rules.model $1/train.rules  $3 > $1/train.features  &
target/start utcompling.mlnsemantics.run.Baseline generate $1/rules.model $1/test.rules   $3 > $1/test.features   &

wait



python scripts/lexEnt.py train $1/rules.model  $1/train.features
python scripts/lexEnt.py test $1/rules.model  $1/train.features >  $1/train.scores
paste <(grep Hi $1/train.scores | awk '{print $2}') $1/train.rules | awk -F "\t" '{print $1"\t"$2"\t"$4}' >  $1/train.scored

python scripts/lexEnt.py test $1/rules.model  $1/test.features >  $1/test.scores
paste <(grep Hi $1/test.scores | awk '{print $2}') $1/test.rules | awk -F "\t" '{print $1"\t"$2"\t"$4}' >  $1/test.scored

echo "lexEnt training result................................................"
tail $1/train.scores
echo "lexEnt testing result................................................"
tail $1/test.scores

bin/mlnsem qa $trainDir  -log OFF  -withNegT false   -negativeEvd false   -softLogic psl -diffRules false  -keepUniv   false  -ner candc  -graphRules 2  -timeout 600000 -graphRuleLengthLimit 5 -coref false  -emRandInit false -emTrainOnBest false  -treeDep true  -noMergeEntity false -vectorSpace $3  $2  -graphRulesFile $1/train.scored  > $1/train.qa.scored  &

bin/mlnsem qa $testDir  -log OFF  -withNegT false   -negativeEvd false   -softLogic psl -diffRules false  -keepUniv   false  -ner candc  -graphRules 2  -timeout 600000 -graphRuleLengthLimit 5 -coref false  -emRandInit false -emTrainOnBest false  -treeDep true  -noMergeEntity false -vectorSpace $3  $2  -graphRulesFile $1/test.scored  > $1/test.qa.scored  &

wait

echo "Validation result with lexEnt................................................"
tail -n 5 $1/train.qa.scored
echo "Testing result with lexEnt................................................"
tail -n 5 $1/test.qa.scored

cat $1/train.qa.scored | grep -E " qid:|Question"  > $1/train.rank
cat $1/test.qa.scored  | grep -E " qid:|Question"  > $1/test.rank

echo "Training ranker................................................"
java -jar bin/RankLib-2.1-patched.jar  -train $1/train.rank -ranker 6 -save $1/rank.model > $1/rank.log

tail $1/rank.log

echo "Evaluating ranker ................................................"


java -jar bin/RankLib-2.1-patched.jar  -load $1/rank.model  -rank $1/train.rank  -score $1/train.rank.scores  &
java -jar bin/RankLib-2.1-patched.jar  -load $1/rank.model  -rank $1/test.rank  -score $1/test.rank.scores    &

wait


cat  $1/train.rank.scores  | awk '{print $1, $3, $2}' | sort -k 1,2 -n  -r  > $1/train.rank.scores.sorted
echo "Validation ranks ................................................"
python scripts/rank.py test $1/train.rank.scores.sorted  | awk '{print $3}' | sort | uniq -c  | sort -k 2,2 -n | head 

cat  $1/test.rank.scores  | awk '{print $1, $3, $2}' | sort -k 1,2 -n  -r  > $1/test.rank.scores.sorted
echo "Testing ranks ................................................"
python scripts/rank.py test $1/test.rank.scores.sorted  | awk '{print $3}' | sort | uniq -c  | sort -k 2,2 -n | head 

#paste -d "\0" <(cat tmp/exp-word/train.qa | grep -E "Question|qid:" |  awk -F ":| " '{print $1, $2":"$3, "1:"$17, "2:"$19, "3:"$21, "4:"$23, "5:"$25}'   )  <(cat tmp/exp-dep/train.qa | grep -E "Question|qid:" |  awk -F ":| " '{print "10:"$17,    "11:"$19, "12:"$21, "13:"$23, "14:"$25, $26}') > tmp/train.rank

#paste -d "\0" <(cat tmp/exp-word/test.qa | grep -E "Question|qid:" |  awk -F ":| " '{print $1, $2":"$3, "1:"$17, "2:"$19, "3:"$21, "4:"$23, "5:"$25}'   )  <(cat tmp/exp-dep/test.qa | grep -E "Question|qid:" |  awk -F ":| " '{print "10:"$17,    "11:"$19, "12:"$21, "13:"$23, "14:"$25, $26}') > tmp/test.rank
