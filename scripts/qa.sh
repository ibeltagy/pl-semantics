bin/mlnsem qa qa/resources/cnn/validation/  1-3924  -log OFF  -withNegT false   -negativeEvd false   -softLogic psl -diffRules false  -keepUniv   false  -ner candc  -graphRules 2  -timeout 600000 -graphRuleLengthLimit 5 -logic dep  -baseline search -coref false  -emRandInit false -emTrainOnBest false  -treeDep true  -noMergeEntity false -vectorSpace ../../GoogleNews-vectors-negative300.bin        > tmp/search-train.qa

grep  -E "^pos\t|^neg\t" tmp/search-train.qa  > tmp/search-train.rules

target/start utcompling.mlnsemantics.run.Baseline generate tmp/model.qa tmp/search-train.rules  ../../GoogleNews-vectors-negative300.bin > tmp/search-train.features

python scripts/lexEnt.py train tmp/search-train.model  tmp/search-train.features

python scripts/lexEnt.py test tmp/search-train.model  tmp/search-train.features >  tmp/search-train.scores

paste <(grep Hi tmp/search-train.scores | awk '{print $2}')  tmp/search-train.rules | awk -F "\t" '{print $1"\t"$2"\t"$4}' >  tmp/search-train.scored

bin/mlnsem qa qa/resources/cnn/validation/  1-3924  -log OFF  -withNegT false   -negativeEvd false   -softLogic psl -diffRules false  -keepUniv   false  -ner candc  -graphRules 2  -timeout 600000 -graphRuleLengthLimit 5 -logic dep  -baseline search -coref false  -emRandInit false -emTrainOnBest false  -treeDep true  -noMergeEntity false -vectorSpace ../../GoogleNews-vectors-negative300.bin   -graphRulesFile tmp/search-train.scored  > tmp/search-train.qa.scored



bin/mlnsem qa qa/resources/cnn/test/  1-3198  -log OFF  -withNegT false   -negativeEvd false   -softLogic psl -diffRules false  -keepUniv   false  -ner candc  -graphRules 2  -timeout 600000 -graphRuleLengthLimit 5 -logic dep  -baseline search -coref false  -emRandInit false -emTrainOnBest false  -treeDep true  -noMergeEntity false -vectorSpace ../../GoogleNews-vectors-negative300.bin      > tmp/search-test.qa


grep  -E "^pos\t|^neg\t" tmp/search-test.qa  > tmp/search-test.rules

utcompling.mlnsemantics.run.Baseline generate tmp/model.qa tmp/search-test.rules  ../../GoogleNews-vectors-negative300.bin > tmp/search-test.features

python scripts/lexEnt.py test tmp/search-train.model  tmp/search-test.features >  tmp/search-test.scores


paste <(grep Hi tmp/search-test.scores | awk '{print $2}')  tmp/search-test.rules | awk -F "\t" '{print $1"\t"$2"\t"$4}' >  tmp/search-test.scored


bin/mlnsem qa qa/resources/cnn/test/  1-3198  -log OFF  -withNegT false   -negativeEvd false   -softLogic psl -diffRules false  -keepUniv   false  -ner candc  -graphRules 2  -timeout 600000 -graphRuleLengthLimit 5 -logic dep  -baseline search -coref false  -emRandInit false -emTrainOnBest false  -treeDep true  -noMergeEntity false -vectorSpace ../../GoogleNews-vectors-negative300.bin   -graphRulesFile tmp/search-test.scored  > tmp/search-test.qa.scored