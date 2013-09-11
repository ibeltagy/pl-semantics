mln-semantics
=============

Set up workspace
----------------

    ~$ git clone git@github.com:islambeltagy/mln-semantics.git
    ~/mln-semantics$ cd mln-semantics
    ~/mln-semantics$ cd scala-logic
    ~/mln-semantics/scala-logic$ git clone git@github.com:utcompling/Scalabha.git scalabha
    ~/mln-semantics/scala-logic$ cd ..
    ~/mln-semantics$ chmod u+x bin/mlnsem

    (In scala-logic/scalabha/build.sbt file, replace the following line:
	"org.scalanlp" % "breeze-learn_2.9.2" % "0.2-SNAPSHOT" changing(),
    by
	"org.scalanlp" % "breeze-learn_2.9.2" % "0.2" changing(),
    )

    ~/mln-semantics$ bin/mlnsem compile
    
    ~/mln-semantics$ cd resources
    ~/mln-semantics/resources$ ln -s /u/beltagy/workspace/deft/STS/ sts
    ~/mln-semantics/resources$ ln -s /u/dhg/Corpora/nytgiga.lem.vc.f2000.m50.wInf.txt full.vs
    ~/mln-semantics/resources$ ln -s /u/dhg/Corpora/polarity-lexicon polarity-lexicon
    ~/mln-semantics/resources$ ln -s /u/ckcuong/RESEARCH/Programs/WordNet-3.0/dict/ wordnet
    ~/mln-semantics/resources$ cd ..


Using Boxer
-----------

    ~/mln-semantics$ bin/mlnsem boxer OPTIONS

You can parse a single sentence with the `-s` option:

    ~/mln-semantics$ bin/mlnsem boxer -s "A dog walks." [OPTIONS]

or an entire file of sentences (one sentence per line):

    ~/mln-semantics$ bin/mlnsem boxer -f sentences.txt [OPTIONS]

Output options:

    -draw true/false (default: true)        Prints a graphical DRT representation
    -boxer true/false (default: true)       Prints logical form in Boxer notation
    -drt true/false (default: true)         Prints the logical form as a DRS
    -fol true/false (default: true)         Prints the logical form in first-order logic
    
Regarding tokenization: The code will automatically tokenize all input sentences, so it does not matter if
the input is given tokenized or not.  It will not do sentence-splitting, however, but this can be included
if it would be useful.

Running STS test suite
----------------------
    ~/mln-semantics$ bin/mlnsem run


Set Eclipse project: 
--------------------
-run bin/sbt...   then type eclipse
-fix configuration path in scalabha project


Running RTE test suite (work by Cuong)
----------------------
    ~/mln-semantics$ mkdir data/out-train
    ~/mln-semantics$ mkdir data/out-test
    ~/mln-semantics$ mkdir data/multiOut-train
    ~/mln-semantics$ mkdir data/multiOut-test

    ~/mln-semantics$ chmod u+x bin/mlnsem-condor-1
    ~/mln-semantics$ chmod u+x bin/mlnsem-condor-2
    ~/mln-semantics$ chmod u+x bin/mlnsem-condor-3

    ~/mln-semantics$ chmod u+x condorSubmit.sh
    ~/mln-semantics$ ./condorSubmit.sh

    ~/mln-semantics$ chmod u+x data/script.sh
    ~/mln-semantics$ cd data
    ~/mln-semantics/data$ ./script.sh merge
    ~/mln-semantics/data$ ./script.sh classify 1
    ~/mln-semantics/data$ ./script.sh classify 2
    ~/mln-semantics/data$ ./script.sh classify 3


RTE descriptions
----------------

* Experimented with prior -1.0

* In Sts.scala:

Employ Lucene for indexing and searching inference rules from external resources.
See `/utcompling/mlnsemantics/util/Lucene.scala` for more details.
Each rule has the following format:
<id> TAB <lhs> TAB <rhs> TAB <entail_score> 

Put type constraints for variables via `FindEventsProbabilisticTheoremProver` class.

* In AlchemyTheoremProver.scala:

Function `createSamePredRule` generates inference rules for each pair of predicates having the same name.
E.g., man_dt(indv0) => man_dh(indv0).
This function also employs WordNet to find similar words.

Function `makeEvidenceFile` removes all evidence predicates that don't appear in inference rules.
This can help to reduce domain size.

Function `makeMlnFile` removes all theme relation predicates and equality constraints in Hypothesis 
because there are no inference rules for them. 
To do that easier, I remove inner parentheses of conjunction expressions (see funtion `_convert`).

* In InferenceRuleInjectingProbabilisticTheoremProver.scala:

Function `checkCompatibleType` checks compatibility between lhs and rhs of an inference rule.

Function `convertParaphraseToFOL` converts paraphrase rules in text format to FOL.

Function `getAllRelations` also gets "agent" and "patient" relations.

* In MergeSameVarPredProbabilisticTheoremProver.scala:

Function `getAllPreds` puts entity type for BoxerNamed.

Do not merge same variable predicates.

* In HardAssumptionAsEvidenceProbabilisticTheoremProver.scala:

Change `renameVars`.

* Remove ("--candc-int-betas" -> "0.00075 0.0003 0.0001 0.00005 0.00001") parameter from 
`/utcompling/mlnsemantics/datagen/CncLemmatizeCorpus.scala` and `/utcompling/scalalogic/discourse/impl/BoxerDiscourseInterpreter.scala`
because we get more parse fails with it.

* In utcompling/scalalogic/discourse/candc/boxer/expression/interpreter/impl/OccurrenceMarkingBoxerExpressionInterpreterDecorator.scala:

Get positions of words in sentence. Words with same name will be treated differently based on their position.


Trento errors
-------------

* Parser errors: 
 - Wrong POS tags. Several verbs are classified as nouns.
 - Wrong semantic representations.

* Problem with negation.

* Problem with directional distributional similarity.

* Problem with singular and plural nouns.

* No vector representation for sentences "A woman fries eggs" and "A woman fries big eggs" in group 5.


Run Aidan's code on local machine
---------------------------------
* git clone git@github.com:raptros/tr-corpus-one.git

* Start the soap server with the correct arguments - `$CANDC_HOME/bin/soap_server --models $CANDC_HOME/models --server localhost:12200 --candc-printer boxer` will do the job

* ./run local trc1.MaxTransform data/rules.in data/sentences.txt data/out


Merging Cuong's fork with main
------------------------------
* Code of running condor is replaced with my scripts for condor. What is still missing is organizging the output and running the scripts on it. 
* Changes in GetPredicatesDeclarationsProbabilisticTheoremProver.scala, HardAssumptionAsEvidenceProbabilisticTheoremProver.scala, InferenceRuleInjectingProbabilisticTheoremProver.scala, AlchemyTheoremProver.scala related to predicates naming _dh, _dt are reverted. Still, it is necessary to declare all predicates as open-world because now evidence predicates are close-world. 
* In AlchemyTheoremProver.scala, there is a function to generate rules based on wordnet, it is commented for now but it should not
* Changes in MergeSameVarPredProbabilisticTheoremProver.scala are reverted. I need to reimplmenet them by, 1)commenting MergeSameVarPredProbabilisticTheoremProver in STS. 2)replace BoxerName with BoxerPred, make sure this applied to Text, Hypothesis and Inference rules as well
* I have no idea what this file does ExistentialEliminatingProbabilisticTheoremProver.scala
* Cuong commented this class FromEntToEqvProbabilisticTheoremProver, do not know why. It remains commented. 
* Search the file InferenceRuleInjectingProbabilisticTheoremProver.scala for FIXIT (they are two), read comment and fix. 
