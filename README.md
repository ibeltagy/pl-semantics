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
    ~/mln-semantics$ bin/mlnsem compile
    
    ~/mln-semantics$ cd resources
    ~/mln-semantics/resources$ ln -s /u/beltagy/workspace/deft/STS/ sts
    ~/mln-semantics/resources$ ln -s /u/dhg/Corpora/nytgiga.lem.vc.f2000.m50.wInf.txt full.vs
    ~/mln-semantics/resources$ ln -s /u/dhg/Corpora/polarity-lexicon polarity-lexicon
    ~/mln-semantics/resources$ ln -s /u/dhg/Corpora/wordnet-3.0/ wordnet
    ~/mln-semantics/resources$ cd ..
   
    export CANDCHOME="/u/beltagy/workspace/deft/candcLatest/bin"
    export ALCHEMYHOME="/u/beltagy/workspace/deft/alchemy-2/bin"
    export PROVER9HOME="/v/filer4b/v16q001/users/dhg/bin/LADR-2009-02A/bin"
    export TUFFYHOME="/u/beltagy/workspace/deft/tuffy"


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
