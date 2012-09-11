mln-semantics
=============

Set up workspace:

    ~$ git clone git@github.com:USERNAME/mln-semantics.git
    ~/mln-semantics$ cd mln-semantics
    ~/mln-semantics$ git clone git@github.com:dhgarrette/scala-logic.git
    ~/mln-semantics$ cd scala-logic
    ~/mln-semantics/scala-logic$ git clone git@github.com:utcompling/Scalabha.git scalabha
    ~/mln-semantics/scala-logic$ cd ..
    ~/mln-semantics$ chmod u+x bin/mlnsem
    ~/mln-semantics$ bin/mlnsem compile
    
    ~/mln-semantics$ cd resources
    ~/mln-semantics/resources$ ln -s /v/filer4b/v16q001/users/dhg/Corpora/semantic-textual-similarity/ semantic-textual-similarity
    ~/mln-semantics/resources$ ln -s /u/dhg/Corpora/nytgiga.lem.vc.f2000.m50.wInf.txt full.vs
    ~/mln-semantics/resources$ ln -s /u/dhg/Corpora/polarity-lexicon polarity-lexicon
    ~/mln-semantics/resources$ cd ..

    export CANDCHOME="/u/dhg/workspace/candc.bkp/bin"
    export ALCHEMYHOME="/v/filer4b/v16q001/users/dhg/bin/alchemy/bin"
	export PROVER9HOME="/v/filer4b/v16q001/users/dhg/bin/LADR-2009-02A/bin"


Using Boxer:
    
    ~/mln-semantics$ bin/mlnsem boxer 

    //val draw = opts.getOrElse("-draw", "true").toBoolean
    //val boxer = opts.getOrElse("-boxer", "true").toBoolean
    //val drt = opts.get("-drs").orElse(opts.get("-drt")).map(_.toBoolean).getOrElse(true)
    //val fol = opts.getOrElse("-fol", "true").toBoolean

    //assert(opts.contains("-f") || opts.contains("-s"), "specify '-f FILENAME' or '-s SENTENCE'")
        
    ~/mln-semantics$ bin/mlnsem sts