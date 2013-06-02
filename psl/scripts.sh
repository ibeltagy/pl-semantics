mvn archetype:generate -DarchetypeArtifactId=psl-archetype-groovy -DarchetypeRepository=https://scm.umiacs.umd.edu/maven/lccd/content/repositories/psl-releases/ -DarchetypeGroupId=edu.umd.cs -DarchetypeVersion=1.0.3

cd psl

mvn compile

mvn dependency:build-classpath -Dmdep.outputFile=cp.txt

java -cp ./target/classes:`cat cp.txt` psl.App 

mvn exec:java -Dexec.mainClass=psl.TextInterface


bin/mlnsem run-vid 1-102 -kbest 1 -task sts -peInf true -noDup false -softLogic psl -keepUniv false >tmp/psl3

