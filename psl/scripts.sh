mvn archetype:generate -DarchetypeArtifactId=psl-archetype-groovy -DarchetypeRepository=https://scm.umiacs.umd.edu/maven/lccd/content/repositories/psl-releases/ -DarchetypeGroupId=edu.umd.cs -DarchetypeVersion=1.0.3

cd psl

mvn compile

mvn dependency:build-classpath -Dmdep.outputFile=cp.txt

java -cp ./target/classes:`cat cp.txt` psl.App 
