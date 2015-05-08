#!/bin/bash


# git clone http://ibeltagy@www.deftware.bbn.com:7990/scm/utaustin/deft.git
# password: spUsr$#2013!
# cd deft
# git remote add adept http://ibeltagy@www.deftware.bbn.com:7990/scm/adept/deft.git
# git remote -v
# git fetch adept
# git pull adept tag v1.10
# pre-commit hook https://www.deftdocs.bbn.com/SitePages/ADEPT%20Software%20Getting%20Started%20Guide.aspx
# http://www.deftware.bbn.com:7990/projects/ADEPT/repos/deft/browse/utaustin
# https://www.deftdocs.bbn.com/SitePages/Software%20Development.aspx

# mvn compile  -pl utaustin/DPLS -am -X
# mvn test  -DskipUnitTests=false  -pl utaustin/DPLS -am -X

# update default configurations
# run PSL make sure it does not break
# copy mln-semantics to adept

# dependencies from Boxer (SWI-Prolog) and for Alchemy (Bison 2.3, Flex 2.5.4, Perl 5.8.8)
# remove parallelColt
# add regression test and benchmark test

# adept/adept-api/src/main/java/adept/common/HltContentContainer.java
#     public void setMessages(List<Message> messages) {  }

# pom.xml  (outer most pom.xml)
#   <requireMavenVersion>
#      <version>3.0.4</version>
#   </requireMavenVersion>    


cd ~/workspace
cp deft/mln-semantics/src/main/scala/ adept/deft/utaustin/DPLS/src/main/ -r
cp deft/mln-semantics/scala-logic/src/main/scala/ adept/deft/utaustin/DPLS/src/main/ -r
rm adept/deft/utaustin/DPLS/src/main/scala/PairwiseWordSimilarity.java
cp deft/mln-semantics/psl/src/main/java/edu/ adept/deft/utaustin/DPLS/src/main/java/ -r


copy alchemy
copy candc
get the updated full.vs  (from stephen)
revise all Configurations 
