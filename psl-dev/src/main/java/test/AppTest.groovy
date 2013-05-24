package testNew;

import edu.umd.cs.psl.groovy.*;
import edu.umd.cs.psl.database.RDBMS.DatabaseDriver;
import edu.umd.cs.psl.model.DistanceNorm;
import edu.umd.cs.psl.model.function.AttributeSimilarityFunction;
import edu.umd.cs.psl.config.*;
import edu.umd.cs.psl.model.predicate.type.*; 
import edu.umd.cs.psl.ui.functions.textsimilarity.*;
println "Hellooooooooooooo"
PSLModel m = new PSLModel(this);
m.add function: "sim" , name1: Text, name2: Text, implementation: new Sim224()
m.add predicate: "all", arg1: Entity
m.add predicate: "man_n_dt", arg0: Entity,  open: true
m.add predicate: "group_topic_a_dh", arg0: Entity,  open: true
m.add predicate: "man_n_dh", arg0: Entity,  open: true
m.add predicate: "group_n_dh", arg0: Entity,  open: true
m.add predicate: "people_n_dt", arg0: Entity,  open: true
m.add predicate: "entailment_h",  open: true
m.add predicate: "dance_v_dt", arg0: Entity,  open: true
m.add predicate: "entailment_t",  open: true
m.add predicate: "r_of_dh", arg0: Entity, arg1: Entity,  open: true
m.add predicate: "group_topic_a_dt", arg0: Entity,  open: true
m.add predicate: "dance_v_dh", arg0: Entity,  open: true
m.add predicate: "r_agent_dt", arg0: Entity, arg1: Entity,  open: true
m.add predicate: "people_n_dh", arg0: Entity,  open: true
m.add predicate: "r_of_dt", arg0: Entity, arg1: Entity,  open: true
m.add predicate: "r_agent_dh", arg0: Entity, arg1: Entity,  open: true
m.add predicate: "group_n_dt", arg0: Entity,  open: true
m.add Prior.Simple, on: man_n_dt, weight: 0.1
m.add Prior.Simple, on: group_topic_a_dh, weight: 0.1
m.add Prior.Simple, on: man_n_dh, weight: 0.1
m.add Prior.Simple, on: group_n_dh, weight: 0.1
m.add Prior.Simple, on: people_n_dt, weight: 0.1
m.add Prior.Simple, on: entailment_h, weight: 0.01
m.add Prior.Simple, on: dance_v_dt, weight: 0.1
m.add Prior.Simple, on: entailment_t, weight: 0.01
m.add Prior.Simple, on: r_of_dh, weight: 0.1
m.add Prior.Simple, on: group_topic_a_dt, weight: 0.1
m.add Prior.Simple, on: dance_v_dh, weight: 0.1
m.add Prior.Simple, on: r_agent_dt, weight: 0.1
m.add Prior.Simple, on: people_n_dh, weight: 0.1
m.add Prior.Simple, on: r_of_dt, weight: 0.1
m.add Prior.Simple, on: r_agent_dh, weight: 0.1
m.add Prior.Simple, on: group_n_dt, weight: 0.1
m.add rule: (dance_v_dt(X1) & sim("dance_v_dt", "group_topic_a_dt")) >> group_topic_a_dt(X1), constraint: true
m.add rule: (((dance_v_dt(X0) & group_n_dt(X1)) & r_agent_dt(X0,X1)) & sim("dance_v_dt&group_n_dt", "dance_v_dt")) >> dance_v_dt(X1), constraint: true
m.add rule: (dance_v_dh(X1) & sim("dance_v_dh", "group_n_dh")) >> group_n_dh(X1), constraint: true
m.add rule: (man_n_dt(X1) & sim("man_n_dt", "group_topic_a_dt")) >> group_topic_a_dt(X1), constraint: true
m.add rule: ((group_n_dt(X1) & all(X0)) & sim("group_n_dt", "group_topic_a_dt&people_n_dt")) >> group_topic_a_dt(X0), constraint: true
m.add rule: (group_n_dt(X1) & sim("group_n_dt", "group_topic_a_dt&people_n_dt")) >> people_n_dt(X1), constraint: true
m.add rule: ((group_n_dt(X1) & all(X0)) & sim("group_n_dt", "group_topic_a_dt&people_n_dt")) >> r_of_dt(X0,X1), constraint: true
m.add rule: (((dance_v_dt(X0) & group_n_dt(X1)) & r_agent_dt(X0,X1)) & sim("dance_v_dt&group_n_dt", "people_n_dt")) >> people_n_dt(X1), constraint: true
m.add rule: ((dance_v_dt(X1) & all(X0)) & sim("dance_v_dt", "dance_v_dt&people_n_dt")) >> dance_v_dt(X0), constraint: true
m.add rule: (dance_v_dt(X1) & sim("dance_v_dt", "dance_v_dt&people_n_dt")) >> people_n_dt(X1), constraint: true
m.add rule: ((dance_v_dt(X1) & all(X0)) & sim("dance_v_dt", "dance_v_dt&people_n_dt")) >> r_agent_dt(X0,X1), constraint: true
m.add rule: (((group_topic_a_dh(X0) & people_n_dh(X1)) & r_of_dh(X0,X1)) & sim("group_topic_a_dh&people_n_dh", "dance_v_dh&group_n_dh")) >> dance_v_dh(X0), constraint: true
m.add rule: (((group_topic_a_dh(X0) & people_n_dh(X1)) & r_of_dh(X0,X1)) & sim("group_topic_a_dh&people_n_dh", "dance_v_dh&group_n_dh")) >> group_n_dh(X1), constraint: true
m.add rule: (((group_topic_a_dh(X0) & people_n_dh(X1)) & r_of_dh(X0,X1)) & sim("group_topic_a_dh&people_n_dh", "dance_v_dh&group_n_dh")) >> r_agent_dh(X0,X1), constraint: true
m.add rule: (man_n_dt(X1) & sim("man_n_dt", "dance_v_dt")) >> dance_v_dt(X1), constraint: true
m.add rule: (((group_topic_a_dh(X0) & people_n_dh(X1)) & r_of_dh(X0,X1)) & sim("group_topic_a_dh&people_n_dh", "man_n_dh")) >> man_n_dh(X1), constraint: true
m.add rule: (((group_n_dt(X0) & man_n_dt(X1)) & r_of_dt(X0,X1)) & sim("group_n_dt&man_n_dt", "dance_v_dt")) >> dance_v_dt(X1), constraint: true
m.add rule: (dance_v_dh(X1) & sim("dance_v_dh", "man_n_dh")) >> man_n_dh(X1), constraint: true
m.add rule: (((dance_v_dh(X0) & people_n_dh(X1)) & r_agent_dh(X0,X1)) & sim("dance_v_dh&people_n_dh", "dance_v_dh&group_n_dh")) >> dance_v_dh(X0), constraint: true
m.add rule: (((dance_v_dh(X0) & people_n_dh(X1)) & r_agent_dh(X0,X1)) & sim("dance_v_dh&people_n_dh", "dance_v_dh&group_n_dh")) >> group_n_dh(X1), constraint: true
m.add rule: (((dance_v_dh(X0) & people_n_dh(X1)) & r_agent_dh(X0,X1)) & sim("dance_v_dh&people_n_dh", "dance_v_dh&group_n_dh")) >> r_agent_dh(X0,X1), constraint: true
m.add rule: (group_topic_a_dh(X1) & sim("group_topic_a_dh", "group_n_dh")) >> group_n_dh(X1), constraint: true
m.add rule: ((group_n_dt(X1) & all(X0)) & sim("group_n_dt", "dance_v_dt&people_n_dt")) >> dance_v_dt(X0), constraint: true
m.add rule: (group_n_dt(X1) & sim("group_n_dt", "dance_v_dt&people_n_dt")) >> people_n_dt(X1), constraint: true
m.add rule: ((group_n_dt(X1) & all(X0)) & sim("group_n_dt", "dance_v_dt&people_n_dt")) >> r_agent_dt(X0,X1), constraint: true
m.add rule: (group_n_dt(X1) & sim("group_n_dt", "group_topic_a_dt")) >> group_topic_a_dt(X1), constraint: true
m.add rule: (((group_topic_a_dh(X0) & people_n_dh(X1)) & r_of_dh(X0,X1)) & sim("group_topic_a_dh&people_n_dh", "group_n_dh")) >> group_n_dh(X1), constraint: true
m.add rule: (((group_n_dt(X0) & man_n_dt(X1)) & r_of_dt(X0,X1)) & sim("group_n_dt&man_n_dt", "dance_v_dt&people_n_dt")) >> dance_v_dt(X0), constraint: true
m.add rule: (((group_n_dt(X0) & man_n_dt(X1)) & r_of_dt(X0,X1)) & sim("group_n_dt&man_n_dt", "dance_v_dt&people_n_dt")) >> people_n_dt(X1), constraint: true
m.add rule: (((group_n_dt(X0) & man_n_dt(X1)) & r_of_dt(X0,X1)) & sim("group_n_dt&man_n_dt", "dance_v_dt&people_n_dt")) >> r_agent_dt(X0,X1), constraint: true
m.add rule: ((man_n_dt(X1) & all(X0)) & sim("man_n_dt", "group_topic_a_dt&people_n_dt")) >> group_topic_a_dt(X0), constraint: true
m.add rule: (man_n_dt(X1) & sim("man_n_dt", "group_topic_a_dt&people_n_dt")) >> people_n_dt(X1), constraint: true
m.add rule: ((man_n_dt(X1) & all(X0)) & sim("man_n_dt", "group_topic_a_dt&people_n_dt")) >> r_of_dt(X0,X1), constraint: true
m.add rule: ((people_n_dh(X1) & all(X0)) & sim("people_n_dh", "group_n_dh&man_n_dh")) >> group_n_dh(X0), constraint: true
m.add rule: (people_n_dh(X1) & sim("people_n_dh", "group_n_dh&man_n_dh")) >> man_n_dh(X1), constraint: true
m.add rule: ((people_n_dh(X1) & all(X0)) & sim("people_n_dh", "group_n_dh&man_n_dh")) >> r_of_dh(X0,X1), constraint: true
m.add rule: ((dance_v_dh(X1) & all(X0)) & sim("dance_v_dh", "dance_v_dh&group_n_dh")) >> dance_v_dh(X0), constraint: true
m.add rule: (dance_v_dh(X1) & sim("dance_v_dh", "dance_v_dh&group_n_dh")) >> group_n_dh(X1), constraint: true
m.add rule: ((dance_v_dh(X1) & all(X0)) & sim("dance_v_dh", "dance_v_dh&group_n_dh")) >> r_agent_dh(X0,X1), constraint: true
m.add rule: (((dance_v_dh(X0) & people_n_dh(X1)) & r_agent_dh(X0,X1)) & sim("dance_v_dh&people_n_dh", "dance_v_dh")) >> dance_v_dh(X1), constraint: true
m.add rule: (((dance_v_dt(X0) & group_n_dt(X1)) & r_agent_dt(X0,X1)) & sim("dance_v_dt&group_n_dt", "group_topic_a_dt&people_n_dt")) >> group_topic_a_dt(X0), constraint: true
m.add rule: (((dance_v_dt(X0) & group_n_dt(X1)) & r_agent_dt(X0,X1)) & sim("dance_v_dt&group_n_dt", "group_topic_a_dt&people_n_dt")) >> people_n_dt(X1), constraint: true
m.add rule: (((dance_v_dt(X0) & group_n_dt(X1)) & r_agent_dt(X0,X1)) & sim("dance_v_dt&group_n_dt", "group_topic_a_dt&people_n_dt")) >> r_of_dt(X0,X1), constraint: true
m.add rule: (((dance_v_dh(X0) & people_n_dh(X1)) & r_agent_dh(X0,X1)) & sim("dance_v_dh&people_n_dh", "man_n_dh")) >> man_n_dh(X1), constraint: true
m.add rule: (((dance_v_dh(X0) & people_n_dh(X1)) & r_agent_dh(X0,X1)) & sim("dance_v_dh&people_n_dh", "group_n_dh&man_n_dh")) >> group_n_dh(X0), constraint: true
m.add rule: (((dance_v_dh(X0) & people_n_dh(X1)) & r_agent_dh(X0,X1)) & sim("dance_v_dh&people_n_dh", "group_n_dh&man_n_dh")) >> man_n_dh(X1), constraint: true
m.add rule: (((dance_v_dh(X0) & people_n_dh(X1)) & r_agent_dh(X0,X1)) & sim("dance_v_dh&people_n_dh", "group_n_dh&man_n_dh")) >> r_of_dh(X0,X1), constraint: true
m.add rule: (((group_n_dt(X0) & man_n_dt(X1)) & r_of_dt(X0,X1)) & sim("group_n_dt&man_n_dt", "group_topic_a_dt&people_n_dt")) >> group_topic_a_dt(X0), constraint: true
m.add rule: (((group_n_dt(X0) & man_n_dt(X1)) & r_of_dt(X0,X1)) & sim("group_n_dt&man_n_dt", "group_topic_a_dt&people_n_dt")) >> people_n_dt(X1), constraint: true
m.add rule: (((group_n_dt(X0) & man_n_dt(X1)) & r_of_dt(X0,X1)) & sim("group_n_dt&man_n_dt", "group_topic_a_dt&people_n_dt")) >> r_of_dt(X0,X1), constraint: true
m.add rule: ((group_topic_a_dh(X1) & all(X0)) & sim("group_topic_a_dh", "dance_v_dh&group_n_dh")) >> dance_v_dh(X0), constraint: true
m.add rule: (group_topic_a_dh(X1) & sim("group_topic_a_dh", "dance_v_dh&group_n_dh")) >> group_n_dh(X1), constraint: true
m.add rule: ((group_topic_a_dh(X1) & all(X0)) & sim("group_topic_a_dh", "dance_v_dh&group_n_dh")) >> r_agent_dh(X0,X1), constraint: true
m.add rule: (group_n_dt(X1) & sim("group_n_dt", "dance_v_dt")) >> dance_v_dt(X1), constraint: true
m.add rule: (((group_topic_a_dh(X0) & people_n_dh(X1)) & r_of_dh(X0,X1)) & sim("group_topic_a_dh&people_n_dh", "dance_v_dh")) >> dance_v_dh(X1), constraint: true
m.add rule: (((dance_v_dh(X0) & people_n_dh(X1)) & r_agent_dh(X0,X1)) & sim("dance_v_dh&people_n_dh", "group_n_dh")) >> group_n_dh(X1), constraint: true
m.add rule: (people_n_dh(X1) & sim("people_n_dh", "group_n_dh")) >> group_n_dh(X1), constraint: true
m.add rule: (dance_v_dt(X1) & sim("dance_v_dt", "people_n_dt")) >> people_n_dt(X1), constraint: true
m.add rule: (people_n_dh(X1) & sim("people_n_dh", "man_n_dh")) >> man_n_dh(X1), constraint: true
m.add rule: ((dance_v_dh(X1) & all(X0)) & sim("dance_v_dh", "group_n_dh&man_n_dh")) >> group_n_dh(X0), constraint: true
m.add rule: (dance_v_dh(X1) & sim("dance_v_dh", "group_n_dh&man_n_dh")) >> man_n_dh(X1), constraint: true
m.add rule: ((dance_v_dh(X1) & all(X0)) & sim("dance_v_dh", "group_n_dh&man_n_dh")) >> r_of_dh(X0,X1), constraint: true
m.add rule: ((people_n_dh(X1) & all(X0)) & sim("people_n_dh", "dance_v_dh&group_n_dh")) >> dance_v_dh(X0), constraint: true
m.add rule: (people_n_dh(X1) & sim("people_n_dh", "dance_v_dh&group_n_dh")) >> group_n_dh(X1), constraint: true
m.add rule: ((people_n_dh(X1) & all(X0)) & sim("people_n_dh", "dance_v_dh&group_n_dh")) >> r_agent_dh(X0,X1), constraint: true
m.add rule: (((dance_v_dt(X0) & group_n_dt(X1)) & r_agent_dt(X0,X1)) & sim("dance_v_dt&group_n_dt", "dance_v_dt&people_n_dt")) >> dance_v_dt(X0), constraint: true
m.add rule: (((dance_v_dt(X0) & group_n_dt(X1)) & r_agent_dt(X0,X1)) & sim("dance_v_dt&group_n_dt", "dance_v_dt&people_n_dt")) >> people_n_dt(X1), constraint: true
m.add rule: (((dance_v_dt(X0) & group_n_dt(X1)) & r_agent_dt(X0,X1)) & sim("dance_v_dt&group_n_dt", "dance_v_dt&people_n_dt")) >> r_agent_dt(X0,X1), constraint: true
m.add rule: (man_n_dt(X1) & sim("man_n_dt", "people_n_dt")) >> people_n_dt(X1), constraint: true
m.add rule: ((group_topic_a_dh(X1) & all(X0)) & sim("group_topic_a_dh", "group_n_dh&man_n_dh")) >> group_n_dh(X0), constraint: true
m.add rule: (group_topic_a_dh(X1) & sim("group_topic_a_dh", "group_n_dh&man_n_dh")) >> man_n_dh(X1), constraint: true
m.add rule: ((group_topic_a_dh(X1) & all(X0)) & sim("group_topic_a_dh", "group_n_dh&man_n_dh")) >> r_of_dh(X0,X1), constraint: true
m.add rule: (((group_n_dt(X0) & man_n_dt(X1)) & r_of_dt(X0,X1)) & sim("group_n_dt&man_n_dt", "people_n_dt")) >> people_n_dt(X1), constraint: true
m.add rule: (people_n_dh(X1) & sim("people_n_dh", "dance_v_dh")) >> dance_v_dh(X1), constraint: true
m.add rule: (group_topic_a_dh(X1) & sim("group_topic_a_dh", "dance_v_dh")) >> dance_v_dh(X1), constraint: true
m.add rule: (group_n_dt(X1) & sim("group_n_dt", "people_n_dt")) >> people_n_dt(X1), constraint: true
m.add rule: ((dance_v_dt(X1) & all(X0)) & sim("dance_v_dt", "group_topic_a_dt&people_n_dt")) >> group_topic_a_dt(X0), constraint: true
m.add rule: (dance_v_dt(X1) & sim("dance_v_dt", "group_topic_a_dt&people_n_dt")) >> people_n_dt(X1), constraint: true
m.add rule: ((dance_v_dt(X1) & all(X0)) & sim("dance_v_dt", "group_topic_a_dt&people_n_dt")) >> r_of_dt(X0,X1), constraint: true
m.add rule: (((dance_v_dt(X0) & group_n_dt(X1)) & r_agent_dt(X0,X1)) & sim("dance_v_dt&group_n_dt", "group_topic_a_dt")) >> group_topic_a_dt(X1), constraint: true
m.add rule: (((group_topic_a_dh(X0) & people_n_dh(X1)) & r_of_dh(X0,X1)) & sim("group_topic_a_dh&people_n_dh", "group_n_dh&man_n_dh")) >> group_n_dh(X0), constraint: true
m.add rule: (((group_topic_a_dh(X0) & people_n_dh(X1)) & r_of_dh(X0,X1)) & sim("group_topic_a_dh&people_n_dh", "group_n_dh&man_n_dh")) >> man_n_dh(X1), constraint: true
m.add rule: (((group_topic_a_dh(X0) & people_n_dh(X1)) & r_of_dh(X0,X1)) & sim("group_topic_a_dh&people_n_dh", "group_n_dh&man_n_dh")) >> r_of_dh(X0,X1), constraint: true
m.add rule: (((group_n_dt(X0) & man_n_dt(X1)) & r_of_dt(X0,X1)) & sim("group_n_dt&man_n_dt", "group_topic_a_dt")) >> group_topic_a_dt(X1), constraint: true
m.add rule: (group_topic_a_dh(X1) & sim("group_topic_a_dh", "man_n_dh")) >> man_n_dh(X1), constraint: true
m.add rule: ((man_n_dt(X1) & all(X0)) & sim("man_n_dt", "dance_v_dt&people_n_dt")) >> dance_v_dt(X0), constraint: true
m.add rule: (man_n_dt(X1) & sim("man_n_dt", "dance_v_dt&people_n_dt")) >> people_n_dt(X1), constraint: true
m.add rule: ((man_n_dt(X1) & all(X0)) & sim("man_n_dt", "dance_v_dt&people_n_dt")) >> r_agent_dt(X0,X1), constraint: true
m.add rule: ((((((group_topic_a_dt(TX2) & r_of_dt(TX2,TX1)) & r_agent_dt(TX0,TX1)) & dance_v_dt(TX0)) & people_n_dt(TX1)) & ((((r_agent_dh(HX0,HX2) & dance_v_dh(HX0)) & r_of_dh(HX2,HX1)) & man_n_dh(HX1)) & group_n_dh(HX2))) >> entailment_h()), constraint: true
class Sim224 implements AttributeSimilarityFunction {
private HashMap sim;
@Override
public double similarity(String a, String b) {
String q = a + "#" + b;
Double score = sim.get(q);
if (score == null)
throw new Exception("score for " + q + " not found")
else return score.value; }	
Sim224 (){
sim = new HashMap<String, Double>();

sim.put("man_n_dt#dance_v_dt&people_n_dt", 0.2352643291749679)
sim.put("group_topic_a_dh#man_n_dh", 0.39778154517496334)
sim.put("group_n_dt&man_n_dt#group_topic_a_dt", 0.8661437533797228)
sim.put("group_topic_a_dh&people_n_dh#group_n_dh&man_n_dh", 0.9076679252869165)
sim.put("dance_v_dt&group_n_dt#group_topic_a_dt", 0.3282968392890522)
sim.put("dance_v_dt#group_topic_a_dt&people_n_dt", 0.13497692142292253)
sim.put("group_n_dt#people_n_dt", 0.5416805510062191)
sim.put("group_topic_a_dh#dance_v_dh", 0.10843834230417412)
sim.put("people_n_dh#dance_v_dh", 0.13979672025317913)
sim.put("group_n_dt&man_n_dt#people_n_dt", 0.6997807176120279)
sim.put("group_topic_a_dh#group_n_dh&man_n_dh", 0.8661437533797228)
sim.put("man_n_dt#people_n_dt", 0.6387997253737102)
sim.put("dance_v_dt&group_n_dt#dance_v_dt&people_n_dt", 0.9819041883073749)
sim.put("people_n_dh#dance_v_dh&group_n_dh", 0.2548513017877567)
sim.put("dance_v_dh#group_n_dh&man_n_dh", 0.15511394628319228)
sim.put("people_n_dh#man_n_dh", 0.6387997253737102)
sim.put("dance_v_dt#people_n_dt", 0.13979672025317913)
sim.put("people_n_dh#group_n_dh", 0.5416805510062191)
sim.put("dance_v_dh&people_n_dh#group_n_dh", 0.17673568261943656)
sim.put("group_topic_a_dh&people_n_dh#dance_v_dh", 0.13497692142292253)
sim.put("group_n_dt#dance_v_dt", 0.10843834230417412)
sim.put("group_topic_a_dh#dance_v_dh&group_n_dh", 0.3282968392890522)
sim.put("group_n_dt&man_n_dt#group_topic_a_dt&people_n_dt", 0.9076679252869165)
sim.put("dance_v_dh&people_n_dh#group_n_dh&man_n_dh", 0.24294120685880732)
sim.put("dance_v_dh&people_n_dh#man_n_dh", 0.2352643291749679)
sim.put("dance_v_dt&group_n_dt#group_topic_a_dt&people_n_dt", 0.3397878354073589)
sim.put("dance_v_dh&people_n_dh#dance_v_dh", 0.9914909277003467)
sim.put("dance_v_dh#dance_v_dh&group_n_dh", 0.9746045809682684)
sim.put("people_n_dh#group_n_dh&man_n_dh", 0.6997807176120279)
sim.put("man_n_dt#group_topic_a_dt&people_n_dt", 0.5466504536378641)
sim.put("group_n_dt&man_n_dt#dance_v_dt&people_n_dt", 0.24294120685880732)
sim.put("group_topic_a_dh&people_n_dh#group_n_dh", 0.9390690843608993)
sim.put("group_n_dt#group_topic_a_dt", 1.0)
sim.put("group_n_dt#dance_v_dt&people_n_dt", 0.17673568261943656)
sim.put("group_topic_a_dh#group_n_dh", 1.0)
sim.put("dance_v_dh&people_n_dh#dance_v_dh&group_n_dh", 0.9819041883073749)
sim.put("dance_v_dh#man_n_dh", 0.15546344035452334)
sim.put("group_n_dt&man_n_dt#dance_v_dt", 0.15511394628319228)
sim.put("group_topic_a_dh&people_n_dh#man_n_dh", 0.5466504536378641)
sim.put("man_n_dt#dance_v_dt", 0.15546344035452334)
sim.put("group_topic_a_dh&people_n_dh#dance_v_dh&group_n_dh", 0.3397878354073589)
sim.put("dance_v_dt#dance_v_dt&people_n_dt", 0.9914909277003467)
sim.put("dance_v_dt&group_n_dt#people_n_dt", 0.2548513017877567)
sim.put("group_n_dt#group_topic_a_dt&people_n_dt", 0.9390690843608993)
sim.put("man_n_dt#group_topic_a_dt", 0.39778154517496334)
sim.put("dance_v_dh#group_n_dh", 0.10843834230417412)
sim.put("dance_v_dt&group_n_dt#dance_v_dt", 0.9746045809682684)
sim.put("dance_v_dt#group_topic_a_dt", 0.10843834230417412)
}}
println m;
DataStore data = new RelationalDataStore(m)
data.setup db : DatabaseDriver.H2
def insert = data.getInserter(r_agent_dt)
insert.insert(1000, 1002)
insert = data.getInserter(dance_v_dt)
insert.insert(1000)
insert = data.getInserter(r_of_dt)
insert.insert(1002, 1001)
insert = data.getInserter(man_n_dt)
insert.insert(1001)
insert = data.getInserter(group_n_dt)
insert.insert(1002)
insert = data.getInserter(group_topic_a_dh)
insert.insert(2002)
insert = data.getInserter(r_of_dh)
insert.insert(2002, 2001)
insert = data.getInserter(r_agent_dh)
insert.insert(2000, 2001)
insert = data.getInserter(dance_v_dh)
insert.insert(2000)
insert = data.getInserter(people_n_dh)
insert.insert(2001)
ConfigManager cm = ConfigManager.getManager();
ConfigBundle exampleBundle = cm.getBundle("example");
def result = m.mapInference(data.getDatabase(), exampleBundle);
result.printAtoms(entailment_h, false)