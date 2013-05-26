package test;

import java.util.Map;

import edu.umd.cs.psl.groovy.*;
import edu.umd.cs.psl.database.RDBMS.DatabaseDriver;
import edu.umd.cs.psl.model.DistanceNorm;
import edu.umd.cs.psl.model.argument.type.ArgumentTypes;
import edu.umd.cs.psl.model.function.AttributeSimilarityFunction;
import edu.umd.cs.psl.config.*;
import edu.umd.cs.psl.model.predicate.type.*;
import edu.umd.cs.psl.ui.ModelUI.PredicateInfo;
import edu.umd.cs.psl.ui.functions.textsimilarity.*;

println "Hellooooooooooooo";

class Sim implements AttributeSimilarityFunction {
	private HashMap sim;
	@Override
	public double similarity(String a, String b) {
		Double score = sim.get(a);
		if (score == null)
			throw new Exception("score for " + a + " not found");
		else return score.value;
	}
	Sim (){
		sim = new HashMap<String, Double>();
		BufferedReader fr =  new BufferedReader(new FileReader("sim/1.txt"));
		String l;
		while((l = fr.readLine()) != null){
			String[] splits = l.split(",");
			sim.put(splits[0], splits[1].toDouble());
		}
	}
}
PSLModel m = new PSLModel(this);
Map<String,PredicateInfo> predicates = new HashMap<String,PredicateInfo>();
m.add function: "sim" , name1: Text, name2: Text, implementation: new Sim();
boolean evdStarted = false;
DataStore data;
BufferedReader fr =  new BufferedReader(new FileReader("sim/1.psl"));
while(( l = fr.readLine()) != null){
	if (l.startsWith("predicate,")) //"predicate,predicateName,argsCount"
	{
		splits = l.split(",");
		arg = new LinkedHashMap();
		for (i = 0;i< Integer.parseInt(splits[2]);i++)
			arg.put("arg"+i, ArgumentTypes.Entity);
		arg.put("open", true)
		pred = m.addPredicate(splits[1], arg);
		predicates.put(splits[1], pred);
	}
	else if (l.startsWith("prior,"))//"prior,predicateName,weight"
	{
		splits = l.split(",");
		arg = new LinkedHashMap();
		arg.put("on", predicates.get(splits[1]))
		arg.put("weight", Double.parseDouble(splits[2]))
		m.addPrior(Prior.Simple, arg)

	}else if (l.startsWith("rule,"))
	{
		m.add rule: (ride_v_dt(X1) & sim("1", "")) >> man_n_dt(X1), constraint: true;
		m.add rule: (((ride_v_dt(X0) & bike_n_dt(X1)) & r_patient_dt(X0,X1)) & sim("2", "")) >> bicycle_n_dt(X1), constraint: true;
		m.add rule: (bicycle_n_dh(X1) & sim("3", "")) >> ride_v_dh(X1), constraint: true;
		m.add rule: (((ride_v_dh(X0) & bicycle_n_dh(X1)) & r_patient_dh(X0,X1)) & sim("4", "")) >> man_n_dh(X1), constraint: true;
		m.add rule: ((bicycle_n_dh(X1) & all(X0)) & sim("5", "")) >> ride_v_dh(X0), constraint: true;
		m.add rule: (bicycle_n_dh(X1) & sim("5", "")) >> bike_n_dh(X1), constraint: true;
		m.add rule: ((bicycle_n_dh(X1) & all(X0)) & sim("5", "")) >> r_patient_dh(X0,X1), constraint: true;
		m.add rule: (((ride_v_dh(X0) & man_n_dh(X1)) & r_agent_dh(X0,X1)) & sim("6", "")) >> man_n_dh(X1), constraint: true;
		m.add rule: (((ride_v_dt(X0) & man_n_dt(X1)) & r_agent_dt(X0,X1)) & sim("7", "")) >> bicycle_n_dt(X1), constraint: true;
		m.add rule: ((bike_n_dt(X1) & all(X0)) & sim("8", "")) >> ride_v_dt(X0), constraint: true;
		m.add rule: (bike_n_dt(X1) & sim("8", "")) >> bicycle_n_dt(X1), constraint: true;
		m.add rule: ((bike_n_dt(X1) & all(X0)) & sim("8", "")) >> r_patient_dt(X0,X1), constraint: true;
		m.add rule: (bike_n_dt(X1) & sim("9", "")) >> man_n_dt(X1), constraint: true;
		m.add rule: ((bike_n_dt(X1) & all(X0)) & sim("10", "")) >> ride_v_dt(X0), constraint: true;
		m.add rule: (bike_n_dt(X1) & sim("10", "")) >> man_n_dt(X1), constraint: true;
		m.add rule: ((bike_n_dt(X1) & all(X0)) & sim("10", "")) >> r_agent_dt(X0,X1), constraint: true;
		m.add rule: (bicycle_n_dh(X1) & sim("11", "")) >> bike_n_dh(X1), constraint: true;
		m.add rule: ((ride_v_dt(X1) & all(X0)) & sim("12", "")) >> ride_v_dt(X0), constraint: true;
		m.add rule: (ride_v_dt(X1) & sim("12", "")) >> bicycle_n_dt(X1), constraint: true;
		m.add rule: ((ride_v_dt(X1) & all(X0)) & sim("12", "")) >> r_patient_dt(X0,X1), constraint: true;
		m.add rule: (man_n_dh(X1) & sim("13", "")) >> bike_n_dh(X1), constraint: true;
		m.add rule: ((ride_v_dh(X1) & all(X0)) & sim("14", "")) >> ride_v_dh(X0), constraint: true;
		m.add rule: (ride_v_dh(X1) & sim("14", "")) >> man_n_dh(X1), constraint: true;
		m.add rule: ((ride_v_dh(X1) & all(X0)) & sim("14", "")) >> r_agent_dh(X0,X1), constraint: true;
		m.add rule: (((ride_v_dh(X0) & bicycle_n_dh(X1)) & r_patient_dh(X0,X1)) & sim("15", "")) >> bike_n_dh(X1), constraint: true;
		m.add rule: (ride_v_dh(X1) & sim("16", "")) >> man_n_dh(X1), constraint: true;
		m.add rule: (((ride_v_dh(X0) & man_n_dh(X1)) & r_agent_dh(X0,X1)) & sim("17", "")) >> ride_v_dh(X1), constraint: true;
		m.add rule: ((ride_v_dh(X1) & all(X0)) & sim("18", "")) >> ride_v_dh(X0), constraint: true;
		m.add rule: (ride_v_dh(X1) & sim("18", "")) >> bike_n_dh(X1), constraint: true;
		m.add rule: ((ride_v_dh(X1) & all(X0)) & sim("18", "")) >> r_patient_dh(X0,X1), constraint: true;
		m.add rule: (man_n_dt(X1) & sim("19", "")) >> bicycle_n_dt(X1), constraint: true;
		m.add rule: (bike_n_dt(X1) & sim("20", "")) >> ride_v_dt(X1), constraint: true;
		m.add rule: (((ride_v_dt(X0) & bike_n_dt(X1)) & r_patient_dt(X0,X1)) & sim("21", "")) >> ride_v_dt(X1), constraint: true;
		m.add rule: (((ride_v_dh(X0) & man_n_dh(X1)) & r_agent_dh(X0,X1)) & sim("22", "")) >> ride_v_dh(X0), constraint: true;
		m.add rule: (((ride_v_dh(X0) & man_n_dh(X1)) & r_agent_dh(X0,X1)) & sim("22", "")) >> bike_n_dh(X1), constraint: true;
		m.add rule: (((ride_v_dh(X0) & man_n_dh(X1)) & r_agent_dh(X0,X1)) & sim("22", "")) >> r_patient_dh(X0,X1), constraint: true;
		m.add rule: (((ride_v_dt(X0) & man_n_dt(X1)) & r_agent_dt(X0,X1)) & sim("23", "")) >> ride_v_dt(X0), constraint: true;
		m.add rule: (((ride_v_dt(X0) & man_n_dt(X1)) & r_agent_dt(X0,X1)) & sim("23", "")) >> bicycle_n_dt(X1), constraint: true;
		m.add rule: (((ride_v_dt(X0) & man_n_dt(X1)) & r_agent_dt(X0,X1)) & sim("23", "")) >> r_patient_dt(X0,X1), constraint: true;
		m.add rule: (bicycle_n_dh(X1) & sim("24", "")) >> man_n_dh(X1), constraint: true;
		m.add rule: (man_n_dt(X1) & sim("25", "")) >> ride_v_dt(X1), constraint: true;
		m.add rule: (((ride_v_dh(X0) & bicycle_n_dh(X1)) & r_patient_dh(X0,X1)) & sim("26", "")) >> ride_v_dh(X0), constraint: true;
		m.add rule: (((ride_v_dh(X0) & bicycle_n_dh(X1)) & r_patient_dh(X0,X1)) & sim("26", "")) >> man_n_dh(X1), constraint: true;
		m.add rule: (((ride_v_dh(X0) & bicycle_n_dh(X1)) & r_patient_dh(X0,X1)) & sim("26", "")) >> r_agent_dh(X0,X1), constraint: true;
		m.add rule: (((ride_v_dh(X0) & man_n_dh(X1)) & r_agent_dh(X0,X1)) & sim("27", "")) >> bike_n_dh(X1), constraint: true;
		m.add rule: ((man_n_dh(X1) & all(X0)) & sim("28", "")) >> ride_v_dh(X0), constraint: true;
		m.add rule: (man_n_dh(X1) & sim("28", "")) >> man_n_dh(X1), constraint: true;
		m.add rule: ((man_n_dh(X1) & all(X0)) & sim("28", "")) >> r_agent_dh(X0,X1), constraint: true;
		m.add rule: (((ride_v_dh(X0) & bicycle_n_dh(X1)) & r_patient_dh(X0,X1)) & sim("29", "")) >> ride_v_dh(X1), constraint: true;
		m.add rule: (((ride_v_dt(X0) & bike_n_dt(X1)) & r_patient_dt(X0,X1)) & sim("30", "")) >> man_n_dt(X1), constraint: true;
		m.add rule: (ride_v_dt(X1) & sim("31", "")) >> bicycle_n_dt(X1), constraint: true;
		m.add rule: (bike_n_dt(X1) & sim("32", "")) >> bicycle_n_dt(X1), constraint: true;
		m.add rule: (((ride_v_dt(X0) & man_n_dt(X1)) & r_agent_dt(X0,X1)) & sim("33", "")) >> man_n_dt(X1), constraint: true;
		m.add rule: ((bicycle_n_dh(X1) & all(X0)) & sim("34", "")) >> ride_v_dh(X0), constraint: true;
		m.add rule: (bicycle_n_dh(X1) & sim("34", "")) >> man_n_dh(X1), constraint: true;
		m.add rule: ((bicycle_n_dh(X1) & all(X0)) & sim("34", "")) >> r_agent_dh(X0,X1), constraint: true;
		m.add rule: (((ride_v_dt(X0) & bike_n_dt(X1)) & r_patient_dt(X0,X1)) & sim("35", "")) >> ride_v_dt(X0), constraint: true;
		m.add rule: (((ride_v_dt(X0) & bike_n_dt(X1)) & r_patient_dt(X0,X1)) & sim("35", "")) >> man_n_dt(X1), constraint: true;
		m.add rule: (((ride_v_dt(X0) & bike_n_dt(X1)) & r_patient_dt(X0,X1)) & sim("35", "")) >> r_agent_dt(X0,X1), constraint: true;
		m.add rule: (ride_v_dh(X1) & sim("36", "")) >> bike_n_dh(X1), constraint: true;
		m.add rule: (((ride_v_dt(X0) & man_n_dt(X1)) & r_agent_dt(X0,X1)) & sim("37", "")) >> ride_v_dt(X1), constraint: true;
		m.add rule: ((man_n_dt(X1) & all(X0)) & sim("38", "")) >> ride_v_dt(X0), constraint: true;
		m.add rule: (man_n_dt(X1) & sim("38", "")) >> bicycle_n_dt(X1), constraint: true;
		m.add rule: ((man_n_dt(X1) & all(X0)) & sim("38", "")) >> r_patient_dt(X0,X1), constraint: true;
		m.add rule: (((ride_v_dh(X0) & bicycle_n_dh(X1)) & r_patient_dh(X0,X1)) & sim("39", "")) >> ride_v_dh(X0), constraint: true;
		m.add rule: (((ride_v_dh(X0) & bicycle_n_dh(X1)) & r_patient_dh(X0,X1)) & sim("39", "")) >> bike_n_dh(X1), constraint: true;
		m.add rule: (((ride_v_dh(X0) & bicycle_n_dh(X1)) & r_patient_dh(X0,X1)) & sim("39", "")) >> r_patient_dh(X0,X1), constraint: true;
		m.add rule: ((ride_v_dt(X1) & all(X0)) & sim("40", "")) >> ride_v_dt(X0), constraint: true;
		m.add rule: (ride_v_dt(X1) & sim("40", "")) >> man_n_dt(X1), constraint: true;
		m.add rule: ((ride_v_dt(X1) & all(X0)) & sim("40", "")) >> r_agent_dt(X0,X1), constraint: true;
		m.add rule: (((ride_v_dt(X0) & bike_n_dt(X1)) & r_patient_dt(X0,X1)) & sim("41", "")) >> ride_v_dt(X0), constraint: true;
		m.add rule: (((ride_v_dt(X0) & bike_n_dt(X1)) & r_patient_dt(X0,X1)) & sim("41", "")) >> bicycle_n_dt(X1), constraint: true;
		m.add rule: (((ride_v_dt(X0) & bike_n_dt(X1)) & r_patient_dt(X0,X1)) & sim("41", "")) >> r_patient_dt(X0,X1), constraint: true;
		m.add rule: ((man_n_dh(X1) & all(X0)) & sim("42", "")) >> ride_v_dh(X0), constraint: true;
		m.add rule: (man_n_dh(X1) & sim("42", "")) >> bike_n_dh(X1), constraint: true;
		m.add rule: ((man_n_dh(X1) & all(X0)) & sim("42", "")) >> r_patient_dh(X0,X1), constraint: true;
		m.add rule: ((man_n_dt(X1) & all(X0)) & sim("43", "")) >> ride_v_dt(X0), constraint: true;
		m.add rule: (man_n_dt(X1) & sim("43", "")) >> man_n_dt(X1), constraint: true;
		m.add rule: ((man_n_dt(X1) & all(X0)) & sim("43", "")) >> r_agent_dt(X0,X1), constraint: true;
		m.add rule: (man_n_dh(X1) & sim("44", "")) >> ride_v_dh(X1), constraint: true;
		m.add rule: ((((((r_patient_dt(TX0,TX1) & r_agent_dt(TX0,TX2)) & ride_v_dt(TX0)) & bicycle_n_dt(TX1)) & man_n_dt(TX2)) & ((((r_patient_dh(HX0,HX1) & r_agent_dh(HX0,HX2)) & ride_v_dh(HX0)) & bike_n_dh(HX1)) & man_n_dh(HX2))) >> entailment_h()), constraint: true;
		
	}
	else if (l.startsWith("data,"))//data,predName,val1,val2
								   //data,predName,val1
	{
		if (!evdStarted)
		{
			data = new RelationalDataStore(m);
			data.setup db : DatabaseDriver.H2;
			evdStarted = true;			
		}
		splits = l.split(",");
		pred = predicates.get(splits[1])
		if (splits.length == 3) //one arg
			data.getInserter(pred).insert(Integer.parseInt(splits[2]));
		else //two args
			data.getInserter(pred).insert(Integer.parseInt(splits[2]), Integer.parseInt(splits[3]));
	}
	else if (l.startsWith("query,"))
	{
		println m
		splits = l.split(",");
		pred = predicates.get(splits[1])
		def result = m.mapInference(data.getDatabase());
		result.printAtoms(pred, false);
	}
	
}


