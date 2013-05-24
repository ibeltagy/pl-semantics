package testNew;

//Imports the standard groovy interface to PSL, the attribute similarity function interface and the database drivers
import edu.umd.cs.psl.groovy.*;
import edu.umd.cs.psl.database.RDBMS.DatabaseDriver;
import edu.umd.cs.psl.model.DistanceNorm;
import edu.umd.cs.psl.model.function.AttributeSimilarityFunction;
import edu.umd.cs.psl.config.*;
import edu.umd.cs.psl.model.predicate.type.*;

//Import this package if you want to use attribute similarity functions that come with PSL
import edu.umd.cs.psl.ui.functions.textsimilarity.*;

/* The first thing we need to do, is initialize a PSLModel which is the core component of PSL.
 * The constructor argument is the context in which the PSLModel is defined. Predicates defined
 * in the PSLModel are also automatically defined in the context.
 */

println "This source file is a place holder for the tree of groovy and java sources for your PSL project."

PSLModel m = new PSLModel(this);

/* We create two predicates in the model, giving their names and list of arguments. Each argument has a specified type which
 * is either Entity or Attribute.
 */
m.add predicate: "man" , arg: Entity, open: true
m.add predicate: "ride" , arg: Entity, open: true
m.add predicate: "drive" , arg: Entity,  open: true
m.add predicate: "bike" , arg3: Entity,  open: true
m.add predicate: "bicycle" , arg3: Entity, open: true
m.add predicate: "agent" , arg1: Entity, arg2: Entity, open: true
m.add predicate: "patient" , arg1: Entity, arg2: Entity, open: true
m.add predicate: "ent", arg1: Entity, arg2: Entity, arg3: Entity, open: true
m.add predicate: "all", arg1: Entity

/*m.add function: "bicycleSim" , name1: Text, name2: Text, implementation: new Sim("bicycle")//, see end of file
m.add function: "driveSim" , name1: Text, name2: Text, implementation: new Sim("drive")//, see end of file
m.add function: "manDriveSim" , name1: Text, name2: Text, implementation: new Sim("manDrive")//, see end of file
m.add function: "manSim" , name1: Text, name2: Text, implementation: new Sim("man")//, see end of file
*/
m.add function: "sim" , name1: Text, name2: Text, implementation: new Sim()

class Sim implements AttributeSimilarityFunction {
	private HashMap sim //= new HashMap<String, Double>();
	 
	Sim (){
		if (sim == null)
		{
			sim = new HashMap<String, Double>();

			BufferedReader fr =  new BufferedReader(new FileReader("sim/template.txt"))
			String l;
			while((l = fr.readLine()) != null)
			{
				String[] splits = l.split(",");
				sim.put(splits[0], splits[1].toDouble());
			}
			/*
			sim.put("bike#bicycle", 0.912)
			sim.put("ride#bicycle", 0.0)
			sim.put("man#bicycle", 0.0)
			sim.put("man&ride#bicycle", 0.0)
			sim.put("ride&bike#bicycle", 0.0)
			
			sim.put("bike#drive", 0.0)
			sim.put("ride#drive", 0.998)
			sim.put("man#drive", 0.0)
			sim.put("man&ride#drive", 0.0)
			sim.put("ride&bike#drive", 0.0)
			
			sim.put("bike#man", 0.0)
			sim.put("ride#man", 0.0)
			sim.put("man#man", 1.0)
			sim.put("man&ride#man", 0.0)
			sim.put("ride&bike#man", 0.0)
			
			sim.put("bike#man&drive", 0.0)
			sim.put("ride#man&drive", 0.0)
			sim.put("man#man&drive", 0.0)
			sim.put("man&ride#man&drive", 0.0)
			sim.put("ride&bike#man&drive", 0.0)
			*/
		}
	}
	@Override
	public double similarity(String a, String b) {
		String q = a + "#" + b;
		Double score = sim.get(q);
		if (score == null)
			throw new Exception("score for " + q + " not found")
		else return score.value; 

	}
}

m.add rule : ( bike(X) & sim("bike", "bicycle") ) >> bicycle(X), constraint: true

m.add rule : ( bike(X) & sim("bike", "bicycle") ) >> bicycle(X), constraint: true
m.add rule : (ride(X) & sim("ride", "bicycle")) >> bicycle(X), constraint: true
m.add rule : (man(X) & sim("man", "bicycle")) >> bicycle(X), constraint: true

m.add rule : (bike(X) & sim("bike", "drive")) >> drive(X), constraint: true
m.add rule : (ride(X) & sim("ride", "drive")) >> drive(X), constraint: true
m.add rule : (man(X) & sim("man", "drive")) >> drive(X), constraint: true

m.add rule : (bike(X) & sim("bike", "man")) >> man(X), constraint: true
m.add rule : (ride(X) & sim("ride", "man")) >> man(X), constraint: true
m.add rule : (man(X) & sim("man", "man")) >> man(X), constraint: true

m.add rule : (man(X) & sim("man", "man&drive") & all(Y)) >> man(X), constraint: true
m.add rule : (man(X) & sim("man", "man&drive") & all(Y)) >> agent(Y, X), constraint: true
m.add rule : (man(X) & sim("man", "man&drive") & all(Y)) >> drive(Y), constraint: true

m.add rule : (man(X) & agent (Y, X) & ride(Y) & sim("man&ride", "man")) >> man(X), constraint: true
m.add rule : (ride(X) & patient (X, Y) & bike(Y) & sim("ride&bike", "man")) >> man(X), constraint: true

m.add rule : (man(X) & (agent (Y, X) & (ride(Y) & (sim("man&ride", "drive"))))) >> drive(X), constraint: true
m.add rule : (ride(X) & patient(X, Y) & bike(Y) & sim("ride&bike", "drive")) >> drive(X), constraint: true

m.add rule : (man(X) & agent (Y, X) & ride(Y) & sim("man&ride", "bicycle")) >> bicycle(X), constraint: true
m.add rule : (ride(X) & patient(X, Y) & bike(Y) & sim("ride&bike", "bicycle")) >> bicycle(X), constraint: true

m.add rule : ( man(X) & agent(Y, X) & drive(Y) & patient(Y, Z) & bicycle(Z) & all(Z) ) >> ent(X, Y, Z), constraint: true

//m.add rule : (  ~man_q(X)& agent(Y, X)) >> ent(X, X, X), weight: 1

//m.add rule : ( man_q(X) & agent(Y, X) & drive(Y)  ) >> ent(X, Y, Y), weight: 1
m.add Prior.Simple, on : ent, weight: 0.01
m.add Prior.Simple, on : man, weight: 0.1
m.add Prior.Simple, on : bicycle, weight: 0.1
m.add Prior.Simple, on : patient, weight: 0.1



//m.add rule : ( man_q(X) ) >> ent(X, X, X), constraint: true
//m.add rule : (man_evd(X, Z) & manSim(Z, Z)) >> man_q(X), constraint: true




//Let's see what our model looks like.
println m;

/* To apply our model to some dataset, we need to be able to load this dataset. PSL provides a range of convenience methods
 * for data loading and, in particular, can interact with any relational database that implements the JDBC interface.
 * So, we first setup a relational database to host the data and to store the results of the inference.
 * 
 * The DataAccess object manages all access to data.
 */
DataStore data = new RelationalDataStore(m)

// Setting up the database. Here we use the Java database H2 (www.h2database.com)
data.setup db : DatabaseDriver.H2

/* To insert data into the database, we can use insertion helpers for a specified predicate.
 * Here we show how one can manually insert data or use the insertion helpers to easily implement
 * custom data loaders.
 */
def insert = data.getInserter(man)
insert.insert(1);

insert = data.getInserter(agent)
insert.insert(2, 1);
//insert.insert(2, 2);
//insert.insert(3, 3);
//insert.insert(1, 1);

insert = data.getInserter(ride)
insert.insert(2);

insert = data.getInserter(patient)
insert.insert(2, 3);

insert = data.getInserter(bike)
insert.insert(3);

insert = data.getInserter(all)
insert.insert(1);
insert.insert(2);
insert.insert(3);

/* After having loaded the data, we are ready to run some inference and see what kind of
 * alignment our model produces. Note that for now, we are using the predefined weights.
 */

ConfigManager cm = ConfigManager.getManager();
ConfigBundle exampleBundle = cm.getBundle("example");

//exampleBundle.putAt("conicreasoner.conicprogramsolver", "edu.umd.cs.psl.optimizer.conic.mosek.MOSEKFactory()")
 
def result = m.mapInference(data.getDatabase(), exampleBundle)

// This prints out the results for our inferexnce predicate


result.printAtoms(man, false)
result.printAtoms(drive, false)
result.printAtoms(bicycle, false)


//
result.printAtoms(ride, false)
result.printAtoms(agent, false)
result.printAtoms(bike, false)
result.printAtoms(patient, false)
result.printAtoms(ent, false)//Atoms(play)