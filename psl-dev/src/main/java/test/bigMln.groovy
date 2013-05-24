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
m.add predicate: "man_q" , arg: Entity, open: true
m.add predicate: "man_evd" , arg: Entity, arg2: Text, open: true
m.add predicate: "ride" , arg: Entity, arg2: Text, open: true
m.add predicate: "drive" , arg: Entity,  open: true
m.add predicate: "bike" , arg3: Entity, arg2: Text, open: true
m.add predicate: "bicycle" , arg3: Entity, open: true
m.add predicate: "agent" , arg1: Entity, arg2: Entity, open: true
m.add predicate: "patient" , arg1: Entity, arg2: Entity, open: true


m.add predicate: "ent", open: true
m.add predicate: "r_r_of__dt" , arg: Entity, arg2: Entity, open: true
m.add predicate: "r_r_agent__dh" , arg: Entity, arg2: Entity, open: true
m.add predicate: "us_n_dh", arg: Entity, open: true
m.add predicate: "lead_v_dh", arg: Entity, open: true
m.add predicate: "reconstruction_n_dh", arg: Entity, open: true
m.add predicate: "drive_out_a_dt", arg: Entity, arg2: Text, open: true
m.add predicate: "r_r_at_1011_dt", arg: Entity, arg2: Entity, open: true
m.add predicate: "militant_group_n_dt", arg: Entity, arg2: Text, open: true
m.add predicate: "r_r_subset_of__dh", arg: Entity, arg2: Entity, open: true
m.add predicate: "fight_v_dh", arg: Entity, open: true
m.add predicate: "iraq_n_dt", arg: Entity, arg2: Text, open: true
m.add predicate: "r_r_agent__dt", arg: Entity, arg2: Entity, open: true
m.add predicate: "effort_n_dh", arg: Entity, open: true
m.add predicate: "militant_n_dh", arg: Entity, open: true
m.add predicate: "r_r_of__dh", arg: Entity, arg2: Entity, open: true
m.add predicate: "r_r_in_2017_dh", arg: Entity, arg2: Entity, open: true
m.add predicate: "troops_n_dt", arg: Entity, arg2: Text, open: true
m.add predicate: "us_supporting_company_n_dt", arg: Entity, arg2: Text, open: true
m.add predicate: "campaign_n_dt", arg: Entity, arg2: Text, open: true
m.add predicate: "abduct_v_dh", arg: Entity, open: true
m.add predicate: "occupation_n_dh", arg: Entity, open: true
m.add predicate: "card_70_dh", arg: Entity, open: true
m.add predicate: "wage_v_dt", arg: Entity, arg2: Text, open: true
m.add predicate: "r_r_patient__dt", arg: Entity, arg2: Entity, open: true
m.add predicate: "r_r_subset_of__dt", arg: Entity, arg2: Entity, open: true
m.add predicate: "about_foreigner_n_dh", arg: Entity, open: true
m.add predicate: "r_r_in_1003_dt", arg: Entity, arg2: Entity, open: true
m.add predicate: "r_r_agent_2007_dh", arg: Entity, arg2: Entity, open: true
m.add predicate: "r_r_patient__dh", arg: Entity, arg2: Entity, open: true
m.add predicate: "kidnapping_n_dt", arg: Entity, arg2: Text, open: true
m.add predicate: "iraq_n_dh", arg: Entity, open: true
m.add predicate: "aim_v_dt", arg: Entity, arg2: Text, open: true

m.add predicate: "all", arg1: Entity


m.add function: "abduct_v_dhSim" , name1: Text, name2: Text, implementation: new Abduct_v_dhSim()
m.add rule : (drive_out_a_dt(X, Z) & abduct_v_dhSim(Z, Z)) >> abduct_v_dh(X), constraint: true
m.add rule : (wage_v_dt(X, Z) & abduct_v_dhSim(Z, Z)) >> abduct_v_dh(X), constraint: true
m.add rule : (aim_v_dt(X, Z) & abduct_v_dhSim(Z, Z)) >> abduct_v_dh(X), constraint: true

m.add rule : (aim_v_dt(X, Z) & abduct_v_dhSim(Z, Z)) >> us_n_dh(X), constraint: true
m.add rule : (aim_v_dt(X, Z) & abduct_v_dhSim(Z, Z)) >> iraq_n_dh(X), constraint: true
m.add rule : (aim_v_dt(X, Z) & abduct_v_dhSim(Z, Z)) >> about_foreigner_n_dh(X), constraint: true
m.add rule : (aim_v_dt(X, Z) & abduct_v_dhSim(Z, Z)) >> card_70_dh(X), constraint: true

m.add rule : (aim_v_dt(X, Z) & abduct_v_dhSim(Z, Z)) >> r_r_agent__dh(X, X), constraint: true
m.add rule : (aim_v_dt(X, Z) & abduct_v_dhSim(Z, Z)) >> fight_v_dh(X), constraint: true
m.add rule : (aim_v_dt(X, Z) & abduct_v_dhSim(Z, Z)) >> occupation_n_dh(X), constraint: true
m.add rule : (aim_v_dt(X, Z) & abduct_v_dhSim(Z, Z)) >> lead_v_dh(X), constraint: true
m.add rule : (aim_v_dt(X, Z) & abduct_v_dhSim(Z, Z)) >> r_r_in_2017_dh(X, X), constraint: true


//m.add rule : (us_n_dh(X0) & iraq_n_dh(X1) & about_foreigner_n_dh(X2) & card_70_dh(X2) & militant_n_dh(X3) & r_r_patient__dh(E4,X2) & abduct_v_dh(E4) & r_r_patient__dh(E5,X0) & r_r_agent__dh(E5,X3) & fight_v_dh(E5) & r_r_agent_2007_dh(E4,X3) & r_r_subset_of__dh(X7,X9) & effort_n_dh(X7) & r_r_of__dh(X7,X6) & reconstruction_n_dh(X6) & r_r_subset_of__dh(X8,X9) & occupation_n_dh(X8) & r_r_patient__dh(E10,X9) & r_r_agent__dh(E10,X2) & lead_v_dh(E10) & r_r_in_2017_dh(X9,X1)) >> ent(), constraint: true


class Abduct_v_dhSim implements AttributeSimilarityFunction {
	@Override
	public double similarity(String a, String b) {
		a = a + "-" + b;
		switch(a){
			case "drive_out_a_dt-drive_out_a_dt": return 0.99;
			case "wage_v_dt-wage_v_dt": return 0.99;
			case "aim_v_dt-aim_v_dt": return 0.99;
		}
	}
}

m.add function: "militant_n_dhSim" , name1: Text, name2: Text, implementation: new Militant_n_dhSim()
class Militant_n_dhSim implements AttributeSimilarityFunction {
	@Override
	public double similarity(String a, String b) {
		a = a + "-" + b;
		switch(a){
			case "militant_group_n_dt-iraq_n_dt": return 1;
			case "campaign_n_dt-kidnapping_n_dt": return 0.99;
			case "us_supporting_company_n_dt-us_supporting_company_n_dt": return 0.99;
			case "troops_n_dt-troops_n_dt": return 0.99;
		}
	}
}
m.add rule : (militant_group_n_dt(X, Z) & iraq_n_dt(Y, T) & r_r_in_1003_dt(X,Y) & militant_n_dhSim(Z, T)) >> militant_n_dh(Y), constraint: true
m.add rule : (campaign_n_dt(X, Z) & kidnapping_n_dt(Y, T) & r_r_of__dt(X,Y) & militant_n_dhSim(Z, T)) >> militant_n_dh(Y), constraint: true
m.add rule : (us_supporting_company_n_dt(X, Z) & militant_n_dhSim(Z, Z)) >> militant_n_dh(X), constraint: true
m.add rule : (troops_n_dt(X, Z) & militant_n_dhSim(Z, Z)) >> militant_n_dh(X), constraint: true

/*
m.add function: "Sim" , name1: Text, name2: Text, implementation: new Sim()
0.346618653667864 (us_supporting_company_n_dt(x1) => effort_n_dh(x0) ^ reconstruction_n_dh(x1) ^ r_r_of__dh(x0,x1))
6.121326973985210 (troops_n_dt(x1) => effort_n_dh(x0) ^ reconstruction_n_dh(x1) ^ r_r_of__dh(x0,x1))
*/

m.add function: "effort_n_dhReconstruction_n_dhSim" , name1: Text, name2: Text, implementation: new Effort_n_dhReconstruction_n_dhSim()
class Effort_n_dhReconstruction_n_dhSim implements AttributeSimilarityFunction {
	@Override
	public double similarity(String a, String b) {
		a = a + "-" + b;
		switch(a){
			case "campaign_n_dt-kidnapping_n_dt": return 0.99;
			case "militant_group_n_dt-iraq_n_dt": return 0.99;
			case "troops_n_dt-troops_n_dt": return 0.99;
		}
	}
}
m.add rule : (campaign_n_dt(X, Z) & kidnapping_n_dt(Y, T) & r_r_of__dt(X,Y) & effort_n_dhReconstruction_n_dhSim(Z, T)) >> effort_n_dh(X), constraint: true
m.add rule : (campaign_n_dt(X, Z) & kidnapping_n_dt(Y, T) & r_r_of__dt(X,Y) & effort_n_dhReconstruction_n_dhSim(Z, T)) >> reconstruction_n_dh(Y), constraint: true
m.add rule : (campaign_n_dt(X, Z) & kidnapping_n_dt(Y, T) & r_r_of__dt(X,Y) & effort_n_dhReconstruction_n_dhSim(Z, T)) >> r_r_of__dh(X, Y), constraint: true

m.add rule : (militant_group_n_dt(X, Z) & iraq_n_dt(Y, T) & r_r_in_1003_dt(X,Y) & effort_n_dhReconstruction_n_dhSim(Z, T)) >> effort_n_dh(X), constraint: true
m.add rule : (militant_group_n_dt(X, Z) & iraq_n_dt(Y, T) & r_r_in_1003_dt(X,Y) & effort_n_dhReconstruction_n_dhSim(Z, T)) >> reconstruction_n_dh(Y), constraint: true
m.add rule : (militant_group_n_dt(X, Z) & iraq_n_dt(Y, T) & r_r_in_1003_dt(X,Y) & effort_n_dhReconstruction_n_dhSim(Z, T)) >> r_r_of__dh(X, Y), constraint: true

/*
m.add function: "Sim" , name1: Text, name2: Text, implementation: new Sim()
7.220802845874089 (militant_group_n_dt(x0) ^ iraq_n_dt(x1) ^ r_r_in_1003_dt(x0,x1) => occupation_n_dh(x1))
0.085723372263040 (us_supporting_company_n_dt(x1) => occupation_n_dh(x1))
2.981705825325305 (troops_n_dt(x1) => occupation_n_dh(x1))
0.760915022238912 (campaign_n_dt(x0) ^ kidnapping_n_dt(x1) ^ r_r_of__dt(x0,x1) => occupation_n_dh(x1))

m.add function: "Sim" , name1: Text, name2: Text, implementation: new Sim()
4.351965310785494 (aim_v_dt(x1) => fight_v_dh(x1))
2.836145836469477 (drive_out_a_dt(x1) => fight_v_dh(x1))
3.808500940991620 (wage_v_dt(x1) => fight_v_dh(x1))

m.add function: "Sim" , name1: Text, name2: Text, implementation: new Sim()
2.123813533393021 (militant_group_n_dt(x0) ^ iraq_n_dt(x1) ^ r_r_in_1003_dt(x0,x1) => about_foreigner_n_dh(x1))
2.189863407210156 (campaign_n_dt(x0) ^ kidnapping_n_dt(x1) ^ r_r_of__dt(x0,x1) => about_foreigner_n_dh(x1))
1.448243761033518 (troops_n_dt(x1) => about_foreigner_n_dh(x1))
0.398161521309346 (us_supporting_company_n_dt(x1) => about_foreigner_n_dh(x1))
*/

//m.add rule : r_r_agent__dt(E,X) >> r_r_agent_2007_dh(E,X), constaint: true
m.add rule : r_r_patient__dt(E,X) >> r_r_patient__dh(E,X), constaint: true
m.add rule : r_r_subset_of__dt(X0,X1) >> r_r_subset_of__dh(X0,X1), constaint: true
m.add rule : r_r_agent__dt(E,X) << r_r_agent_2007_dh(E,X), constaint: true




m.add rule : (all(X0) & us_n_dh(X0) & iraq_n_dh(X1) & about_foreigner_n_dh(X2) & card_70_dh(X2) & militant_n_dh(X3) & r_r_patient__dh(E4,X2) & abduct_v_dh(E4) & r_r_patient__dh(E5,X0) & r_r_agent__dh(E5,X3) & fight_v_dh(E5) & r_r_agent_2007_dh(E4,X3) & r_r_subset_of__dh(X7,X9) & effort_n_dh(X7) & r_r_of__dh(X7,X6) & reconstruction_n_dh(X6) & r_r_subset_of__dh(X8,X9) & occupation_n_dh(X8) & r_r_patient__dh(E10,X9) & r_r_agent__dh(E10,X2) & lead_v_dh(E10) & r_r_in_2017_dh(X9,X1)) >> ent(), constraint: true


//m.add rule : (about_foreigner_n_dh(X2) & card_70_dh(X2) & r_r_patient__dh(E4,X2) & r_r_agent__dh(E10,X2) ) >> ent(), constraint: true
//m.add rule : (about_foreigner_n_dh(X2) & card_70_dh(X2) & r_r_patient__dh(E4,X2) & r_r_agent__dh(E10,X2) ) >> ent(), constraint: true

//m.add rule : ( man_q(X1) & agent(E2, X1) & drive(E2) & patient(E2, X3) & bicycle(X3) ) >> ent(), constraint: true

/*
m.add function: "bicycleSim" , name1: Text, name2: Text, implementation: new BicycleSim()//, see end of file
m.add function: "driveSim" , name1: Text, name2: Text, implementation: new DriveSim()//, see end of file
m.add function: "manSim" , name1: Text, name2: Text, implementation: new ManSim()//, see end of file
m.add function: "manDriveSim" , name1: Text, name2: Text, implementation: new ManDriveSim()//, see end of file


m.add rule : (bike(X, Z) & bicycleSim(Z, Z)) >> bicycle(X), constraint: true
m.add rule : (ride(X, Z) & bicycleSim(Z, Z)) >> bicycle(X), constraint: true
m.add rule : (man_evd(X, Z) & bicycleSim(Z, Z)) >> bicycle(X), constraint: true

m.add rule : (bike(X, Z) & driveSim(Z, Z)) >> drive(X), constraint: true
m.add rule : (ride(X, Z) & driveSim(Z, Z)) >> drive(X), constraint: true
m.add rule : (man_evd(X, Z) & driveSim(Z, Z)) >> drive(X), constraint: true

m.add rule : (bike(X, Z) & manSim(Z, Z)) >> man_q(X), constraint: true
m.add rule : (ride(X, Z) & manSim(Z, Z)) >> man_q(X), constraint: true
m.add rule : (man_evd(X, Z) & manSim(Z, Z)) >> man_q(X), constraint: true


m.add rule : (man_evd(X, Z) & agent (Y, X) & ride(Y, T) & manSim(Z, T)) >> man_q(X), constraint: true
m.add rule : (ride(X, Z) & agent (Y, X) & bike(Y, T) & manSim(Z, T)) >> man_q(X), constraint: true

m.add rule : (man_evd(X, Z) & agent (Y, X) & ride(Y, T) & manSim(Z, T)) >> drive(X), constraint: true
m.add rule : (ride(X, Z) & agent (Y, X) & bike(Y, T) & manSim(Z, T)) >> drive(X), constraint: true

m.add rule : (man_evd(X, Z) & agent (Y, X) & ride(Y, T) & manSim(Z, T)) >> bicycle(X), constraint: true
m.add rule : (ride(X, Z) & agent (Y, X) & bike(Y, T) & manSim(Z, T)) >> bicycle(X), constraint: true


//m.add rule : (man_evd(X, Z) & manDriveSim(Z, Z)) >> man_q(X)  , constraint: true
//m.add rule : (man_evd(X, Z) & manDriveSim(Z, Z)) >> agent(Y, X) , constraint: true
//m.add rule : (man_evd(X, Z) & manDriveSim(Z, Z)) >> drive(Y) , constraint: true


m.add rule : ( man_q(X) & agent(Y, X) & drive(Y) & patient(Y, Z) & bicycle(Z) ) >> ent(X, Y, Z), constraint: true


class BicycleSim implements AttributeSimilarityFunction {
	@Override
	public double similarity(String a, String b) {
		a = a + "-" + b;
		switch(a){
			case "bike-bike": return 0.9;
			case "ride-ride": return 0.2;
			case "man-man": return 0.3;
			case "man-ride": return 0.3;
			case "ride-bike": return 0.8;
		}
	}
}

class DriveSim implements AttributeSimilarityFunction {
	@Override
	public double similarity(String a, String b) {
		a = a + "-" + b;
		switch(a){
			case "bike-bike": return 0.4;
			case "ride-ride": return 0.9;
			case "man-man": return 0.3;
			case "man-ride": return 0.6;
			case "ride-bike": return 0.7;
		}
	}
}

class ManSim implements AttributeSimilarityFunction {
	@Override
	public double similarity(String a, String b) {
		a = a + "-" + b;
		switch(a){
			case "bike-bike": return 0.1;
			case "ride-ride": return 0.2;
			case "man-man": return 0.99;
			case "man-ride": return 0.7;
			case "ride-bike": return 0.3;
		}
	}
}

class ManDriveSim implements AttributeSimilarityFunction {
	@Override
	public double similarity(String a, String b) {
		a = a + "-" + b;
		switch(a){
			case "bike-bike": return 0.1;
			case "ride-ride": return 0.2;
			case "man-man": return 0.99;
			case "man-ride": return 0.7;
			case "ride-bike": return 0.3;
		}
	}
}
*/


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
def insert = data.getInserter(iraq_n_dt)
insert.insert(0, "iraq_n_dt");
insert.insert(100, "iraq_n_dt");

insert = data.getInserter(militant_group_n_dt)
insert.insert(1, "militant_group_n_dt");
insert.insert(100, "militant_group_n_dt");

insert = data.getInserter(r_r_patient__dt)
insert.insert(2, 10);
insert.insert(100, 100);

insert = data.getInserter(r_r_agent__dt)
insert.insert(2, 1);
insert.insert(100, 100);

insert = data.getInserter(wage_v_dt)
insert.insert(2, "wage_v_dt");
insert.insert(100, "wage_v_dt");

insert = data.getInserter(drive_out_a_dt)
insert.insert(3, "drive_out_a_dt");
insert.insert(100, "drive_out_a_dt");

insert = data.getInserter(r_r_patient__dt)
insert.insert(3, 6);
//insert.insert(100, 100);

insert = data.getInserter(r_r_agent__dt)
insert.insert(3, 7);
insert.insert(23, 7);
//insert.insert(100, 100);

insert = data.getInserter(r_r_subset_of__dt)
insert.insert(4, 6);
insert.insert(100, 100);

insert = data.getInserter(troops_n_dt)
insert.insert(4, "troops_n_dt");
insert.insert(100, "troops_n_dt");

insert = data.getInserter(r_r_subset_of__dt)
insert.insert(5, 6);
//insert.insert(100, 100);

insert = data.getInserter(us_supporting_company_n_dt)
insert.insert(5, "us_supporting_company_n_dt");
insert.insert(100, "us_supporting_company_n_dt");

insert = data.getInserter(r_r_patient__dt)
insert.insert(8, 10);
//insert.insert(100, 100);

insert = data.getInserter(aim_v_dt)
insert.insert(8, "aim_v_dt");
insert.insert(100, "aim_v_dt");

insert = data.getInserter(campaign_n_dt)
insert.insert(10, "campaign_n_dt");
insert.insert(100, "campaign_n_dt");

insert = data.getInserter(r_r_of__dt)
insert.insert(10, 9);
insert.insert(100, 100);

insert = data.getInserter(kidnapping_n_dt)
insert.insert(9, "kidnapping_n_dt");
insert.insert(100, "kidnapping_n_dt");

insert = data.getInserter(r_r_in_1003_dt)
insert.insert(1, 0);
insert.insert(100, 100);


insert = data.getInserter(man_evd)
insert.insert(1, "man");

insert = data.getInserter(agent)
insert.insert(2, 1);

insert = data.getInserter(ride)
insert.insert(2, "ride");

insert = data.getInserter(patient)
insert.insert(2, 3);

insert = data.getInserter(bike)
insert.insert(3, "bike");

insert = data.getInserter(all)
insert.insert(0);
insert.insert(1);
insert.insert(2);
insert.insert(3);
insert.insert(4);
insert.insert(5);
insert.insert(6);
insert.insert(7);
insert.insert(8);
insert.insert(9);
insert.insert(10);
insert.insert(11);
insert.insert(12);
insert.insert(13);
insert.insert(14);
insert.insert(15);
insert.insert(16);
insert.insert(17);
insert.insert(18);
insert.insert(100);



/* After having loaded the data, we are ready to run some inference and see what kind of
 * alignment our model produces. Note that for now, we are using the predefined weights.
 */

ConfigManager cm = ConfigManager.getManager();
ConfigBundle exampleBundle = cm.getBundle("example");


def result = m.mapInference(data.getDatabase(), exampleBundle)

// This prints out the results for our inferexnce predicate


//result.printAtoms(drive, false)
//result.printAtoms(man_q, false)
//result.printAtoms(bicycle, false)

result.printAtoms(us_n_dh, false)
result.printAtoms(iraq_n_dh, false)
result.printAtoms(about_foreigner_n_dh, false)
result.printAtoms(card_70_dh, false)
result.printAtoms(militant_n_dh, false)
result.printAtoms(r_r_patient__dh, false)
result.printAtoms(abduct_v_dh, false)
result.printAtoms(r_r_patient__dh, false)
result.printAtoms(r_r_agent__dh, false)
result.printAtoms(fight_v_dh, false)
result.printAtoms(r_r_agent_2007_dh, false)
result.printAtoms( r_r_subset_of__dh, false)
result.printAtoms(effort_n_dh, false)
result.printAtoms(r_r_of__dh, false)
result.printAtoms(reconstruction_n_dh, false)
result.printAtoms(r_r_subset_of__dh, false)
result.printAtoms(occupation_n_dh, false)
result.printAtoms(r_r_patient__dh, false)
result.printAtoms(r_r_agent__dh, false)
result.printAtoms(lead_v_dh, false)
result.printAtoms(r_r_in_2017_dh, false)
result.printAtoms(ent, false)