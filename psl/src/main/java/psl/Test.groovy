package psl;
import edu.umd.cs.psl.groovy.*;
import edu.umd.cs.psl.database.RDBMS.DatabaseDriver;
import edu.umd.cs.psl.model.function.AttributeSimilarityFunction;
import edu.umd.cs.psl.config.*;
import edu.umd.cs.psl.ui.functions.textsimilarity.*;
PSLModel m = new PSLModel(this);
m.add predicate: "a" , arg0: Entity, open: true
m.add predicate: "b" , arg0: Entity, open: true
m.add predicate: "c" , arg0: Entity, open: true
m.add predicate: "d" , arg0: Entity, open: true
m.add predicate: "all" , arg0: Entity, open: true
m.add predicate: "res", open: true
//m.add rule :  (~a(X)& all(X))  >> c(X),  constraint: true
//m.add rule :  (~b(X)& all(X))  >> c(X),  constraint: true
//m.add rule :  (~c(X)& all(X))  >> d(X),  constraint: true
m.add rule :  (a(X)&b(X)) >> c(X),  constraint: true

//m.add rule :  (~a(X)& all(X))  >> res(),  constraint: true
//m.add rule :  (~a(X) )  >> res(),  constraint: true
//m.add rule :  (b(X))  >> res(),  weight: 0.001
//m.add rule :  a(X)  >> res(),  constraint: true
//m.add rule :  a(X)  >>  res(),  constraint: true
//m.add rule :  (~a(X) & all(X))  >> (res()),  constraint: true
//m.add PredicateConstraint.PartialFunctional , on : samePersons
//m.add PredicateConstraint.PartialInverseFunctional , on : samePersons
//m.add Prior.Simple, on : a, weight: 1
//m.add Prior.Simple, on : b, weight: 1
m.add Prior.Simple, on : c, weight: 1000
m.add Prior.Simple, on : d, weight: 100
//m.add Prior.Simple, on : res, weight: 0.0001
println m;
DataStore data = new RelationalDataStore(m)
data.setup db : DatabaseDriver.H2
data.getInserter(a).insertValue(0.3, 2);
data.getInserter(b).insertValue(0.4, 3);
data.getInserter(c).insertValue(0.6, 4);
data.getInserter(all).insert(2);
//data.getInserter(res).insertValue(0.5);
/*
data.getInserter(all).insert(2);
data.getInserter(all).insert(3);
data.getInserter(all).insert(4);
data.getInserter(all).insert(5);
*/

ConfigManager cm = ConfigManager.getManager();
ConfigBundle exampleBundle = cm.getBundle("example");
def result = m.mapInference(data.getDatabase(), exampleBundle)
//result.printAtoms(all)

result.printAtoms(a)
result.printAtoms(b)
result.printAtoms(c)
//result.printAtoms(d)

return