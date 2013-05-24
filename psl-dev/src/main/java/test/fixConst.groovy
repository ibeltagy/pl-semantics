package testNew;

import edu.umd.cs.psl.groovy.*;
import edu.umd.cs.psl.database.RDBMS.DatabaseDriver;
import edu.umd.cs.psl.config.*;
import edu.umd.cs.psl.model.predicate.type.*;

PSLModel m = new PSLModel(this);

m.add predicate: "lhs", arg: Entity, arg2: Text,  open: true
m.add predicate: "rhs", arg: Entity, open: true
//m.add rule :  (lhs(X) & X.is(3) )  >> rhs(X),  weight : 0.97
m.add rule :  (lhs(X, "ali")  )  >> rhs(X),  weight : 0.97

println m;

DataStore data = new RelationalDataStore(m)
data.setup db : DatabaseDriver.H2
def insert = data.getInserter(lhs)
insert.insert(2, "ali");
insert.insert(3, "otros");

 
def result = m.mapInference(data.getDatabase())
result.printAtoms(lhs)
result.printAtoms(rhs)