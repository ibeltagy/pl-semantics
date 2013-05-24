package testNew;

import edu.umd.cs.psl.groovy.*;
import edu.umd.cs.psl.database.RDBMS.DatabaseDriver;
import edu.umd.cs.psl.config.*;
import edu.umd.cs.psl.model.predicate.type.*;

PSLModel m = new PSLModel(this);

//m.add predicate: "eat" , arg: Entity
//m.add predicate: "play" , arg: Entity,  open: true  
//m.add predicate: "ent", arg: Entity,  open: true

m.add predicate: "lhs", arg: Entity, open: true
m.add predicate: "rhs", arg: Entity, open: true
m.add predicate: "mhs", arg: Entity, open: true
m.add rule :  (lhs(X) )  >> rhs(X),  weight : 0.97
m.add rule :  (lhs(X) )  >> mhs(X),  weight : 0.97
//m.add rule :  mhs(X)  >> mhs(X),  weight : 0.77
//m.add rule : ( B1(V1,V2)  & V1.is("constant1") ) >> H(V1,V2),  weight : 1

//m.add rule : (eat(X) & X.is(1))>> ent(X),  weight : 1
//m.add rule : play(X)  >> ent(X),  weight : 1

m.add Prior.Simple, on : rhs, weight: 0.6

println m;

DataStore data = new RelationalDataStore(m)
data.setup db : DatabaseDriver.H2
def insert = data.getInserter(lhs)
insert.insert(1);
insert.insert(2);
insert.insert(3);

 
def result = m.mapInference(data.getDatabase())
result.printAtoms(lhs)
result.printAtoms(rhs)