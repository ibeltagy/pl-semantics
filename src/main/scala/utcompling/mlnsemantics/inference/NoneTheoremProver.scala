package utcompling.mlnsemantics.inference
import utcompling.scalalogic.fol.expression.FolExpression
import utcompling.mlnsemantics.inference.support.WeightedExpression


//import aima.core.logic.fol.CNFConverter

class NoneTheoremProver
  extends ProbabilisticTheoremProver[FolExpression] {
   
  override def prove(
    constants: Map[String, Set[String]],
    declarations: Map[FolExpression, Seq[String]],
    evidence: List[FolExpression],
    assumptions: List[WeightedExpression[FolExpression]],
    goal: FolExpression): Option[Double] = {
    
    return Some(0.0)
    
  }


}