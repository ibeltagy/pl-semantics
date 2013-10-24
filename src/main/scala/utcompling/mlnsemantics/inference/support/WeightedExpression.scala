package utcompling.mlnsemantics.inference.support

sealed trait WeightedExpression[T] {
  val expression: T
}

case class PriorExpression[T](override val expression: T, weight: Double) extends WeightedExpression[T] {

}

case class GoalExpression[T](override val expression: T, weight: Double) extends WeightedExpression[T] {
  
}

case class SoftWeightedExpression[T](override val expression: T, weight: Double) extends WeightedExpression[T] {

}

case class HardWeightedExpression[T](override val expression: T) extends WeightedExpression[T] {
//case class TextExpression[T](override val expression: T) extends WeightedExpression[T] {

}
