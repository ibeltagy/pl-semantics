package utcompling.mlnsemantics.inference.support

trait WeightedExpression[T] {
  val expression: T
}

case class SoftWeightedExpression[T](override val expression: T, weight: Double) extends WeightedExpression[T] {

}

case class HardWeightedExpression[T](override val expression: T) extends WeightedExpression[T] {

}
