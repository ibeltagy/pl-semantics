package utcompling.mlnsemantics
import breeze.linalg.{DenseVector, SparseVector, Vector => BrVector}

package object vecspace {
  type BowVector = BrVector[Double]
  type BowVectorSpace = Map[String, BowVector]

  implicit def breeze2distance(self: BowVector) = new BowVectorWithDistances(self)
  implicit def distance2breeze(bowd: BowVectorWithDistances): BowVector = bowd.self
}

