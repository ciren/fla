package fla

import org.scalacheck._
import org.scalacheck.Prop._

import scalaz.scalacheck.ScalaCheckBinding._
import scalaz.syntax.foldable1._

import cilib._
import Generators._
import walks._

object RandomWalksTests extends Properties("Random Walks") {

  def inDomain(x: Position[Double]) = (x.pos zip x.boundary) all { case (xi, di) => di contains xi }

  def validate(walk: Walk, steps: Int) =
    (walk.size == steps) :| "number of steps" &&
    (walk all inDomain)  :| "in domain"

  property("simple random walk") = forAll(walkParamGen) { case (domain, steps, stepSize) =>
    val walk = SimpleRandomWalk(domain, steps, stepSize) eval RNG.fromTime
    validate(walk, steps)
  }

  property("progressive walk") = forAll(walkParamGen) { case (domain, steps, stepSize) =>
    val walk = RandomProgressiveWalk(domain, steps, stepSize) eval RNG.fromTime
    validate(walk, steps)
  }

  property("progressive manhattan walk") = forAll(walkParamGen) { case (domain, steps, stepSize) =>
    val walk = RandomProgressiveManhattanWalk(domain, steps, stepSize) eval RNG.fromTime
    validate(walk, steps)
  }

}
