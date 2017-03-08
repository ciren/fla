package fla

import org.scalacheck._
import org.scalacheck.Prop._

import scalaz.scalacheck.ScalaCheckBinding._
import scalaz.syntax.foldable1._

import spire.math.Bounded

import cilib._
import Generators._
import walks._

object RandomWalksTests extends Properties("Random Walks") {

  def inDomain(x: Position[Double]) = (x.pos zip x.boundary) all { case (xi, di) => di contains xi }

  property("domain") = forAll(domainGen) { domain =>
    domain.size >= 1 &&
    domain.all(_.nonEmpty) &&
    domain.all(i => i.lowerValue < i.upperValue)
  }

  property("manhattan progressive walk") = forAll(walkParamGen) { case (domain, steps, stepSizePercent) =>
    val stepSize = stepSizeValue(stepSizePercent, domain.head)
    val walk = RandomProgressiveManhattanWalk(domain, steps, stepSize) eval RNG.fromTime

    (walk.size == steps)  :| "number of steps" &&
    (walk all inDomain)   :| "in domain" &&
    (inDomain(walk.head)) :| "first step in domain"
  }

  property("progressive walk") = forAll(walkParamGen) { case (domain, steps, stepSizePercent) =>
    val stepSize = stepSizeValue(stepSizePercent, domain.head)
    val walk = RandomProgressiveWalk(domain, steps, stepSize) eval RNG.fromTime

    (walk.size == steps)  :| "number of steps" &&
    (walk all inDomain)   :| "in domain" &&
    (inDomain(walk.head)) :| "first step in domain"
  }
}
