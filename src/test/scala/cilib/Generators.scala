package cilib
package fla

import org.scalacheck.Gen

import spire.implicits._
import spire.math.Interval

import scalaz.NonEmptyList
import scalaz.scalacheck.ScalaCheckBinding._
import scalaz.std.list._
import scalaz.syntax.apply._
import scalaz.NonEmptyList._

import benchmarks.Benchmarks
import Eval._
import Metrics._

object Generators {

  val intervalGen = for {
    a <- Gen.posNum[Double]
    b <- Gen.posNum[Double] suchThat (_ > a)
  } yield Interval(a, b)

  def positiveGen(lower: Double) = Gen.posNum[Double] suchThat (_ > lower)
  def negativeGen(upper: Double) = Gen.negNum[Double] suchThat (_ < upper)
  val badThresholdGen = for {
    p <- positiveGen(1.0)
    n <- negativeGen(0.0)
    o <- Gen.oneOf(List(p, n))
  } yield o
  val goodThresholdGen = Gen.choose(0.1, 0.9)

  val dimGen = Gen.posNum[Int] suchThat (_ >= 2)
  val domainGen = (intervalGen |@| dimGen) { _ ^ _ }

  val stepSizeGen = Gen.choose(0.0, 0.3)
  val stepGen = Gen.posNum[Int] suchThat (_ > 2) map (_ * 10)
  def stepSizeValue(s: Double, i: Interval[Double]) = s * (i.upperValue - i.lowerValue)

  val walkParamGen = (domainGen |@| stepGen |@| stepSizeGen) { (_, _, _) }

  def progressiveWalkGen(stepSizePercent: Double) = (domainGen |@| stepGen) { (domain, steps) =>
    val stepSize = stepSizeValue(stepSizePercent, domain.head)
    RandomWalks.progressive(domain, steps, stepSize)
  }

  val progressiveManhattanWalkGen = walkParamGen.map { case (domain, steps, stepSizePercent) =>
    val stepSize = stepSizeValue(stepSizePercent, domain.head)
    (RandomWalks.progressiveManhattan(domain, steps, stepSize), stepSize)
  }

  val pointsGen = (domainGen |@| stepGen) { (domain, n) =>
    for {
      head <- Position.createPosition(domain)
      rest <- Position.createPositions(domain, n - 1)
    } yield NonEmptyList(head, rest: _*)
  }

  def problemNel(f: NonEmptyList[Double] => Double) = Eval.unconstrained(f)

  val problemGen: Gen[Eval[Double]] = Gen.oneOf(List(
    problemNel(Benchmarks.rastrigin),
    problemNel(Benchmarks.absoluteValue),
    problemNel(Benchmarks.ackley),
    problemNel(Benchmarks.spherical)
  ))

  val pointsProblemGen = (pointsGen |@| problemGen) { (_, _) }
  def walkProblemGen(stepSizePercent: Double) = (progressiveWalkGen(stepSizePercent) |@| problemGen) { (_, _) }
  val manhattanWalkProblemGen = (progressiveManhattanWalkGen |@| problemGen) { (_, _) }

  val badDispersionGen = (pointsGen |@| problemGen |@| badThresholdGen) { (_, _, _) }
  val goodDispersionGen = (pointsGen |@| problemGen |@| goodThresholdGen) { (_, _, _) }
}
