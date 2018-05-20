package fla

import org.scalacheck._

import spire.implicits._
import spire.math.Interval

import scalaz._
import Scalaz._
import scalaz.scalacheck.ScalaCheckBinding._

import cilib._
import benchmarks.Benchmarks
import walks._

object Generators {

  val intervalGen = for {
    a <- Gen.posNum[Double]
    b <- Gen.posNum[Double] suchThat (_ > a)
  } yield Interval(a, b)

  val badThresholdGen =
    for {
      p <- Gen.posNum[Double] suchThat (_ > 1.0)
      n <- Gen.negNum[Double]
      o <- Gen.oneOf(List(p, n))
    } yield o

  val goodThresholdGen = Gen.choose(0.1, 0.9)

  val domainGen = intervalGen map { _ ^ 2 }

  val stepSizeGen = Gen.choose(0.0, 0.3)
  val stepGen = Gen.posNum[Int] suchThat (_ > 2) map (_ * 10)

  def stepSizeValue(s: Double, i: Interval[Double]) =
    s * (i.upperValue - i.lowerValue)

  val walkParamGen =
    (domainGen |@| stepGen |@| stepSizeGen) { (domain, steps, stepSize) =>
      (domain, steps, stepSizeValue(stepSize, domain.head))
    }

  def progressiveWalkGen(stepSizePercent: Double) =
    (domainGen |@| stepGen) { (domain, steps) =>
      val stepSize = stepSizeValue(stepSizePercent, domain.head)
      RandomProgressiveWalk(domain, steps, stepSize)
    }

  val progressiveManhattanWalkGen =
    walkParamGen.map { case (domain, steps, stepSize) =>
      (RandomProgressiveManhattanWalk(domain, steps, stepSize), stepSize)
    }

  val pointsGen =
    (domainGen |@| stepGen) { (domain, n) =>
      positiveInt(n) { value => Position.createPositions(domain, value) }
    }

  val problemGen = Gen.oneOf(List(
    Eval.unconstrained(Benchmarks.spherical[NonEmptyList,Double]),
    Eval.unconstrained(Benchmarks.absoluteValue[NonEmptyList,Double]),
    Eval.unconstrained(Benchmarks.ackley[NonEmptyList,Double]),
    Eval.unconstrained(Benchmarks.spherical[NonEmptyList,Double])
  ))


  final case class PointsProblem(
    points: RVar[NonEmptyList[Position[Double]]],
    problem: Eval[NonEmptyList, Double],
    seed: Long
  )

  implicit val pointsProblem =
    Arbitrary {
      (pointsGen |@| problemGen |@| Arbitrary.arbitrary[Long] ) { PointsProblem.apply }
    }

  final case class WalkProblem(
    walk: RVar[NonEmptyList[Position[Double]]],
    problem: Eval[NonEmptyList, Double],
    seed: Long
  )

  def walkProblemGen(stepSizePercent: Double) =
    (progressiveWalkGen(stepSizePercent) |@| problemGen |@| Arbitrary.arbitrary[Long]) {
      WalkProblem.apply
    }

  trait Micro
  trait Macro

  implicit val microWalkProblemGen: Arbitrary[WalkProblem @@ Micro] =
    Arbitrary {
      walkProblemGen(0.01).map(x => Tag.apply[WalkProblem, Micro](x))
    }

  implicit val macroWalkProblemGen: Arbitrary[WalkProblem @@ Macro] =
    Arbitrary {
      walkProblemGen(0.1).map(x => Tag.apply[WalkProblem, Macro](x))
    }

  final case class ManhattanWalkProblem(
    walk: RVar[NonEmptyList[Position[Double]]],
    stepSize: Double,
    problem: Eval[NonEmptyList, Double],
    seed: Long
  )

  implicit val manhattanWalkProblemGen: Arbitrary[ManhattanWalkProblem] =
    Arbitrary {
      (progressiveManhattanWalkGen |@| problemGen |@| Arbitrary.arbitrary[Long]) { case ((walk,s), prob, seed) =>
        ManhattanWalkProblem.apply(walk, s, prob, seed)
      }
    }

  val badDispersionGen =
    (pointsGen |@| problemGen |@| badThresholdGen |@| Arbitrary.arbitrary[Long]) { Tuple4.apply }

  val goodDispersionGen =
    (pointsGen |@| problemGen |@| goodThresholdGen |@| Arbitrary.arbitrary[Long]) { Tuple4.apply }
}
