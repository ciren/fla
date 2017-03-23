package fla

import org.scalacheck._
import org.scalacheck.Prop._

import scalaz.{NonEmptyList,\/}
import scalaz.scalacheck.ScalaCheckBinding._
import scalaz.syntax.traverse._

import spire.math.sqrt

import cilib._
import metrics._
import Generators._

object MetricsTests extends Properties("Metrics") {

  type Sample[A] = RVar[NonEmptyList[Position[A]]]
  type Test[A] = String \/ A => Boolean
  val min = Comparison dominance Min

  def validate[A:spire.math.Numeric,R](points: Sample[A], fm: FunctionMetric[A,R], problem: Eval[A], test: Test[R]) = {
    val experiment = for {
      ps        <- Step.pointR(points)
      solutions <- ps traverseU Step.evalF[A]
      metric    <- fm(solutions)
    } yield metric

    val result = experiment.run(min)(problem) eval RNG.fromTime
    test(result)
  }

  property("points not evaluated") = forAll(pointsProblemGen) { case (points, problem) =>
    val metrics = NonEmptyList(
      Dispersion(0.1),
      FirstEntropicMeasure.metric,
      FitnessDistanceCorrelation.metric,
      Gradient.avg(0.1),
      Gradient.max(0.1),
      Gradient.dev(0.1),
      InformationLandscape.metric
    )
    val experiment = for {
      ps <- Step.pointR(points)
      ms <- metrics.traverseU(_(ps))
    } yield ms

    val result = experiment.run(min)(problem) eval RNG.fromTime
    result.all(_.isLeft)
  }

  property("dispersion (bad threshold)") = forAll(badDispersionGen) { case (points, problem, threshold) =>
    val test: Test[Double] = (r) => r.isLeft
    validate(points, Dispersion(threshold), problem, test)
  }

  property("dispersion (good threshold)") = forAll(goodDispersionGen) { case (points, problem, threshold) =>
    val dim = points.map(_.head.boundary.size).eval(RNG.fromTime)
    val disp = sqrt(3.0 * dim) / 4.0 - 0.1
    val test: Test[Double] = (r) => r forall (m => m > -disp && m < sqrt(dim.toDouble) - disp)
    validate(points, Dispersion(threshold), problem, test)
  }

  property("first entropic measure (micro)") = forAll(walkProblemGen(0.01)) { case (walk, problem) =>
    val test: Test[Double] = (r) => r forall (m => m >= 0.0 && m <= 1.0)
    validate(walk, FirstEntropicMeasure.metric, problem, test)
  }

  property("first entropic measure (macro)") = forAll(walkProblemGen(0.1)) { case (walk, problem) =>
    val test: Test[Double] = (r) => r forall (m => m >= 0.0 && m <= 1.0)
    validate(walk, FirstEntropicMeasure.metric, problem, test)
  }

  property("fitness distance correlation") = forAll(pointsProblemGen) { case (points, problem) =>
    val test: Test[Double] = (r) => r forall (m => m >= -1.0 && m <= 1.0)
    validate(points, FitnessDistanceCorrelation.metric, problem, test)
  }

  property("gradient average") = forAll(manhattanWalkProblemGen) { case ((walk, s), problem) =>
    val test: Test[Double] = (r) => r forall (m => m >= 0.0)
    validate(walk, Gradient.avg(s), problem, test)
  }

  property("gradient deviation") = forAll(manhattanWalkProblemGen) { case ((walk, s), problem) =>
    val test: Test[Double] = (r) => r forall (m => m >= 0.0)
    validate(walk, Gradient.dev(s), problem, test)
  }

  property("gradient maximum") = forAll(manhattanWalkProblemGen) { case ((walk, s), problem) =>
    val test: Test[Double] = (r) => r forall (m => m >= 0.0)
    validate(walk, Gradient.max(s), problem, test)
  }

  property("information landscape") = forAll(pointsProblemGen) { case (points, problem) =>
    val test: Test[Double] = (r) => r forall (m => m >= 0.0 && m <= 1.0)
    validate(points, InformationLandscape.metric, problem, test)
  }

  property("fitness cloud index (cognitive)") = forAll(pointsProblemGen) { case (points, problem) =>
    val test: Test[Double] = (r) => r forall (m => m >= 0.0 && m <= 1.0)
    validate(points, FitnessCloudIndex.cognitive, problem, test)
  }

  property("fitness cloud index (social)") = forAll(pointsProblemGen) { case (points, problem) =>
    val test: Test[Double] = (r) => r forall (m => m >= 0.0 && m <= 1.0)
    validate(points, FitnessCloudIndex.social, problem, test)
  }

}
