package fla

import org.scalacheck._
import org.scalacheck.Prop._

import scalaz._
import Scalaz._
import spire.math.sqrt

import cilib._
import metrics._
import Generators._

object MetricsTests extends Properties("Metrics") {

  type Sample[A] = RVar[NonEmptyList[Position[A]]]
  type Test[A] = A => Boolean

  def validate[R](
    points: Sample[Double],
    fm: FunctionMetric[Double,R],
    problem: Eval[NonEmptyList,Double],
    seed: Long,
    test: Test[R]
  ) = {
    val experiment = for {
      ps        <- Step.pointR(points)
      solutions <- ps traverse Step.evalP[Double]
      metric    <- fm(solutions)
    } yield metric

    val env =
      Environment(
        cmp = Comparison dominance Min,
        eval = problem.eval)

    val result = experiment.run(env) eval RNG.init(seed)

    result.fold(l = _ => false, r = test)
  }

  property("points not evaluated") =
    forAll { (x: PointsProblem) =>
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
        ps <- Step.pointR(x.points)
        ms <- metrics.traverse(_(ps))
      } yield ms

      val env =
        Environment(
          cmp = Comparison dominance Min,
          eval = x.problem.eval)

      val result = experiment.run(env) eval RNG.init(x.seed)

      result.fold(l = _ => true, r = _ => false)
    }

  property("dispersion (bad threshold)") =
    forAll(badDispersionGen) { case (points, problem, threshold, seed) =>
      val test: Test[Double] = (r) => false
      validate(points, Dispersion(threshold), problem, seed, test) == false
    }

  property("dispersion (good threshold)") =
    forAll(goodDispersionGen) { case (points, problem, threshold, seed) =>
      val dim = points.map(_.head.boundary.size).eval(RNG.fromTime)
      val disp = sqrt(3.0 * dim) / 4.0 - 0.1
      val test: Test[Double] = (r) =>  r > -disp && r < sqrt(dim.toDouble) - disp
      validate(points, Dispersion(threshold), problem, seed, test)
    }

  property("first entropic measure (micro)") =
    forAll { (x: WalkProblem @@ Micro) =>
      val test: Test[Double] = (r) => r >= 0.0 && r <= 1.0
      val z = Tag.unwrap(x)
      validate(z.walk, FirstEntropicMeasure.metric, z.problem, z.seed, test)
    }

  property("first entropic measure (macro)") =
    forAll { (x: WalkProblem @@ Macro) =>
      val test: Test[Double] = (r) => r >= 0.0 && r <= 1.0
      val z = Tag.unwrap(x)
      validate(z.walk, FirstEntropicMeasure.metric, z.problem, z.seed, test)
    }

  property("fitness distance correlation") =
    forAll { (x: PointsProblem) =>
      val test: Test[Double] = (r) => r >= -1.0 && r <= 1.0
      validate(x.points, FitnessDistanceCorrelation.metric, x.problem, x.seed, test)
    }

  property("gradient average") =
    forAll { (x: ManhattanWalkProblem) =>
      val test: Test[Double] = (r) => r >= 0.0
      validate(x.walk, Gradient.avg(x.stepSize), x.problem, x.seed, test)
    }

  property("gradient deviation") =
    forAll { (x: ManhattanWalkProblem) =>
      val test: Test[Double] =
        (r) => r >= 0.0//) true else {
        //   println(s"gradient deviation error!!!! r: $r")
        //   println(s"walk: ${x.walk}")
        //   false
        // }
      validate(x.walk, Gradient.dev(x.stepSize), x.problem, x.seed, test)
    }

  property("gradient maximum") =
    forAll { (x: ManhattanWalkProblem) =>
      val test: Test[Double] = (r) => r >= 0.0
      validate(x.walk, Gradient.max(x.stepSize), x.problem, x.seed, test)
    }

  property("information landscape") =
    forAll { (x: PointsProblem) =>
      val test: Test[Double] = (r) => r >= 0.0 && r <= 1.0
      validate(x.points, InformationLandscape.metric, x.problem, x.seed, test)
    }

  property("fitness cloud index (cognitive)") =
    forAll { (x: PointsProblem) =>
      val test: Test[Double] = (r) => r >= 0.0 && r <= 1.0
      validate(x.points, FitnessCloudIndex.cognitive, x.problem, x.seed, test)
    }

  property("fitness cloud index (social)") =
    forAll { (x: PointsProblem) =>
      val test: Test[Double] = (r) => if (r >= 0.0 && r <= 1.0) true else { println(s"fci index error!!!! r: $r"); false }
      validate(x.points, FitnessCloudIndex.social, x.problem, x.seed, test)
    }

}
