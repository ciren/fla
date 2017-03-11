package fla
package example

import scalaz._
import Scalaz._
import scalaz.effect._
import scalaz.effect.IO.putStrLn

import spire.math.Interval
import spire.implicits._

import cilib._
import cilib.benchmarks.Benchmarks
import Eval._
import metrics.FitnessDistanceCorrelation

object FitnessDistanceCorrelationExample extends SafeApp {
  val domain = Interval(-10.0, 10.0)^2
  val points = for {
    first <- Position.createPosition(domain)
    rest  <- Position.createPositions(domain, 99)
  } yield NonEmptyList(first, rest: _*)

  val fdc = for {
    ps        <- Step.pointR(points)
    solutions <- ps traverseU Step.evalF[Double]
    metric    <- FitnessDistanceCorrelation(solutions)
  } yield metric

  val min     = Comparison dominance Min
  val problem = Eval unconstrained Benchmarks.spherical[NonEmptyList, Double]

  override val runc: IO[Unit] = {
    val result = fdc.run(min)(problem) eval RNG.fromTime
    putStrLn(result.toString)
  }
}
