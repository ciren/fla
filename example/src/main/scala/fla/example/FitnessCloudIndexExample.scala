package fla
package example

import scalaz._
import Scalaz._
import scalaz.effect._
import scalaz.effect.IO.putStrLn

import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._

import spire.math.Interval
import spire.implicits._

import cilib._
import benchmarks.Benchmarks
import metrics.FitnessCloudIndex

object FitnessCloudIndexExample extends SafeApp {
  val domain = Interval(-100.0, 100.0)^1

  val x: Int Refined Positive = 500

  val points = Position.createPositions(domain, 500)

  val fci =
    Step.pointR(points)
      .flatMap(ps =>
        (FitnessCloudIndex.cognitive(ps) |@|
          FitnessCloudIndex.social(ps) |@|
          FitnessCloudIndex.meanOfStdDev(30)(ps)) { Tuple3.apply })

  val f = Eval.unconstrained(Benchmarks.spherical[NonEmptyList,Double])

  val env = Environment(
    cmp = Comparison dominance Min,
    eval = f.eval
  )

  override val runc: IO[Unit] = {
    val result = fci.run(env) eval RNG.fromTime
    putStrLn(result.toString)
  }
}
