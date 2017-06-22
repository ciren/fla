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
import metrics.FitnessCloudIndex

object FitnessCloudIndexExample extends SafeApp {
  val domain = Interval(-100.0, 100.0)^1
  val points = for {
    first <- Position.createPosition(domain)
    rest  <- Position.createPositions(domain, 499)
  } yield NonEmptyList(first, rest: _*)

  val fci = for {
    ps  <- Step.pointR(points)
    cog <- FitnessCloudIndex cognitive ps
    soc <- FitnessCloudIndex social ps
    dev <- FitnessCloudIndex.meanOfStdDev(30)(ps)
  } yield (cog |@| soc |@| dev) { (_, _, _) }

  val min     = Comparison dominance Min
  val problem = Eval unconstrained Benchmarks.spherical[NonEmptyList, Double]

  override val runc: IO[Unit] = {
    val result = fci.run(min)(problem) eval RNG.fromTime
    putStrLn(result.toString)
  }
}
