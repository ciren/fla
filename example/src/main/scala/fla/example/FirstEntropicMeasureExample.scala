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
import metrics.FirstEntropicMeasure
import walks.RandomProgressiveWalk

object FirstEntropicMeasureExample extends SafeApp {

  def stepSize(percent: Double, i: Interval[Double]) = percent * (i.upperValue - i.lowerValue)

  val domain = Interval(-10.0, 10.0)^2

  def femFromStepSize(ss: Double) =
    for {
      points    <- Step pointR RandomProgressiveWalk(domain, 100, stepSize(ss, domain.head))
      solutions <- points traverseU Step.evalF[Double]
      fem       <- FirstEntropicMeasure(solutions)
    } yield fem

  val femMacro = femFromStepSize(.1)
  val femMicro = femFromStepSize(.01)
  val both = (femMacro |@| femMicro) { (_, _) }

  val min     = Comparison dominance Min
  val problem = Eval unconstrained Benchmarks.spherical[NonEmptyList, Double]

  override val runc: IO[Unit] = {
    val result = both.run(min)(problem) eval RNG.fromTime
    putStrLn(result.toString)
  }
}
