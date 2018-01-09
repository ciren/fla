package fla
package example

import scalaz._
import Scalaz._
import scalaz.effect._
import scalaz.effect.IO.putStrLn

import shapeless._
import spire.math.Interval
import spire.implicits._

import cilib._
import benchmarks.Benchmarks
import benchmarks.implicits._
import metrics.FirstEntropicMeasure
import walks.RandomProgressiveWalk

object FirstEntropicMeasureExample extends SafeApp {

  def stepSize(percent: Double, i: Interval[Double]) = percent * (i.upperValue - i.lowerValue)

  val domain = Interval(-10.0, 10.0)^2

  def femFromStepSize(ss: Double) =
    for {
      points    <- Step pointR RandomProgressiveWalk(domain, 100, stepSize(ss, domain.head))
      solutions <- points traverseU Step.evalP[Double]
      fem       <- FirstEntropicMeasure(solutions)
    } yield fem

  val femMacro = femFromStepSize(.1)
  val femMicro = femFromStepSize(.01)
  val both = (femMacro |@| femMicro) { (_, _) }

  val f = Benchmarks.spherical[nat._2,Double] _

  val env = Environment(
    cmp = Comparison dominance Min,
    eval = f.unconstrained.eval,
    bounds = domain
  )

  override val runc: IO[Unit] = {
    val result = both.run(env) eval RNG.fromTime
    putStrLn(result.toString)
  }
}
