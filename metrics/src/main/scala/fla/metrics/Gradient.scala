package fla
package metrics

import scalaz.Scalaz._

import spire.math.{abs,sqrt}
import spire.implicits._

import cilib._
import Helpers._

object Gradient {
  val gradients: Double => FunctionMetric[Double,List[Double]] =
    stepSize => solutions => Step.withCompare { o =>
      val sorted = sort(solutions, o)
      val fits = sorted flatMap fitnesses

      val fMax = fits.map(_.last)
      val fMin = fits.map(_.head)
      val fitRange = (fMax |@| fMin) { _ - _ }

      val domainRange = solutions.head.boundary.map(i => i.upperValue - i.lowerValue).suml
      val deltaX = stepSize / domainRange

      fitnesses(solutions).flatMap(_.toList.sliding(2).toList.traverseU {
        case Seq(s1, s2) => for {
          fr    <- fitRange
          deltaY = (s2 - s1) / fr
        } yield deltaY / deltaX
      })
    }

  val avg: Double => SimpleFunctionMetric[Double] =
    stepSize => solutions =>
      for {
        g  <- gradients(stepSize)(solutions)
        sum = g.map(_.map(abs).sum)
      } yield sum.map(_ / (solutions.count))

  val dev: Double => SimpleFunctionMetric[Double] =
    stepSize => solutions =>
      for {
        a     <- avg(stepSize)(solutions)
        g     <- gradients(stepSize)(solutions)
        dev    = (a |@| g) { (ai, gs) => gs.map(gt => (ai - abs(gt)) ** 2).sum }
      } yield dev.map(dv => sqrt(dv / (solutions.count - 1 - 1)))

  val max: Double => SimpleFunctionMetric[Double] =
    stepSize => solutions =>
      for {
        g <- gradients(stepSize)(solutions)
      } yield g.map(_.map(abs).max)

}
