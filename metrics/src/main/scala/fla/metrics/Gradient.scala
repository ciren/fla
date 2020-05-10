package fla
package metrics

import scalaz.Scalaz._

import spire.math.{abs,sqrt}
import spire.implicits._

import cilib._
import Helpers._

object Gradient {
  val gradients: Double => FunctionMetric[Double,List[Double]] =
    stepSize => solutions => {
      val sorted = sort(solutions)
      val fits = sorted flatMap fitnesses

      val fMax = fits.map(_.last)
      val fMin = fits.map(_.head)
      val fitRange = (fMax |@| fMin) { _ - _ }

      val domainRange = solutions.head.boundary.map(i => i.upperValue - i.lowerValue).suml
      val deltaX = stepSize / domainRange

      fitnesses(solutions).flatMap(_.toList.sliding(2).toList.traverse {
        case Seq(s1, s2) => for {
          fr    <- fitRange
          deltaY = (s2 - s1) / fr
        } yield deltaY / deltaX
      })
    }

  val avg: Double => SimpleFunctionMetric[Double] =
    stepSize => solutions =>
      gradients(stepSize)(solutions)
        .map(_.map(abs).sum / solutions.count.toDouble)

  val dev: Double => SimpleFunctionMetric[Double] =
    stepSize => solutions => {
      (avg(stepSize)(solutions) |@| gradients(stepSize)(solutions)) { (ai, gs) =>
        val result = gs.map(gt => (ai - abs(gt)) ** 2).sum
        sqrt(result / (solutions.size - 1).toDouble)
      }
    }

  val max: Double => SimpleFunctionMetric[Double] =
    stepSize => solutions => gradients(stepSize)(solutions).map(_.map(abs).max)

}
