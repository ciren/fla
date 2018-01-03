package fla
package metrics

import scalaz.Scalaz._

import spire.math.sqrt

import cilib._
import Helpers._

object Dispersion {
  def apply(threshold: Double) = metric(threshold)

  val metric: Double => SimpleFunctionMetric[Double] =
    threshold => solutions => Step.withCompare { o =>
      val dimension = solutions.head.boundary.size
      // approximation of full dispersion
      val fullDispersion = sqrt(3.0 * dimension) / 4.0 - 0.1

      val amount = (solutions.size * threshold).toInt
      if (amount > solutions.count)
        "Threshold is a percentage and must be a number between 0 and 1".left[Double]
      else {
        val bestPoints = sort(solutions, o).map(_.toList take amount)

        def normalise(p: Position[Double]) = {
          val norm = (p.pos zip p.boundary).map { case (xi, bound) =>
            val upper = bound.upperValue
            val lower = bound.lowerValue
            (xi - lower) / (upper - lower)
          }
          Point(norm, p.boundary)
        }

        for {
          best       <- bestPoints
          normalised  = best map normalise
          dist        = normalised.sliding(2).toList.map { case Seq(a, b) => euclid(a, b) }
          avg        <- dist.length match {
            case 0 => "No points to calculate distance. Try increasing the dispersion threshold".left[Double]
            case l => (dist.sum / l).right[String]
          }
        } yield avg - fullDispersion
      }
    }
}
