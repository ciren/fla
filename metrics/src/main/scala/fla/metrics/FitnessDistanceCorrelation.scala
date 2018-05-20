package fla
package metrics

import scalaz._
import scalaz.Scalaz._

import spire.math.sqrt
import spire.implicits._

import cilib._
import Helpers._

object FitnessDistanceCorrelation {

  def apply(solutions: NonEmptyList[Position[Double]]) = metric(solutions)

  val metric: SimpleFunctionMetric[Double] =
    solutions => {
      val fits = fitnesses(solutions)
      val best = fittest(solutions)

      for {
        fs    <- fits
        b     <- best
        fbar   = fs.suml / fs.length
        dstar  = solutions.map(s => euclid(s, b))
        dbar   = dstar.suml / dstar.length
        numer  = (fs zip dstar).map { case (fi, di) => (fi - fbar) * (di - dbar) }.suml
        denom1 = sqrt(fs.map(fi => (fi - fbar) ** 2).suml)
        denom2 = sqrt(dstar.map(di => (di - dbar) ** 2).suml)
      } yield numer / (denom1 * denom2)
    }

}
