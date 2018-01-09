package fla
package metrics

import scalaz.NonEmptyList
import scalaz.Scalaz._

import spire.math.abs
import spire.implicits._

import cilib._
import Helpers._

object InformationLandscape {

  def apply(solutions: NonEmptyList[Position[Double]]) = metric(solutions)

  val metric: SimpleFunctionMetric[Double] =
    solutions => Step.withCompare { o =>

      val fits = fitnesses(solutions)
      val F1 = fits flatMap toSized2And

      def il(a: Double, b: Double) =
        if (a < b) 1.0
        else if (a == b) 0.5
        else 0

      def ilVector(x: NonEmptyList[Double]): List[Double] =
        x.toList.sliding(2).toList.map { case Seq(f1, f2) => il(f1, f2) }

      val best = fittest(solutions, o)
      val pointsIlVector = F1 map ilVector

      val sphere = (reference: Position[Double]) => (x: Position[Double]) =>
         (x zip reference) foldMap1 { case (pi, bi) => (pi - bi) ** 2 }

      val sphereSolutions = best map { solutions map sphere(_) }
      val sphereSolutionsIlVector = sphereSolutions flatMap { toSized2And(_) map ilVector }

      def dist(a: List[Double], b: List[Double]) =
        (a zip b).map { case (ai, bi) => abs(ai - bi) }.sum / a.length

      for {
        pv <- pointsIlVector
        sv <- sphereSolutionsIlVector
      } yield dist(pv, sv)
    }
}
