package fla
package metrics

import scalaz._
import scalaz.Scalaz._

import spire.math.abs
import spire.implicits._

import cilib._
import Helpers._

object InformationLandscape {

  def apply(solutions: NonEmptyList[Position[Double]]) = metric(solutions)

  val metric: SimpleFunctionMetric[Double] =
    solutions => {
      val fits = fitnesses(solutions)
      val F1 =
        fits.flatMap(x => toSized2And(x) match {
          case -\/(error) => Step.failString[Double, NonEmptyList[Double]](error)
          case \/-(value) => Step.point[Double, NonEmptyList[Double]](value)
        })

      def il(a: Double, b: Double) =
        if (a < b) 1.0
        else if (a == b) 0.5
        else 0

      def ilVector(x: NonEmptyList[Double]): List[Double] =
        x.toList.sliding(2).toList.map { case Seq(f1, f2) => il(f1, f2) }

      val best: Step[Double, Position[Double]] = fittest(solutions)
      val pointsIlVector: Step[Double,List[Double]] = F1 map ilVector

      val sphere = (reference: Position[Double]) => (x: Position[Double]) =>
         (x zip reference) foldMap1 { case (pi, bi) => (pi - bi) ** 2 }

      val sphereSolutions: Step[Double,scalaz.NonEmptyList[Double]] =
        best map { solutions map sphere(_) }

      val sphereSolutionsIlVector: Step[Double, List[Double]] =
        sphereSolutions.flatMap(x => toSized2And(x) match {
          case -\/(error) => Step.failString[Double, List[Double]](error)
          case \/-(value) => Step.point[Double, List[Double]](ilVector(value))
        })

      def dist(a: List[Double], b: List[Double]) =
        (a zip b).map { case (ai, bi) => abs(ai - bi) }.sum / a.length

      for {
        pv <- pointsIlVector
        sv <- sphereSolutionsIlVector
      } yield dist(pv, sv)
    }
}
