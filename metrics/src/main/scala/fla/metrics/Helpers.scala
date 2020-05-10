package fla
package metrics

import scalaz.{NonEmptyList,\/}
import scalaz.Scalaz._

import spire.math.sqrt
import spire.implicits._

import cilib._

object Helpers {
  implicit val positionFoldable = Position.positionFoldable1

  def euclid = MetricSpace.minkowski[Position,Double,Double](2).dist _

  def toSized2And[A](x: NonEmptyList[A]): String \/ NonEmptyList[A] =
    if (x.length >= 2) x.right[String]
    else "Cannot convert input to Sized2And".left[NonEmptyList[A]]

  def fitnesses(solutions: NonEmptyList[Position[Double]]): Step[Double, NonEmptyList[Double]] =
    solutions
      .traverse(_.objective match {
        case Some(Single(f, _)) => Step.point(f.fold(_.original.v, _.v, _.v))
        case Some(Multi(_))     => Step.failString("Multi objective solutions are not supported")
        case None               => Step.failString("Unevaluated position located")
      })

  def fittest[A](solutions: NonEmptyList[Position[A]]): Step[Double, Position[A]] =
    solutions
      .traverse(_ match {
        case s @ Solution(_, _, _) => Step.point[Double,Position[A]](s)
        case Point(_, _) => Step.failString[Double, Position[A]]("One or more points have not been evaluated")
      })
      .flatMap(x => Step.withCompare(comp => x.foldLeft1((a, b) => Comparison.compare(a, b) apply comp)))

  def sort[A](solutions: NonEmptyList[Position[A]]): Step[A, NonEmptyList[Position[A]]] =
    solutions
      .traverse(_ match {
        case s @ Solution(_, _, _) => Step.point[A, Position[A]](s)
        case Point(_, _) => Step.failString[A, Position[A]]("One or more points have not been evaluated")
      })
      .flatMap(x => Step.withCompare(comp => x.sortWith((a, b) => Comparison.fitter(a, b) apply comp)))

  def mean(xs: NonEmptyList[Double]) =
    xs.suml1 / xs.count

  def dev(xs: NonEmptyList[Double]) = {
    val avg = mean(xs)
    val variance = xs.map(xi => (xi - avg) ** 2).suml1 / xs.count
    sqrt(variance)
  }

}
