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

  def fitnesses(solutions: NonEmptyList[Position[Double]]): String \/ NonEmptyList[Double] =
    solutions
      .traverse(_.objective)
      .toRightDisjunction("Points have not been evaluated")
      .flatMap(_.traverseU(_ match {
        case Single(f, _) => f.fold(_.original.v, _.v, _.v).right[String]
        case Multi(_)     => "Multi objective solutions are not supported".left[Double]
      }))

  def fittest[A](solutions: NonEmptyList[Position[A]], comp: Comparison): String \/ Position[A] =
    solutions
      .traverseU(_ match {
        case s @ Solution(_, _, _) => s.right[String]
        case Point(_, _) => "One or more points have not been evaluated".left[Position[A]]
      })
      .map(_.foldLeft1((a, b) => Comparison.compare(a, b) apply comp))

  def sort[A](solutions: NonEmptyList[Position[A]], comp: Comparison): String \/ NonEmptyList[Position[A]] =
    solutions
      .traverseU(_ match {
        case s @ Solution(_, _, _) => s.right[String]
        case Point(_, _) => "One or more points have not been evaluated".left[Position[A]]
      })
      .map(_.sortWith((a, b) => Comparison.fittest(a, b) apply comp))

  def mean(xs: NonEmptyList[Double]) =
    xs.suml1 / xs.count

  def dev(xs: NonEmptyList[Double]) = {
    val avg = mean(xs)
    val variance = xs.map(xi => (xi - avg) ** 2).suml1 / xs.count
    sqrt(variance)
  }

}
