package fla
package metrics

import scalaz.{NonEmptyList,\/}
import scalaz.Scalaz._

import spire.algebra.{NRoot,Field}
import spire.math.sqrt
import spire.implicits._

import cilib._
import cilib.Sized.Sized2And

object Helpers {
  implicit val positionFoldable = Position.positionFoldable1
  def euclid = MetricSpace.minkowski[Position,Double,Double](2).dist _

  def toSized2And[A](x: NonEmptyList[A]): String \/ Sized2And[List,A] =
    x.toList match {
      case a :: b :: rest => Sized2And(a, b, rest).right[String]
      case _ => "Cannot convert input to Sized2And".left[Sized2And[List,A]]
    }

  def fitnesses(solutions: NonEmptyList[Position[Double]]): String \/ NonEmptyList[Double] =
    solutions
      .traverse(_.objective)
      .toRight("Points have not been evaluated")
      .flatMap(_.traverseU(_ match {
        case Single(f, _) => f.fold(_.original.v, _.v, _.v).right[String]
        case Multi(_) => "Multi objective solutions are not supported".left[Double]
      }))

  def fittest[A](solutions: NonEmptyList[Position[A]], comp: Comparison): String \/ Position[A] =
    solutions
      .traverseU(s => s match {
        case Solution(_, _, _) => s.right[String]
        case Point(_, _) => "One or more points have not been evaluated".left[Position[A]]
      })
      .map(_.foldLeft1((a, b) => Comparison.compare(a, b) apply comp))

  def sort[A](solutions: NonEmptyList[Position[A]], comp: Comparison): String \/ NonEmptyList[Position[A]] =
    solutions
      .traverseU(s => s match {
        case Solution(_, _, _) => s.right[String]
        case Point(_, _) => "One or more points have not been evaluated".left[Position[A]]
      })
      .map(_.sortWith((a, b) => Comparison.fittest(a, b) apply comp))
}
