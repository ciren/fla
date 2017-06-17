package fla
package metrics

import scalaz.NonEmptyList
import scalaz.Scalaz._

import spire.math.{abs,log,pow}
import spire.implicits._

import cilib._
import cilib.Sized.Sized2And
import Helpers._

object FirstEntropicMeasure {

  def apply(solutions: NonEmptyList[Position[Double]]) = metric(solutions)

  val metric: SimpleFunctionMetric[Double] =
    solutions => {

      def S(e: Double, x: Sized2And[List,Double]): List[Int] = {
        def calcS(a: Double, b: Double): Int =
          if (b - a < -e) -1
          else if (abs(b - a) <= e) 0
          else 1

          x.toList.sliding(2).toList.map { case Seq(a, b) => calcS(a, b) }
      }

      def isLandscapeFlat(x: List[Int]) = x.forall(_ === 0)

      def infoStability(x: Sized2And[List,Double]): Double = {
        // eb, es, e, eo
        type IS = (Double, Double, Double, Int)

        def fbo(is: IS): IS = {
          val (eb, es, e, eo) = is
          val s = S(e, x)
          if (isLandscapeFlat(s)) (eb, es / eb, e, eo)
          else fbo((eb, es * eb, es, eo + 1))
        }

        val is: IS = fbo((10, 10, 0, 0))

        val smallestStep = 0.01 * pow(10, is._4.toDouble)

        def nfp(is: IS): IS = {
          val (eb, es, e, eo) = is
          val s = S(e, x)
          if (isLandscapeFlat(s)) {
            if (es <= smallestStep) is
            else {
              val e1  = e - es
              val es1 = es / eb
              val e2  = e1 + es1
              nfp((eb, es1, e2, eo))
            }
          } else nfp((eb, es, e + es, eo))
        }

        val is1 = nfp(is)
        val e = is1._3
        e
      }

      def infoContent(e: Double, x: Sized2And[List, Double]) = {
        def hash(p: Int, q: Int): Int = {
          val x = -p + 2 * q
          if (x < 0) x + 3
          else x + 2
        }

        val s = S(e, x)

        val grouped = s.sliding(2).toList
          .filter  { case Seq(p, q) => p =/= q }
          .map     { case Seq(p, q) => hash(p, q) }
          .groupBy { x => x }

        val entropy = grouped.values.map(_.length).map { ent =>
          val prob = ent.toDouble / (s.length - 1)
          prob * (log(prob) / log(6))
        }

        -entropy.sum
      }

      val fits = fitnesses(solutions)

      val increment = 0.05
      val numEpsilons = (1.0 / increment).toInt + 1

      def getEpsilons(e: List[Double], mult: Double, i: Int, es: Double): List[Double] =
        if (i < numEpsilons) getEpsilons(e.updated(i, es * mult), mult + increment, i + 1, es)
        else e

      val max = for {
        f          <- fits
        x2         <- toSized2And(f)
        epsilonStar = infoStability(x2)
        epsilons    = getEpsilons(List.fill(numEpsilons)(0), 0.0, 0, epsilonStar)
        es          = epsilons map { e => infoContent(e, x2) }
      } yield es.max

      Step.point(max)
    }
}
