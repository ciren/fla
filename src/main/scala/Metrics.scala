package fla

import scalaz._
import Scalaz._
import scalaz.{Maybe,NonEmptyList}
import spire.implicits._
import spire.math.Interval
import spire.math.{abs,log,pow,sqrt}
import spire.algebra.{Field,NRoot}

import cilib._
import cilib.Sized._

object Metrics {

  type FunctionMetric[A,R] = NonEmptyList[Position[A]] => Step[A,\/[String,R]]
  type SimpleFunctionMetric[A] = FunctionMetric[A,A]

  def toSized2And[A](x: NonEmptyList[A]): \/[String,Sized2And[List,A]] =
    x.toList match {
      case a :: b :: rest => Sized2And(a, b, rest).right[String]
      case _ => "Cannot convert input to Sized2And".left[Sized2And[List,A]]
    }

  def euclid[A:NRoot](a: Position[A], b: Position[A])(implicit A: Field[A]) =
    sqrt((a - b).pos.map(_ ** 2).foldLeft(A.zero)(_ + _))

  def fitnesses(solutions: NonEmptyList[Position[Double]]): \/[String,NonEmptyList[Double]] =
    solutions
      .traverse(_.objective)
      .toRight("Points have not been evaluated")
      .flatMap(_.traverseU(_ match {
        case Single(f, _) => f.fold(_.original.v, _.v, _.v).right[String]
        case Multi(_) => "Multi objective solutions are not supported".left[Double]
      }))

  def fittest[A](solutions: NonEmptyList[Position[A]], comp: Comparison): \/[String,Position[A]] =
    solutions
      .traverse(s => s match {
        case Solution(_, _, _) => s.just
        case Point(_, _) => Maybe.empty[Position[A]]
      })
      .toRight("Points have not been evaluated")
      .map(_.foldLeft1((a, b) => Comparison.compare(a, b) apply comp))

  def sort[A](solutions: NonEmptyList[Position[A]], comp: Comparison): \/[String,NonEmptyList[Position[A]]] =
    solutions
      .traverse(s => s match {
        case Solution(_, _, _) => s.just
        case Point(_, _) => Maybe.empty[Position[A]]
      })
      .toRight("Points have not been evaluated")
      .map(_.sortWith((a, b) => Comparison.fittest(a, b) apply comp))

  def dispersion(threshold: Double): SimpleFunctionMetric[Double] =
    solutions => Step.liftK { o =>
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

  val fem: SimpleFunctionMetric[Double] =
    solutions => {

      def S(e: Double, x: Sized2And[List,Double]): List[Int] = {
        def calcS(a: Double, b: Double): Int =
          if (b - a < -e) -1
          else if (math.abs(b - a) <= e) 0
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

  val fdc: SimpleFunctionMetric[Double] =
    solutions => Step.liftK { o =>
      val fits = fitnesses(solutions)
      val best = fittest(solutions, o)

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

  val informationLandscape: SimpleFunctionMetric[Double] =
    solutions => Step.liftK { o =>

      val fits = fitnesses(solutions)
      val F1 = fits flatMap toSized2And

      def il(a: Double, b: Double) =
        if (a < b) 1.0
        else if (a == b) 0.5
        else 0

      def ilVector(x: Sized2And[List,Double]): List[Double] =
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

  def gradients(s: Double): FunctionMetric[Double,List[Double]] =
    solutions => Step.liftK { o =>
      val sorted = sort(solutions, o)
      val fits = sorted flatMap fitnesses

      val fMax = fits.map(_.last)
      val fMin = fits.map(_.head)
      val fitRange = (fMax |@| fMin) { _ - _ }

      val domainRange = solutions.head.boundary.map(i => i.upperValue - i.lowerValue).suml
      val deltaX = s / domainRange

      fitnesses(solutions).flatMap(_.toList.sliding(2).toList.traverseU {
        case Seq(s1, s2) => for {
          fr    <- fitRange
          deltaY = (s2 - s1) / fr
        } yield deltaY / deltaX
      })
    }

  def gradientAvg(s: Double): SimpleFunctionMetric[Double] =
    solutions =>
      for {
        g  <- gradients(s)(solutions)
        sum = g.map(_.map(abs).sum)
      } yield sum.map(_ / (solutions.count))

  def gradientDev(s: Double): SimpleFunctionMetric[Double] =
    solutions =>
      for {
        a     <- gradientAvg(s)(solutions)
        g     <- gradients(s)(solutions)
        dev    = (a |@| g) { (ai, gs) => gs.map(gt => (ai - abs(gt)) ** 2).sum }
      } yield dev.map(dv => sqrt(dv / (solutions.count - 1 - 1)))

  def gradientMax(s: Double): SimpleFunctionMetric[Double] =
    solutions =>
      for {
        g <- gradients(s)(solutions)
      } yield g.map(_.map(abs).max)

}
