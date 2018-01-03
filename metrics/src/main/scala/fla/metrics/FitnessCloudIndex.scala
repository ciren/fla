package fla
package metrics

import spire.math.abs
import spire.implicits._

import scalaz._
import Scalaz._

import cilib._
import cilib.pso._
import cilib.pso.PSO._

object FitnessCloudIndex {

  private[this] def createParticle(pos: Position[Double]): Step[Double,Entity[Mem[Double],Double]] = {
      // 4
      val z = (pos.pos zip pos.boundary) traverse { case (xi, i) =>
        val range = abs(i.upperValue - i.lowerValue)
        for {
          zi     <- Dist.gaussian(0.0, range * 0.1)
          xiStar  = xi + zi
        } yield {
          if (i contains xiStar) xiStar
          else if (xiStar < i.lowerValue) i.lowerValue
          else i.upperValue
        }
      }
      // 5, 6, 7, 10
      for {
        point    <- Step evalP pos
        newPoint <- Step pointR z.map(Point(_, pos.boundary))
        newSol   <- Step evalP newPoint
        sorted   <- Step.withCompare { o =>
          NonEmptyList(point, newSol).sortWith((a, b) => Comparison.fittest(a, b) apply o)
        }
      } yield Entity(Mem(sorted.head, pos.zeroed), sorted.last)
    }

  private[this] def inBounds[A](p: Entity[A,Double]) =
    Foldable1[NonEmptyList].foldLeft(p.pos.pos zip p.pos.boundary, true)((a,c) => a && (c._2.contains(c._1)))

  def apply(pso: PSOAlg) = metric(pso)

  val metric: PSOAlg => SimpleFunctionMetric[Double] =
    alg => solutions => {
      // 3
      val iteration0 = solutions traverseU createParticle

      for {
        i0     <- iteration0
        i1     <- alg run i0
        i2     <- alg run i1
        zipped  = (i0 zip i2).list.filter { case (i0i, i2i) => inBounds(i2i) }
        better <- Step.withCompare { o =>
          zipped filter { case (i0i, i2i) => Comparison.fittest(i2i.pos, i0i.pos) apply o }
        }
      } yield zipped.length match {
        case 0 => "Not enough final particles to calculate fitness cloud index".left[Double]
        case l => (better.length.toDouble / l).right[String]
      }
    }

  val cognitive: SimpleFunctionMetric[Double] = {
    val cognitiveIteration: NonEmptyList[Particle[Mem[Double],Double]] => Particle[Mem[Double],Double] => Step[Double,Particle[Mem[Double],Double]] =
      collection => x => {
        val w     = 0.7298
        val c1    = 1.496
        val guide = Guide.pbest[Mem[Double],Double]
        for {
          cog     <- guide(collection, x)
          v       <- singleComponentVelocity(x, cog, w, c1)
          p       <- stdPosition(x, v)
          p2      <- evalParticle(p)
          p3      <- updateVelocity(p2, v)
          updated <- updatePBestBounds(p3)
        } yield updated
      }

    metric(Iteration sync cognitiveIteration)
  }

  val social: SimpleFunctionMetric[Double] = {
    val socialIteration: NonEmptyList[Particle[Mem[Double],Double]] => Particle[Mem[Double],Double] => Step[Double,Particle[Mem[Double],Double]] =
      collection => x => {
        val w     = 0.7298
        val c1    = 1.496
        val guide = Guide.gbest[Mem[Double]]
        for {
          soc     <- guide(collection, x)
          v       <- singleComponentVelocity(x, soc, w, c1)
          p       <- stdPosition(x, v)
          p2      <- evalParticle(p)
          p3      <- updateVelocity(p2, v)
          updated <- updatePBestBounds(p3)
        } yield updated
      }

    metric(Iteration sync socialIteration)
  }

  val meanOfStdDev: Int => SimpleFunctionMetric[Double] =
    repeats => solutions => {
      val samples = (0 until repeats).toList.toNel.get
      val cognitives = samples traverseU { i => cognitive(solutions) } map { _.sequenceU }
      val socials    = samples traverseU { i => social(solutions)    } map { _.sequenceU }

      for {
        c <- cognitives
        s <- socials
      } yield (c |@| s) { (cs, ss) => (Helpers.dev(cs) + Helpers.dev(ss)) / 2 }
    }

}
