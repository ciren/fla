package fla
package walks

import scalaz._
import Scalaz._

import spire.math.Interval
import spire.implicits._

import cilib._

object RandomProgressiveManhattanWalk {

  def apply(domain: NonEmptyList[Interval[Double]], steps: Int, stepSize: Double) =
    walk(domain, steps, stepSize)

  val walk: WalkGenerator =
    (domain, steps, stepSize) => {
      def doWalk: StateT[RVar, (StartingZones, WalkStep), WalkStep] =
        for {
          state <- S.get
          (zones, awalk) = state
          r <- hoist.liftM(Dist.uniformInt(Interval(0, domain.size - 1)))
          w = (zones zip awalk.pos zip domain).zipWithIndex.map {
            case (((si, wsi), interval), i)  =>
              if (i =/= r) (si, wsi)
              else {
                val inc = if (si) -1 else 1
                val wsRD = wsi + (inc * stepSize)
                if (interval.contains(wsRD)) (si, wsRD)
                else (!si, wsi - (inc * stepSize))
              }
          }
          newZone    = w.map(_._1)
          newPosList = w.map(_._2)
          newPos     = Point(newPosList, domain)
          _ <- S.put((newZone, newPos))
        } yield newPos

      Helpers.initialState(domain) flatMap { state =>
         doWalk
          .replicateM(steps - 1)
          .eval(state)
          .map(walks => NonEmptyList.nel(state._2, walks.toIList))
      }
    }

}
