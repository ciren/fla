package fla
package walks

import scalaz._
import Scalaz._

import spire.math.Interval
import spire.implicits._

import cilib._

object RandomProgressiveWalk {

  def apply(domain: NonEmptyList[Interval[Double]], steps: Int, stepSize: Double) =
    walk(domain, steps, stepSize)

  val walk: WalkGenerator =
    (domain, steps, stepSize) => {
      def doWalk: StateT[RVar, (StartingZones, WalkStep), WalkStep] =
        for {
          state <- S.get
          (zones, awalk) = state
          w <- hoist.liftM((zones zip awalk.pos zip domain).traverse1 {
            case ((s1, ws1), i) =>
              val r = Dist.uniform(Interval(0.0, stepSize)).map(ri => if (s1) -ri else ri)
              r.map(ws1 + _).map { wss =>
                if (wss < i.lowerValue) (!s1, i.lowerValue + (i.lowerValue - wss))
                else if (wss > i.upperValue) (!s1, i.upperValue - (wss - i.upperValue))
                else (s1, wss)
              }
          })
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
