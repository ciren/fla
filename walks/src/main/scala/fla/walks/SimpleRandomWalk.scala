package fla
package walks

import scalaz._
import Scalaz._

import spire.math.Interval
import spire.implicits._

import cilib._

object SimpleRandomWalk {

  val S = StateT.stateTMonadState[WalkStep, RVar]
  val hoist = StateT.StateMonadTrans[WalkStep]

  def apply(domain: NonEmptyList[Interval[Double]], steps: Int, stepSize: Double) =
    walk(domain, steps, stepSize)

  val walk: WalkGenerator =
    (domain, steps, stepSize) => {
      def doWalk: StateT[RVar, WalkStep, WalkStep] =
        for {
          awalk <- S.get
          w <- hoist.liftM((awalk.pos zip domain).traverse1 {
            case (ws1i, i) =>
              // repeat-until as described by K.Malan
              // probably want a more efficient way to do this
              def randomDimensionInBounds: RVar[Double] = {
                Dist.uniform(Interval(-stepSize, stepSize)) flatMap { r =>
                  val dim = ws1i + r
                  if (i.contains(dim)) RVar.point(dim)
                  else randomDimensionInBounds
                }
              }
              randomDimensionInBounds
          })
          newPos = Point(w, domain)
          _ <- S.put(newPos)
        } yield newPos

      Position.createPosition(domain) flatMap { state =>
        doWalk
          .replicateM(steps - 1)
          .eval(state)
          .map(walks => NonEmptyList.nel(state, walks.toIList))
      }
    }
}
