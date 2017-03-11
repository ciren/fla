package fla

import spire.math.Interval
import scalaz.{NonEmptyList,StateT}
import cilib.{Position,RVar}

package object walks {
  type Walk = NonEmptyList[Position[Double]]
  type StartingZones = NonEmptyList[Boolean]
  type WalkStep = Position[Double]
  type WalkGenerator = (NonEmptyList[Interval[Double]], Int, Double) => RVar[Walk]

  val S = StateT.stateTMonadState[(StartingZones, WalkStep), RVar]
  val hoist = StateT.StateMonadTrans[(StartingZones, WalkStep)]
}
