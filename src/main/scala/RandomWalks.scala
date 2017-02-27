package fla

import scalaz._
import Scalaz._

import spire.math.Interval
import spire.implicits._

import cilib._

object RandomWalks {

  type Walk = List[Position[Double]]
  type StartingZones = NonEmptyList[Boolean]
  type WalkStep = Position[Double]

  val S = StateT.stateTMonadState[(StartingZones, WalkStep), RVar]
  val hoist = StateT.StateMonadTrans[(StartingZones, WalkStep)]

  def progressive(domain: NonEmptyList[Interval[Double]], steps: Int, stepSize: Double) = {
    // random bits indicating starting zone per dimension
    val start: RVar[StartingZones] = domain.traverse(_ => RVar.next[Boolean])

    val zipped: RVar[NonEmptyList[((Boolean, Interval[Double]), Int)]] =
      start.map(s => s.zip(domain).zipWithIndex)

    val walk0: RVar[WalkStep] = zipped.flatMap(z => z.traverse {
      case ((b, di), i) =>
        val max = di.upperValue
        val min = di.lowerValue
        for {
          r  <- Dist.uniform(Interval(0.0, (max - min) / 2.0))
          rd <- Dist.uniformInt(Interval(0, domain.size - 1))
          w0  = if (b) max - r else min + r
        } yield
          if (rd =/= i) w0
          else if (b) max
          else min
    }).map(Point(_, domain))

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

    (start |@| walk0) { (_, _) } flatMap { state =>
       doWalk.replicateM(steps - 1).eval(state).map(walks => NonEmptyList.nel(state._2, walks.toIList))
    }
  }

  def progressiveManhattan(domain: NonEmptyList[Interval[Double]], steps: Int, stepSize: Double) = {
    // random bits indicating starting zone per dimension
    val start: RVar[StartingZones] = domain.traverse(_ => RVar.next[Boolean])

    val zipped: RVar[NonEmptyList[((Boolean, Interval[Double]), Int)]] =
      start.map(s => s.zip(domain).zipWithIndex)

    val walk0: RVar[WalkStep] = zipped.flatMap(z => z.traverse {
      case ((b, di), i) =>
        val max = di.upperValue
        val min = di.lowerValue
        for {
          r  <- Dist.uniform(Interval(0.0, (max - min) / 2.0))
          rd <- Dist.uniformInt(Interval(0, domain.size - 1))
          w0i = if (b) max - r else min + r
        } yield
          if (rd =/= i) w0i
          else if (b) max
          else min
    }).map(Point(_, domain))

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

    (start |@| walk0) { (_, _) } flatMap {
      state => doWalk.replicateM(steps - 1).eval(state).map(walks => NonEmptyList.nel(state._2, walks.toIList))
    }
  }
}
