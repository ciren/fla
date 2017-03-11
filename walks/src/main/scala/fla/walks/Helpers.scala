package fla
package walks

import scalaz.NonEmptyList
import scalaz.Scalaz._

import spire.math.Interval
import spire.implicits._

import cilib._

object Helpers {

  def initialState(domain: NonEmptyList[Interval[Double]]) = {
    // random bits indicating starting zone per dimension
    val start: RVar[StartingZones] = domain.traverse(_ => RVar.next[Boolean])

    val zipped = start.map(s => s.zip(domain).zipWithIndex)

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

    (start |@| walk0) { (_, _) }
  }

}
