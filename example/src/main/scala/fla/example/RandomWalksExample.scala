package fla
package example

import scalaz.Scalaz._
import scalaz.effect._
import scalaz.effect.IO.putStrLn

import spire.math.Interval
import spire.implicits._

import cilib._
import walks._

object RandomWalksExample extends SafeApp {
  val domain = Interval(-10.0, 10.0)^2
  val steps = 20
  val stepSize = 1.0

  val progressive = RandomProgressiveWalk(domain, steps, stepSize)
  val manhattan   = RandomProgressiveManhattanWalk(domain, steps, stepSize)

  val both = (progressive |@| manhattan) { (_, _) }

  override val runc: IO[Unit] = {
    val result = both eval RNG.fromTime
    putStrLn(result.toString)
  }
}
