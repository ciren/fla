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

  val walks = for {
    simple      <- SimpleRandomWalk(domain, steps, stepSize)
    progressive <- RandomProgressiveWalk(domain, steps, stepSize)
    manhattan   <- RandomProgressiveManhattanWalk(domain, steps, stepSize)
  } yield (simple, progressive, manhattan)

  override val runc: IO[Unit] = {
    val result = walks eval RNG.fromTime
    putStrLn(result.toString)
  }
}
