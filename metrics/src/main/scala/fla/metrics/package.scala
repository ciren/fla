package fla

import scalaz.{NonEmptyList,\/}

import cilib.{Position,Step}

package object metrics {
  type FunctionMetric[A,R] = NonEmptyList[Position[A]] => Step[A,String \/ R]
  type SimpleFunctionMetric[A] = FunctionMetric[A,A]
}
