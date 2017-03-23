package fla

import scalaz.{Kleisli,NonEmptyList,\/}

import cilib.{Mem,Position,Step}
import cilib.pso.Particle

package object metrics {
  type FunctionMetric[A,R] = NonEmptyList[Position[A]] => Step[A,String \/ R]
  type SimpleFunctionMetric[A] = FunctionMetric[A,A]

  type Alg[A,B] = Kleisli[Step[A,?],List[B],List[B]]
  type PSOAlg = Alg[Double,Particle[Mem[Double],Double]]
}
