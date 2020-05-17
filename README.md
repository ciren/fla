# fla

[![codecov.io](https://codecov.io/github/cirg-up/fla/coverage.svg?branch=master)](https://codecov.io/github/cirg-up/fla?branch=master)

A collection of traversal algorithms and function metrics used in Fitness Landscape Analysis

All function metrics have been implemented for use on continuous-domain problems.

## What's included

Currently all function metrics and random walk algorithms have been implemented from techniques described in *Characterising Continuous Optimisation Problems for Particle Swarm Optimisation Performance Prediction* by K. Malan.

### Function Metrics

The following function metrics are included:

* Dispersion
* First Entropic Measure
* Fitness Cloud Index
* Fitness Distance Correlation
* Gradient Measures (mean, max, std. deviation)
* Information Landscape

### Random Walks

The following random walk algorithms are included:

* Simple Random Walk
* Progressive Random Walk
* Manhattan Progressive Random Walk

## Definition

Function metrics are defined as the following:

```scala
type FunctionMetric[A,R] = NonEmptyList[Position[A]] => Step[A,String \/ R]
```

or in most cases, simply:

```scala
type SimpleFunctionMetric[A] = FunctionMetric[A,A]
```

The random walk algorithms have the types:

```scala
type Walk = NonEmptyList[Position[Double]]
type WalkGenerator = (NonEmptyList[Interval[Double]], Int, Double) => RVar[Walk]
```

## Usage

Function metrics are used in the following way:

```scala
/*
 * The following example calculates dispersion for the
 * spherical problem using 100 points in the domain [-10, 10]^2
 */

val domain = Interval(-10.0, 10.0)^2

// generate list of points
val points = Position.createPositions(domain, 100)

// evaluate points and apply function metric to them
val dispersion = for {
  ps        <- Step.pointR(points)
  solutions <- ps traverse Step.evalP[Double]
  metric    <- Dispersion(.1)(solutions)
} yield metric

// define fitness landscape
val f = Benchmarks.spherical[nat._2,Double] _

// create environment
val env = Environment(
  cmp    = Comparison dominance Min,
  eval   = f.unconstrained.eval,
  bounds = domain
)

// evaluate the metric using a time-seeded RNG
val result = dispersion.run(env) eval RNG.fromTime
```

Random walks can be generated in the following way:

```scala
/*
 * The following example generates a progressive random walk
 * of 100 points in the domain [-10, 10]^2, using a step size
 * of 1.0
 */

val domain = Interval(-10.0, 10.0)^2
val steps = 100
val stepSize = 1.0

val progressive = RandomProgressiveWalk(domain, steps, stepSize)
val result = progressive eval RNG.fromTime
```
