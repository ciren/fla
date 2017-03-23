# fla
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

The random walk algorithms have the type:

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
val points: RVar[NonEmptyList[Position[Double]]] = for {
  first <- Position.createPosition(domain)
  rest  <- Position.createPositions(domain, 99)
} yield NonEmptyList(first, rest: _*)

// evaluate points and apply function metric to them
val dispersion = for {
  ps        <- Step.pointR(points)
  solutions <- ps traverseU Step.evalF[Double]
  metric    <- Dispersion(.1)(solutions)
} yield metric

// minimazation problem
val min     = Comparison dominance Min
// spherical benchmark function
val problem = Eval unconstrained Benchmarks.spherical[NonEmptyList, Double]

// evaluate the metric using a time-seeded RNG
val result = dispersion.run(min)(problem) eval RNG.fromTime
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
val result = both eval RNG.fromTime
```
