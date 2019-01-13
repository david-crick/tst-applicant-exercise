# dcrick
# TST applicant exercise for dcrick

#### Prerequisites

- Install JDK 8, either the [Oracle version](http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html) or [OpenJDK](http://openjdk.java.net/projects/jdk8/)
- Install [Scala](http://scala-lang.org/download/)
- Install [SBT](http://www.scala-sbt.org/download.html)

##### Get the repository
First of all, either clone the repository via git

```sh
$ git clone https://github.com/tst-applicant-excercise/dcrick.git
```

#### Running the app

Go into the project's root directory, run `sbt run`

```sh
$ sbt run
```

`
PROBLEM 1
=========
BEST GROUP PRICES
List(BestGroupPrice(CB,M1,230.0,Military), BestGroupPrice(CB,S1,245.0,Senior), BestGroupPrice(CA,M1,200.0,Military), BestGroupPrice(CA,S1,225.0,Senior))

PROBLEM 2
=========
ALL PROMOTION COMBINATIONS
List(PromotionCombo(Vector(P3, P5, P4)), PromotionCombo(Vector(P3, P2)), PromotionCombo(Vector(P5, P1, P4)), PromotionCombo(Vector(P2, P1)))

COMBINABLE PROMOTIONS FOR P1
List(PromotionCombo(Vector(P1, P5, P4)), PromotionCombo(Vector(P1, P2)))

COMBINABLE PROMOTIONS FOR P3
List(PromotionCombo(Vector(P3, P5, P4)), PromotionCombo(Vector(P3, P2)))
`
