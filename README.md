# functional-kniffle
This is a console version of the kniffel / yahtzee game.
sbt run to play.
Alternately - and recommended - you can build a native image:
sbt stage
sbt universal:packageBin
target/universal/stage/bin/kniffle
the last command will start an instance up quickly without having to wait for sbt to start
