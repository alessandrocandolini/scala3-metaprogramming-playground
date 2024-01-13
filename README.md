[![Scala CI](https://github.com/alessandrocandolini/scala3-metaprogramming-playground/actions/workflows/ci.yml/badge.svg)](https://github.com/alessandrocandolini/scala3-metaprogramming-playground/actions/workflows/ci.yml)

# scala3-metaprogramming-playground

## Run

To run unit tests
```shell
sbt test
```

To run integration tests
```shell
sbt it/test
```

To run the project through sbt
```shell
sbt run
```

## Build fat jar 

The project uses  [sbt-assembly](https://github.com/sbt/sbt-assembly) to create a "fat" jar
```
sbt assembly
```
The generated jar is `target/scala3-metaprogramming-playground.jar`

The fat jar can be run locally using
```
java -jar target/scala3-metaprogramming-playground.jar
```
