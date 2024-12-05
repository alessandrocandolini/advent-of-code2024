[![CI](https://github.com/alessandrocandolini/advent-of-code2024/actions/workflows/ci.yml/badge.svg)](https://github.com/alessandrocandolini/advent-of-code2024/actions/workflows/ci.yml) [![codecov](https://codecov.io/gh/alessandrocandolini/advent-of-code2024/graph/badge.svg?token=yDHcPy0Gtx)](https://codecov.io/gh/alessandrocandolini/advent-of-code2024)

# advent-of-code2024

## How to build and run locally

The project uses the [Haskell tool stack](https://docs.haskellstack.org/en/stable/README/). The recommended way to install stack is by using [ghcup](https://www.haskell.org/ghcup/), although it's also possible to use [the nix package manager](https://nixos.org/). 

Assuming `stack` is installed in the system, to **build** the project use 
```
stack build
```
To **build and run the tests**, run
```
stack test
```
which is equivalent to
```
stack build --test
```
For **faster feedback loop** during development, it's possible to run tests continuously on every file change: 
```
stack test --fast --file-watch
```
To run tests with **test coverage** instrumentation, 
```
stack test --coverage
```
which generates a textual and HTML report. Tests are automatically run in the CI and test coverage reports are uploaded to codecov. 

To **run the executable via stack**,
```
stack exec aoc2024
```
or passing arguments
```
stack exec aoc2024 -- run -d <day> -f <filename> 
```
To run the **benchmarks**
```
stack bench --benchmark-arguments="--output report.html"
```
which generates a `report.html` HTML report. 
Benchmarks are NOT run as part of the CI, to keep the CI fast.

To **install** the executable under `~/.local/bin`, 
```
stack install
```
and the executable can be run with `aoc2024` or passing arguments like 
```
aoc2024 run -d 1 -f inputs/day1
```
assuming `~/.local/bin` is in the `$PATH` variable. 

To run a version of **ghci** compatible with the resolver 
```
stack ghci
```
For more information, refer to the `stack` official docs.
