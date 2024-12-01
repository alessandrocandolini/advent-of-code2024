[![CI](https://github.com/alessandrocandolini/advent-of-code2024/actions/workflows/ci.yml/badge.svg)](https://github.com/alessandrocandolini/advent-of-code2024/actions/workflows/ci.yml) [![codecov](https://codecov.io/gh/alessandrocandolini/advent-of-code2024/graph/badge.svg?token=yDHcPy0Gtx)](https://codecov.io/gh/alessandrocandolini/advent-of-code2024)

# advent-of-code2024

## How to build and run locally

The project uses the [Haskell tool stack](https://docs.haskellstack.org/en/stable/README/).

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
To run with **test coverage**
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
stack exec aoc2024 -- -d <day> -f <filename> 
```

For **faster feedback loop**,
```
stack test --fast --file-watch
```

To run **benchmarks**
```
 stack bench --benchmark-arguments="--output report.html"
```
Benchmarks are run by the CI, and reports are only uploaded as part of the CI artifacts, ie, there is no way to track performance evolution. 

To install the executable under `~/.local/bin`, 
```
 stack install
```
and the executable can be run with `aoc2024` or passing arguments like `aoc2024 -d 1 -f inputs/day1`, assuming `~/.local/bin` is in the `$PATH` variable. 

To run **ghci** (with a version compatible with the resolver) run
```
stack ghci
```
For more information, refer to the `stack` official docs.
