[![CI](https://github.com/alessandrocandolini/advent-of-code2024/actions/workflows/ci.yml/badge.svg)](https://github.com/alessandrocandolini/advent-of-code2024/actions/workflows/ci.yml) [![codecov](https://codecov.io/gh/alessandrocandolini/advent-of-code2024/graph/badge.svg?token=yDHcPy0Gtx)](https://codecov.io/gh/alessandrocandolini/advent-of-code2024)

# advent-of-code2024

## How to build and run locally

The project uses the [Haskell tool stack](https://docs.haskellstack.org/en/stable/README/).

Assuming `stack` is installed in the system, the project can be build by running
```
stack build
```
To build and also run the tests, run
```
stack test
```
which is equivalent to
```
stack build --test
```
To run with test coverage
```
stack test --coverage
```
which generates a textual and HTML report.

To run the executable,
```
stack exec advent-of-code2024-exe
```
or passing arguments
```
stack exec advent-of-code2024-exe -- -d <day> -f <filename> 
```

For faster feedback loop,
```
stack test --fast --file-watch
```
To run `ghci` (with a version compatible with the resolver) run
```
stack ghci
```
For more information, refer to the `stack` official docs.
