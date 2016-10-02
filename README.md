# Autobahn

A tool for inferring strictness annotations in Haskell programs using a genetic algorithm.

## Build and Run

Autobahn uses [stack](https://github.com/commercialhaskell/stack) to build and run.
It has been tested with [stack lts-7.2](https://github.com/fpco/lts-haskell/blob/master/lts-7.2.yaml).

Build with

```
> stack init --solver
> stack setup
> stack build
> stack install
```

For newer versions of GHC, `stack init` may fail to resolve the necessary packages,
in which case you will need to initialize with `stack init --solver --install-ghc`.

WARNING: Autobahn will overwrite the original source of your project. You should
copy or clone your project into a fresh / temporary directory.

Then run the executable with e.g. (assuming `$HOME/.local/bin` is in your PATH):

```
> cd test/hello
> Autobahn
```

Autobahn takes either a configuration file, named `config.atb` in the present working
directory, or asks the user for inputs on the command line to craft a configuration
in the event that `config.atb` is not found.

### Limitations

Currently,

- Autobahn requires the program builds with cabal.
- You must specify `ghc-options: -rtsopts "-with-rtsopts=-T"` in your `executable` section.
- The name of the project and the basename of `main-is` must be the same
- Any files that Autobahn profiles must already have the `BangPatterns` extension enabled
  in the `LANGUAGE` section at the top of each file.

See `test/hello` for a minimal example, as taken and modified from
[http://www.haskell.org/hello/](http://www.haskell.org/hello/). We are working on making
Autobahn more flexible in terms of the cabal configurations Autobahn will accept.

### Config File Specification

The specification of a configuration file for a run of Autobahn is as follows

- *projectDirectory* : The path to the source files.
- *budgetTime* : The number of hours Autobahn can spend optimizing the program.
  While in hours, decimal numbers can be used. Autobahn uses this to determine
  - number of generations (rounds) to evolve the program
  - number of chromosomes (programs) to create per generation
  - number of chromosomes to archive (save) from a generation
- *coverage* : The files Autobahn will add strictness annotations to.
- *targetMetric* : The metric Autobahn will optimize the program for. The current choices are
  - "peakAlloc" : Autobahn will minimize the bytes allocated by the program
  - "runtime" : The runtime of the program
  - ""gc" : The time a program spends in garbage collection
- *inputArg* : The input the program needs to run

#### Example Config File

When Pat and Chris want to run their program `bintree` through Autobahn, they
create the file below called `config.atb`.

```
projectDirectory = "/home/pat/haskell-projects/bintree"
budgetTime = 0.1h
coverage = "Main.hs"
targetMetric = "peakAlloc"
inputArg = "5 7 8"
```

## Output

Autobahn will display statistics about each generation on the command line as it goes.

The final output appears in the project directory under two directories

- autobahn-survivor : Holds the edited sources of the best scoring program
- autobahn-results  : Holds an HTML file detailing the specific configuration Autobahn used
                      as well as the results of all the programs in the final round that did
                      not time out or end in error
