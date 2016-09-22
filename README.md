# Autobahn

A tool for inferring strictness annotations in Haskell programs using a genetic algorithm.

## Build and Run

Autobahn uses stack to build and run.

Build with

```
> stack setup
> stack build
```

Then run the executable with

```
> stack exec Autobahn
```

Autobahn will either take a configuration file or ask the user for inputs on the command line
to craft a configuration.

Currently, Autobahn requires the program builds with cabal.

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
