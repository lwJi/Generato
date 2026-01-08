# Generato

Generate C/C++ Code from Tensor Expression and So On

## Pre-Requisites

* Install free Wolfram Engine for Devolopers from https://www.wolfram.com/developer/.
(If you are rich, you can install the totally worth-it Mathematica instead, versions that support `wolframscript`.)

* Download xAct from http://www.xact.es and install it properly.

## Installation

* clone repo: `git clone https://github.com/lwJi/Generato.git`

* set enviroment variable: `export GENERATO="/your/repo/path"`

## Example

* Small toy

```bash
cd test/CarpetX

Generato test.wl
```

* GHG formulation of Einstein's equations

Please take a look at the example script for GHG system: [test/Nmesh/GHG_rhs.ipynb](https://github.com/lwJi/Generato/blob/main/test/Nmesh/GHG_rhs.ipynb). Or

```bash
cd test/Nmesh

Generato GHG_rhs.wl
```

## Recomendations

* Use lower case to name variables (to aviod conflict with say `Pi`)

## Tips

* Use `SetEQNDelayed` if your right-hand side (rhs) includes an `If` statement to handle different component cases. Otherwise, `SetEQN` is preferred because it has an option, `CheckRHS`, which checks if all the terms appearing in the rhs are well-defined.

* `IsDefined[term]` is very useful for debugging.

## Running Tests

```bash
# Run all tests (recommended - runs unit tests + regression tests)
wolframscript -script test/AllTests.wl

# Run with verbose output
wolframscript -script test/AllTests.wl --verbose

# Run unit tests only (skip regression tests)
wolframscript -script test/AllTests.wl --unit-only

# Alternative: Run via bash script
./test/run_tests.sh
./test/run_tests.sh --verbose

# Update golden files with new outputs
./test/run_tests.sh --generate
```
