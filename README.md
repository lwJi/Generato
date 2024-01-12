# Generato

Generate C/C++ code from tensor expression and so on (using xAct)

## Pre-Requisites

* Install free Wolfram Engine for Devolopers from https://www.wolfram.com/developer/.
(If you are rich, you can install the totally worth-it Mathematica instead, versions that support 'wolframscript'.)

* Download xAct from http://www.xact.es and install it properly.

## Installation

* clone repo: `git clone git@github.com:lwJi/Generato.git`

* set enviroment variable: `export GENERATO="/your/repo/path"`

## Example

* Small toy

```bash
wolframscript -f test/unit/test.wl
```

* GHG formulation of Einstein's equations

Please take a look at the example script for GHG system: [test/integration/GHG_rhs.ipynb](https://github.com/lwJi/Generato/blob/main/test/integration/GHG_rhs.ipynb). Or

```bash
wolframscript -f test/integration/GHG_rhs.wl
```

## Recomendations

* Use lower case to name variables (to aviod conflict with say `Pi`)

## Tips

* Use `SetEQNDelayed` if your right-hand side (rhs) includes an `If` statement to handle different component cases. Otherwise, `SetEQN` is preferred because it has an option, `CheckRHS`, which checks if all the terms appearing in the rhs are well-defined.

* `IsDefined[term]` is very useful for debugging.
