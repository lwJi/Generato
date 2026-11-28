# Generato

Wolfram Language code generator that produces C/C++ from symbolic tensor expressions using xAct.

## Running

```bash
export GENERATO="/path/to/repo"
Generato test.wl                    # Single file
Generato file1.wl file2.wl          # Multiple files
```

## Design rules

Before adding code, read `agent_docs/design-rules.md`, the rules the code is being arranged to.

## Architecture

A script builds a value: setters, declarations, bindings, equations and one `Generate[backend, file, program]` call. Four stages turn it into a file: declare, bind, lower, render.

### Core (src/)
- **Generato.wl** - Package loader: loads the files from Failure.wl to Stencil.wl, in the order listed
- **Failure.wl** - `Stop`, the one error path: a message, then a throw tagged `GeneratoFailure`
- **Context.wl** - User configuration in `$CurrentContext`; one `defineConfig` call per key
- **Variable.wl** - declare: var specs, `DefTensors`, the variable records
- **Component.wl** - bind: `GridTensors`, `TileTensors`, `TempTensors`, the component records
- **Equation.wl** - `SetEQN`/`SetEQNDelayed`, right-hand-side evaluation
- **Backend.wl** - `DefineBackend`, the shared formatters, `Render`
- **Emit.wl** - `Emit`, the one write
- **Program.wl** - `Load`, `Store`, `Generate` (check, lower, render, emit); stubs for the removed names
- **Stencil.wl** - Finite difference stencil coefficients
- **Driver.wl** - What the `Generato` wrapper runs: clears a stale output, sets the exit status

### Backends (codes/)
Each file is one `DefineBackend` call, a table of formatters keyed by kind (`"Load"`, `"Store"`), mode and storage:
- CarpetXGPU.wl, AMReX.wl, Nmesh.wl

### Key Patterns
- **State**: `$CurrentContext` holds user configuration only, and a key is written by its setter. A variable's facts are its record in Variable.wl; the mode of a step is an argument of its node
- **Stages**: declare → bind → lower → render; a formatter reads the statement it is handed and nothing else
- **Errors**: Raise with `Stop[symbol::tag, args...]`. Only Driver.wl catches a failed run and exits
- **Equations**: Use `SetEQN` normally; use `SetEQNDelayed` when RHS contains `If` statements

## Tests

```bash
wolframscript -script test/AllTests.wl              # All tests
wolframscript -script test/AllTests.wl --unit-only  # Unit tests only
```

## Tips

Shell escaping with backticks causes errors. Use pipe instead:
```bash
echo 'Needs["Generato`"]; Print["success"]' | wolframscript
```
