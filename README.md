# Generato

A Wolfram Language code generator that produces C/C++ from symbolic tensor expressions using xAct. Designed for numerical relativity and computational physics applications.

## Prerequisites

- [Wolfram Engine](https://www.wolfram.com/developer/) (free) or Mathematica
- [xAct](http://www.xact.es) tensor algebra package

## Installation

```bash
git clone https://github.com/lwJi/Generato.git
export GENERATO="/path/to/Generato"
```

## Quick Start

```wolfram
(* Load Generato *)
Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"}]]

(* Define 3D manifold and coordinates *)
DefManifold[M3, 3, IndexRange[a, z]];
DefChart[cart, M3, {1, 2, 3}, {X[], Y[], Z[]}, ChartColor -> Blue];
DefMetric[1, euclid[-i, -j], CD];
MetricInBasis[euclid, -cart, DiagonalMatrix[{1, 1, 1}]];

(* Declare tensors and bind them to grid storage *)
OutputVars = GridTensors[{rU[i], PrintAs -> "r"}];
InputVars = GridTensors[{uU[i], PrintAs -> "u"}];

(* Set equations *)
SetEQN[rU[i_], 2 uU[i]];

(* Generate code: the backend, the output file and the program *)
Generate["CarpetXGPU", "output.hxx", {
  Load[OutputVars, "Out"],
  Load[InputVars, "In"],
  "",
  Store[OutputVars, "Out"]
}];
```

Run with: `Generato script.wl`

The program is a list. A string is a line of output, `Load[varlist, mode]` brings the components of its variables into scope, and `Store[varlist, mode]` writes their equations. The file is written once, when every line of it has been built. A run that fails prints a message, writes no file, and makes `Generato` exit non-zero.

## Examples

**Simple example:**
```bash
cd test/regression/CarpetXGPU && Generato test.wl
```

**GHG formulation of Einstein's equations:**
- Notebook: [test/regression/Nmesh/GHG_rhs.ipynb](https://github.com/lwJi/Generato/blob/main/test/regression/Nmesh/GHG_rhs.ipynb)
- Script: `cd test/regression/Nmesh && Generato GHG_rhs.wl`

## Architecture

### A script is a value

A script is setters, declarations, bindings, equations and one `Generate` call. Each stage reads a value the stage before it produced:

1. **declare**: a var spec becomes a variable record with its rank, its symmetry and the variable it differentiates (`DefTensors`).
2. **bind**: a variable gets its storage and one record per independent component (`GridTensors`, `TileTensors`, `TempTensors`).
3. **lower**: `Generate` checks every node of the program, then turns each into one statement per component, with its right-hand side evaluated.
4. **render**: the backend table turns each statement into lines, and the file is written once.

### Core Modules (src/)

| Module | Purpose |
|--------|---------|
| Generato.wl | Package loader |
| Failure.wl | `Stop`, the one error path |
| Context.wl | User configuration (`$CurrentContext`), one `defineConfig` call per key |
| Variable.wl | Declarations: `DefTensors`, var specs, the variable records |
| Component.wl | Bindings: `GridTensors`, `TileTensors`, `TempTensors`, the component records |
| Equation.wl | `SetEQN`, `SetEQNDelayed`, right-hand-side evaluation |
| Backend.wl | `DefineBackend`, the shared formatters, `Render` |
| Emit.wl | The one write |
| Program.wl | `Load`, `Store`, `Generate` |
| Stencil.wl | Finite difference stencils |
| Driver.wl | What the `Generato` wrapper runs: it clears a stale output and sets the exit status |

### Backends (codes/)

Each file is one `DefineBackend` call: a table of formatters keyed by kind of step, mode and storage. The keys of a table are the modes the backend accepts.

| Backend | Output | Description |
|---------|--------|-------------|
| CarpetXGPU.wl | .hxx | GPU-enabled CarpetX |
| AMReX.wl | .hxx | AMReX AMR library |
| Nmesh.wl | .c | Structured mesh (C output) |

| Backend | `Load` mode | For variables bound by |
|---------|-------------|------------------------|
| CarpetXGPU | `"In"`, `"Out"` | `GridTensors`, `TileTensors` |
| CarpetXGPU | `"Derivs"` | `TempTensors`, declaring `DerivativeOf` |
| CarpetXGPU | `"Temp"` | `TempTensors` |
| AMReX | `"In"`, `"Out"` | `GridTensors` |
| Nmesh | `"In"`, `"MoreInOut"` | `GridTensors` |
| Nmesh | `"Out"` | `GridTensors`, declaring `TimeDerivativeOf` |
| Nmesh | `"Temp"` | `TempTensors` |

All three take the `Store` modes `"Out"`, `"AddTo"` and `"Temp"`, for any storage.

A script selects a backend by its name in the `Generate` call. It may register one of its own first, which states only what differs from the backend it extends:

```wolfram
DefineBackend["CarpetXPoint", <|
  "Extends" -> "CarpetXGPU",
  "Load" -> <|"In" -> <|"Grid" -> Function[statement, {...}]|>|>
|>];

Generate["CarpetXPoint", "output.hxx", {...}];
```

A formatter takes one statement, a record with the variable, the component and the configuration, and returns its lines. `test/regression/CarpetXGPU/testVariant.wl` is a complete example.

## API Reference

### Declaring and Binding Tensors

| Function | Description |
|----------|-------------|
| `DefTensors[var, ...]` | Declare tensors without binding them to storage; this is what a file in `module/` does |
| `GridTensors[var, ...]` | Declare tensors and bind them to grid storage (C names carry the grid point index) |
| `TileTensors[var, ...]` | Declare tensors and bind them to tile storage (C names carry the tile point index) |
| `TempTensors[var, ...]` | Declare tensors and bind them as temporaries (no point index) |

A var spec is a list: the tensor with its indices, and optionally a symmetry, `PrintAs -> "name"`, and one of `DerivativeOf -> v` and `TimeDerivativeOf -> v`. The index letter carries meaning: `i` onward is spatial.

```wolfram
{dgamt[-k, -i, -j], Symmetric[{-i, -j}], DerivativeOf -> gamt, PrintAs -> "dgamt"}
```

- `DerivativeOf -> v` declares a spatial derivative of the declared variable `v`. The derivative indices are the leading ones.
- `TimeDerivativeOf -> v` declares the time derivative of `v`.
- A binding also takes the list `DefTensors` returned, so a script binds what a module declared: `ADMVarlist = GridTensors[ADMVars]`. A variable is bound once.

### Code Generation

| Function | Description |
|----------|-------------|
| `Generate[backend, file, program]` | Check the program, then write `file` through the backend named |
| `Load[varlist, mode]` | A node: bring the components of the variables into scope. Option: `ChartName` |
| `Store[varlist, mode]` | A node: write the equations of the variables. Options: `SuffixName`, `ChartName`, `ExtraReplaceRules` |

- The program is a list of strings, nodes and lists of these, nested to any depth. Anything that returns nodes may build it: `Table`, `If`, a function of your own. `Do` returns nothing, so a loop over nodes is a `Table`.
- A node takes one component in place of a varlist, written in xAct's notation: `Store[dg[{3, -cart}, {0, -cart}, {1, -cart}], "Out"]`.
- `Generate` checks every node before anything is computed and reports every problem with the position of its node: a mode the backend lacks, a variable that is not bound, a storage the mode is not for, a missing relation, a `Store` without an equation.

### Equation Setting

| Function | When to Use |
|----------|-------------|
| `SetEQN[lhs, rhs]` | Default choice; validates RHS terms |
| `SetEQNDelayed[lhs, rhs]` | When RHS contains `If` statements |

Options: `SuffixName`, `CheckRHS` (SetEQN only)

### Configuration

Settings are setters at the top of the script. Those that shape a C name, `SetGridPointIndex` and `SetTilePointIndex`, come before the first binding. The ones a script usually sets:

| Setter | Default | Sets |
|--------|---------|------|
| `SetGridPointIndex[string]` | `""` | The index appended to a grid variable's C name |
| `SetTilePointIndex[string]` | `""` | The index appended to a tile variable's C name |
| `SetTempVariableType[string]` | `"double"` | The C type of a temporary variable |
| `SetProject[string]` | `"TEST"` | The project name, for a backend that prints it |
| `SetPrintDate[bool]` | `True` | Whether the date is printed in the header of the output file |
| `SetPrintHeaderMacro[bool]` | `True` | Whether the output file is wrapped in a header guard |
| `SetSimplifyEquation[bool]` | `True` | Whether a right-hand side is simplified before it is written |
| `SetUseLetterForTensorComponent[bool]` | `False` | Whether a component is named by letters (`t`, `x`, `y`, `z`) in place of numbers |
| `SetPVerbose[bool]` | `False` | Whether progress messages are printed |

## Porting a script

Scripts written before `Generate` held their output program in `SetMainPrint` and wrote it when their last line imported a backend file. Those names were removed. A script that still calls one stops at the call with a message that points here, writes no file, and makes `Generato` exit non-zero.

### The tail of the script

`SetOutputFile`, `SetMainPrint` and the `Import` of the backend file become one `Generate` call. The body of `SetMainPrint` becomes a list: statements separated by `;` become elements separated by `,`.

Before:

```wolfram
DerivVarlist = GridTensors[{dphi[-i], PrintAs -> "dphi"}];

SetOutputFile[FileNameJoin[{Directory[], "testDerivs.hxx"}]];

SetMainPrint[
  pr[];
  PrintInitializations[{Mode -> "MainIn"}, EvolVarlist];
  pr[];
  PrintInitializations[{Mode -> "Derivs", TensorType -> "Vect", DerivsOrder -> 1, DerivsAccuracy -> 4}, DerivVarlist];
  pr[];
  PrintEquations[{Mode -> "Temp"}, TempVarlist];
];

Import[FileNameJoin[{Environment["GENERATO"], "codes/CarpetXGPU.wl"}]];
```

After:

```wolfram
DerivVarlist = TempTensors[{dphi[-i], DerivativeOf -> phi, PrintAs -> "dphi"}];

Generate["CarpetXGPU", "testDerivs.hxx", {
  "",
  Load[EvolVarlist, "In"],
  "",
  Load[DerivVarlist, "Derivs"],
  "",
  Store[TempVarlist, "Temp"]
}];
```

### What each call becomes

| Before | After |
|--------|-------|
| `SetOutputFile[file]`, `SetMainPrint[body]`, `Import[".../codes/Backend.wl"]` | `Generate["Backend", file, {program}]` |
| `pr["text"]`, `pr[]` | `"text"`, `""` |
| `PrintInitializations[{Mode -> "MainIn"}, v]` | `Load[v, "In"]` |
| `PrintInitializations[{Mode -> "MainOut"}, v]` | `Load[v, "Out"]` |
| `PrintInitializations[{Mode -> "MainIn", StorageType -> "Tile"}, v]` | `Load[v, "In"]`, with `v` bound by `TileTensors` |
| `PrintInitializations[{Mode -> "Derivs", TensorType -> "Vect", DerivsOrder -> 1, DerivsAccuracy -> 4}, v]` | `Load[v, "Derivs"]`, with `v` bound by `TempTensors` and declaring `DerivativeOf` |
| `PrintInitializations[{Mode -> "Derivs", StorageType -> "Tile", TensorType -> "Vect"}, v]` | `Load[v, "In"]`, with `v` bound by `TileTensors` and declaring `DerivativeOf` |
| `PrintInitializations[{Mode -> "MoreInOut"}, v]` | `Load[v, "MoreInOut"]` |
| `PrintInitializations[{Mode -> "Temp"}, v]` | `Load[v, "Temp"]`, with `v` bound by `TempTensors` |
| `PrintEquations[{Mode -> "Temp"}, v]` | `Store[v, "Temp"]` |
| `PrintEquations[{Mode -> "MainOut"}, v]` | `Store[v, "Out"]` |
| `PrintEquations[{Mode -> "AddToMainOut"}, v]` | `Store[v, "AddTo"]` |
| `PrintEquations[{Mode -> "MainOut", SuffixName -> "Msqr", ChartName -> cart}, v]` | `Store[v, "Out", SuffixName -> "Msqr", ChartName -> cart]` |
| `WithMode[{"EquationsMode" -> "MainOut"}, PrintComponentEquation[chart, comp, rules]]` | `Store[comp, "Out", ChartName -> chart, ExtraReplaceRules -> rules]` |
| `Do[...print calls..., {k, 3}]` | `Table[...nodes..., {k, 3}]` |

### What a step no longer states

- **Storage comes from the binding.** No node takes `StorageType`. A formatter is looked up under the storage the variable was bound with, so bind each variable as what it is in the generated code: `GridTensors` for a grid function, `TileTensors` for a tile, `TempTensors` for a local. A derivative computed inline with `"Derivs"` is a local, so it is bound with `TempTensors`; a derivative read from a tile is bound with `TileTensors` and loaded with `"In"`.
- **A derivative declares what it differentiates.** No node takes `TensorType`, `DerivsOrder` or `DerivsAccuracy`. Add `DerivativeOf -> v` to the var spec of a spatial derivative and `TimeDerivativeOf -> v` to that of a time derivative. The order is the difference in rank, and the index layout follows from the components of `v`. Nothing is read from a name any more, so the `d` and `dt` prefixes carry no meaning.
- **A relation is required where a backend reads it.** CarpetXGPU's `"Derivs"` load reads `DerivativeOf`, and Nmesh's `"Out"` load reads `TimeDerivativeOf`. A variable that does not declare it fails the check with the rule to add.
- **The suffix is an option.** A `SetSuffixName` call around a print call becomes `SuffixName -> suffix` on the `Store` node. `SetEQN` is unchanged.
- **Modes are not set around a step.** `WithMode` is gone: the mode is the second argument of the node.
- **A module declares and names no storage.** A shared file that called `GridTensors` or `TempTensors` becomes `Vars = DefTensors[...]`, and each script binds the list it returned: `Varlist = GridTensors[Vars]`.

### What a failure looks like

`Generate` checks the whole program before the slow part starts, so one run reports every problem:

```text
Generate::EMode: Node 4: backend "CarpetXGPU" has no Load mode "MoreInOut". It accepts "In", "Out", "Derivs", "Temp".
Generate::EProblems: 1 problem in the program, nothing was generated.
```

A file that an earlier run generated is removed before the script starts, when it is named after the script (`script.wl` writes `script.hxx`, `.cxx`, `.c` or `.h`), so a failed run leaves no stale output behind. Given several scripts, `Generato` runs every one, then exits non-zero if any failed and names the ones that did.

## Tips

- **Variable naming**: Use lowercase to avoid conflicts with built-ins like `Pi`
- **Debugging**: Use `IsDefined[term]` to check if a tensor/component is defined
- **Conditional code**: Use the `SuffixName` option to set several equations for one variable, and put each `Store` between the strings of a C branch

## Running Tests

```bash
wolframscript -script test/AllTests.wl              # All tests
wolframscript -script test/AllTests.wl --verbose    # Show progress
wolframscript -script test/AllTests.wl --unit-only  # Unit tests only
wolframscript -script test/AllTests.wl --generate   # Update golden files
```

**Environment options:**
- `QUIET=1` - Suppress xAct banners and processing messages

### Test Structure

```
test/
├── AllTests.wl           # Test runner
├── test_cases.txt        # Regression cases
├── unit/                 # Unit tests (one per module)
└── regression/           # Backend integration tests
    ├── golden/           # Reference outputs, and the messages of the runs that must fail
    └── <Backend>/        # Test scripts
```

### Adding Tests

**Unit test** - Create `test/unit/MyTests.wl`:
```wolfram
AppendTo[$AllTests, VerificationTest[expr, expected, TestID -> "MyTest"]];
```

**Regression test**:
1. Create `test/regression/<Backend>/testname.wl`
2. Add to `test/test_cases.txt`: `Backend:testname:extension`
3. Run `--generate` to create golden file

**A run that must fail**:
1. Create `test/regression/<Backend>/failname.wl`
2. Add to `test/test_cases.txt`: `Backend:failname:extension:fails`
3. Write `test/regression/golden/<Backend>/failname.err.golden` by hand: the lines the run must print. `--generate` leaves it alone.
