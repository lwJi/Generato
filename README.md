# Generato

Generate C/C++ Code from Tensor Expression and So On

## Pre-Requisites

* Install free Wolfram Engine for Developers from https://www.wolfram.com/developer/.
(If you are rich, you can install the totally worth-it Mathematica instead, versions that support `wolframscript`.)

* Download xAct from http://www.xact.es and install it properly.

## Installation

* clone repo: `git clone https://github.com/lwJi/Generato.git`

* set environment variable: `export GENERATO="/your/repo/path"`

## Example

* Small toy

```bash
cd test/regression/CarpetX

Generato test.wl
```

* GHG formulation of Einstein's equations

Please take a look at the example script for GHG system: [test/regression/Nmesh/GHG_rhs.ipynb](https://github.com/lwJi/Generato/blob/main/test/regression/Nmesh/GHG_rhs.ipynb). Or

```bash
cd test/regression/Nmesh

Generato GHG_rhs.wl
```

## Architecture

### Core Modules (src/)
- **Generato.wl** - Package loader, imports modules in dependency order
- **Basic.wl** - Global config state, tensor utilities, equation setting (SetEQN/SetEQNDelayed)
- **ParseMode.wl** - Mode management system tracking execution state (SetComp/PrintComp phases)
- **Component.wl** - Maps tensor components to varlist indices, handles symmetries
- **Varlist.wl** - Processes tensor definitions via ParseVarlist
- **Interface.wl** - Public API: DefTensors, GridTensors, TileTensors, TempTensors, SetComponents, PrintEquations, PrintInitializations
- **BackendCommon.wl** - Shared backend utilities
- **Derivation.wl** - Testing utilities
- **Writefile.wl** - Output buffering (SetMainPrint/GetMainPrint)
- **stencils/FiniteDifferenceStencils.wl** - Finite difference stencil generation

### Backends (codes/)
Each backend implements `PrintComponentInitialization[varinfo, compname]` and `PrintComponentEquation[coordinate, compname, extrareplacerules]`:

| Backend | Output | Description |
|---------|--------|-------------|
| CarpetX.wl | `.hxx` | CarpetX AMR with GF3D2 storage |
| CarpetXGPU.wl | `.hxx` | GPU-enabled CarpetX |
| CarpetXPointDesc.wl | `.hxx` | Point descriptor variant |
| Carpet.wl | `.hxx` | Original Carpet framework |
| AMReX.wl | `.hxx` | AMReX AMR library |
| Nmesh.wl | `.c` | Structured mesh (C output) |

### Public API (Interface.wl)

| Function | Description |
|----------|-------------|
| `DefTensors[var1, ...]` | Define tensors without setting components |
| `GridTensors[var1, ...]` | Define tensors with grid point indices |
| `TileTensors[var1, ...]` | Define tensors with tile point indices |
| `TempTensors[var1, ...]` | Define temporary tensors (no grid index) |
| `SetComponents[varlist]` | Map tensor components to varlist indices |
| `PrintEquations[{opts}, varlist]` | Generate C/C++ equations |
| `PrintInitializations[{opts}, varlist]` | Generate initialization code |

## Recommendations

* Use lower case to name variables (to avoid conflict with say `Pi`)

## Tips

* Use `SetEQNDelayed` if your right-hand side (rhs) includes an `If` statement to handle different component cases. Otherwise, `SetEQN` is preferred because it has an option, `CheckRHS`, which checks if all the terms appearing in the rhs are well-defined.

* `IsDefined[term]` is very useful for debugging.

## Running Tests

```bash
# Run all tests (runs unit tests + regression tests)
wolframscript -script test/AllTests.wl

# Run with verbose output
wolframscript -script test/AllTests.wl --verbose

# Run unit tests only (skip regression tests)
wolframscript -script test/AllTests.wl --unit-only

# Update golden files with new outputs
wolframscript -script test/AllTests.wl --generate
```

### Output Control Options

- **`--verbose`** - Controls test runner output (test progress, results)
- **`QUIET=1`** - Environment variable that suppresses xAct loading banners and Generato processing messages

These operate at different levels and can be combined:

```bash
# Quiet test run (default) - minimal output
wolframscript -script test/AllTests.wl

# Verbose test run - shows test progress and results
wolframscript -script test/AllTests.wl --verbose

# Combined - verbose test results but suppressed xAct banners
QUIET=1 wolframscript -script test/AllTests.wl --verbose
```

The test suite includes:

- **Unit tests** (`test/unit/`) - Test individual module functions
- **Integration tests** - Generate output for each backend
- **Golden file regression** (`test/regression/golden/`) - Compare outputs against expected results

### Test Directory Structure

```
test/
├── AllTests.wl              # Master test runner
├── TestConfig.wl            # Shared configuration module
├── test_cases.txt           # Regression test case definitions
├── compare_golden.wl        # Golden file comparison module
├── unit/                    # Unit tests (one file per module)
│   ├── BasicTests.wl
│   ├── ComponentTests.wl
│   ├── VarlistTests.wl
│   └── ...
└── regression/              # Regression tests
    ├── golden/              # Reference outputs (.golden files)
    │   ├── CarpetX/
    │   ├── Nmesh/
    │   └── ...
    ├── CarpetX/             # Backend test directories
    ├── Nmesh/
    └── ...
```

### Adding New Unit Tests

Create a new file in `test/unit/` following this pattern:

```wolfram
If[Environment["QUIET"] =!= "1", Print["Loading MyModuleTests.wl..."]];
Needs["Generato`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"}]];
SetPVerbose[False];
SetPrintDate[False];

AppendTo[$AllTests,
  VerificationTest[
    (* test code *),
    (* expected result *),
    TestID -> "MyModule-DescriptiveTestName"
  ]
];

If[Environment["QUIET"] =!= "1", Print["MyModuleTests.wl completed."]];
```

### Adding New Regression Tests

1. Create test file in `test/regression/<Backend>/testname.wl`
2. Add entry to `test/test_cases.txt`:
   ```
   Backend:testname:extension
   ```
   Format: `backend:testname:extension` (e.g., `CarpetX:mytest:.hxx`)
3. Run `wolframscript -script test/AllTests.wl --generate` to create golden file

### Test Examples

- `test/regression/CarpetX/test.wl` - Simple CarpetX example with 3 vectors
- `test/regression/Nmesh/GHG_rhs.wl` - GHG formulation of Einstein's equations (complex example)
- `test/regression/Nmesh/GHG_rhs.ipynb` - Jupyter notebook with documentation
