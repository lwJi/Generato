# Generato

A Wolfram Language code generator that produces C/C++ from symbolic tensor expressions using xAct. Designed for numerical relativity and computational physics applications.

## Pre-Requisites

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

(* Define tensors *)
OutputVars = GridTensors[{rU[i], PrintAs -> "r"}];
InputVars = GridTensors[{uU[i], PrintAs -> "u"}];

(* Set equations *)
SetEQN[rU[i_], 2 uU[i]];

(* Generate code *)
SetOutputFile["output.hxx"];
SetMainPrint[
  PrintInitializations[{Mode -> "MainOut"}, OutputVars];
  PrintInitializations[{Mode -> "MainIn"}, InputVars];
  PrintEquations[{Mode -> "MainOut"}, OutputVars];
];
Import[FileNameJoin[{Environment["GENERATO"], "codes/CarpetX.wl"}]];
```

Run with: `Generato script.wl`

## Examples

**Simple example:**
```bash
cd test/regression/CarpetX && Generato test.wl
```

**GHG formulation of Einstein's equations:**
- Notebook: [test/regression/Nmesh/GHG_rhs.ipynb](https://github.com/lwJi/Generato/blob/main/test/regression/Nmesh/GHG_rhs.ipynb)
- Script: `cd test/regression/Nmesh && Generato GHG_rhs.wl`

## Architecture

### Two-Phase Processing

1. **SetComp phase**: Map tensor components to C/C++ variable indices
2. **PrintComp phase**: Generate initialization and equation code

### Core Modules (src/)

| Module | Purpose |
|--------|---------|
| Generato.wl | Package loader |
| Context.wl | Global state (`$CurrentContext`) |
| Basic.wl | Config, SetEQN/SetEQNDelayed |
| ParseMode.wl | Phase management, WithMode |
| Component.wl | Component-to-varlist mapping |
| Varlist.wl | Tensor definitions (ParseVarlist) |
| Interface.wl | Public API |
| BackendCommon.wl | Shared backend utilities |
| Writefile.wl | Output buffering |
| stencils/*.wl | Finite difference stencils |

### Backends (codes/)

| Backend | Output | Description |
|---------|--------|-------------|
| CarpetX.wl | .hxx | CarpetX AMR with GF3D2 storage |
| CarpetXGPU.wl | .hxx | GPU-enabled CarpetX |
| CarpetXPointDesc.wl | .hxx | Point descriptor variant |
| Carpet.wl | .hxx | Original Carpet framework |
| AMReX.wl | .hxx | AMReX AMR library |
| Nmesh.wl | .c | Structured mesh (C output) |

## API Reference

### Tensor Definition

| Function | Description |
|----------|-------------|
| `DefTensors[var, ...]` | Define tensors without setting components |
| `GridTensors[var, ...]` | Define tensors with grid point indices |
| `TileTensors[var, ...]` | Define tensors with tile point indices |
| `TempTensors[var, ...]` | Define temporary tensors (no grid index) |

### Code Generation

| Function | Key Options |
|----------|-------------|
| `SetComponents[varlist]` | `ChartName`, `IndependentVarlistIndex` |
| `PrintInitializations[{opts}, varlist]` | `Mode` ("MainOut", "MainIn", "Derivs", "Temp"), `TensorType`, `StorageType` |
| `PrintEquations[{opts}, varlist]` | `Mode`, `SuffixName`, `ChartName`, `ExtraReplaceRules` |

### Equation Setting

| Function | When to Use |
|----------|-------------|
| `SetEQN[lhs, rhs]` | Default choice; validates RHS terms |
| `SetEQNDelayed[lhs, rhs]` | When RHS contains `If` statements |

Options: `SuffixName`, `CheckRHS` (SetEQN only)

## Tips

- **Variable naming**: Use lowercase to avoid conflicts with built-ins like `Pi`
- **Debugging**: Use `IsDefined[term]` to check if a tensor/component is defined
- **Conditional code**: Use `SuffixName` option to generate different code paths

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
├── unit/                 # Unit tests (one per module)
└── regression/           # Backend integration tests
    ├── golden/           # Reference outputs
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
