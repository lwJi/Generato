# Repository Guidelines

## Project Overview

Generato is a Wolfram Language-based code generator that produces C/C++ implementations from symbolic tensor expressions. It uses the xAct tensor algebra framework and targets numerical relativity simulations and tensor-based PDEs.

## Running Generato

```bash
# Prerequisites: Wolfram Engine with wolframscript, xAct tensor package installed
export GENERATO="/path/to/repo"

# Execute code generation
Generato test.wl                    # Single file
Generato file1.wl file2.wl file3.wl # Multiple files
```

Output files (`.hxx` or `.c`) are generated alongside the input `.wl` files.

## Architecture

### Core Modules (src/)

- **Generato.wl** - Package loader, imports modules in dependency order
- **Basic.wl** - Global config state (`$CheckInputEquations`, `$OutputFile`, etc.), tensor utilities, equation setting (`SetEQN`, `SetEQNDelayed`)
- **ParseMode.wl** - Multi-level mode management system tracking execution state (SetComp/PrintComp phases, initialization modes, storage types)
- **Component.wl** - Maps tensor components to varlist indices, handles symmetries
- **Varlist.wl** - Processes tensor definitions via `ParseVarlist`, creates xAct tensor definitions
- **Interface.wl** - Public API: `DefTensors`, `GridTensors`, `TileTensors`, `TempTensors`, `SetComponents`, `PrintEquations`, `PrintInitializations`
- **Writefile.wl** - Output buffering (`SetMainPrint`/`GetMainPrint`) and file writing
- **stencils/FiniteDifferenceStencils.wl** - Finite difference stencil generation for orders 2-12

### Framework Backends (codes/)

Each backend implements `PrintComponentInitialization` and `PrintComponentEquation` for its target framework:

- **CarpetX.wl** - CarpetX AMR with GF3D2 storage
- **CarpetXGPU.wl** - GPU-enabled CarpetX
- **CarpetXPointDesc.wl** - Point description variant
- **Carpet.wl** - Original Carpet framework
- **AMReX.wl** - AMReX AMR library
- **Nmesh.wl** - Structured mesh framework (C output)

## Code Generation Workflow

1. User creates `.wl` file with xAct manifold setup and tensor definitions
2. Define variables using `GridTensors`/`TileTensors`/`TempTensors`
3. Set equations with `SetEQN` or `SetEQNDelayed`
4. Call `SetMainPrint` with `PrintEquations` and `PrintInitializations`
5. Import framework-specific backend
6. Run `Generato script.wl`

## Key Concepts

- **ParseMode System**: Tracks execution phases (SetComp vs PrintComp) and sub-modes for initialization types
- **SuffixName**: Controls conditional equation generation
- **Component Mapping**: Logical tensor indices map to varlist positions
- **Symmetry Handling**: Symmetric vs Antisymmetric (AB) notations for 2-index tensors

## Running Tests

```bash
# Run all tests (recommended - runs unit tests + regression tests)
wolframscript -f test/AllTests.wl

# Alternative: Run via bash script
./test/run_tests.sh

# Update golden files with new outputs
./test/run_tests.sh --generate
```

The test suite includes:

- **Unit tests** (`test/unit/`) - Test individual module functions
- **Integration tests** - Generate output for each backend
- **Golden file regression** (`test/golden/`) - Compare outputs against expected results

## Test Examples

- `test/CarpetX/test.wl` - Simple CarpetX example with 3 vectors
- `test/Nmesh/GHG_rhs.wl` - GHG formulation of Einstein's equations (complex example)
- `test/Nmesh/GHG_rhs.ipynb` - Jupyter notebook with documentation
