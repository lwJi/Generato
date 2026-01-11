# Repository Guidelines

Generato is a Wolfram Language code generator that produces C/C++ from symbolic tensor expressions using xAct.

## Running

```bash
export GENERATO="/path/to/repo"
Generato test.wl                    # Single file
Generato file1.wl file2.wl          # Multiple files
```

## Architecture

### Core (src/)
- **Generato.wl** - Package loader
- **Basic.wl** - Config state, tensor utilities, SetEQN/SetEQNDelayed
- **ParseMode.wl** - Mode management (SetComp/PrintComp phases)
- **Component.wl** - Tensor component to varlist index mapping
- **Varlist.wl** - Tensor definitions via ParseVarlist
- **Interface.wl** - Public API: DefTensors, GridTensors, TileTensors, TempTensors, SetComponents, PrintEquations, PrintInitializations
- **BackendCommon.wl** - Shared backend utilities
- **Derivation.wl** - Testing utilities (TestEQN)
- **Writefile.wl** - Output buffering (SetMainPrint/GetMainPrint)
- **stencils/FiniteDifferenceStencils.wl** - FD stencils for orders 2-12

### Backends (codes/)
Each implements PrintComponentInitialization and PrintComponentEquation:
- **CarpetX.wl**, **CarpetXGPU.wl**, **CarpetXPointDesc.wl** - CarpetX variants
- **Carpet.wl** - Original Carpet framework
- **AMReX.wl** - AMReX library
- **Nmesh.wl** - Structured mesh (C output)

## Tests

```bash
wolframscript -script test/AllTests.wl              # All tests
wolframscript -script test/AllTests.wl --unit-only  # Unit tests only
```

## Wolframscript Tips

Shell escaping with backticks causes misleading errors. Use pipe instead:
```bash
echo 'Needs["Generato`ParseMode`"]; Print["success"]' | wolframscript
```
