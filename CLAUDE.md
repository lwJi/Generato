# Generato

Wolfram Language code generator that produces C/C++ from symbolic tensor expressions using xAct.

## Running

```bash
export GENERATO="/path/to/repo"
Generato test.wl                    # Single file
Generato file1.wl file2.wl          # Multiple files
```

## Architecture

### Core (src/)
- **Generato.wl** - Package loader, imports modules in dependency order
- **Context.wl** - Global state via `$CurrentContext`, accessor generation
- **Basic.wl** - Config, tensor utilities, SetEQN/SetEQNDelayed
- **ParseMode.wl** - Phase management, `WithMode[{...}, body]` for scoped settings
- **Component.wl** - Tensor component to varlist mapping
- **Varlist.wl** - Tensor definitions via ParseVarlist
- **Interface.wl** - Public API: DefTensors, GridTensors, TileTensors, TempTensors, SetComponents, PrintEquations, PrintInitializations
- **BackendCommon.wl** - Shared backend utilities
- **Writefile.wl** - Output buffering (SetMainPrint/GetMainPrint)
- **stencils/FiniteDifferenceStencils.wl** - Finite difference stencil generation

### Backends (codes/)
Each implements `PrintComponentInitialization` and `PrintComponentEquation`:
- CarpetX.wl, CarpetXGPU.wl, CarpetXPointDesc.wl, Carpet.wl, AMReX.wl, Nmesh.wl

### Key Patterns
- **State**: All in `$CurrentContext`; use `WithMode[{...}, body]` for scoped settings
- **Two phases**: SetComp (map components) → PrintComp (generate code)
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
