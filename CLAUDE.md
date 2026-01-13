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
- **Context.wl** - Global state via `$CurrentContext`
- **Basic.wl** - Config, tensor utilities, SetEQN/SetEQNDelayed
- **ParseMode.wl** - Phase management, WithMode for scoped settings
- **Component.wl** - Tensor component to varlist mapping
- **Varlist.wl** - Tensor definitions via ParseVarlist
- **Interface.wl** - Public API (DefTensors, GridTensors, TileTensors, TempTensors, SetComponents, PrintEquations, PrintInitializations)
- **BackendCommon.wl** - Shared backend utilities
- **Writefile.wl** - Output buffering (SetMainPrint/GetMainPrint)

### Backends (codes/)
Each implements `PrintComponentInitialization` and `PrintComponentEquation`:
- CarpetX.wl, CarpetXGPU.wl, CarpetXPointDesc.wl, Carpet.wl, AMReX.wl, Nmesh.wl

### Key Patterns
- **State**: All in `$CurrentContext`; use `WithMode[{...}, body]` for scoped settings
- **Two phases**: SetComp (map components) then PrintComp (generate code)

## Tests

```bash
wolframscript -script test/AllTests.wl              # All tests
wolframscript -script test/AllTests.wl --unit-only  # Unit tests only
```

## Wolframscript Tips

Shell escaping with backticks causes errors. Use pipe instead:
```bash
echo 'Needs["Generato`"]; Print["success"]' | wolframscript
```
