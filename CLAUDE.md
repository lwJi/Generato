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
- **Context.wl** - Context-based state management (CreateContext, GetCtx, SetCtx, UpdateCtx, WithContext)
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
Each implements `PrintComponentInitialization[ctx, varinfo, compname]` and `PrintComponentEquation[ctx, coordinate, compname, extrareplacerules]`:
- **CarpetX.wl**, **CarpetXGPU.wl**, **CarpetXPointDesc.wl** - CarpetX variants
- **Carpet.wl** - Original Carpet framework
- **AMReX.wl** - AMReX library
- **Nmesh.wl** - Structured mesh (C output)

Common backend code is in `BackendCommon.wl`.

### Context-Based State Management

All state is encapsulated in a `GeneratoContext` association. Functions take context as first parameter.

```wolfram
ctx = CreateContext[];
ctx = SetGridPointIndex[ctx, "[[ijk]]"];
```

### Two-Phase Processing

Code generation uses explicit phases:

1. **SetComp Phase**: Maps tensor components to varlist indices
```wolfram
ctx = WithSetCompPhase[ctx,
  SetComponents[ctx, {}, varlist]
];
```

2. **PrintComp Phase**: Generates C/C++ code
```wolfram
ctx = WithPrintCompPhase[ctx,
  PrintInitializations[ctx, {Mode -> "MainOut"}, varlist];
  PrintEquations[ctx, {Mode -> "Temp"}, varlist];
];
```

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
