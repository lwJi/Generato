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
- **ParseMode.wl** - Phase management (SetComp/PrintComp phases), WithMode for scoped settings
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

### State Management

All state is stored in `$CurrentContext`, a flat association that serves as the single source of truth. Global getters/setters read/write `$CurrentContext` directly, and context-aware functions take `ctx` as the first parameter.

```wolfram
(* Global API - reads/writes $CurrentContext *)
SetGridPointIndex["[[ijk]]"];
GetGridPointIndex[];

(* Context-aware API - functional, returns new context *)
ctx = CreateContext[];
ctx = SetGridPointIndex[ctx, "[[ijk]]"];
```

Use `WithMode` for scoped settings that auto-restore:
```wolfram
WithMode[{
  "Phase" -> "PrintComp",
  "PrintCompType" -> "Equations"
},
  (* body - settings restored after *)
];
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
