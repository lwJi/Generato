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
- **Context.wl** - Global state management via `$CurrentContext`
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
Each implements `PrintComponentInitialization[varinfo, compname]` and `PrintComponentEquation[coordinate, compname, extrareplacerules]`:
- **CarpetX.wl**, **CarpetXGPU.wl**, **CarpetXPointDesc.wl** - CarpetX variants
- **Carpet.wl** - Original Carpet framework
- **AMReX.wl** - AMReX library
- **Nmesh.wl** - Structured mesh (C output)

Common backend code is in `BackendCommon.wl`.

### State Management

All state is stored in `$CurrentContext`, a flat association that serves as the single source of truth. All getters/setters read/write `$CurrentContext` directly.

```wolfram
(* Global API - reads/writes $CurrentContext *)
SetGridPointIndex["[[ijk]]"];
GetGridPointIndex[];
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
WithMode[{"Phase" -> "SetComp"},
  SetComponents[varlist]
];
```

2. **PrintComp Phase**: Generates C/C++ code
```wolfram
WithMode[{
  "Phase" -> "PrintComp",
  "PrintCompType" -> "Initializations",
  "InitializationsMode" -> "MainOut"
},
  PrintInitializations[varlist]
];

WithMode[{
  "Phase" -> "PrintComp",
  "PrintCompType" -> "Equations",
  "EquationsMode" -> "Temp"
},
  PrintEquations[varlist]
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
