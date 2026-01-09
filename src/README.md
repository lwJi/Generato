# Generato Source Modules

This directory contains the core Wolfram Language modules for Generato's code generation system.

## Module Overview

| Module | Description |
|--------|-------------|
| Generato.wl | Package loader, imports modules in dependency order |
| Basic.wl | Global configuration state and tensor utilities |
| ParseMode.wl | Multi-level execution phase management |
| Component.wl | Tensor component to varlist index mapping |
| Varlist.wl | Variable list and tensor definition processing |
| Interface.wl | Public API for tensor definition and code generation |
| Writefile.wl | Output buffering and file writing |
| Derivation.wl | Testing utilities |
| stencils/FiniteDifferenceStencils.wl | Finite difference stencil generation |

## Module Loading Order

Generato.wl loads modules in this dependency order:

```
Basic → ParseMode → Component → Varlist → Interface → Derivation → Writefile → FiniteDifferenceStencils
```

## Module Details

### Basic.wl

Global configuration and tensor utilities.

**Configuration (Get/Set pairs):**
- `CheckInputEquations` - Input equation validation
- `PVerbose` - Verbose output control
- `PrintDate` - Include date in output files
- `PrintHeaderMacro` - C header guard generation
- `GridPointIndex` - Grid point suffix (default: `[ijk]`)
- `TilePointIndex` - Tile point suffix
- `SuffixUnprotected` - Protected symbol suffix
- `OutputFile` - Output filename
- `Project` - Project name

**Equation handling:**
- `SetEQN[var, varrhs]` - Set tensor component equations
- `SetEQNDelayed[var, varrhs]` - Delayed equation setting
- `RHSOf[var]` - Get right-hand side symbol (`var$RHS`)
- `IsDefined[term]` - Check if tensor/expression is defined

**Utilities:**
- `GetDefaultChart` - Get default coordinate system
- `GetDim` - Get manifold dimension
- `IndexType`, `IndexType3D` - Abstract index checking

### ParseMode.wl

Multi-level mode system tracking execution phases.

**Top-level modes:**
- `SetComp` - Component-setting phase
- `PrintComp` - Component-printing phase

**SetComp sub-modes:**
- `IndependentVarlistIndex` - Reset indices per varlist item
- `WithoutGridPointIndex` - Omit grid point suffix
- `UseTilePointIndex` - Use tile point instead of grid point

**PrintComp sub-modes:**
- `Initializations` vs `Equations` - Code type
- `InitMode` - Initialization phase: `MainOut`, `MainIn`, `Derivs`, `MoreInOut`, `Temp`
- `EQNMode` - Equation phase: `NewVar`, `Main`, `AddToMain`
- `TensorType` - Data structure: `Scal`, `Vect`, `Smat`
- `StorageType` - Memory layout: `GF`, `Tile`

**API pattern:** `GetParseMode[category]`, `SetParseMode[category, value]`, `CleanParseMode[category]`

### Component.wl

Maps tensor components to varlist indices.

**Component mapping:**
- `GetMapComponentToVarlist` / `SetMapComponentToVarlist` - Global component index map

**Configuration:**
- `SimplifyEquation` - Equation simplification
- `UseLetterForTensorComponent` - Index notation (letter vs number)
- `TempVariableType` - C type for temporaries (default: `double`)
- `InterfaceWithNonCoordBasis` - Non-coordinate basis support
- `SuffixName` - Variable suffix for conditional generation
- `PrefixDt` - Time derivative prefix (default: `dt`)

**Core functions:**
- `ParseComponent[varinfo, compindexlist, coordinate, extrareplacerules]` - Dispatch to SetComponent or PrintComponent based on mode
- `PrintComponent[...]` - Delegates to backend-specific `PrintComponentInitialization` or `PrintComponentEquation`

### Varlist.wl

Processes variable lists and tensor definitions.

**Key functions:**
- `ParseVarlist[varlist, chartname]` - Iterate through varlist, handle each tensor
  - Options: `ExtraReplaceRules`
  - Handles scalar, 1-4 index tensors
  - Supports symmetries: `Symmetric`, `Antisymmetric`, `GenSet`
- `ParseVar[var]` - Parse single variable specification → `{varname, symmetry, printname}`
- `DefineTensor[varname, symmetry, printname]` - Create xAct tensor via DefTensor

### Interface.wl

Public API for tensor definition and code generation.

**Tensor definition:**
- `DefTensors[var1, var2, ...]` - Define tensors without setting components
- `GridTensors[var1, var2, ...]` - Define with grid point indices
- `TileTensors[var1, var2, ...]` - Define with tile point indices
- `TempTensors[var1, var2, ...]` - Define without grid point indices

**Component setting:**
- `SetComponents[varlist]` - Process varlist, set all components
  - Options: `ChartName`, `IndependentIndexForEachVar`, `WithoutGridPointIndex`, `UseTilePointIndex`

**Equation generation:**
- `PrintEquations[varlist]` - Output component equations
  - Modes: `Temp` (NewVar), `Main`, `AddToMain`
  - Options: `ChartName`, `SuffixName`, `Mode`, `ExtraReplaceRules`

**Initialization generation:**
- `PrintInitializations[varlist]` - Output variable declarations
  - Modes: `MainOut`, `MainIn`, `Derivs`, `MoreInOut`, `Temp`
  - Options: `ChartName`, `Mode`, `TensorType` (Scal/Vect/Smat), `StorageType` (GF/Tile), `DerivsOrder`, `AccuracyOrder`

### Writefile.wl

Output buffering and file writing.

**Key functions:**
- `SetMainPrint[content]` - Buffer content for output (uses delayed evaluation)
- `GetMainPrint[]` - Retrieve and evaluate buffered content
- `WriteToFile[outputfile]` - Write to file with header/footer and C guards
- `ReplaceGFIndexName[filename, rule]` - Post-processing string replacement

### Derivation.wl

Testing utilities.

- `TestEQN[bool, label]` - Test assertion function (prints SUCCEED/FAILED)

### stencils/FiniteDifferenceStencils.wl

Finite difference coefficient computation.

**Key functions:**
- `GetFiniteDifferenceCoefficients[sample, order]` - Compute FD coefficients
- `GetCenteringStencils[order]` - Get centered stencil points (orders 2, 4, 6, 8, 10, 12)
- `GetUpwindCoefficients[sample]` - Upwind FD with symmetric/antisymmetric parts

## Code Generation Workflow

```
1. Define manifold/coordinates (xAct setup)
           ↓
2. Define tensors: GridTensors/TileTensors/TempTensors
           ↓
3. Set equations: SetEQN/SetEQNDelayed
           ↓
4. Buffer output: SetMainPrint[PrintEquations[...]; PrintInitializations[...]]
           ↓
5. Import backend (codes/*.wl)
           ↓
6. Write file: WriteToFile[outputfile]
```

## Extension Points

Backend frameworks (in `codes/`) implement two key functions:

- `PrintComponentInitialization[...]` - Generate variable declarations
- `PrintComponentEquation[...]` - Generate assignment statements

Available backends: CarpetX, CarpetXGPU, CarpetXPointDesc, Carpet, AMReX, Nmesh
