(* ::Package:: *)

(* Generato`Context`, context-based state management for Generato *)

(* (c) Liwei Ji, 01/2026 *)

BeginPackage["Generato`Context`"];

$CurrentContext::usage = "$CurrentContext is the global context state.";

$ContextDefaults::usage = "$ContextDefaults contains the default values for all context keys.";

(* Default context values *)
$ContextDefaults = <|
  (* Basic.wl state *)
  "CheckInputEquations" -> False,
  "PVerbose" -> False,
  "QuietMode" -> If[Environment["QUIET"] === "1", True, False],
  "PrintDate" -> True,
  "PrintHeaderMacro" -> True,
  "GridPointIndex" -> "",
  "TilePointIndex" -> "",
  "SuffixUnprotected" -> "$Upt",
  "OutputFile" -> "output.c",
  "Project" -> "TEST",

  (* ParseMode.wl state - flattened *)
  "Phase" -> None,
  "IndependentVarlistIndex" -> True,
  "WithoutGridPointIndex" -> False,
  "UseTilePointIndex" -> False,
  "PrintCompType" -> None,
  "InitializationsMode" -> None,
  "TensorType" -> None,
  "StorageType" -> None,
  "DerivsOrder" -> 1,
  "DerivsAccuracy" -> 4,
  "EquationsMode" -> None,

  (* Component.wl state *)
  "MapComponentToVarlist" -> <||>,
  "ProcessNewVarlist" -> True,
  "SimplifyEquation" -> True,
  "UseLetterForTensorComponent" -> False,
  "TempVariableType" -> "double",
  "InterfaceWithNonCoordBasis" -> False,
  "SuffixName" -> "",
  "PrefixDt" -> "dt",

  (* Writefile.wl state *)
  "MainPrint" -> Null
|>;

(* Valid values for context keys - used by setters for validation *)
$ContextModeValidValues::usage = "$ContextModeValidValues contains validation rules for context keys.";

$ContextModeValidValues = <|
  (* Basic.wl keys *)
  "CheckInputEquations" -> {True, False},
  "PVerbose" -> {True, False},
  "PrintDate" -> {True, False},
  "PrintHeaderMacro" -> {True, False},
  "GridPointIndex" -> _String,
  "TilePointIndex" -> _String,
  "SuffixUnprotected" -> _String,
  "OutputFile" -> _String,
  "Project" -> _String,

  (* ParseMode.wl keys *)
  "Phase" -> {None, "SetComp", "PrintComp"},
  "IndependentVarlistIndex" -> {True, False},
  "WithoutGridPointIndex" -> {True, False},
  "UseTilePointIndex" -> {True, False},
  "PrintCompType" -> {None, "Initializations", "Equations"},
  "InitializationsMode" -> {None, "MainOut", "MainIn", "Derivs", "Derivs1st", "Derivs2nd", "MoreInOut", "Temp"},
  "TensorType" -> {None, "Scal", "Vect", "Smat"},
  "StorageType" -> {None, "GF", "Tile"},
  "DerivsOrder" -> _Integer,
  "DerivsAccuracy" -> _Integer,
  "EquationsMode" -> {None, "Temp", "MainOut", "AddToMainOut"},

  (* Component.wl keys *)
  "MapComponentToVarlist" -> _Association,
  "ProcessNewVarlist" -> {True, False},
  "SimplifyEquation" -> {True, False},
  "UseLetterForTensorComponent" -> {True, False},
  "TempVariableType" -> _String,
  "InterfaceWithNonCoordBasis" -> {True, False},
  "SuffixName" -> _String,
  "PrefixDt" -> _String
|>;

(* Validate value for context key *)
ValidateContextModeValue::usage = "ValidateContextModeValue[key, value] returns True if value is valid for key.";

ValidateContextModeValue[contextKey_String, contextValue_] :=
  Module[{validValues},
    If[!KeyExistsQ[$ContextModeValidValues, contextKey],
      True,
      validValues = $ContextModeValidValues[contextKey];
      If[ListQ[validValues],
        MemberQ[validValues, contextValue],
        MatchQ[contextValue, validValues]
      ]
    ]
  ];

(* ========================================= *)
(* Accessor Usage Declarations *)
(* ========================================= *)

(* Basic.wl state accessors *)
GetCheckInputEquations::usage = "GetCheckInputEquations[] returns True if input equation checking is enabled.";
SetCheckInputEquations::usage = "SetCheckInputEquations[bool] enables or disables checking of input equations.";
GetPVerbose::usage = "GetPVerbose[] returns True if verbose printing of messages is enabled.";
SetPVerbose::usage = "SetPVerbose[bool] enables or disables verbose printing of messages.";
GetPrintDate::usage = "GetPrintDate[] returns True if the date is printed in output files.";
SetPrintDate::usage = "SetPrintDate[bool] enables or disables printing the date in output files.";
GetPrintHeaderMacro::usage = "GetPrintHeaderMacro[] returns True if header guard macros are printed in output files.";
SetPrintHeaderMacro::usage = "SetPrintHeaderMacro[bool] enables or disables printing header guard macros in output files.";
GetGridPointIndex::usage = "GetGridPointIndex[] returns the grid point index string appended to variable names.";
SetGridPointIndex::usage = "SetGridPointIndex[index] sets the grid point index string appended to variable names.";
GetTilePointIndex::usage = "GetTilePointIndex[] returns the tile point index string appended to variable names.";
SetTilePointIndex::usage = "SetTilePointIndex[index] sets the tile point index string appended to variable names.";
GetSuffixUnprotected::usage = "GetSuffixUnprotected[] returns the suffix added to variable names that would conflict with system symbols.";
SetSuffixUnprotected::usage = "SetSuffixUnprotected[suffix] sets the suffix added to variable names that would conflict with system symbols.";
GetOutputFile::usage = "GetOutputFile[] returns the output file name.";
SetOutputFile::usage = "SetOutputFile[name] sets the output file name.";
GetProject::usage = "GetProject[] returns the project name used in code generation.";
SetProject::usage = "SetProject[name] sets the project name used in code generation.";

(* ParseMode.wl state accessors *)
GetPhase::usage = "GetPhase[] returns current phase.";
SetPhase::usage = "SetPhase[phase] sets the current phase.";
InSetCompPhase::usage = "InSetCompPhase[] returns True if in SetComp phase.";
InPrintCompPhase::usage = "InPrintCompPhase[] returns True if in PrintComp phase.";
GetPrintCompType::usage = "GetPrintCompType[] returns current print comp type.";
SetPrintCompType::usage = "SetPrintCompType[type] sets the current print comp type.";
InInitializationsMode::usage = "InInitializationsMode[] returns True if in Initializations mode.";
InEquationsMode::usage = "InEquationsMode[] returns True if in Equations mode.";
GetInitializationsMode::usage = "GetInitializationsMode[] returns current initialization mode.";
SetInitializationsMode::usage = "SetInitializationsMode[mode] sets the current initialization mode.";
GetTensorType::usage = "GetTensorType[] returns current tensor type.";
SetTensorType::usage = "SetTensorType[type] sets the current tensor type.";
GetStorageType::usage = "GetStorageType[] returns current storage type.";
SetStorageType::usage = "SetStorageType[type] sets the current storage type.";
GetDerivsOrder::usage = "GetDerivsOrder[] returns derivative order.";
SetDerivsOrder::usage = "SetDerivsOrder[order] sets the derivative order.";
GetDerivsAccuracy::usage = "GetDerivsAccuracy[] returns derivative accuracy.";
SetDerivsAccuracy::usage = "SetDerivsAccuracy[accuracy] sets the derivative accuracy.";
GetEquationsMode::usage = "GetEquationsMode[] returns current equation mode.";
SetEquationsMode::usage = "SetEquationsMode[mode] sets the current equation mode.";
GetIndependentVarlistIndex::usage = "GetIndependentVarlistIndex[] returns IndependentVarlistIndex setting.";
SetIndependentVarlistIndex::usage = "SetIndependentVarlistIndex[val] sets IndependentVarlistIndex.";
GetWithoutGridPointIndex::usage = "GetWithoutGridPointIndex[] returns WithoutGridPointIndex setting.";
SetWithoutGridPointIndex::usage = "SetWithoutGridPointIndex[val] sets WithoutGridPointIndex.";
GetUseTilePointIndex::usage = "GetUseTilePointIndex[] returns UseTilePointIndex setting.";
SetUseTilePointIndex::usage = "SetUseTilePointIndex[val] sets UseTilePointIndex.";

(* Component.wl state accessors *)
GetMapComponentToVarlist::usage = "GetMapComponentToVarlist[] returns the Association mapping tensor components to varlist indices.";
SetMapComponentToVarlist::usage = "SetMapComponentToVarlist[map] sets the mapping in global state.";
GetProcessNewVarlist::usage = "GetProcessNewVarlist[] returns the ProcessNewVarlist flag.";
SetProcessNewVarlist::usage = "SetProcessNewVarlist[bool] sets whether the next varlist is treated as a new varlist for index numbering.";
GetSimplifyEquation::usage = "GetSimplifyEquation[] returns True if equations are simplified before output.";
SetSimplifyEquation::usage = "SetSimplifyEquation[bool] enables or disables simplification of equations before output.";
GetUseLetterForTensorComponent::usage = "GetUseLetterForTensorComponent[] returns True if letters are used for tensor component indices instead of numbers.";
SetUseLetterForTensorComponent::usage = "SetUseLetterForTensorComponent[bool] sets whether to use letters for tensor component indices instead of numbers.";
GetTempVariableType::usage = "GetTempVariableType[] returns the C type string used for temporary variables.";
SetTempVariableType::usage = "SetTempVariableType[type] sets the C type string used for temporary variables.";
GetInterfaceWithNonCoordBasis::usage = "GetInterfaceWithNonCoordBasis[] returns True if interfacing with non-coordinate basis tensors.";
SetInterfaceWithNonCoordBasis::usage = "SetInterfaceWithNonCoordBasis[bool] enables or disables interfacing with non-coordinate basis tensors.";
GetSuffixName::usage = "GetSuffixName[] returns the suffix appended to variable names in the current varlist.";
SetSuffixName::usage = "SetSuffixName[suffix] sets the suffix appended to variable names in the current varlist.";
GetPrefixDt::usage = "GetPrefixDt[] returns the prefix used for time derivatives of variables.";
SetPrefixDt::usage = "SetPrefixDt[prefix] sets the prefix used for time derivatives of variables.";

(* Writefile.wl state accessors *)
SetMainPrint::usage = "SetMainPrint[content] sets the content to be written as the main body of the output file.";
GetMainPrint::usage = "GetMainPrint[] returns and evaluates the content set by SetMainPrint.";

(* Initialize global context with defaults *)
$CurrentContext = $ContextDefaults;

Begin["`Private`"];

(* ========================================= *)
(* Accessor Generation Infrastructure *)
(* ========================================= *)

(* Generate error message text based on validation rule *)
expectedValueText[contextKey_String] :=
  Module[{validValues},
    If[!KeyExistsQ[$ContextModeValidValues, contextKey],
      "a valid value",
      validValues = $ContextModeValidValues[contextKey];
      If[ListQ[validValues],
        StringJoin[ToString /@ Riffle[validValues, " or "]],
        Switch[validValues,
          _String, "a String",
          _Integer, "an Integer",
          _Association, "an Association",
          _, ToString[validValues]
        ]
      ]
    ]
  ];

(* Common setter implementation - validates and updates context *)
setContextValue[contextKey_String, setterSymbol_Symbol, contextValue_] :=
  If[!ValidateContextModeValue[contextKey, contextValue],
    Throw @ Message[MessageName[setterSymbol, "EInvalidValue"], contextValue, expectedValueText[contextKey]],
    $CurrentContext = Append[$CurrentContext, contextKey -> contextValue]
  ];

(* Macro to define standard getter/setter pairs *)
defineAccessor[contextKey_String, getterSymbol_Symbol, setterSymbol_Symbol] := (
  getterSymbol[] := $CurrentContext[contextKey];
  setterSymbol[val_] := setContextValue[contextKey, setterSymbol, val];
  MessageName[setterSymbol, "EInvalidValue"] = "Invalid value: `1`. Expected `2`.";
);

(* ========================================= *)
(* Generate All Standard Accessors *)
(* ========================================= *)

(* Basic.wl state accessors *)
defineAccessor["CheckInputEquations", GetCheckInputEquations, SetCheckInputEquations];
defineAccessor["PVerbose", GetPVerbose, SetPVerbose];
defineAccessor["PrintDate", GetPrintDate, SetPrintDate];
defineAccessor["PrintHeaderMacro", GetPrintHeaderMacro, SetPrintHeaderMacro];
defineAccessor["GridPointIndex", GetGridPointIndex, SetGridPointIndex];
defineAccessor["TilePointIndex", GetTilePointIndex, SetTilePointIndex];
defineAccessor["SuffixUnprotected", GetSuffixUnprotected, SetSuffixUnprotected];
defineAccessor["OutputFile", GetOutputFile, SetOutputFile];
defineAccessor["Project", GetProject, SetProject];

(* ParseMode.wl state accessors *)
defineAccessor["Phase", GetPhase, SetPhase];
defineAccessor["PrintCompType", GetPrintCompType, SetPrintCompType];
defineAccessor["InitializationsMode", GetInitializationsMode, SetInitializationsMode];
defineAccessor["TensorType", GetTensorType, SetTensorType];
defineAccessor["StorageType", GetStorageType, SetStorageType];
defineAccessor["DerivsOrder", GetDerivsOrder, SetDerivsOrder];
defineAccessor["DerivsAccuracy", GetDerivsAccuracy, SetDerivsAccuracy];
defineAccessor["EquationsMode", GetEquationsMode, SetEquationsMode];
defineAccessor["IndependentVarlistIndex", GetIndependentVarlistIndex, SetIndependentVarlistIndex];
defineAccessor["WithoutGridPointIndex", GetWithoutGridPointIndex, SetWithoutGridPointIndex];
defineAccessor["UseTilePointIndex", GetUseTilePointIndex, SetUseTilePointIndex];

(* Component.wl state accessors *)
defineAccessor["MapComponentToVarlist", GetMapComponentToVarlist, SetMapComponentToVarlist];
defineAccessor["ProcessNewVarlist", GetProcessNewVarlist, SetProcessNewVarlist];
defineAccessor["SimplifyEquation", GetSimplifyEquation, SetSimplifyEquation];
defineAccessor["UseLetterForTensorComponent", GetUseLetterForTensorComponent, SetUseLetterForTensorComponent];
defineAccessor["TempVariableType", GetTempVariableType, SetTempVariableType];
defineAccessor["InterfaceWithNonCoordBasis", GetInterfaceWithNonCoordBasis, SetInterfaceWithNonCoordBasis];
defineAccessor["SuffixName", GetSuffixName, SetSuffixName];
defineAccessor["PrefixDt", GetPrefixDt, SetPrefixDt];

(* ========================================= *)
(* Convenience Helpers *)
(* ========================================= *)

InSetCompPhase[] := $CurrentContext["Phase"] === "SetComp";
InPrintCompPhase[] := $CurrentContext["Phase"] === "PrintComp";
InInitializationsMode[] := $CurrentContext["PrintCompType"] === "Initializations";
InEquationsMode[] := $CurrentContext["PrintCompType"] === "Equations";

(* ========================================= *)
(* Special Accessors for MainPrint *)
(* ========================================= *)

(* GetMainPrint evaluates held content *)
GetMainPrint[] :=
  Module[{mainPrint},
    mainPrint = $CurrentContext["MainPrint"];
    If[Head[mainPrint] === Hold,
      ReleaseHold[mainPrint],
      mainPrint
    ]
  ];

(* SetMainPrint has HoldAll attribute to wrap content in Hold *)
SetAttributes[SetMainPrint, HoldAll];

SetMainPrint[content_] :=
  $CurrentContext = Append[$CurrentContext, "MainPrint" -> Hold[content]];

(* ========================================= *)
(* Protect All Public Symbols *)
(* ========================================= *)

Protect[
  (* Basic.wl accessors *)
  GetCheckInputEquations, SetCheckInputEquations,
  GetPVerbose, SetPVerbose,
  GetPrintDate, SetPrintDate,
  GetPrintHeaderMacro, SetPrintHeaderMacro,
  GetGridPointIndex, SetGridPointIndex,
  GetTilePointIndex, SetTilePointIndex,
  GetSuffixUnprotected, SetSuffixUnprotected,
  GetOutputFile, SetOutputFile,
  GetProject, SetProject,
  (* ParseMode.wl accessors *)
  GetPhase, SetPhase,
  InSetCompPhase, InPrintCompPhase,
  GetPrintCompType, SetPrintCompType,
  InInitializationsMode, InEquationsMode,
  GetInitializationsMode, SetInitializationsMode,
  GetTensorType, SetTensorType,
  GetStorageType, SetStorageType,
  GetDerivsOrder, SetDerivsOrder,
  GetDerivsAccuracy, SetDerivsAccuracy,
  GetEquationsMode, SetEquationsMode,
  GetIndependentVarlistIndex, SetIndependentVarlistIndex,
  GetWithoutGridPointIndex, SetWithoutGridPointIndex,
  GetUseTilePointIndex, SetUseTilePointIndex,
  (* Component.wl accessors *)
  GetMapComponentToVarlist, SetMapComponentToVarlist,
  GetProcessNewVarlist, SetProcessNewVarlist,
  GetSimplifyEquation, SetSimplifyEquation,
  GetUseLetterForTensorComponent, SetUseLetterForTensorComponent,
  GetTempVariableType, SetTempVariableType,
  GetInterfaceWithNonCoordBasis, SetInterfaceWithNonCoordBasis,
  GetSuffixName, SetSuffixName,
  GetPrefixDt, SetPrefixDt,
  (* Writefile.wl accessors *)
  GetMainPrint, SetMainPrint
];

End[];

EndPackage[];
