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

ValidateContextModeValue[key_String, value_] :=
  Module[{validValues},
    If[!KeyExistsQ[$ContextModeValidValues, key],
      (* No validation for this key - allow any value *)
      True
      ,
      validValues = $ContextModeValidValues[key];
      If[ListQ[validValues],
        MemberQ[validValues, value]
        ,
        (* It's a pattern (e.g., _Integer) - use pattern matching *)
        MatchQ[value, validValues]
      ]
    ]
  ];

(* Initialize global context with defaults *)
$CurrentContext = $ContextDefaults;

Begin["`Private`"];

(* ========================================= *)
(* Accessor Generation Helpers *)
(* ========================================= *)

(* Generate error message text based on validation rule *)
expectedValueText[key_String] :=
  Module[{validValues},
    If[!KeyExistsQ[$ContextModeValidValues, key],
      "a valid value"
      ,
      validValues = $ContextModeValidValues[key];
      If[ListQ[validValues],
        StringJoin[ToString /@ Riffle[validValues, " or "]]
        ,
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
setContextValue[key_String, setterSymbol_Symbol, value_] :=
  Module[{},
    If[!ValidateContextModeValue[key, value],
      Throw @ Message[MessageName[setterSymbol, "EInvalidValue"], value, expectedValueText[key]]
    ];
    $CurrentContext = Append[$CurrentContext, key -> value]
  ];

(* ========================================= *)
(* Basic.wl State Accessors *)
(* ========================================= *)

GetCheckInputEquations[] := $CurrentContext["CheckInputEquations"];
SetCheckInputEquations[val_] := setContextValue["CheckInputEquations", SetCheckInputEquations, val];
SetCheckInputEquations::EInvalidValue = "Invalid value: `1`. Expected `2`.";

GetPVerbose[] := $CurrentContext["PVerbose"];
SetPVerbose[val_] := setContextValue["PVerbose", SetPVerbose, val];
SetPVerbose::EInvalidValue = "Invalid value: `1`. Expected `2`.";

GetPrintDate[] := $CurrentContext["PrintDate"];
SetPrintDate[val_] := setContextValue["PrintDate", SetPrintDate, val];
SetPrintDate::EInvalidValue = "Invalid value: `1`. Expected `2`.";

GetPrintHeaderMacro[] := $CurrentContext["PrintHeaderMacro"];
SetPrintHeaderMacro[val_] := setContextValue["PrintHeaderMacro", SetPrintHeaderMacro, val];
SetPrintHeaderMacro::EInvalidValue = "Invalid value: `1`. Expected `2`.";

GetGridPointIndex[] := $CurrentContext["GridPointIndex"];
SetGridPointIndex[val_] := setContextValue["GridPointIndex", SetGridPointIndex, val];
SetGridPointIndex::EInvalidValue = "Invalid value: `1`. Expected `2`.";

GetTilePointIndex[] := $CurrentContext["TilePointIndex"];
SetTilePointIndex[val_] := setContextValue["TilePointIndex", SetTilePointIndex, val];
SetTilePointIndex::EInvalidValue = "Invalid value: `1`. Expected `2`.";

GetSuffixUnprotected[] := $CurrentContext["SuffixUnprotected"];
SetSuffixUnprotected[val_] := setContextValue["SuffixUnprotected", SetSuffixUnprotected, val];
SetSuffixUnprotected::EInvalidValue = "Invalid value: `1`. Expected `2`.";

GetOutputFile[] := $CurrentContext["OutputFile"];
SetOutputFile[val_] := setContextValue["OutputFile", SetOutputFile, val];
SetOutputFile::EInvalidValue = "Invalid value: `1`. Expected `2`.";

GetProject[] := $CurrentContext["Project"];
SetProject[val_] := setContextValue["Project", SetProject, val];
SetProject::EInvalidValue = "Invalid value: `1`. Expected `2`.";

Protect[GetCheckInputEquations, SetCheckInputEquations];
Protect[GetPVerbose, SetPVerbose];
Protect[GetPrintDate, SetPrintDate];
Protect[GetPrintHeaderMacro, SetPrintHeaderMacro];
Protect[GetGridPointIndex, SetGridPointIndex];
Protect[GetTilePointIndex, SetTilePointIndex];
Protect[GetSuffixUnprotected, SetSuffixUnprotected];
Protect[GetOutputFile, SetOutputFile];
Protect[GetProject, SetProject];

(* ========================================= *)
(* ParseMode.wl State Accessors *)
(* ========================================= *)

(* Phase helpers *)
GetPhase[] := $CurrentContext["Phase"];
SetPhase[val_] := setContextValue["Phase", SetPhase, val];
SetPhase::EInvalidValue = "Invalid value: `1`. Expected `2`.";

InSetCompPhase[] := $CurrentContext["Phase"] === "SetComp";
InPrintCompPhase[] := $CurrentContext["Phase"] === "PrintComp";

Protect[GetPhase, SetPhase, InSetCompPhase, InPrintCompPhase];

(* PrintComp type helpers *)
GetPrintCompType[] := $CurrentContext["PrintCompType"];
SetPrintCompType[val_] := setContextValue["PrintCompType", SetPrintCompType, val];
SetPrintCompType::EInvalidValue = "Invalid value: `1`. Expected `2`.";

InInitializationsMode[] := $CurrentContext["PrintCompType"] === "Initializations";
InEquationsMode[] := $CurrentContext["PrintCompType"] === "Equations";

Protect[GetPrintCompType, SetPrintCompType, InInitializationsMode, InEquationsMode];

(* Initializations mode helpers *)
GetInitializationsMode[] := $CurrentContext["InitializationsMode"];
SetInitializationsMode[val_] := setContextValue["InitializationsMode", SetInitializationsMode, val];
SetInitializationsMode::EInvalidValue = "Invalid value: `1`. Expected `2`.";

GetTensorType[] := $CurrentContext["TensorType"];
SetTensorType[val_] := setContextValue["TensorType", SetTensorType, val];
SetTensorType::EInvalidValue = "Invalid value: `1`. Expected `2`.";

GetStorageType[] := $CurrentContext["StorageType"];
SetStorageType[val_] := setContextValue["StorageType", SetStorageType, val];
SetStorageType::EInvalidValue = "Invalid value: `1`. Expected `2`.";

GetDerivsOrder[] := $CurrentContext["DerivsOrder"];
SetDerivsOrder[val_] := setContextValue["DerivsOrder", SetDerivsOrder, val];
SetDerivsOrder::EInvalidValue = "Invalid value: `1`. Expected `2`.";

GetDerivsAccuracy[] := $CurrentContext["DerivsAccuracy"];
SetDerivsAccuracy[val_] := setContextValue["DerivsAccuracy", SetDerivsAccuracy, val];
SetDerivsAccuracy::EInvalidValue = "Invalid value: `1`. Expected `2`.";

Protect[GetInitializationsMode, SetInitializationsMode];
Protect[GetTensorType, SetTensorType];
Protect[GetStorageType, SetStorageType];
Protect[GetDerivsOrder, SetDerivsOrder];
Protect[GetDerivsAccuracy, SetDerivsAccuracy];

(* Equations mode helper *)
GetEquationsMode[] := $CurrentContext["EquationsMode"];
SetEquationsMode[val_] := setContextValue["EquationsMode", SetEquationsMode, val];
SetEquationsMode::EInvalidValue = "Invalid value: `1`. Expected `2`.";

Protect[GetEquationsMode, SetEquationsMode];

(* IndexOptions helpers *)
GetIndependentVarlistIndex[] := $CurrentContext["IndependentVarlistIndex"];
SetIndependentVarlistIndex[val_] := setContextValue["IndependentVarlistIndex", SetIndependentVarlistIndex, val];
SetIndependentVarlistIndex::EInvalidValue = "Invalid value: `1`. Expected `2`.";

GetWithoutGridPointIndex[] := $CurrentContext["WithoutGridPointIndex"];
SetWithoutGridPointIndex[val_] := setContextValue["WithoutGridPointIndex", SetWithoutGridPointIndex, val];
SetWithoutGridPointIndex::EInvalidValue = "Invalid value: `1`. Expected `2`.";

GetUseTilePointIndex[] := $CurrentContext["UseTilePointIndex"];
SetUseTilePointIndex[val_] := setContextValue["UseTilePointIndex", SetUseTilePointIndex, val];
SetUseTilePointIndex::EInvalidValue = "Invalid value: `1`. Expected `2`.";

Protect[GetIndependentVarlistIndex, SetIndependentVarlistIndex];
Protect[GetWithoutGridPointIndex, SetWithoutGridPointIndex];
Protect[GetUseTilePointIndex, SetUseTilePointIndex];

(* ========================================= *)
(* Component.wl State Accessors *)
(* ========================================= *)

GetMapComponentToVarlist[] := $CurrentContext["MapComponentToVarlist"];
SetMapComponentToVarlist[val_] := setContextValue["MapComponentToVarlist", SetMapComponentToVarlist, val];
SetMapComponentToVarlist::EInvalidValue = "Invalid value: `1`. Expected `2`.";

GetProcessNewVarlist[] := $CurrentContext["ProcessNewVarlist"];
SetProcessNewVarlist[val_] := setContextValue["ProcessNewVarlist", SetProcessNewVarlist, val];
SetProcessNewVarlist::EInvalidValue = "Invalid value: `1`. Expected `2`.";

GetSimplifyEquation[] := $CurrentContext["SimplifyEquation"];
SetSimplifyEquation[val_] := setContextValue["SimplifyEquation", SetSimplifyEquation, val];
SetSimplifyEquation::EInvalidValue = "Invalid value: `1`. Expected `2`.";

GetUseLetterForTensorComponent[] := $CurrentContext["UseLetterForTensorComponent"];
SetUseLetterForTensorComponent[val_] := setContextValue["UseLetterForTensorComponent", SetUseLetterForTensorComponent, val];
SetUseLetterForTensorComponent::EInvalidValue = "Invalid value: `1`. Expected `2`.";

GetTempVariableType[] := $CurrentContext["TempVariableType"];
SetTempVariableType[val_] := setContextValue["TempVariableType", SetTempVariableType, val];
SetTempVariableType::EInvalidValue = "Invalid value: `1`. Expected `2`.";

GetInterfaceWithNonCoordBasis[] := $CurrentContext["InterfaceWithNonCoordBasis"];
SetInterfaceWithNonCoordBasis[val_] := setContextValue["InterfaceWithNonCoordBasis", SetInterfaceWithNonCoordBasis, val];
SetInterfaceWithNonCoordBasis::EInvalidValue = "Invalid value: `1`. Expected `2`.";

GetSuffixName[] := $CurrentContext["SuffixName"];
SetSuffixName[val_] := setContextValue["SuffixName", SetSuffixName, val];
SetSuffixName::EInvalidValue = "Invalid value: `1`. Expected `2`.";

GetPrefixDt[] := $CurrentContext["PrefixDt"];
SetPrefixDt[val_] := setContextValue["PrefixDt", SetPrefixDt, val];
SetPrefixDt::EInvalidValue = "Invalid value: `1`. Expected `2`.";

Protect[GetMapComponentToVarlist, SetMapComponentToVarlist];
Protect[GetProcessNewVarlist, SetProcessNewVarlist];
Protect[GetSimplifyEquation, SetSimplifyEquation];
Protect[GetUseLetterForTensorComponent, SetUseLetterForTensorComponent];
Protect[GetTempVariableType, SetTempVariableType];
Protect[GetInterfaceWithNonCoordBasis, SetInterfaceWithNonCoordBasis];
Protect[GetSuffixName, SetSuffixName];
Protect[GetPrefixDt, SetPrefixDt];

(* ========================================= *)
(* Writefile.wl State Accessors *)
(* ========================================= *)

(* Special getter - reads from $CurrentContext and evaluates held content *)
GetMainPrint[] :=
  Module[{mainPrint},
    mainPrint = $CurrentContext["MainPrint"];
    If[Head[mainPrint] === Hold,
      ReleaseHold[mainPrint]
      ,
      mainPrint
    ]
  ];

Protect[GetMainPrint];

(* Special setter - has HoldAll attribute, wraps content in Hold *)
SetAttributes[SetMainPrint, HoldAll];

SetMainPrint[content_] :=
  Module[{},
    $CurrentContext = Append[$CurrentContext, "MainPrint" -> Hold[content]]
  ];

Protect[SetMainPrint];

End[];

EndPackage[];
