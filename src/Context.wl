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

End[];

EndPackage[];
