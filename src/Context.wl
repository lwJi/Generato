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

(* Initialize global context with defaults *)
$CurrentContext = $ContextDefaults;

Begin["`Private`"];

End[];

EndPackage[];
