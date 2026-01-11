(* ::Package:: *)

(* Generato`Context`, context-based state management for Generato *)

(* (c) Liwei Ji, 01/2026 *)

BeginPackage["Generato`Context`"];

CreateContext::usage = "CreateContext[] creates a new GeneratoContext with default values.\nCreateContext[overrides] creates a context with specified overrides.";

GetCtx::usage = "GetCtx[ctx, key] gets a value from context.";

SetCtx::usage = "SetCtx[ctx, key, value] returns new context with updated value.";

UpdateCtx::usage = "UpdateCtx[ctx, updates] returns new context with multiple updates.\nUpdates can be an Association or a list of {key, value} pairs.";

WithContext::usage = "WithContext[ctx, body] evaluates body with ctx as current context.";

$CurrentContext::usage = "$CurrentContext is the global context for backwards compatibility during transition.";

(* Default context values - mirrors current global defaults *)
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

CreateContext[] := $ContextDefaults;

CreateContext[overrides_Association] := Join[$ContextDefaults, overrides];

GetCtx[ctx_Association, key_String] := ctx[key];

SetCtx[ctx_Association, key_String, value_] := Append[ctx, key -> value];

UpdateCtx[ctx_Association, updates_Association] := Join[ctx, updates];

UpdateCtx[ctx_Association, updates_List] :=
  Fold[SetCtx[#1, #2[[1]], #2[[2]]] &, ctx, updates];

(* Global context for backwards compatibility during transition *)
$CurrentContext = CreateContext[];

SetAttributes[WithContext, HoldRest];

WithContext[ctx_Association, body_] :=
  Block[{$CurrentContext = ctx}, body];

Begin["`Private`"];

(* Private helper functions can go here if needed *)

End[];

EndPackage[];
