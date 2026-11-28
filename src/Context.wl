(* ::Package:: *)

(* Generato`Context`, the user configuration: each key is declared once, with its getter and its setter *)

(* (c) Liwei Ji, 01/2026 *)

BeginPackage["Generato`Context`", {"Generato`Failure`"}];

$CurrentContext::usage = "$CurrentContext is the user configuration: an association from key to value, with one key per defineConfig call in Context.wl. It is assigned in that file only, and a key is written by its setter.";

GetConfiguration::usage = "GetConfiguration[] returns the user configuration as an association from key to value: the settings a script makes to say how code is generated.";

ResetContext::usage = "ResetContext[] sets every key of the user configuration back to its default, through its setter.";

PrintVerbose::usage = "PrintVerbose[expr, ...] prints its arguments if PVerbose is set.";

Begin["`Private`"];

(*
  The user configuration is the whole of the context. What a step of a
  program does is written on its node, and what is known of a variable is in
  its record, so neither is a key here.
*)
$CurrentContext = <||>;

(* One function per key, which sets the key to its default *)
$Resets = {};

(* What a valid value is, for a message and for the usage of a setter *)
ExpectedText[Verbatim[True | False]] := "True or False";

ExpectedText[Verbatim[_String]] := "a String";

ExpectedText[valid_] := ToString[valid];

(*
  defineConfig - declares one key of the user configuration:

    defineConfig[key, getter, setter, default, valid, description]

  getter[] returns the value of the key. setter[value] is its one writer: it
  sets the key to a value that matches the pattern valid, and stops the run
  at one that does not. The key starts at default, set through the setter
  like any other value. description completes "getter[] returns" and
  "setter[value] sets" in the usage of the two.
*)
defineConfig[key_String, getter_Symbol, setter_Symbol, default_, valid_, description_String] := (
  getter::usage = SymbolName[getter] <> "[] returns " <> description <> ".";
  setter::usage = SymbolName[setter] <> "[value] sets " <> description <> ". value is " <> ExpectedText[valid] <> ".";
  MessageName[setter, "EInvalidValue"] = "Invalid value: `1`. Expected `2`.";
  getter[] := $CurrentContext[key];
  setter[value_] :=
    If[MatchQ[value, valid],
      $CurrentContext = Append[$CurrentContext, key -> value];
      ,
      Stop[MessageName[setter, "EInvalidValue"], value, ExpectedText[valid]]
    ];
  AppendTo[$Resets, Function[setter[default]]];
  setter[default];
  Protect[getter, setter];
);

End[];

(*
  The keys. A key is one call, and the call names its getter and its setter,
  so a search for either lands here. The calls are outside the private
  context, which makes the two names symbols of the package; defineConfig is
  private and is named through its context.
*)

`Private`defineConfig["CheckInputEquations", GetCheckInputEquations, SetCheckInputEquations, False, True | False, "whether the input equations are only being checked, in which case a binding declares its tensors and binds nothing"];

`Private`defineConfig["PVerbose", GetPVerbose, SetPVerbose, False, True | False, "whether progress messages are printed"];

`Private`defineConfig["QuietMode", GetQuietMode, SetQuietMode, Environment["QUIET"] === "1", True | False, "whether the banners of the packages and the lines that report a write are left out; the default is True when the environment variable QUIET is 1"];

`Private`defineConfig["PrintDate", GetPrintDate, SetPrintDate, True, True | False, "whether the date is printed in the header of the output file"];

`Private`defineConfig["PrintHeaderMacro", GetPrintHeaderMacro, SetPrintHeaderMacro, True, True | False, "whether the output file is wrapped in a header guard"];

`Private`defineConfig["GridPointIndex", GetGridPointIndex, SetGridPointIndex, "", _String, "the index appended to a grid variable's C name"];

`Private`defineConfig["TilePointIndex", GetTilePointIndex, SetTilePointIndex, "", _String, "the index appended to a tile variable's C name"];

`Private`defineConfig["SuffixUnprotected", GetSuffixUnprotected, SetSuffixUnprotected, "$Upt", _String, "the suffix that keeps a tensor head apart from a system symbol, and that a C name leaves out"];

`Private`defineConfig["Project", GetProject, SetProject, "TEST", _String, "the name of the project, for a backend that prints it"];

`Private`defineConfig["SimplifyEquation", GetSimplifyEquation, SetSimplifyEquation, True, True | False, "whether a right-hand side is simplified before it is written"];

`Private`defineConfig["UseLetterForTensorComponent", GetUseLetterForTensorComponent, SetUseLetterForTensorComponent, False, True | False, "whether a component is named by the letters t, x, y and z of its indices, in place of their numbers"];

`Private`defineConfig["TempVariableType", GetTempVariableType, SetTempVariableType, "double", _String, "the C type of a temporary variable"];

`Private`defineConfig["InterfaceWithNonCoordBasis", GetInterfaceWithNonCoordBasis, SetInterfaceWithNonCoordBasis, False, True | False, "whether every variable is bound without a point index, for an interface with tensors in a non-coordinate basis"];

`Private`defineConfig["PrefixDt", GetPrefixDt, SetPrefixDt, "dt", _String, "the prefix of the name of a time derivative"];

Begin["`Private`"];

GetConfiguration[] := $CurrentContext;

ResetContext[] := Scan[#[]&, $Resets];

PrintVerbose[var___] :=
  If[GetPVerbose[],
    System`Print[var]
  ];

Protect[GetConfiguration, ResetContext, PrintVerbose];

End[];

EndPackage[];
