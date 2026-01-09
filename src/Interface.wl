(* ::Package:: *)

(* (c) Liwei Ji, 01/2024 *)

BeginPackage["Generato`Interface`"];

Needs["Generato`Basic`"];

Needs["Generato`ParseMode`"];

Needs["Generato`Component`"];

Needs["Generato`Varlist`"];

If[Environment["QUIET"] =!= "1",
  System`Print["------------------------------------------------------------"];
  System`Print["Package Generato`Interface`, {2024, 1, 11}"];
  System`Print["------------------------------------------------------------"];
];

DefTensors::usage = "DefTensors[var1, var2, ...] defines tensors without setting their components.";

GridTensors::usage = "GridTensors[var1, var2, ...] defines tensors with grid point indices, sets their components, and returns the varlist.";

TileTensors::usage = "TileTensors[var1, var2, ...] defines tensors with tile point indices, sets their components, and returns the varlist.";

TempTensors::usage = "TempTensors[var1, var2, ...] defines temporary tensors without grid point indices, sets their components, and returns the varlist.";

SetComponents::usage = "SetComponents[varlist] sets tensor components for the given varlist.\nSetComponents[{opts}, varlist] sets components with options ChartName, IndependentIndexForEachVar, WithoutGridPointIndex, UseTilePointIndex.";

PrintEquations::usage = "PrintEquations[varlist] prints equations for tensor components in the varlist.\nPrintEquations[{opts}, varlist] prints with options ChartName, SuffixName, Mode, ExtraReplaceRules.";

NewVar::usage = "NewVar is a PrintEquations Mode option that declares new temporary variables before assignment.";

Protect[NewVar];

Main::usage = "Main is a PrintEquations Mode option that generates the main equation assignments.";

Protect[Main];

AddToMain::usage = "AddToMain is a PrintEquations Mode option that adds terms to existing equation assignments.";

Protect[AddToMain];

PrintInitializations::usage = "PrintInitializations[varlist] prints initialization code for tensor components.\nPrintInitializations[{opts}, varlist] prints with options ChartName, Mode, TensorType, StorageType, DerivsOrder, AccuracyOrder.";

MainOut::usage = "MainOut is a PrintInitializations Mode option that generates output variable declarations."

Protect[MainOut];

MainIn::usage = "MainIn is a PrintInitializations Mode option that generates input variable declarations."

Protect[MainIn];

Derivs::usage = "Derivs is a PrintInitializations Mode option that generates derivative variable declarations."

Protect[Derivs];

DerivsOrder::usage = "DerivsOrder is a PrintInitializations option that specifies the derivative order (default 1)."

Protect[DerivsOrder];

DerivsAccuracy::usage = "DerivsAccuracy is a PrintInitializations option that specifies the finite difference accuracy order (default 4)."

Protect[DerivsAccuracy];

MoreInOut::usage = "MoreInOut is a PrintInitializations Mode option that generates additional input/output declarations."

Protect[MoreInOut];

Temp::usage = "Temp is a PrintInitializations Mode option that generates temporary variable declarations."

Protect[Temp];

Scal::usage = "Scal is a PrintInitializations TensorType option indicating a scalar quantity."

Protect[Scal];

Vect::usage = "Vect is a PrintInitializations TensorType option indicating a vector quantity."

Protect[Vect];

Smat::usage = "Smat is a PrintInitializations TensorType option indicating a symmetric matrix quantity."

Protect[Smat];

GF::usage = "GF is a PrintInitializations StorageType option indicating grid function storage."

Protect[GF];

Tile::usage = "Tile is a PrintInitializations StorageType option indicating tile-based storage."

Protect[Tile];

Begin["`Private`"];

(*
    Higher functions
*)

DefTensors[vars__] :=
  Module[{arglist = List[vars], varname, symmetry, printname},
    Do[
      {varname, symmetry, printname} = ParseVar[arglist[[ivar]]];
      DefineTensor[varname, symmetry, printname]
      ,
      {ivar, 1, Length[arglist]}
    ];
    Return[arglist]
  ];

Protect[DefTensors];

GridTensors[vars__] :=
  Module[{arglist = List[vars]},
    If[GetCheckInputEquations[],
      DefTensors[vars]
      ,
      SetComponents[arglist]
    ];
    Return[arglist]
  ];

Protect[GridTensors];

TileTensors[vars__] :=
  Module[{arglist = List[vars]},
    If[GetCheckInputEquations[],
      DefTensors[vars]
      ,
      SetComponents[{UseTilePointIndex -> True}, arglist]
    ];
    Return[arglist]
  ];

Protect[TileTensors];

TempTensors[vars__] :=
  Module[{arglist = List[vars]},
    If[GetCheckInputEquations[],
      DefTensors[vars]
      ,
      SetComponents[{WithoutGridPointIndex -> True}, arglist]
    ];
    Return[arglist]
  ];

Protect[TempTensors];

(*
    Basic functions
*)

Options[SetComponents] :=
  {ChartName -> GetDefaultChart[], IndependentIndexForEachVar -> True, WithoutGridPointIndex -> False, UseTilePointIndex -> False};

SetComponents[OptionsPattern[], varlist_?ListQ] :=
  Module[{chartname, indepidx, nogpidx, tlidx},
    {chartname, indepidx, nogpidx, tlidx} = OptionValue[{ChartName, IndependentIndexForEachVar, WithoutGridPointIndex, UseTilePointIndex}];
    SetParseMode[{SetComp -> True, PrintComp -> False}];
    SetParseSetCompMode[IndependentVarlistIndex -> indepidx];
    SetParseSetCompMode[WithoutGridPointIndex -> nogpidx];
    SetParseSetCompMode[UseTilePointIndex -> tlidx];
    ParseVarlist[varlist, chartname];
  ];

Protect[SetComponents];

(**
 * \brief Print Equations for each tensor components from a Varlist.
 *
 *   \option ExtraReplaceRules: not needed in most of the cases, they are introduced to replace say coordinates representation of metric.
 *)

Options[PrintEquations] :=
  {ChartName -> GetDefaultChart[], SuffixName -> Null, Mode -> "Main", ExtraReplaceRules -> {}};

PrintEquations[OptionsPattern[], varlist_?ListQ] :=
  Module[{chartname, suffixname, mode, extrareplacerules},
    {chartname, suffixname, mode, extrareplacerules} = OptionValue[{ChartName, SuffixName, Mode, ExtraReplaceRules}];
    If[suffixname =!= Null,
      SetSuffixName[suffixname]
    ];
    SetParseMode[{PrintComp -> True, SetComp -> False}];
    SetParsePrintCompMode[{Equations -> True, Initializations -> False}];
    Which[
      StringMatchQ[mode, "Temp"],
        SetParsePrintCompEQNMode[NewVar -> True]
      ,
      StringMatchQ[mode, "Main"],
        SetParsePrintCompEQNMode[Main -> True]
      ,
      StringMatchQ[mode, "AddToMain"],
        SetParsePrintCompEQNMode[AddToMain -> True]
      ,
      True,
        Throw @ Message[PrintEquations::EMode, mode]
    ];
    ParseVarlist[{ExtraReplaceRules -> extrareplacerules}, varlist, chartname];
    CleanParsePrintCompEQNMode[];
    SetSuffixName[""];
  ];

PrintEquations::EMode = "PrintEquations mode '`1`' unsupported yet!";

Protect[PrintEquations];

(**
 * \brief Print Initialization of each tensor components from a Varlist.
 *)

Options[PrintInitializations] :=
  {ChartName -> GetDefaultChart[], Mode -> "Temp", TensorType -> "Scal", StorageType -> "GF", DerivsOrder -> 1, AccuracyOrder -> 4};

PrintInitializations[OptionsPattern[], varlist_?ListQ] :=
  Module[{chartname, mode, tensortype, storagetype, derivsorder, accuracyorder},
    {chartname, mode, tensortype, storagetype, derivsorder, accuracyorder} = OptionValue[{ChartName, Mode, TensorType, StorageType, DerivsOrder, AccuracyOrder}];
    SetParseMode[{PrintComp -> True, SetComp -> False}];
    SetParsePrintCompMode[{Initializations -> True, Equations -> False}];
    Which[
      StringMatchQ[mode, "MainOut"],
        SetParsePrintCompInitMode[MainOut -> True]
      ,
      StringMatchQ[mode, "MainIn"],
        SetParsePrintCompInitMode[MainIn -> True]
      ,
      StringMatchQ[mode, "Derivs"],
        SetParsePrintCompInitMode[Derivs -> True];
        SetParsePrintCompInitMode[DerivsOrder -> derivsorder];
        SetParsePrintCompInitMode[DerivsAccuracy -> accuracyorder]
      ,
      StringMatchQ[mode, "MoreInOut"],
        SetParsePrintCompInitMode[MoreInOut -> True]
      ,
      StringMatchQ[mode, "Temp"],
        SetParsePrintCompInitMode[Temp -> True]
      ,
      True,
        Throw @ Message[PrintInitializations::EMode, mode]
    ];
    Which[
      StringMatchQ[tensortype, "Scal"],
        SetParsePrintCompInitTensorType[Scal -> True]
      ,
      StringMatchQ[tensortype, "Vect"],
        SetParsePrintCompInitTensorType[Vect -> True]
      ,
      StringMatchQ[tensortype, "Smat"],
        SetParsePrintCompInitTensorType[Smat -> True]
      ,
      True,
        Throw @ Message[PrintInitializations::ETensorType, tensortype]
    ];
    Which[
      StringMatchQ[storagetype, "GF"],
        SetParsePrintCompInitStorageType[GF -> True]
      ,
      StringMatchQ[storagetype, "Tile"],
        SetParsePrintCompInitStorageType[Tile -> True]
      ,
      True,
        Throw @ Message[PrintInitializations::EStorageType, storagetype]
    ];
    ParseVarlist[varlist, chartname];
    CleanParsePrintCompInitMode[];
    CleanParsePrintCompInitTensorType[];
    CleanParsePrintCompInitStorageType[];
  ];

PrintInitializations::EMode = "PrintInitializations mode '`1`' unsupported yet!";

PrintInitializations::ETensorType = "PrintInitializations tensor type '`1`' unsupported yet!";

PrintInitializations::EStorageType = "PrintInitializations storage type '`1`' unsupported yet!";

Protect[PrintInitializations];

End[];

EndPackage[];
