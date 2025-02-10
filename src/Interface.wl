(* ::Package:: *)

(* (c) Liwei Ji, 01/2024 *)

BeginPackage["Generato`Interface`"];

Needs["Generato`Basic`"];

Needs["Generato`ParseMode`"];

Needs["Generato`Component`"];

Needs["Generato`Varlist`"];

Print["------------------------------------------------------------"];

Print["Package Generato`Interface`, {2024, 1, 11}"];

Print["------------------------------------------------------------"];

DefTensors::usage = "DefTensors[vars] define tensors (without setting its components)";

GridTensors::usage = "GridTensors[vars] define grid tensors (with grid point index), set components and return the varlist";

TempTensors::usage = "TempTensors[vars] define temp tensors (without grid point index), set components and return the varlist";

SetComponents::usage = "SetComponents[{ChartName->..., IndependentIndexForEachVar->..., WithoutGridPointIndex->...}, varlist] set components of varlist.";

PrintEquations::usage = "PrintEquations[{ChartName->..., SuffixName->..., Mode->...}, varlist] print equations of varlist.";

NewVar::usage = "PrintEquations option.";

Protect[NewVar];

Main::usage = "PrintEquations option.";

Protect[Main];

AddToMain::usage = "PrintEquations option.";

Protect[AddToMain];

PrintInitializations::usage = "PrintInitializations[{ChartName->..., Mode->...}, varlist] print initialization of varlist.";

MainOut::usage = "PrintInitializations option."

Protect[MainOut];

MainIn::usage = "PrintInitializations option."

Protect[MainIn];

DerivsOrder::usage = "PrintInitializations option."

Protect[DerivsOrder];

DerivsAccuracy::usage = "PrintInitializations option."

Protect[DerivsAccuracy];

MoreInOut::usage = "PrintInitializations option."

Protect[MoreInOut];

Temp::usage = "PrintInitializations option."

Protect[Temp];

Scal::usage = "PrintInitializations tensor type option."

Protect[Scal];

Vect::usage = "PrintInitializations tensor type option."

Protect[Vect];

Smat::usage = "PrintInitializations tensor type option."

Protect[Smat];

GF::usage = "PrintInitializations storage type option."

Protect[GF];

Tile::usage = "PrintInitializations storage type option."

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
    Baisc functions
*)

Options[SetComponents] :=
  {ChartName -> GetDefaultChart[], IndependentIndexForEachVar -> True, WithoutGridPointIndex -> False};

SetComponents[OptionsPattern[], varlist_?ListQ] :=
  Module[{chartname, indepidx, nogpidx},
    {chartname, indepidx, nogpidx} = OptionValue[{ChartName, IndependentIndexForEachVar, WithoutGridPointIndex}];
    SetParseMode[{SetComp -> True, PrintComp -> False}];
    SetParseSetCompMode[IndependentVarlistIndex -> indepidx];
    SetParseSetCompMode[WithoutGridPointIndex -> nogpidx];
    ParseVarlist[varlist, chartname];
  ];

Protect[SetComponents];

Options[PrintEquations] :=
  {ChartName -> GetDefaultChart[], SuffixName -> Null, Mode -> "Main"};

PrintEquations[OptionsPattern[], varlist_?ListQ] :=
  Module[{chartname, suffixname, mode},
    {chartname, suffixname, mode} = OptionValue[{ChartName, SuffixName, Mode}];
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
    ParseVarlist[varlist, chartname];
    CleanParsePrintCompEQNMode[];
    SetSuffixName[""];
  ];

PrintEquations::EMode = "PrintEquations mode '`1`' unsupported yet!";

Protect[PrintEquations];

Options[PrintInitializations] :=
  {ChartName -> GetDefaultChart[], Mode -> "Temp", TensorType -> "Scal", StorageType -> "GF", AccuracyOrder -> 4};

PrintInitializations[OptionsPattern[], varlist_?ListQ] :=
  Module[{chartname, mode},
    {chartname, mode, tensortype, storagetype, accuracyorder} = OptionValue[{ChartName, Mode, TensorType, StorageType, AccuracyOrder}];
    SetParseMode[{PrintComp -> True, SetComp -> False}];
    SetParsePrintCompMode[{Initializations -> True, Equations -> False}];
    Which[
      StringMatchQ[mode, "MainOut"],
        SetParsePrintCompInitMode[MainOut -> True]
      ,
      StringMatchQ[mode, "MainIn"],
        SetParsePrintCompInitMode[MainIn -> True]
      ,
      StringMatchQ[mode, "Derivs1st"],
        SetParsePrintCompInitMode[DerivsOrder -> 1];
        SetParsePrintCompInitMode[DerivsAccuracy -> accuracyorder]
      ,
      StringMatchQ[mode, "Derivs2nd"],
        SetParsePrintCompInitMode[DerivsOrder -> 2];
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
