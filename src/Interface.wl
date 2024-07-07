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

GridTensors::usage = "GridTensors[vars] define grid tensors (with grid point index), set components and return the varlist";

TempTensors::usage = "TempTensors[vars] define temp tensors (without grid point index), set components and return the varlist";

DefTensors::usage = "DefTensors[vars] define tensors (without setting its components)";

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
MoreInOut::usage = "PrintInitializations option."
Protect[MoreInOut];
Temp::usage = "PrintInitializations option."
Protect[Temp];

Begin["`Private`"];

(*
    Higher functions
*)

GridTensors[vars__] :=
  Module[{arglist = List[vars]},
    SetComponents[arglist];
    Return[arglist]
  ];

Protect[GridTensors];

TempTensors[vars__] :=
  Module[{arglist = List[vars]},
    SetComponents[{WithoutGridPointIndex -> True}, arglist];
    Return[arglist]
  ];

Protect[TempTensors];

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

(*
    Baisc functions
*)

Options[SetComponents] :=
  {ChartName -> GetDefaultChart[], IndependentIndexForEachVar -> True, 
    WithoutGridPointIndex -> False};

SetComponents[OptionsPattern[], varlist_?ListQ] :=
  Module[{chartname, indepidx, nogpidx},
    {chartname, indepidx, nogpidx} = OptionValue[{ChartName, IndependentIndexForEachVar,
       WithoutGridPointIndex}];
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
    {chartname, suffixname, mode} = OptionValue[{ChartName, SuffixName,
       Mode}];
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
  ];

PrintEquations::EMode = "PrintEquations mode '`1`' unsupported yet!";

Protect[PrintEquations];

Options[PrintInitializations] :=
  {ChartName -> GetDefaultChart[], Mode -> "Temp"};

PrintInitializations[OptionsPattern[], varlist_?ListQ] :=
  Module[{chartname, mode},
    {chartname, mode} = OptionValue[{ChartName, Mode}];
    SetParseMode[{PrintComp -> True, SetComp -> False}];
    SetParsePrintCompMode[{Initializations -> True, Equations -> False}];
    Which[
      StringMatchQ[mode, "MainOut"],
        SetParsePrintCompInitMode[MainOut -> True]
      ,
      StringMatchQ[mode, "MainIn"],
        SetParsePrintCompInitMode[MainIn -> True]
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
    ParseVarlist[varlist, chartname];
    CleanParsePrintCompInitMode[];
  ];

PrintInitializations::EMode = "PrintInitializations mode '`1`' unsupported yet!";

Protect[PrintInitializations];

End[];

EndPackage[];
