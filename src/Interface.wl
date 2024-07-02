(* ::Package:: *)

(* (c) Liwei Ji, 01/2024 *)

BeginPackage["Generato`Interface`"];

Needs["Generato`Basic`"];

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

PrintInitializations::usage = "PrintInitializations[{ChartName->..., Mode->...}, varlist] print initialization of varlist.";

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
    ]
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
    SetParseModeAllToFalse[];
    SetParseMode[SetComp -> True];
    If[indepidx,
      SetParseMode[SetCompIndep -> True]
    ];
    If[nogpidx,
      SetParseMode[SetCompNoGPIndex -> True]
    ];
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
    SetParseModeAllToFalse[];
    SetParseMode[PrintComp -> True];
    SetParseMode[PrintCompEQN -> True];
    Which[
      StringMatchQ[mode, "Temp"],
        SetParseMode[PrintCompEQNNewVar -> True]
      ,
      StringMatchQ[mode, "Main"],
        SetParseMode[PrintCompEQNMain -> True]
      ,
      StringMatchQ[mode, "AddToMain"],
        SetParseMode[PrintCompEQNAddToMain -> True]
      ,
      True,
        Throw @ Message[PrintEquations::EMode, mode]
    ];
    ParseVarlist[varlist, chartname];
  ];

PrintEquations::EMode = "PrintEquations mode '`1`' unsupported yet!";

Protect[PrintEquations];

Options[PrintInitializations] :=
  {ChartName -> GetDefaultChart[], Mode -> "Temp"};

PrintInitializations[OptionsPattern[], varlist_?ListQ] :=
  Module[{chartname, mode},
    {chartname, mode} = OptionValue[{ChartName, Mode}];
    SetParseModeAllToFalse[];
    SetParseMode[PrintComp -> True];
    SetParseMode[PrintCompInit -> True];
    Which[
      StringMatchQ[mode, "MainOut"],
        SetParseMode[PrintCompInitMainOut -> True]
      ,
      StringMatchQ[mode, "MainIn"],
        SetParseMode[PrintCompInitMainIn -> True]
      ,
      StringMatchQ[mode, "MoreInOut"],
        SetParseMode[PrintCompInitMoreInOut -> True]
      ,
      StringMatchQ[mode, "Temp"],
        SetParseMode[PrintCompInitTemp -> True]
      ,
      StringMatchQ[mode, "GF3D2"],
        SetParseMode[PrintCompInitGF3D2 -> True]
      ,
      StringMatchQ[mode, "GF3D5"],
        SetParseMode[PrintCompInitGF3D5 -> True]
      ,
      StringMatchQ[mode, "VecGF3D2"],
        SetParseMode[PrintCompInitVecGF3D2 -> True]
      ,
      StringMatchQ[mode, "VecGF3D5"],
        SetParseMode[PrintCompInitVecGF3D5 -> True]
      ,
      StringMatchQ[mode, "SmatGF3D2"],
        SetParseMode[PrintCompInitSmatGF3D2 -> True]
      ,
      StringMatchQ[mode, "SmatGF3D5"],
        SetParseMode[PrintCompInitSmatGF3D5 -> True]
      ,
      True,
        Throw @ Message[PrintInitializations::EMode, mode]
    ];
    ParseVarlist[varlist, chartname];
  ];

PrintInitializations::EMode = "PrintInitializations mode '`1`' unsupported yet!";

Protect[PrintInitializations];

End[];

EndPackage[];
