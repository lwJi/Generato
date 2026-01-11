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

PrintInitializations::usage = "PrintInitializations[varlist] prints initialization code for tensor components.\nPrintInitializations[{opts}, varlist] prints with options ChartName, Mode, TensorType, StorageType, DerivsOrder, DerivsAccuracy.";

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
    WithMode[{
      {"Phase"} -> "SetComp",
      {"SetComp", "IndependentVarlistIndex"} -> indepidx,
      {"SetComp", "WithoutGridPointIndex"} -> nogpidx,
      {"SetComp", "UseTilePointIndex"} -> tlidx
    },
      ParseVarlist[varlist, chartname]
    ]
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
  Module[{chartname, suffixname, mode, extrareplacerules, eqnMode},
    {chartname, suffixname, mode, extrareplacerules} = OptionValue[{ChartName, SuffixName, Mode, ExtraReplaceRules}];
    If[suffixname =!= Null,
      SetSuffixName[suffixname]
    ];
    (* Map string mode to internal mode *)
    eqnMode = Which[
      StringMatchQ[mode, "Temp"], "NewVar",
      StringMatchQ[mode, "Main"], "Main",
      StringMatchQ[mode, "AddToMain"], "AddToMain",
      True, Throw @ Message[PrintEquations::EMode, mode]
    ];
    WithMode[{
      {"Phase"} -> "PrintComp",
      {"PrintComp", "Type"} -> "Equations",
      {"PrintComp", "Eqn", "Mode"} -> eqnMode
    },
      ParseVarlist[{ExtraReplaceRules -> extrareplacerules}, varlist, chartname]
    ];
    SetSuffixName[""];
  ];

PrintEquations::EMode = "PrintEquations mode '`1`' unsupported yet!";

Protect[PrintEquations];

(**
 * \brief Print Initialization of each tensor components from a Varlist.
 *)

Options[PrintInitializations] :=
  {ChartName -> GetDefaultChart[], Mode -> "Temp", TensorType -> "Scal", StorageType -> "GF", DerivsOrder -> 1, DerivsAccuracy -> 4};

PrintInitializations[OptionsPattern[], varlist_?ListQ] :=
  Module[{chartname, mode, tensortype, storagetype, derivsorder, accuracyorder},
    {chartname, mode, tensortype, storagetype, derivsorder, accuracyorder} =
      OptionValue[{ChartName, Mode, TensorType, StorageType, DerivsOrder, DerivsAccuracy}];

    WithMode[{
      {"Phase"} -> "PrintComp",
      {"PrintComp", "Type"} -> "Initializations",
      {"PrintComp", "Init", "Mode"} -> mode,
      {"PrintComp", "Init", "TensorType"} -> tensortype,
      {"PrintComp", "Init", "StorageType"} -> storagetype,
      {"PrintComp", "Init", "DerivsOrder"} -> derivsorder,
      {"PrintComp", "Init", "DerivsAccuracy"} -> accuracyorder
    },
      ParseVarlist[varlist, chartname]
    ]
  ];

Protect[PrintInitializations];

End[];

EndPackage[];
