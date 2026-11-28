(* ::Package:: *)

(* (c) Liwei Ji, 01/2024 *)

BeginPackage["Generato`Interface`"];

Needs["Generato`Basic`"];

Needs["Generato`ParseMode`"];

Needs["Generato`Component`"];

If[Environment["QUIET"] =!= "1",
  System`Print["------------------------------------------------------------"];
  System`Print["Package Generato`Interface`, {2024, 1, 11}"];
  System`Print["------------------------------------------------------------"];
];

PrintEquations::usage = "PrintEquations[varlist] prints equations for tensor components in the varlist.\nPrintEquations[{opts}, varlist] prints with options ChartName, SuffixName, Mode, ExtraReplaceRules.";

PrintInitializations::usage = "PrintInitializations[varlist] prints initialization code for tensor components.\nPrintInitializations[{opts}, varlist] prints with options ChartName, Mode, TensorType, StorageType, DerivsOrder, DerivsAccuracy.";

Begin["`Private`"];

(*
  PrintVarlist - walks the component records of a varlist.
  Each var spec resolves through its head to the record of a bound
  variable, and each component record is handed to the backend in the
  order BindVariable gave it.
*)
PrintVarlist[varlist_List, chartname_, extrareplacerules_] :=
  Scan[
    Function[var,
      With[{variable = VariableRecord[var]},
        Scan[PrintComponent[chartname, variable, #, extrareplacerules]&, variable["Components"]]
      ]
    ]
    ,
    varlist
  ];

(**
 * \brief Print Equations for each tensor components from a Varlist.
 *
 *   \option ExtraReplaceRules: not needed in most of the cases, they are introduced to replace say coordinates representation of metric.
 *)

Options[PrintEquations] :=
  {ChartName -> GetDefaultChart[], SuffixName -> Null, Mode -> "MainOut", ExtraReplaceRules -> {}};

(* Original API: PrintEquations[{opts}, varlist] *)
PrintEquations[OptionsPattern[], varlist_?ListQ] :=
  Module[{chartname, suffixname, mode, extrareplacerules},
    {chartname, suffixname, mode, extrareplacerules} = OptionValue[{ChartName, SuffixName, Mode, ExtraReplaceRules}];
    WithMode[{
      "Phase" -> "PrintComp",
      "PrintCompType" -> "Equations",
      "EquationsMode" -> mode,
      "SuffixName" -> If[suffixname === Null, "", suffixname]
    },
      PrintVarlist[varlist, chartname, extrareplacerules]
    ]
  ];

Protect[PrintEquations];

(**
 * \brief Print Initialization of each tensor components from a Varlist.
 *)

Options[PrintInitializations] :=
  {ChartName -> GetDefaultChart[], Mode -> "Temp", TensorType -> "Scal", StorageType -> "GF", DerivsOrder -> 1, DerivsAccuracy -> 4};

(* Original API: PrintInitializations[{opts}, varlist] *)
PrintInitializations[OptionsPattern[], varlist_?ListQ] :=
  Module[{chartname, mode, tensortype, storagetype, derivsorder, accuracyorder},
    {chartname, mode, tensortype, storagetype, derivsorder, accuracyorder} =
      OptionValue[{ChartName, Mode, TensorType, StorageType, DerivsOrder, DerivsAccuracy}];

    WithMode[{
      "Phase" -> "PrintComp",
      "PrintCompType" -> "Initializations",
      "InitializationsMode" -> mode,
      "TensorType" -> tensortype,
      "StorageType" -> storagetype,
      "DerivsOrder" -> derivsorder,
      "DerivsAccuracy" -> accuracyorder
    },
      PrintVarlist[varlist, chartname, {}]
    ]
  ];

Protect[PrintInitializations];

End[];

EndPackage[];
