(* ::Package:: *)

(* (c) Liwei Ji, 01/2024 *)

BeginPackage["Generato`Interface`"];

Needs["Generato`Failure`"];

Needs["Generato`Basic`"];

Needs["Generato`ParseMode`"];

Needs["Generato`Component`"];

If[Environment["QUIET"] =!= "1",
  System`Print["------------------------------------------------------------"];
  System`Print["Package Generato`Interface`, {2024, 1, 11}"];
  System`Print["------------------------------------------------------------"];
];

PrintEquations::usage = "PrintEquations[varlist] prints equations for tensor components in the varlist.\nPrintEquations[{opts}, varlist] prints with options ChartName, SuffixName, Mode, ExtraReplaceRules.";

PrintInitializations::usage = "PrintInitializations[varlist] prints initialization code for tensor components.\nPrintInitializations[{opts}, varlist] prints with options ChartName, Mode, StorageType. TensorType, DerivsOrder and DerivsAccuracy are accepted and not read: the index layout and the order of a derivative follow from the DerivativeOf its variable declares. TensorType -> \"Vect\" or \"Smat\" on a variable that declares no DerivativeOf stops the run.";

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
 *
 *   \option TensorType, DerivsOrder, DerivsAccuracy: accepted and not read. What they
 *           stated follows from the DerivativeOf a variable declares: the index
 *           layout (IndexSuffix in BackendCommon.wl) and the order of the derivative.
 *
 *   The one exception is the guard below. TensorType -> "Vect" or "Smat" used to
 *   give a two-index layout, and a variable that declares no DerivativeOf now gets
 *   the one index [v]. A call that states either for such a variable would write
 *   a file with other indices than it wrote before, so it stops with the rule to
 *   add. "Scal", the default, and any TensorType on a variable that declares
 *   DerivativeOf pass.
 *   The guard reads an option of the old print call and nothing in a backend, so
 *   it is deleted with this adapter in Phase 10, when TensorType is deleted.
 *)

Options[PrintInitializations] :=
  {ChartName -> GetDefaultChart[], Mode -> "Temp", TensorType -> "Scal", StorageType -> "GF", DerivsOrder -> 1, DerivsAccuracy -> 4};

(* Original API: PrintInitializations[{opts}, varlist] *)
PrintInitializations[OptionsPattern[], varlist_?ListQ] :=
  Module[{chartname, mode, tensortype, storagetype, derivsorder, accuracyorder},
    {chartname, mode, tensortype, storagetype, derivsorder, accuracyorder} =
      OptionValue[{ChartName, Mode, TensorType, StorageType, DerivsOrder, DerivsAccuracy}];

    (* the guard: checked for the whole varlist, before a component of it is printed *)
    If[MatchQ[tensortype, "Vect" | "Smat"],
      Scan[
        Function[var,
          With[{variable = VariableRecord[var]},
            If[variable["DerivativeOf"] === None,
              Stop[PrintInitializations::ETensorType, variable["Head"], tensortype]
            ]
          ]
        ]
        ,
        varlist
      ]
    ];

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

PrintInitializations::ETensorType = "`1` is loaded with TensorType -> \"`2`\" but declares no DerivativeOf. Add DerivativeOf -> v to its declaration, where v is the variable `1` is the derivative of.";

Protect[PrintInitializations];

End[];

EndPackage[];
