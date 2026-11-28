(* ::Package:: *)

(* (c) Liwei Ji, 01/2024 *)

BeginPackage["Generato`Interface`"];

Needs["Generato`Failure`"];

Needs["Generato`Context`"];

Needs["Generato`Basic`"];

Needs["Generato`ParseMode`"];

Needs["Generato`Component`"];

Needs["Generato`Backend`"];

Needs["Generato`Emit`"];

Needs["Generato`BackendCommon`"];

If[Environment["QUIET"] =!= "1",
  System`Print["------------------------------------------------------------"];
  System`Print["Package Generato`Interface`, {2024, 1, 11}"];
  System`Print["------------------------------------------------------------"];
];

PrintEquations::usage = "PrintEquations[varlist] prints equations for tensor components in the varlist.\nPrintEquations[{opts}, varlist] prints with options ChartName, SuffixName, Mode, ExtraReplaceRules.";

PrintComponentEquation::usage = "PrintComponentEquation[coordinate, comp, extrareplacerules] prints the equation of one component, written in xAct's notation, say gam[{1, -cart}, {2, -cart}]. The mode and the suffix name are the ones in force: a script calls it inside WithMode with \"EquationsMode\" set. comp must be an independent component of a bound variable.";

PrintInitializations::usage = "PrintInitializations[varlist] prints initialization code for tensor components.\nPrintInitializations[{opts}, varlist] prints with options ChartName, Mode, StorageType. TensorType, DerivsOrder and DerivsAccuracy are accepted and not read: the index layout and the order of a derivative follow from the DerivativeOf its variable declares. TensorType -> \"Vect\" or \"Smat\" on a variable that declares no DerivativeOf stops the run.";

Begin["`Private`"];

(*
  PrintVarlist - walks the component records of a varlist.
  Each var spec resolves through its head to the record of a bound
  variable, and print is given the variable record and each of its
  component records, in the order BindVariable gave them.
*)
PrintVarlist[varlist_List, print_] :=
  Scan[
    Function[var,
      With[{variable = VariableRecord[var]},
        Scan[print[variable, #]&, variable["Components"]]
      ]
    ]
    ,
    varlist
  ];

(*
  The equation print calls are an adapter: each builds one Store statement
  per component and prints the lines the backend of the write under way
  renders it to. They hold no formatting, and they are deleted when a script
  hands Generate its program.

  A mode of the old print calls is a mode of a Store statement. caller is
  the print call whose mode it is, and its message says what it accepts.
*)
$StoreModes = <|"Temp" -> "Temp", "MainOut" -> "Out", "AddToMainOut" -> "AddTo"|>;

StoreMode[caller_Symbol, mode_] :=
  If[KeyExistsQ[$StoreModes, mode],
    $StoreModes[mode]
    ,
    Stop[MessageName[caller, "EMode"], mode]
  ];

(*
  StoreStatement - one component and its right-hand side as a statement.
  The statement is self-contained: it carries the records, the evaluated
  right-hand side and a copy of the user configuration, so the formatter
  that gets it reads nothing else. The replace rules are applied to the
  right-hand side and to what the equation assigns to, the "Expr" of the
  statement's copy of the component record; the "Name" a declaration prints
  is not rewritten.
*)
StoreStatement[mode_String, suffixname_String, coordinate_, variable_Association, component_Association, extrareplacerules_] :=
  <|
    "Kind" -> "Store",
    "Mode" -> mode,
    "Variable" -> variable,
    "Component" -> MapAt[ReplaceAll[extrareplacerules], component, "Expr"],
    "Base" -> BaseRecord[variable],
    "RHS" -> ComputeRHSValue[coordinate, component["Comp"], suffixname, extrareplacerules],
    "Config" -> GetConfiguration[]
  |>;

PrintStore[table_Association, mode_String, suffixname_String, coordinate_, variable_Association, component_Association, extrareplacerules_] :=
  Module[{},
    PrintVerbose["    PrintComponentEquation ", component["Comp"], "..."];
    Scan[Global`pr, Render[table, {StoreStatement[mode, suffixname, coordinate, variable, component, extrareplacerules]}]]
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
  Module[{chartname, suffixname, mode, extrareplacerules, storemode, table},
    {chartname, suffixname, mode, extrareplacerules} = OptionValue[{ChartName, SuffixName, Mode, ExtraReplaceRules}];
    storemode = StoreMode[PrintEquations, mode];
    suffixname = If[suffixname === Null, "", suffixname];
    If[!StringQ[suffixname],
      Stop[PrintEquations::ESuffixName, suffixname]
    ];
    table = CurrentBackend[];
    PrintVarlist[
      varlist
      ,
      Function[{variable, component},
        RequireChart[variable, component, chartname];
        PrintStore[table, storemode, suffixname, chartname, variable, component, extrareplacerules]
      ]
    ]
  ];

PrintEquations::EMode = "PrintEquations has no Mode \"`1`\". It takes \"Temp\", \"MainOut\" and \"AddToMainOut\".";

PrintEquations::ESuffixName = "SuffixName -> `1` is not a string. It is the suffix name an equation was set with, say SuffixName -> \"Msqr\".";

Protect[PrintEquations];

(*
  PrintComponentEquation - the same path for one component.
  A script that prints single components in its own order calls it inside
  WithMode, so the mode and the suffix name are read from the keys in force.
  The component is the one the script wrote, in the chart it wrote it in;
  coordinate is the chart the right-hand side is evaluated in.
*)
PrintComponentEquation[coordinate_, compname_, extrareplacerules_] :=
  PrintStore[
    CurrentBackend[],
    StoreMode[PrintComponentEquation, GetEquationsMode[]],
    GetSuffixName[],
    coordinate,
    VariableRecord[Head[compname]],
    ComponentRecord[compname],
    extrareplacerules
  ];

PrintComponentEquation::EMode = "PrintComponentEquation is called with the EquationsMode `1` in force. It takes \"Temp\", \"MainOut\" and \"AddToMainOut\", set by WithMode[{\"EquationsMode\" -> mode}, ...].";

Protect[PrintComponentEquation];

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
      PrintVarlist[varlist, PrintComponent[chartname, #1, #2]&]
    ]
  ];

PrintInitializations::ETensorType = "`1` is loaded with TensorType -> \"`2`\" but declares no DerivativeOf. Add DerivativeOf -> v to its declaration, where v is the variable `1` is the derivative of.";

Protect[PrintInitializations];

End[];

EndPackage[];
