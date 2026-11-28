(* ::Package:: *)

(* Nmesh.wl, set up functions adapted to Nmesh code *)

(* (c) Liwei Ji, 01/2024 *)

(* One letter per slot of a variable: "x" for a spatial index, "t" for a spacetime one *)
GetInitialComp[slots_List] :=
  StringJoin[If[Is3DAbstractIndex[#], "x", "t"]& /@ slots];

(*
    Print initialization of each component of a tensor

    variable, component: the records of the variable and of the component.
    base: the record of the variable a declared relation names, or None.

    A variable is read at the index of its first component plus the slot of
    the component. A right-hand side is stored at the index of the variable
    it is the time derivative of, so "MainOut" needs TimeDerivativeOf.
*)

PrintComponentInitialization[variable_, component_, base_] :=
  Module[{mode = GetInitializationsMode[], name = component["Name"], initialcomp, offset},
    initialcomp = GetInitialComp[variable["Slots"]];
    offset =
      If[component["Slot"] == 0,
        ""
        ,
        "+" <> ToString[component["Slot"]]
      ];
    pr[
      Which[
        mode === "MainOut",
          RequireRelation[variable, "TimeDerivativeOf", mode];
          "double *" <> name <> " = Vard(node, Vind(vlr," <> ToString[GetProject[]] <> "->i_" <> base["Name"] <> initialcomp <> offset <> "));"
        ,
        mode === "MainIn",
          "double *" <> name <> " = Vard(node, Vind(vlu," <> ToString[GetProject[]] <> "->i_" <> variable["Name"] <> initialcomp <> offset <> "));"
        ,
        mode === "MoreInOut",
          "double *" <> name <> " = Vard(node, i" <> variable["Name"] <> initialcomp <> offset <> ");"
        ,
        mode === "Temp",
          "double " <> name <> ";"
        ,
        True,
          Stop[PrintComponentInitialization::EMode]
      ]
    ];
  ];

PrintComponentInitialization::EMode = "PrintComponentInitialization mode unrecognized!";

Protect[PrintComponentInitialization];


(******************************************************************************)
(*               Print equation of each component of a tensor                 *)
(******************************************************************************)
(**
 * \param extrareplacerules: not needed in most of the cases, they are
 *        introduced to replace say coordinates representation of metric.
 *)

PrintComponentEquation[coordinate_, compname_, extrareplacerules_] :=
  Module[{compToValue, rhssToValue},
    compToValue = compname // ToValues;
    rhssToValue = ComputeRHSValue[coordinate, compname, extrareplacerules];
    PrintEquationByMode[compToValue, rhssToValue,
      (* MainOut formatter - standard assignment *)
      Function[{comp, rhs},
        Global`pr[CText[comp]];
        Global`pr["="];
        Global`pr[CText[rhs]];
        Global`pr[";\n"]
      ]
      (* Temp uses default: no const prefix, CForm[compToValue] *)
    ]
  ];

Protect[PrintComponentEquation];

(*
    Write to files
*)

WriteToFile[GetOutputFile[]];
