(* ::Package:: *)

(* CarpetXGPU.wl, set up functions adapted to CarpetXGPU code *)

(* (c) Liwei Ji, 01/2025 *)


(******************************************************************************)
(*            Print initialization of each component of a tensor              *)
(******************************************************************************)
(**
 * \param variable, component: the records of the variable and of the component.
 * \param base: the record of the variable a declared relation names, or None.
 *
 * A grid function is read from gf_<variable> and a tile from tl_<variable>,
 * at the index the layout rule gives. A derivative that is not stored on a
 * tile is computed from the variable it differentiates, so it must declare
 * DerivativeOf: its order is the number of slots it has more, and the
 * derivative directions are its leading indices.
 *)

PrintComponentInitialization[variable_, component_, base_] :=
  Module[{mode = GetInitializationsMode[], name = component["Name"], order},
    pr[
      Which[
        (mode === "MainIn" || mode === "MainOut" || mode === "Derivs") && GetStorageType[] === "Tile",
          "const auto " <> name <> " = tl_" <> variable["Name"] <> IndexSuffix[variable, component, base] <> ".ptr;"
        ,
        mode === "MainIn" || mode === "MainOut",
          "const auto " <> name <> " = gf_" <> variable["Name"] <> IndexSuffix[variable, component, base] <> ";"
        ,
        mode === "Derivs",
          RequireRelation[variable, "DerivativeOf", mode];
          order = Length[variable["Slots"]] - Length[base["Slots"]];
          "const auto " <> name
          <> " = calcderivs" <> ToString[order] <> "_"
          <> StringJoin[ToString /@ Take[component["Indices"], order]]
          <> "(" <> BaseComponent[component, base]["Name"] <> ", p.i, p.j, p.k);"
        ,
        mode === "Temp",
          "auto " <> name <> ";"
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
    compToValue = (compname // ToValues) /. extrareplacerules;
    rhssToValue = ComputeRHSValue[coordinate, compname, extrareplacerules];
    PrintEquationByMode[compToValue, rhssToValue,
      (* MainOut formatter - standard assignment *)
      Function[{comp, rhs},
        Global`pr[CText[comp]];
        Global`pr["="];
        Global`pr[CText[rhs]];
        Global`pr[";\n"]
      ],
      (* Temp formatter - CarpetXGPU specific: const prefix, the bare name of the component *)
      Function[{comp, rhs},
        Global`pr["const " <> GetTempVariableType[] <> " "];
        Global`pr[ComponentRecord[compname]["Name"]];
        Global`pr["="];
        Global`pr[CText[rhs]];
        Global`pr[";\n"]
      ]
    ]
  ];

Protect[PrintComponentEquation];


(******************************************************************************)
(*                                Write to files                              *)
(******************************************************************************)

WriteToFile[GetOutputFile[]];
