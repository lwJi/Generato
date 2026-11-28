(* ::Package:: *)

(* AMReX.wl, set up functions adapted to AMReX code *)

(* (c) Liwei Ji, 08/2025 *)


(******************************************************************************)
(*            Print initialization of each component of a tensor              *)
(******************************************************************************)
(**
 * \param variable, component: the records of the variable and of the component.
 * \param base: the record of the variable a declared relation names, or None.
 *
 * A variable is read from the array of its name, at the index the layout
 * rule gives. A derivative that is not stored on a tile is computed from
 * the variable it differentiates, so it must declare DerivativeOf: its
 * order is the number of slots it has more, and the derivative directions
 * are its leading indices.
 *)

PrintComponentInitialization[variable_, component_, base_] :=
  Module[{mode = GetInitializationsMode[], name = component["Name"], order},
    pr[
      Which[
        mode === "MainIn" || mode === "MainOut",
          "auto &" <> name <> " = " <> variable["Name"] <> IndexSuffix[variable, component, base] <> ";"
        ,
        mode === "Derivs" && GetStorageType[] === "Tile",
          "const auto " <> name <> " = tl_" <> variable["Name"] <> IndexSuffix[variable, component, base] <> ".ptr;"
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
          "const auto &" <> name <> " = " <> variable["Name"] <> IndexSuffix[variable, component, base] <> ";"
        ,
        True,
          Stop[PrintComponentInitialization::EMode]
      ]
    ];
  ];

PrintComponentInitialization::EMode = "PrintComponentInitialization mode unrecognized!";

Protect[PrintComponentInitialization];


(******************************************************************************)
(*                             The backend table                              *)
(******************************************************************************)
(**
 * An equation is rendered through this table. The backend states what
 * differs from the defaults in src/Backend.wl: a temporary is declared
 * const, by the bare name of the component.
 *)

DefineBackend["AMReX", <|"Store" -> <|"Temp" -> <|All -> DeclareAssign|>|>|>];


(******************************************************************************)
(*                                Write to files                              *)
(******************************************************************************)

WriteToFile["AMReX", GetOutputFile[]];
