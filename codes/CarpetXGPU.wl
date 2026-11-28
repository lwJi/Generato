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
(*                             The backend table                              *)
(******************************************************************************)
(**
 * An equation is rendered through this table. The backend states what
 * differs from the defaults in src/Backend.wl: a temporary is declared
 * const, by the bare name of the component.
 *)

DefineBackend["CarpetXGPU", <|"Store" -> <|"Temp" -> <|All -> DeclareAssign|>|>|>];


(******************************************************************************)
(*                                Write to files                              *)
(******************************************************************************)

WriteToFile["CarpetXGPU", GetOutputFile[]];
