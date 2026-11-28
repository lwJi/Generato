(* ::Package:: *)

(* CarpetXGPU.wl, the backend table of CarpetXGPU code *)

(* (c) Liwei Ji, 01/2025 *)

(*
    The file is one DefineBackend call. Its formatters are bound by the With
    around the call, so the file assigns no symbol. A formatter takes one
    statement and returns its lines, and reads nothing else.

    A load brings one component into scope as a local of its bare name:

      gridLoad    a grid function is read from gf_<variable>, at the index the
                  layout rule gives
      tileLoad    a tile is read from tl_<variable>, at the same index
      derivLoad   a derivative that is not stored is computed from the
                  component it differentiates. Its order is the number of
                  slots it has more than that variable, and the derivative
                  directions are its leading indices. It reads the relation,
                  so it is registered with Requires
      declare     a temporary is declared, to be assigned later

    A store is rendered by the defaults in src/Backend.wl, except that a
    temporary is declared const, by the bare name of the component.
*)

With[
  {
    gridLoad =
      Function[statement,
        {
          "const auto " <> statement["Component", "Name"]
          <> " = gf_" <> statement["Variable", "Name"]
          <> IndexSuffix[statement["Variable"], statement["Component"], statement["Base"]] <> ";"
        }
      ],
    tileLoad =
      Function[statement,
        {
          "const auto " <> statement["Component", "Name"]
          <> " = tl_" <> statement["Variable", "Name"]
          <> IndexSuffix[statement["Variable"], statement["Component"], statement["Base"]] <> ".ptr;"
        }
      ],
    derivLoad =
      Function[statement,
        With[{order = Length[statement["Variable", "Slots"]] - Length[statement["Base", "Slots"]]},
          {
            "const auto " <> statement["Component", "Name"]
            <> " = calcderivs" <> ToString[order] <> "_"
            <> StringJoin[ToString /@ Take[statement["Component", "Indices"], order]]
            <> "(" <> BaseComponent[statement["Component"], statement["Base"]]["Name"] <> ", p.i, p.j, p.k);"
          }
        ]
      ],
    declare =
      Function[statement,
        {"auto " <> statement["Component", "Name"] <> ";"}
      ]
  },
  DefineBackend["CarpetXGPU", <|
    "Load" -> <|
      "In" -> <|"Grid" -> gridLoad, "Tile" -> tileLoad|>,
      "Out" -> <|"Grid" -> gridLoad, "Tile" -> tileLoad|>,
      "Derivs" -> <|"Temp" -> Requires["DerivativeOf", derivLoad]|>,
      "Temp" -> <|"Temp" -> declare|>
    |>,
    "Store" -> <|"Temp" -> <|All -> DeclareAssign|>|>
  |>]
];
