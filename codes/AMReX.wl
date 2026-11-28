(* ::Package:: *)

(* AMReX.wl, the backend table of AMReX code *)

(* (c) Liwei Ji, 08/2025 *)

(*
    The file is one DefineBackend call. Its formatter is bound by the With
    around the call, so the file assigns no symbol. A formatter takes one
    statement and returns its lines, and reads nothing else.

    A load brings one component into scope as a reference of its bare name:

      gridLoad    a variable is read from the array of its name, at the index
                  the layout rule gives

    A store is rendered by the defaults in src/Backend.wl, except that a
    temporary is declared const, by the bare name of the component.
*)

With[
  {
    gridLoad =
      Function[statement,
        {
          "auto &" <> statement["Component", "Name"]
          <> " = " <> statement["Variable", "Name"]
          <> IndexSuffix[statement["Variable"], statement["Component"], statement["Base"]] <> ";"
        }
      ]
  },
  DefineBackend["AMReX", <|
    "Load" -> <|
      "In" -> <|"Grid" -> gridLoad|>,
      "Out" -> <|"Grid" -> gridLoad|>
    |>,
    "Store" -> <|"Temp" -> <|All -> DeclareAssign|>|>
  |>]
];

(* Write to files *)

WriteToFile["AMReX", GetOutputFile[]];
