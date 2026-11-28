(* ::Package:: *)

(* Nmesh.wl, the backend table of Nmesh code *)

(* (c) Liwei Ji, 01/2024 *)

(*
    The file is one DefineBackend call. Its formatters are bound by the Withs
    around the call, so the file assigns no symbol. A formatter takes one
    statement and returns its lines, and reads nothing else.

    A variable is found by an index: the name of the variable, one letter per
    slot, "x" for a spatial index and "t" for a spacetime one, which is the
    index of its first component, and the slot of the component after that.

    A load brings one component into scope as a pointer of its bare name:

      inLoad          an evolved variable is read from vlu, at its index in
                      the project
      outLoad         a right-hand side is read from vlr, at the index of the
                      variable it is the time derivative of. It reads the
                      relation, so it is registered with Requires
      moreInOutLoad   any other variable of the node is read at its own index
      declare         a temporary is declared, to be assigned later

    A store is rendered by the defaults in src/Backend.wl: the table states no
    "Store" key.
*)

With[
  {
    index =
      Function[{name, statement},
        name
        <> StringJoin[If[Is3DAbstractIndex[#], "x", "t"]& /@ statement["Variable", "Slots"]]
        <> If[statement["Component", "Slot"] == 0, "", "+" <> ToString[statement["Component", "Slot"]]]
      ]
  },
  With[
    {
      inLoad =
        Function[statement,
          {
            "double *" <> statement["Component", "Name"]
            <> " = Vard(node, Vind(vlu," <> statement["Config", "Project"]
            <> "->i_" <> index[statement["Variable", "Name"], statement] <> "));"
          }
        ],
      outLoad =
        Function[statement,
          {
            "double *" <> statement["Component", "Name"]
            <> " = Vard(node, Vind(vlr," <> statement["Config", "Project"]
            <> "->i_" <> index[statement["Base", "Name"], statement] <> "));"
          }
        ],
      moreInOutLoad =
        Function[statement,
          {
            "double *" <> statement["Component", "Name"]
            <> " = Vard(node, i" <> index[statement["Variable", "Name"], statement] <> ");"
          }
        ],
      declare =
        Function[statement,
          {"double " <> statement["Component", "Name"] <> ";"}
        ]
    },
    DefineBackend["Nmesh", <|
      "Load" -> <|
        "In" -> <|"Grid" -> inLoad|>,
        "Out" -> <|"Grid" -> Requires["TimeDerivativeOf", outLoad]|>,
        "MoreInOut" -> <|"Grid" -> moreInOutLoad|>,
        "Temp" -> <|"Temp" -> declare|>
      |>
    |>]
  ]
];

(* Write to files *)

WriteToFile["Nmesh", GetOutputFile[]];
