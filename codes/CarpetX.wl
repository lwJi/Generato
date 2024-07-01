(* ::Package:: *)

(* CarpetX.wl, set up functions adapted to CarpetX code *)

(* (c) Liwei Ji, 06/2024 *)

(*
    Print initialization of each component of a tensor
*)

PrintComponentInitialization[varname_, compname_] :=
  Module[{varlistindex = GetMapComponentToVarlist[][compname], compToValue
     = compname // ToValues, buf},
    Which[
      GetParseMode[PrintCompInitMainOut] || GetParseMode[PrintCompInitMainIn
        ] || GetParseMode[PrintCompInitMoreInOut],
        buf = "CCTK_REAL &" <> StringTrim[ToString[compToValue], GetGridPointIndex[
          ]] <> " = gf_" <> StringTrim[ToString[varname[[0]]], GetSuffixUnprotected[
          ]] <> ".elts[" <> ToString[varlistindex] <> "];"
      ,
      GetParseMode[PrintCompInitTemp],
        buf = "CCTK_REAL " <> ToString[compToValue] <> ";"
      ,
      True,
        Throw @ Message[PrintComponentInitialization::EMode]
    ];
    pr[buf];
  ];

PrintComponentInitialization::EMode = "PrintComponentInitialization mode unrecognized!";

(*Protect[PrintComponentInitialization];*)

(*
    Write to files
*)

Module[{outputfile = GetOutputFile[], filepointer},
  Print["Writing to \"", outputfile, "\"...\n"];
  If[FileExistsQ[outputfile],
    Print["\"", outputfile, "\" already exist, replacing it ...\n"];
    DeleteFile[outputfile]
  ];
  (* define pr *)
  filepointer = OpenAppend[outputfile];
  pr[x_:""] :=
    Module[{},
      If[x == "double ",
        WriteString[filepointer, x]
        ,
        WriteLine[filepointer, x]
      ]
    ];
  (* print first few lines *)
  pr["/* " <> FileNameTake[outputfile] <> " */"];
  pr[
    "/* Produced with Mathematica" <>
      If[GetPrintDate[],
        " on " <> DateString[{"Month", "/", "Day", "/", "Year"}]
        ,
        ""
      ] <> " */"
  ];
  pr[];
  $MainPrint[];
  pr[];
  pr["/* " <> FileNameTake[outputfile] <> " */"];
  Print["Done generating \"", outputfile, "\"\n"];
  Close[filepointer]
];
