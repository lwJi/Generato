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
      GetParseMode[PrintCompInitMainOut] || GetParseMode[PrintCompInitMoreInOut
        ],
        buf = "vreal &" <> StringTrim[ToString[compToValue], GetGridPointIndex[
          ]] <> " = gf_" <> StringTrim[ToString[varname[[0]]], GetSuffixUnprotected[
          ]] <> "(mask, index2).elts[" <> ToString[varlistindex] <> "];"
      ,
      GetParseMode[PrintCompInitMainIn],
        buf = "vreal &" <> StringTrim[ToString[compToValue], GetGridPointIndex[
          ]] <> " = gf_" <> StringTrim[ToString[varname[[0]]], GetSuffixUnprotected[
          ]] <> "(mask, index5).elts[" <> ToString[varlistindex] <> "];"
      ,
      GetParseMode[PrintCompInitTemp],
        buf = "vreal " <> ToString[compToValue] <> ";"
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
      If[x == "vreal ",
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
