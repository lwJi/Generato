(* ::Package:: *)

(* Nmesh.wl, set up functions adapted to Nmesh code *)

(* (c) Liwei Ji, 01/2024 *)

GetInitialComp[varname_] :=
  Module[{initialcomp = ""},
    Do[
      If[Is3DAbstractIndex[varname[[icomp]]],
        initialcomp = initialcomp <> "x"
        ,
        initialcomp = initialcomp <> "t"
      ]
      ,
      {icomp, 1, Length[varname]}
    ];
    initialcomp
  ];

(*
    Print initialization of each component of a tensor
*)

PrintComponentInitialization[varname_, compname_] :=
  Module[{varlistindex = GetMapComponentToVarlist[][compname], compToValue
     = compname // ToValues, buf},
    Which[
      GetParseMode[PrintCompInitMainOut],
        buf =
          "double *" <> StringTrim[ToString[compToValue], GetGridPointIndex[
            ]] <> " = Vard(node, Vind(vlr," <> ToString[GetProject[]] <> "->i_" <> 
            StringTrim[ToString[varname[[0]]], (GetPrefixDt[] | GetSuffixUnprotected[
            ])] <> GetInitialComp[varname] <>
            If[varlistindex == 0,
              ""
              ,
              "+" <> ToString[varlistindex]
            ] <> "));"
      ,
      GetParseMode[PrintCompInitMainIn],
        buf =
          "double *" <> StringTrim[ToString[compToValue], GetGridPointIndex[
            ]] <> " = Vard(node, Vind(vlu," <> ToString[GetProject[]] <> "->i_" <> 
            StringTrim[ToString[varname[[0]]], GetSuffixUnprotected[]] <> GetInitialComp[
            varname] <>
            If[varlistindex == 0,
              ""
              ,
              "+" <> ToString[varlistindex]
            ] <> "));"
      ,
      GetParseMode[PrintCompInitMoreInOut],
        buf =
          "double *" <> StringTrim[ToString[compToValue], GetGridPointIndex[
            ]] <> " = Vard(node, i" <> StringTrim[ToString[varname[[0]]], GetSuffixUnprotected[
            ]] <> GetInitialComp[varname] <>
            If[varlistindex == 0,
              ""
              ,
              "+" <> ToString[varlistindex]
            ] <> ");"
      ,
      GetParseMode[PrintCompInitTemp],
        buf = "double " <> ToString[compToValue] <> ";"
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
