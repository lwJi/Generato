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

PrintComponentInitialization[varinfo_, compname_] :=
  Module[{varlistindex = GetMapComponentToVarlist[][compname], compToValue = compname // ToValues, varname, buf},
    varname = varinfo[[1]];
    Which[
      GetParsePrintCompInitMode[MainOut],
        buf =
          "double *" <> StringTrim[ToString[compToValue], GetGridPointIndex[]] <> " = Vard(node, Vind(vlr," <> ToString[GetProject[]] <> "->i_" <> StringTrim[ToString[varname[[0]]], (GetPrefixDt[] | GetSuffixUnprotected[])] <> GetInitialComp[varname] <>
            If[varlistindex == 0,
              ""
              ,
              "+" <> ToString[varlistindex]
            ] <> "));"
      ,
      GetParsePrintCompInitMode[MainIn],
        buf =
          "double *" <> StringTrim[ToString[compToValue], GetGridPointIndex[]] <> " = Vard(node, Vind(vlu," <> ToString[GetProject[]] <> "->i_" <> StringTrim[ToString[varname[[0]]], GetSuffixUnprotected[]] <> GetInitialComp[varname] <>
            If[varlistindex == 0,
              ""
              ,
              "+" <> ToString[varlistindex]
            ] <> "));"
      ,
      GetParsePrintCompInitMode[MoreInOut],
        buf =
          "double *" <> StringTrim[ToString[compToValue], GetGridPointIndex[]] <> " = Vard(node, i" <> StringTrim[ToString[varname[[0]]], GetSuffixUnprotected[]] <> GetInitialComp[varname] <>
            If[varlistindex == 0,
              ""
              ,
              "+" <> ToString[varlistindex]
            ] <> ");"
      ,
      GetParsePrintCompInitMode[Temp],
        buf = "double " <> ToString[compToValue] <> ";"
      ,
      True,
        Throw @ Message[PrintComponentInitialization::EMode]
    ];
    pr[buf];
  ];

PrintComponentInitialization::EMode = "PrintComponentInitialization mode unrecognized!";

(*Protect[PrintComponentInitialization];*)

PrintComponentEquation[coordinate_, compname_] :=
  Module[{outputfile = GetOutputFile[], compToValue, rhssToValue},
    compToValue = compname // ToValues;
    rhssToValue =
      (compname /. {compname[[0]] -> RHSOf[compname[[0]], GetSuffixName[]]}) //
      DummyToBasis[coordinate] //
      TraceBasisDummy //
      ToValues;
    If[GetSimplifyEquation[],
      rhssToValue = rhssToValue // Simplify
    ];
    Which[
      GetParsePrintCompEQNMode[NewVar],
        Module[{},
          Global`pr[GetTempVariableType[] <> " "];
          PutAppend[CForm[compToValue], outputfile];
          Global`pr["="];
          PutAppend[CForm[rhssToValue], outputfile];
          Global`pr[";\n"]
        ]
      ,
      GetParsePrintCompEQNMode[Main],
        Module[{},
          PutAppend[CForm[compToValue], outputfile];
          Global`pr["="];
          PutAppend[CForm[rhssToValue], outputfile];
          Global`pr[";\n"]
        ]
      ,
      GetParsePrintCompEQNMode[AddToMain],
        Module[{},
          PutAppend[CForm[compToValue], outputfile];
          Global`pr["+="];
          PutAppend[CForm[rhssToValue], outputfile];
          Global`pr[";\n"]
        ]
      ,
      True,
        Throw @ Message[PrintComponentEquation::EMode]
    ]
  ];

PrintComponentEquation::EMode = "PrintEquationMode unrecognized!";

(*Protect[PrintComponentEquation];*)

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