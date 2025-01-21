(* ::Package:: *)

(* Carpet.wl, set up functions adapted to Carpet code *)

(* (c) Liwei Ji, 01/2025 *)

GetInterfaceName[compname_] :=
  Module[{intfname=ToString[compname[[0]]], colist= {"t", "x", "y", "z"}},
    Do[
      coindex = compname[[icomp]][[1]];
      intfname = intfname <> colist[[coindex+1]]
      ,
      {icomp, 1, Length[compname]}];
    intfname = ToString[CForm[ToExpression[intfname <> GetGridPointIndex[]]]];
    Return[intfname];
  ];

(*
    Print initialization of each component of a tensor
*)

PrintComponentInitialization[varinfo_, compname_] :=
  Module[{varlistindex = GetMapComponentToVarlist[][compname], compToValue = compname // ToValues, varname, symmetry, buf, ranks},
    {varname, symmetry} = varinfo;
    ranks = Length[varname];
    buf =
      Which[
        GetParsePrintCompInitMode[MainIn],
          "const auto &" <> StringTrim[ToString[compToValue], GetGridPointIndex[]] <> " = " <> GetInterfaceName[compname] <> ";"
        ,
        GetParsePrintCompInitMode[Derivs1st],
          "const auto " <> ToString[compToValue] <> " = fd_1st(" <> StringDrop[StringDrop[ToString[compToValue], 1], {-ranks, -ranks+0}]
          <> ", i, j, k, " <> ToString[compname[[1]][[1]]] <> ");"
        ,
        GetParsePrintCompInitMode[Derivs2nd],
          "const auto " <> ToString[compToValue] <> " = fd_2nd(" <> StringDrop[StringDrop[ToString[compToValue], 2], {-ranks, -ranks+1}]
          <> ", i, j, k, " <> ToString[compname[[1]][[1]]] <> ", " <> ToString[compname[[2]][[1]]] <> ");"
        ,
        GetParsePrintCompInitMode[Temp],
          buf = "auto " <> ToString[compToValue] <> ";"
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
          Global`pr["const " <> GetTempVariableType[] <> " "];
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
      If[x == "auto ",
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
  If[GetPrintHeaderMacro[],
    pr["#ifndef " <> StringReplace[ToUpperCase[FileNameTake[outputfile]], "." -> "_"]];
    pr["#define " <> StringReplace[ToUpperCase[FileNameTake[outputfile]], "." -> "_"]];
    pr[]
  ];
  $MainPrint[];
  pr[];
  If[GetPrintHeaderMacro[],
    pr["#endif // #ifndef " <> StringReplace[ToUpperCase[FileNameTake[outputfile]], "." -> "_"]];
    pr[]
  ];
  pr["/* " <> FileNameTake[outputfile] <> " */"];
  Print["Done generating \"", outputfile, "\"\n"];
  Close[filepointer]
];
