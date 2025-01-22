(* ::Package:: *)

(* Carpet.wl, set up functions adapted to Carpet code *)

(* (c) Liwei Ji, 01/2025 *)

(* Function to get GF index name *)
GetGFIndexName[index_?IntegerQ] :=
  Module[{gfindex},
    gfindex =
      Which[
        index > 0, "p" <> ToString[index],
        index < 0, "m" <> ToString[Abs[index]],
        True, "c0"
      ];
    ToExpression[gfindex]
  ];

(* Function to print 3D indexes *)
PrintIndexes3D[accuracyord_?IntegerQ, fdord_?IntegerQ] :=
  Module[{stencils, solution, index, buf},
    stencils = GetCenteringStencils[accuracyord];
    solution = GetFiniteDifferenceCoefficients[stencils, fdord];
    Do[
      index = stencils[[i]];
      If[(Subscript[c, index] /. solution) == 0, Continue[]];

      buf = "  const int " <> ToString[GetGFIndexName[index]] <>
      If[index == 0,
        " = CCTK_GFINDEX3D(cctkGH, i, j, k);"
        ,
        " = CCTK_GFINDEX3D(cctkGH, "
          <> "i + (dir == 1 ? " <> ToString[index] <> " : 0), "
          <> "j + (dir == 2 ? " <> ToString[index] <> " : 0), "
          <> "j + (dir == 3 ? " <> ToString[index] <> " : 0));"
      ];
      pr[buf]
      ,
      {i, 1, Length[stencils]}
    ];
  ];

(* Function to print FD expression *)
PrintFDExpression[accuracyord_?IntegerQ, fdord_?IntegerQ] :=
  Module[{stencils, solution, buf},
    stencils = GetCenteringStencils[accuracyord];
    solution = GetFiniteDifferenceCoefficients[stencils, fdord];

    buf = "    " <> ToString[CForm[
      Sum[
        index = stencils[[i]];
        (Subscript[c, index] /. solution) gf[[GetGFIndexName[index]]], {i, 1, Length[stencils]}]
      Product[idx[[dir-1]], {i, 1, fdord}]
      (*// Simplify*)
    ]] <> ";";
    pr[buf];
  ];

(* Function to get varialbe name in interface.ccl *)
GetInterfaceName[compname_] :=
  Module[{intfname = ToString[compname[[0]]], colist = {"t", "x", "y", "z"}},
    Do[
      coindex = compname[[icomp]][[1]];
      intfname = intfname <> colist[[coindex + 1]]
      ,
      {icomp, 1, Length[compname]}
    ];
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
          "const auto " <> ToString[compToValue] <> " = fd_1st(" <> StringDrop[StringDrop[ToString[compToValue], 1], {-ranks, -ranks + 0}]
                                                                 <> ", i, j, k, " <> ToString[compname[[1]][[1]]] <> ");"
        ,
        GetParsePrintCompInitMode[Derivs2nd],
          "const auto " <> ToString[compToValue] <> " = fd_2nd(" <> StringDrop[StringDrop[ToString[compToValue], 2], {-ranks, -ranks + 1}]
                                                                 <> ", i, j, k, " <> ToString[compname[[1]][[1]]] <> ", " <> ToString[compname[[2]][[1]]] <> ");"
        ,
        GetParsePrintCompInitMode[PreDerivs1st],
          ToString[CForm[compToValue]] <> " = fd_1st(" <> StringDrop[StringDrop[StringTrim[ToString[compToValue], GetGridPointIndex[]], 1], {-ranks, -ranks + 0}]
                                                       <> ", i, j, k, " <> ToString[compname[[1]][[1]]] <> ");"
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
