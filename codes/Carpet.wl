(* ::Package:: *)

(* Carpet.wl, set up functions adapted to Carpet code *)

(* (c) Liwei Ji, 01/2025 *)


(******************************************************************************)
(*                    Finite difference stencil function                      *)
(******************************************************************************)

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

GetGFIndexNameMix2nd[index1_?IntegerQ, index2_?IntegerQ] :=
  Module[{gfindex},
    gfindex = ToString[GetGFIndexName[index1]] <> ToString[GetGFIndexName[index2]];
    ToExpression[gfindex]
  ];

(* Function to print 3D indexes *)

PrintIndexes3D[accuracyord_?IntegerQ, fdord_?IntegerQ] :=
  Module[{stencils, solution},
    stencils = GetCenteringStencils[accuracyord];
    solution = GetFiniteDifferenceCoefficients[stencils, fdord];
    pr["  constexpr int DI = D - 1;"];
    Do[
      index = stencils[[i]];
      If[(Subscript[c, index] /. solution) == 0, Continue[]];
      buf = "  const int " <> ToString[GetGFIndexName[index]] <>
        If[index == 0,
          " = CCTK_GFINDEX3D(cctkGH, i, j, k);"
          ,
          " = CCTK_GFINDEX3D(cctkGH, "
            <> "i + (D == 1 ? " <> ToString[index] <> " : 0),\n"
            <> "                                        "
            <> "j + (D == 2 ? " <> ToString[index] <> " : 0),\n"
            <> "                                        "
            <> "k + (D == 3 ? " <> ToString[index] <> " : 0));"
        ];
      pr[buf]
      ,
      {i, 1, Length[stencils]}
    ];
  ];

PrintIndexes3DMix2nd[accuracyord_?IntegerQ] :=
  Module[{stencils, solution},
    stencils = GetCenteringStencils[accuracyord];
    solution = GetFiniteDifferenceCoefficients[stencils, 1];
    pr["  constexpr int DI1 = D1 - 1;"];
    pr["  constexpr int DI2 = D2 - 1;"];
    Do[
      index1 = stencils[[i]];
      index2 = stencils[[j]];
      If[(Subscript[c, index1] /. solution) == 0 || (Subscript[c, index2] /. solution) == 0,
        Continue[]
      ];

      buf = "  const int " <> ToString[GetGFIndexNameMix2nd[index1, index2]] <>
      If[index1 != 0 && index2 != 0,
        If[index1 == index2,
          " = CCTK_GFINDEX3D(cctkGH, "
            <> "i + (D1 != 1 && D2 != 1 ? 0 : " <> ToString[index1] <> "),\n"
            <> "                                          "
            <> "j + (D1 != 2 && D2 != 2 ? 0 : " <> ToString[index1] <> "),\n"
            <> "                                          "
            <> "k + (D1 != 3 && D2 != 3 ? 0 : " <> ToString[index1] <> "));"
          ,
          " = CCTK_GFINDEX3D(cctkGH, "
            <> "i + (D1 != 1 && D2 != 1 ? 0 : (D1 == 1 ? " <> ToString[index1] <> " : " <> ToString[index2] <> ")),\n"
            <> "                                          "
            <> "j + (D1 != 2 && D2 != 2 ? 0 : (D1 == 2 ? " <> ToString[index1] <> " : " <> ToString[index2] <> ")),\n"
            <> "                                          "
            <> "k + (D1 != 3 && D2 != 3 ? 0 : (D1 == 3 ? " <> ToString[index1] <> " : " <> ToString[index2] <> ")));"
        ]
        ,
        If[index1 == 0 && index2 == 0,
          " = CCTK_GFINDEX3D(cctkGH, i, j, k);"
          ,
          If[index1 == 0,
            " = CCTK_GFINDEX3D(cctkGH, "
              <> "i + (D2 == 1 ? " <> ToString[index2] <> " : 0), "
              <> "j + (D2 == 2 ? " <> ToString[index2] <> " : 0), "
              <> "k + (D2 == 3 ? " <> ToString[index2] <> " : 0));"
            ,
            " = CCTK_GFINDEX3D(cctkGH, "
              <> "i + (D1 == 1 ? " <> ToString[index1] <> " : 0), "
              <> "j + (D1 == 2 ? " <> ToString[index1] <> " : 0), "
              <> "k + (D1 == 3 ? " <> ToString[index1] <> " : 0));"
          ]
        ]
      ];
      pr[buf]
      ,
      {i, 1, Length[stencils]}, {j, 1, Length[stencils]}
    ];
  ];

(* Function to print FD expression *)

PrintFDExpression[accuracyord_?IntegerQ, fdord_?IntegerQ] :=
  Module[{stencils, solution, buf},
    stencils = GetCenteringStencils[accuracyord];
    solution = GetFiniteDifferenceCoefficients[stencils, fdord];
    buf = "    " <> ToString[CForm[
      (Sum[
        index = stencils[[i]];
        (Subscript[c, index] /. solution) gf[[GetGFIndexName[index]]],
        {i, 1, Length[stencils]}] // Simplify)
      Product[idx[[DI]], {i, 1, fdord}]
    ]] <> ";";
    pr[buf];
  ];

PrintFDExpressionMix2nd[accuracyord_?IntegerQ] :=
  Module[{stencils, solution, buf},
    stencils = GetCenteringStencils[accuracyord];
    solution = GetFiniteDifferenceCoefficients[stencils, 1];
    buf = "    " <> ToString[CForm[
      (Sum[
        index1 = stencils[[i]];
        index2 = stencils[[j]];
        (Subscript[c, index1] /. solution) (Subscript[c, index2] /. solution) gf[[GetGFIndexNameMix2nd[index1, index2]]],
        {i, 1, Length[stencils]}, {j, 1, Length[stencils]}] // Simplify)
      idx[[DI1]] idx[[DI2]]
    ]] <> ";";
    pr[buf];
  ];

(******************************************************************************)
(*                               Misc functions                               *)
(******************************************************************************)

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


(******************************************************************************)
(*            Print initialization of each component of a tensor              *)
(******************************************************************************)

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


(******************************************************************************)
(*               Print equation of each component of a tensor                 *)
(******************************************************************************)

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


(******************************************************************************)
(*                                Write to files                              *)
(******************************************************************************)

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
    "/* Produced with Generato" <>
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
