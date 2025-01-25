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
    gfindex = ToString[GetGFIndexName[index1]]
           <> ToString[GetGFIndexName[index2]];
    ToExpression[gfindex]
  ];

(* Function to print 3D indexes *)

PrintIndexes3D[accuracyOrd_?IntegerQ, fdOrd_?IntegerQ, strDir_?StringQ] :=
  Module[{stencils, solution},
    (* Get stencils and finite difference coefficients *)
    stencils = GetCenteringStencils[accuracyOrd];
    solution = GetFiniteDifferenceCoefficients[stencils, fdOrd];
    (* Construct the finite difference index expression *)
    pr["  constexpr int D = " <> strDir <> " - 1;"];
    Do[
      index = stencils[[i]];
      If[(Subscript[c, index] /. solution) == 0, Continue[]];
      buf = "  const int " <> ToString[GetGFIndexName[index]] <>
        If[index == 0,
          " = CCTK_GFINDEX3D(cctkGH, i, j, k);"
          ,
          " = CCTK_GFINDEX3D(cctkGH, "
            <> "i + (D == 0 ? " <> ToString[index] <> " : 0), "
            <> "j + (D == 1 ? " <> ToString[index] <> " : 0), "
            <> "k + (D == 2 ? " <> ToString[index] <> " : 0));"
        ];
      pr[buf]
      ,
      {i, 1, Length[stencils]}
    ];
  ];

PrintIndexes3DMix2nd[accuracyOrd_?IntegerQ,
                     strDir1_?StringQ, strDir2_?StringQ] :=
  Module[{stencils, solution},
    (* Get stencils and finite difference coefficients *)
    stencils = GetCenteringStencils[accuracyOrd];
    solution = GetFiniteDifferenceCoefficients[stencils, 1];
    (* Construct the finite difference index expression *)
    pr["  constexpr int D1 = " <> strDir1 <> " - 1;"];
    pr["  constexpr int D2 = " <> strDir2 <> " - 1;"];
    Do[
      index1 = stencils[[i]];
      index2 = stencils[[j]];
      If[(Subscript[c, index1] /. solution) == 0 ||
         (Subscript[c, index2] /. solution) == 0,
        Continue[]
      ];
      buf = "  const int " <> ToString[GetGFIndexNameMix2nd[index1, index2]] <>
        If[index1 != 0 && index2 != 0,
          If[index1 == index2,
            " = CCTK_GFINDEX3D(cctkGH, "
              <> "i + (D1 != 0 && D2 != 0 ? 0 : " <> ToString[index1] <> "), "
              <> "j + (D1 != 1 && D2 != 1 ? 0 : " <> ToString[index1] <> "), "
              <> "k + (D1 != 2 && D2 != 2 ? 0 : " <> ToString[index1] <> "));"
            ,
            " = CCTK_GFINDEX3D(cctkGH, "
              <> "i + (D1 != 0 && D2 != 0 ? 0 : (D1 == 0 ? "
              <> ToString[index1] <> " : " <> ToString[index2] <> ")),\n"
              <> "                                          "
              <> "j + (D1 != 1 && D2 != 1 ? 0 : (D1 == 1 ? "
              <> ToString[index1] <> " : " <> ToString[index2] <> ")),\n"
              <> "                                          "
              <> "k + (D1 != 2 && D2 != 2 ? 0 : (D1 == 2 ? "
              <> ToString[index1] <> " : " <> ToString[index2] <> ")));"
          ]
          ,
          If[index1 == 0 && index2 == 0,
            " = CCTK_GFINDEX3D(cctkGH, i, j, k);"
            ,
            If[index1 == 0,
              " = CCTK_GFINDEX3D(cctkGH, "
                <> "i + (D2 == 0 ? " <> ToString[index2] <> " : 0), "
                <> "j + (D2 == 1 ? " <> ToString[index2] <> " : 0), "
                <> "k + (D2 == 2 ? " <> ToString[index2] <> " : 0));"
              ,
              " = CCTK_GFINDEX3D(cctkGH, "
                <> "i + (D1 == 0 ? " <> ToString[index1] <> " : 0), "
                <> "j + (D1 == 1 ? " <> ToString[index1] <> " : 0), "
                <> "k + (D1 == 2 ? " <> ToString[index1] <> " : 0));"
            ]
          ]
        ];
      pr[buf]
      ,
      {i, 1, Length[stencils]}, {j, 1, Length[stencils]}
    ];
  ];

(* Function to print FD expression *)

PrintFDExpression[accuracyOrd_?IntegerQ, fdOrd_?IntegerQ, strDir_?StringQ,
                  strIdx_?StringQ] :=
  Module[{stencils, solution, buf, rule},
    (* Rules for string replacements *)
    rule = {
      "invdx" -> strIdx <> "[" <> strDir <> "]"
    };
    (* Get stencils and finite difference coefficients *)
    stencils = GetCenteringStencils[accuracyOrd];
    solution = GetFiniteDifferenceCoefficients[stencils, fdOrd];
    (* Construct the finite difference expression *)
    buf = "    " <> ToString[CForm[
      (Sum[
        index = stencils[[i]];
        (Subscript[c, index] /. solution) gf[[GetGFIndexName[index]]],
        {i, 1, Length[stencils]}] // Simplify)
      Product[invdx, {i, 1, fdOrd}]
    ]] <> ";";
    pr[StringReplace[buf, rule]];
  ];

PrintFDExpressionMix2nd[accuracyOrd_?IntegerQ, strDir1_?StringQ, strDir2_?StringQ,
                        strIdx_?StringQ] :=
  Module[{stencils, solution, buf, rule},
    (* Rules for string replacements *)
    rule = {
      "invdx1" -> strIdx <> "[" <> strDir1 <> "]",
      "invdx2" -> strIdx <> "[" <> strDir2 <> "]"
    };
    (* Get stencils and finite difference coefficients *)
    stencils = GetCenteringStencils[accuracyOrd];
    solution = GetFiniteDifferenceCoefficients[stencils, 1];
    (* Construct the finite difference expression *)
    buf = "    " <> ToString[CForm[
      (Sum[
        index1 = stencils[[i]];
        index2 = stencils[[j]];
        (Subscript[c, index1] /. solution) (Subscript[c, index2] /. solution)
        gf[[GetGFIndexNameMix2nd[index1, index2]]],
      {i, 1, Length[stencils]}, {j, 1, Length[stencils]}] // Simplify)
      invdx1 invdx2
    ]] <> ";";
    pr[StringReplace[buf, rule]];
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
  Module[{varlistindex, compToValue, varname, symmetry, buf, len},
    varlistindex = GetMapComponentToVarlist[][compname];
    compToValue = compname // ToValues;
    {varname, symmetry} = varinfo;
    len = Length[varname];

    (* set subbuf *)
    subbuf =
      Which[
        GetParsePrintCompInitTensorType[Scal],
          If[len == 0,
            ""
            ,
            "[" <> ToString[varlistindex] <> "]"
          ]
        ,
        GetParsePrintCompInitTensorType[Vect],
          Which[
            len == 1,
              subbuf = "(" <> ToString[compname[[1]][[1]] - 1] <> ")"
            ,
            len == 2,
              subbuf = "(" <> ToString[compname[[2]][[1]] - 1] <> ")("
                           <> ToString[compname[[1]][[1]] - 1] <> ")"
            ,
            len == 3,
              If[symmetry =!= Null,
                subbuf = "(" <> ToString[compname[[2]][[1]] - 1] <> ","
                             <> ToString[compname[[3]][[1]] - 1] <> ")("
                             <> ToString[compname[[1]][[1]] - 1] <> ")"
                ,
                subbuf = "(" <> ToString[compname[[2]][[1]] - 1] <> ")("
                             <> ToString[compname[[3]][[1]] - 1] <> ")("
                             <> ToString[compname[[1]][[1]] - 1] <> ")"
              ]
            ,
            len == 4,
              subbuf = "(" <> ToString[compname[[2]][[1]] - 1] <> ")("
                           <> ToString[compname[[3]][[1]] - 1] <> ","
                           <> ToString[compname[[4]][[1]] - 1] <> ")("
                           <> ToString[compname[[1]][[1]] - 1] <> ")"
            ,
            True,
              Throw @ Message[PrintComponentInitialization::EVarLength]
          ]
        ,
        GetParsePrintCompInitTensorType[Smat],
          Which[
            len == 2,
              subbuf = "(" <> ToString[compname[[1]][[1]] - 1] <> ","
                           <> ToString[compname[[2]][[1]] - 1] <> ")"
            ,
            len == 3,
              subbuf = "(" <> ToString[compname[[3]][[1]] - 1] <> ")("
                           <> ToString[compname[[1]][[1]] - 1] <> ","
                           <> ToString[compname[[2]][[1]] - 1] <> ")"
            ,
            len == 4,
              subbuf = "(" <> ToString[compname[[3]][[1]] - 1] <> ","
                           <> ToString[compname[[4]][[1]] - 1] <> ")("
                           <> ToString[compname[[1]][[1]] - 1] <> ","
                           <> ToString[compname[[2]][[1]] - 1] <> ")"
            ,
            True,
              Throw @ Message[PrintComponentInitialization::EVarLength]
          ]
        ,
        True,
          Throw @ Message[PrintComponentInitialization::EMode]
      ];

    (* set buf *)
    buf =
      Which[
        GetParsePrintCompInitMode[MainIn] || GetParsePrintCompInitMode[MainOut],
          "const auto &"
          <> StringTrim[ToString[compToValue], GetGridPointIndex[]]
          <> " = gf_" <> StringTrim[ToString[varname[[0]]]] <> subbuf <> ";"
        ,
        GetParsePrintCompInitMode[Derivs1st],
          "const auto " <> ToString[compToValue]
          <> " = fd_1st<" <> ToString[compname[[1]][[1]]] <> ">(cctkGH, "
          <> StringDrop[StringDrop[ToString[compToValue], 1], {-len, -len + 0}]
          <> ", i, j, k, idx);"
        ,
        GetParsePrintCompInitMode[Derivs2nd],
          "const auto " <> ToString[compToValue]
          <> " = fd_2nd<" <> ToString[compname[[1]][[1]]] <> ", "
          <> ToString[compname[[2]][[1]]] <> ">(cctkGH, "
          <> StringDrop[StringDrop[ToString[compToValue], 2], {-len, -len + 1}]
          <> ", i, j, k, idx);"
        ,
        GetParsePrintCompInitMode[PreDerivs1st],
          ToString[CForm[compToValue]] <> " = fd_1st("
          <> StringDrop[StringDrop[StringTrim[
            ToString[compToValue], GetGridPointIndex[]], 1], {-len, -len + 0}]
          <> ", i, j, k, idx);"
        ,
        GetParsePrintCompInitMode[Temp],
          buf = "auto " <> ToString[compToValue] <> ";"
        ,
        True,
          Throw @ Message[PrintComponentInitialization::EMode]
      ];
    pr[buf];
  ];

PrintComponentInitialization::EMode =
  "PrintComponentInitialization mode unrecognized!";

PrintComponentInitialization::EVarLength =
  "PrintComponentInitialization variable's tensor type unsupported!";

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

WriteToFile[GetOutputFile[]];
