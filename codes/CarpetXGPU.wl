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

(* Function to print FD expression *)

PrintFDExpression[accuracyOrd_?IntegerQ, fdOrd_?IntegerQ, strDir_?StringQ] :=
  Module[{stencils, solution, buf, cterm, rule},
    (* Helper function to format coefficients *)
    cterm[sign_, x_, dir_] :=
      "p.I" <> sign
            <> If[ToExpression[x] == 1, "", x <> "*"] <> "p.DI[" <> dir <> "]";
    (* Rules for string replacements *)
    rule = {
      "dx" -> "p.DX[" <> strDir <> "]",
      "[" -> "(",
      "]" -> ")",
      "c0" -> "p.I",
      "p" ~~ x : DigitCharacter .. :> cterm["+", x, strDir],
      "m" ~~ x : DigitCharacter .. :> cterm["-", x, strDir]
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
      / Product[dx, {i, 1, fdOrd}]
    ]] <> ";";
    pr[StringReplace[buf, rule]];
  ];

PrintFDExpressionMix2nd[accuracyOrd_?IntegerQ, strDir1_?StringQ, strDir2_?StringQ] :=
  Module[{stencils, solution, buf, cterm, rule},
    (* Helper function to format coefficients *)
    cterm[sign1_, x_, dir1_, sign2_, y_, dir2_] :=
      "p.I" <> sign1
            <> If[ToExpression[x] == 1, "", x <> "*"] <> "p.DI[" <> dir1 <> "]"
            <> sign2
            <> If[ToExpression[y] == 1, "", y <> "*"] <> "p.DI[" <> dir2 <> "]";
    (* Rules for string replacements *)
    rule = {
      "dx1" -> "p.DX[" <> strDir1 <> "]",
      "dx2" -> "p.DX[" <> strDir2 <> "]",
      "[" -> "(",
      "]" -> ")",
      "p" ~~ x : DigitCharacter .. ~~ "p" ~~ y : DigitCharacter ..
        :> cterm["+", x, strDir1, "+", y, strDir2],
      "p" ~~ x : DigitCharacter .. ~~ "m" ~~ y : DigitCharacter ..
        :> cterm["+", x, strDir1, "-", y, strDir2],
      "m" ~~ x : DigitCharacter .. ~~ "p" ~~ y : DigitCharacter ..
        :> cterm["-", x, strDir1, "+", y, strDir2],
      "m" ~~ x : DigitCharacter .. ~~ "m" ~~ y : DigitCharacter ..
        :> cterm["-", x, strDir1, "-", y, strDir2]
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
      / (dx1 dx2)
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
  Module[{varlistindex, compToValue, varname, symmetry, buf, subbuf, len},
    varlistindex = GetMapComponentToVarlist[][compname];
    compToValue = compname // ToValues;
    {varname, symmetry} = varinfo;
    len = Length[varname];

    (* set subbuf *)
    subbuf =
      Which[
        GetParsePrintCompInitTensorType[Scal],
          Which[
            len == 0, ""
            ,
            len == 1, "(" <> ToString[compname[[1]][[1]] - 1] <> ")"
            ,
            len == 2,
              If[symmetry =!= Null,
                "(" <> ToString[compname[[1]][[1]] - 1] <> ","
                    <> ToString[compname[[2]][[1]] - 1] <> ")"
                ,
                "(" <> ToString[compname[[1]][[1]] - 1] <> ")("
                    <> ToString[compname[[2]][[1]] - 1] <> ")"
              ]
            ,
            len == 3, "(" <> ToString[compname[[1]][[1]] - 1] <> ")("
                          <> ToString[compname[[2]][[1]] - 1] <> ","
                          <> ToString[compname[[3]][[1]] - 1] <> ")"
            ,
            True,
              Throw @ Message[PrintComponentInitialization::EVarLength]
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
          <> " = fd_1st<" <> ToString[compname[[1]][[1]]] <> ">("
          <> StringDrop[StringDrop[ToString[compToValue], 1], {-len, -len + 0}]
          <> ", p);"
        ,
        GetParsePrintCompInitMode[Derivs2nd],
          "const auto " <> ToString[compToValue]
          <> " = fd_2nd<" <> ToString[compname[[1]][[1]]] <> ", "
          <> ToString[compname[[2]][[1]]] <> ">("
          <> StringDrop[StringDrop[ToString[compToValue], 2], {-len, -len + 1}]
          <> ", p);"
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
ReplaceGFIndexName[
  GetOutputFile[],
  StringDrop[
    ToString[CForm[ToExpression["gf" <> GetGridPointIndex[]]]]
    ,
    2
  ] -> "(p.I)"
];
