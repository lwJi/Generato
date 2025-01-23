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

replaceRule = Module[{cterm},
  cterm[sign_, x_, di_] :=
    "p.I" <> sign <> If[ToExpression[x] == 1, "", x <> "*"]
          <> "p.DI[" <> di <> "]";
  {
    "dx[DI]" -> "p.DX[D]",
    "[" -> "(",
    "]" -> ")",
    "c0" -> "p.I",
    "p" ~~ x : DigitCharacter .. :> cterm["+", x, "D"],
    "m" ~~ x : DigitCharacter .. :> cterm["-", x, "D"]
  }
];

PrintFDExpression[accuracyord_?IntegerQ, fdord_?IntegerQ] :=
  Module[{stencils, solution, buf},
    stencils = GetCenteringStencils[accuracyord];
    solution = GetFiniteDifferenceCoefficients[stencils, fdord];
    buf = "    " <> ToString[CForm[
      (Sum[
        index = stencils[[i]];
        (Subscript[c, index] /. solution) gf[[GetGFIndexName[index]]],
        {i, 1, Length[stencils]}] // Simplify)
      / Product[dx[[DI]], {i, 1, fdord}]
    ]] <> ";";
    pr[StringReplace[buf, replaceRule]];
  ];

replaceRuleMix2nd = Module[{cterm},
  cterm[sign1_, x_, di1_, sign2_, y_, di2_] :=
    "p.I" <> sign1
          <> If[ToExpression[x] == 1, "", x <> "*"] <> "p.DI[" <> di1 <> "]"
          <> sign2
          <> If[ToExpression[y] == 1, "", y <> "*"] <> "p.DI[" <> di2 <> "]";
  {
    "dx[DI1]" -> "p.DX[D1]",
    "dx[DI2]" -> "p.DX[D2]",
    "[" -> "(",
    "]" -> ")",
    "p" ~~ x : DigitCharacter .. ~~ "p" ~~ y : DigitCharacter ..
      :> cterm["+", x, "D1", "+", y, "D2"],
    "p" ~~ x : DigitCharacter .. ~~ "m" ~~ y : DigitCharacter ..
      :> cterm["+", x, "D1", "-", y, "D2"],
    "m" ~~ x : DigitCharacter .. ~~ "p" ~~ y : DigitCharacter ..
      :> cterm["-", x, "D1", "+", y, "D2"],
    "m" ~~ x : DigitCharacter .. ~~ "m" ~~ y : DigitCharacter ..
      :> cterm["-", x, "D1", "-", y, "D2"]
  }
];

PrintFDExpressionMix2nd[accuracyord_?IntegerQ] :=
  Module[{stencils, solution, buf},
    stencils = GetCenteringStencils[accuracyord];
    solution = GetFiniteDifferenceCoefficients[stencils, 1];
    buf = "    " <> ToString[CForm[
      (Sum[
        index1 = stencils[[i]];
        index2 = stencils[[j]];
        (Subscript[c, index1] /. solution) (Subscript[c, index2] /. solution)
        gf[[GetGFIndexNameMix2nd[index1, index2]]],
      {i, 1, Length[stencils]}, {j, 1, Length[stencils]}] // Simplify)
      / (dx[[DI1]] dx[[DI2]])
    ]] <> ";";
    pr[StringReplace[buf, replaceRuleMix2nd]];
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
    buf =
      Which[
        GetParsePrintCompInitMode[MainIn],
          "const auto &"
          <> StringTrim[ToString[compToValue], GetGridPointIndex[]]
          <> " = " <> GetInterfaceName[compname] <> ";"
        ,
        GetParsePrintCompInitMode[Derivs1st],
          "const auto " <> ToString[compToValue]
          <> " = fd_1st<" <> ToString[compname[[1]][[1]]] <> ">("
          <> StringDrop[StringDrop[ToString[compToValue], 1], {-len, -len + 0}]
          <> ", p);"
        ,
        GetParsePrintCompInitMode[Derivs2nd],
          "const auto " <> ToString[compToValue]
          <> " = fd_2nd<" <> ToString[compname[[1]][[1]]] <> "><"
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
