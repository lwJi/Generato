(* ::Package:: *)

(* Carpet.wl, set up functions adapted to Carpet code *)

(* (c) Liwei Ji, 01/2025 *)


(******************************************************************************)
(*                    Finite difference stencil function                      *)
(******************************************************************************)

(* GetGFIndexName and GetGFIndexNameMix2nd are now in BackendCommon.wl *)

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

PrintFDExpressionMix2nd[accuracyOrd_?IntegerQ,
                        strDir1_?StringQ, strDir2_?StringQ] :=
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
        (Subscript[c, index1] /. solution) (Subscript[c, index2] /. solution) *
        gf[[GetGFIndexNameMix2nd[index1, index2]]],
      {i, 1, Length[stencils]}, {j, 1, Length[stencils]}] // Simplify)
      / (dx1 dx2)
    ]] <> ";";
    pr[StringReplace[buf, rule]];
  ];


(* GetInterfaceName is now in BackendCommon.wl *)


(******************************************************************************)
(*            Print initialization of each component of a tensor              *)
(******************************************************************************)

PrintComponentInitialization[ctx_Association, varinfo_, compname_] :=
  Module[{varlistindex, compToValue, varname, symmetry, buf, subbuf, len},
    (* Extract common component info using shared function *)
    {varlistindex, compToValue, varname, symmetry, len} =
      ExtractComponentInfo[ctx, varinfo, compname];

    (* set subbuf *)
    subbuf = If[len == 0, "", "[" <> ToString[varlistindex] <> "]"];

    (* set buf *)
    buf =
      Which[
        GetInitializationsMode[ctx] === "MainIn" || GetInitializationsMode[ctx] === "MainOut",
          "const auto &"
          <> StringTrim[ToString[compToValue], GetGridPointIndex[ctx]]
          <> " = gf_" <> StringTrim[ToString[varname[[0]]]] <> subbuf <> ";"
        ,
        GetInitializationsMode[ctx] === "Derivs1st",
          "const auto " <> ToString[compToValue]
          <> " = fd_1st<" <> ToString[compname[[1]][[1]]] <> ">("
          <> StringDrop[StringDrop[ToString[compToValue], 1], {-len, -len + 0}]
          <> ", p);"
        ,
        GetInitializationsMode[ctx] === "Derivs2nd",
          "const auto " <> ToString[compToValue]
          <> " = fd_2nd<" <> ToString[compname[[1]][[1]]] <> ", "
          <> ToString[compname[[2]][[1]]] <> ">("
          <> StringDrop[StringDrop[ToString[compToValue], 2], {-len, -len + 1}]
          <> ", p);"
        ,
        GetInitializationsMode[ctx] === "Temp",
          buf = "auto " <> ToString[compToValue] <> ";"
        ,
        True,
          Throw @ Message[PrintComponentInitialization::EMode]
      ];
    pr[buf];
  ];

(* Error messages PrintComponentInitialization::EMode and ::EVarLength are in BackendCommon.wl *)

Protect[PrintComponentInitialization];


(******************************************************************************)
(*               Print equation of each component of a tensor                 *)
(******************************************************************************)
(**
 * \param extrareplacerules: not needed in most of the cases, they are
 *        introduced to replace say coordinates representation of metric.
 *)

PrintComponentEquation[ctx_Association, coordinate_, compname_, extrareplacerules_] :=
  Module[{outputfile = GetOutputFile[ctx], compToValue, rhssToValue},
    compToValue = compname // ToValues;
    rhssToValue = ComputeRHSValue[ctx, coordinate, compname, extrareplacerules];
    PrintEquationByMode[ctx, compToValue, rhssToValue,
      (* MainOut formatter - standard assignment *)
      Function[{comp, rhs},
        PutAppend[CForm[comp], outputfile];
        Global`pr["="];
        PutAppend[CForm[rhs], outputfile];
        Global`pr[";\n"]
      ],
      (* Temp formatter - CarpetXPointDesc specific: const prefix *)
      Function[{comp, rhs},
        Global`pr["const " <> GetTempVariableType[ctx] <> " "];
        PutAppend[CForm[comp], outputfile];
        Global`pr["="];
        PutAppend[CForm[rhs], outputfile];
        Global`pr[";\n"]
      ]
    ]
  ];

Protect[PrintComponentEquation];


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
