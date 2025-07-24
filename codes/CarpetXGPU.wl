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
          " = layout.linear(i, j, k);"
          ,
          " = layout.linear("
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
            " = layout.linear("
              <> "i + (D1 != 0 && D2 != 0 ? 0 : " <> ToString[index1] <> "), "
              <> "j + (D1 != 1 && D2 != 1 ? 0 : " <> ToString[index1] <> "), "
              <> "k + (D1 != 2 && D2 != 2 ? 0 : " <> ToString[index1] <> "));"
            ,
            " = layout.linear("
              <> "i + (D1 != 0 && D2 != 0 ? 0 : (D1 == 0 ? "
              <> ToString[index1] <> " : " <> ToString[index2] <> ")), "
              <> "j + (D1 != 1 && D2 != 1 ? 0 : (D1 == 1 ? "
              <> ToString[index1] <> " : " <> ToString[index2] <> ")), "
              <> "k + (D1 != 2 && D2 != 2 ? 0 : (D1 == 2 ? "
              <> ToString[index1] <> " : " <> ToString[index2] <> ")));"
          ]
          ,
          If[index1 == 0 && index2 == 0,
            " = layout.linear(i, j, k);"
            ,
            If[index1 == 0,
              " = layout.linear("
                <> "i + (D2 == 0 ? " <> ToString[index2] <> " : 0), "
                <> "j + (D2 == 1 ? " <> ToString[index2] <> " : 0), "
                <> "k + (D2 == 2 ? " <> ToString[index2] <> " : 0));"
              ,
              " = layout.linear("
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

Options[PrintFDExpression] := {ForDissipation -> False};

PrintFDExpression[OptionsPattern[],
                  accuracyOrd_?IntegerQ, fdOrd_?IntegerQ, strIdx_?StringQ] :=
  Module[{stencils, solution, buf, rule, fordiss},
    {fordiss} = OptionValue[{ForDissipation}];
    (* Rules for string replacements *)
    rule = {
      "invdx" -> strIdx <> "[D]"
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
      If[fordiss, invdx, Product[invdx, {i, 1, fdOrd}]]
    ]] <> ";";
    pr[StringReplace[buf, rule]];
  ];

PrintFDExpressionMix2nd[accuracyOrd_?IntegerQ, strIdx_?StringQ] :=
  Module[{stencils, solution, buf, rule},
    (* Rules for string replacements *)
    rule = {
      "invdx1" -> strIdx <> "[D1]",
      "invdx2" -> strIdx <> "[D2]"
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
  Module[{varlistindex, compToValue, varname, symmetry, buf, subbuf, len,
          fdorder, fdaccuracy},
    varlistindex = GetMapComponentToVarlist[][compname];
    compToValue = compname // ToValues;
    {varname, symmetry} = varinfo;
    len = Length[varname];

    (* set subbuf *)
    Which[
      GetParsePrintCompInitTensorType[Scal],
        subbuf = If[len == 0, "", "[" <> ToString[varlistindex] <> "]"]
      ,
      GetParsePrintCompInitTensorType[Vect],
        Which[
          len == 1,
            subbuf = "[" <> ToString[varlistindex] <> "]"
          ,
          len == 2,
            subbuf = "[" <> ToString[Mod[varlistindex, 3]] <> "]["
                         <> ToString[Quotient[varlistindex, 3]] <> "]"
          ,
          len == 3,
            subbuf = "[" <> ToString[Mod[varlistindex, 6]] <> "]["
                         <> ToString[Quotient[varlistindex, 6]] <> "]"
          ,
          True,
            Throw @ Message[PrintComponentInitialization::EVarLength]
        ]
      ,
      GetParsePrintCompInitTensorType[Smat],
        Which[
          len == 2,
            subbuf = "[" <> ToString[varlistindex] <> "]"
          ,
          len == 3,
            subbuf = "[" <> ToString[Mod[varlistindex, 3]] <> "]["
                         <> ToString[Quotient[varlistindex, 3]] <> "]"
          ,
          len == 4,
            subbuf = "[" <> ToString[Mod[varlistindex, 6]] <> "]["
                         <> ToString[Quotient[varlistindex, 6]] <> "]"
          ,
          True,
            Throw @ Message[PrintComponentInitialization::EVarLength]
        ]
      ,
      True,
        Throw @ Message[PrintComponentInitialization::EMode]
    ];
    fdorder = GetParsePrintCompInitMode[DerivsOrder];
    fdaccuracy = GetParsePrintCompInitMode[DerivsAccuracy];

    (* set buf *)
    buf =
      Which[
        GetParsePrintCompInitMode[MainIn] || GetParsePrintCompInitMode[MainOut],
          If[GetParsePrintCompInitStorageType[Tile],
            "const auto " <> StringTrim[ToString[compToValue], GetGridPointIndex[]]
            <> " = tl_" <> StringTrim[ToString[varname[[0]]]] <> subbuf <> ".ptr;"
            ,
            "const auto "
            <> StringTrim[ToString[compToValue], GetGridPointIndex[]]
            <> " = gf_" <> StringTrim[ToString[varname[[0]]]] <> subbuf <> ";"
          ]
        ,
        GetParsePrintCompInitMode[Derivs],
          offset = fdorder - 1;
          If[GetParsePrintCompInitStorageType[Tile],
            "const auto " <> StringTrim[ToString[compToValue], GetTilePointIndex[]]
            <> " = tl_" <> StringTrim[ToString[varname[[0]]]] <> subbuf <> ".ptr;"
            ,
            tensorname = StringDrop[ToString[compToValue], {-len, -len + offset}];
            pos1stbackwards =
              If[StringStartsQ[tensorname, "d"],
                fdorder
                ,
                StringPosition[tensorname, "d"][[-1]][[1]]
              ];
            "const auto " <> ToString[compToValue]
            <> " = calcderivs" <> ToString[fdorder] <> "_"
            <> StringRiffle[
                Table[ToString[compname[[i]][[1]]], {i, 1, fdorder}], ""]
            <> "("
            <> StringDrop[tensorname, {pos1stbackwards - offset, pos1stbackwards}]
            <> ", p.i, p.j, p.k);"
          ]
        ,
        (*
        GetParsePrintCompInitMode[Derivs],
          offset = fdorder - 1;
          "const auto " <> ToString[compToValue]
          <> " = fd_" <> ToString[fdorder] <> "_o" <> ToString[fdaccuracy]
          <> "<"
          <> StringRiffle[
              Table[ToString[compname[[i]][[1]]], {i, 1, fdorder}], ", "]
          <> ">(layout2, "
          <> StringDrop[
              StringDrop[ToString[compToValue], fdorder], {-len, -len + offset}]
          <> ", p.i, p.j, p.k, invDxyz);"
        ,
        *)
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
(**
 * \param extrareplacerules: not needed in most of the cases, they are
 *        introduced to replace say coordinates representation of metric.
 *)

PrintComponentEquation[coordinate_, compname_, extrareplacerules_] :=
  Module[{outputfile = GetOutputFile[], compToValue, rhssToValue},
    compToValue = compname // ToValues;
    rhssToValue =
      (compname /. {compname[[0]] -> RHSOf[compname[[0]], GetSuffixName[]]}) //
      DummyToBasis[coordinate] // TraceBasisDummy // ToValues;
    If[GetSimplifyEquation[],
      rhssToValue = rhssToValue // Simplify
    ];
    If[Length[extrareplacerules] > 0,
      rhssToValue = (rhssToValue // ToValues) /. extrareplacerules
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
