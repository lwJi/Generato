(* ::Package:: *)

(* AMReX.wl, set up functions adapted to AMReX code *)

(* (c) Liwei Ji, 08/2025 *)


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

PrintIndexes3D[accuracyOrd_?IntegerQ, fdOrd_?IntegerQ, strDir_?StringQ] :=
  Module[{stencils, solution},
    (* Get stencils and finite difference coefficients *)
    stencils = GetCenteringStencils[accuracyOrd];
    solution = GetFiniteDifferenceCoefficients[stencils, fdOrd];
    (* Construct the finite difference index expression *)
    Do[
      index = stencils[[i]];
      If[(Subscript[c, index] /. solution) == 0, Continue[]];
      buf = "  const T " <> ToString[GetGFIndexName[index]] <>
        If[index == 0,
          " = gf(i, j, k, comp);"
          ,
          " = gf("
            <> "i + (D == 0 ? " <> ToString[index] <> " : 0), "
            <> "j + (D == 1 ? " <> ToString[index] <> " : 0), "
            <> "k + (D == 2 ? " <> ToString[index] <> " : 0), "
            <> "comp);"
        ];
      pr[buf]
      ,
      {i, 1, Length[stencils]}
    ];
  ];

PrintFDExpression[accuracyOrd_?IntegerQ, fdOrd_?IntegerQ, strIdx_?StringQ] :=
  Module[{stencils, solution, buf, rule},
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
        (Subscript[c, index] /. solution) GetGFIndexName[index],
        {i, 1, Length[stencils]}] // Simplify) *
      Product[invdx, {i, 1, fdOrd}]
    ]] <> ";";
    pr[StringReplace[buf, rule]];
  ];


(******************************************************************************)
(*                               Misc functions                               *)
(******************************************************************************)


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
            subbuf = "[" <> ToString[Quotient[varlistindex, 3]] <> "]["
                         <> ToString[Mod[varlistindex, 3]] <> "]"
          ,
          len == 3,
            subbuf = "[" <> ToString[Quotient[varlistindex, 3]] <> "]["
                         <> ToString[Mod[varlistindex, 3]] <> "]"
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
            subbuf = "[" <> ToString[Quotient[varlistindex, 6]] <> "]["
                         <> ToString[Mod[varlistindex, 6]] <> "]"
          ,
          len == 4,
            subbuf = "[" <> ToString[Quotient[varlistindex, 6]] <> "]["
                         <> ToString[Mod[varlistindex, 6]] <> "]"
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
          "auto &" <> ToString[compToValue]
          <> " = " <> StringTrim[ToString[varname[[0]]]] <> subbuf <> ";"
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
        GetParsePrintCompInitMode[Temp],
          "const auto &" <> ToString[compToValue]
          <> " = " <> StringTrim[ToString[varname[[0]]]] <> subbuf <> ";"
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
    compToValue = (compname // ToValues) /. extrareplacerules;
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
          Global`pr[StringTrim[ToString[compToValue], GetGridPointIndex[]]];
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
