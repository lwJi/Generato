(* ::Package:: *)

(* Carpet.wl, set up functions adapted to Carpet code *)

(* (c) Liwei Ji, 01/2025 *)


(******************************************************************************)
(*                    Finite difference stencil function                      *)
(******************************************************************************)

(* GetGFIndexName and GetGFIndexNameMix2nd are now in BackendCommon.wl *)

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
        {i, 1, Length[stencils]}] // Simplify) *
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
        (Subscript[c, index1] /. solution) (Subscript[c, index2] /. solution) *
        gf[[GetGFIndexNameMix2nd[index1, index2]]],
      {i, 1, Length[stencils]}, {j, 1, Length[stencils]}] // Simplify) *
      invdx1 invdx2
    ]] <> ";";
    pr[StringReplace[buf, rule]];
  ];


(* GetInterfaceName is now in BackendCommon.wl *)


(******************************************************************************)
(*            Print initialization of each component of a tensor              *)
(******************************************************************************)

PrintComponentInitialization[ctx_Association, varinfo_, compname_] :=
  Module[{varlistindex, compToValue, varname, symmetry, buf, subbuf, len,
          fdorder, fdaccuracy},
    (* Extract common component info using shared function *)
    {varlistindex, compToValue, varname, symmetry, len} =
      ExtractComponentInfo[ctx, varinfo, compname];

    (* set subbuf - CarpetXGPU uses [Mod][Quotient] ordering *)
    Which[
      GetTensorType[ctx] === "Scal",
        subbuf = If[len == 0, "", "[" <> ToString[varlistindex] <> "]"]
      ,
      GetTensorType[ctx] === "Vect",
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
      GetTensorType[ctx] === "Smat",
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
    fdorder = GetDerivsOrder[ctx];
    fdaccuracy = GetDerivsAccuracy[ctx];

    (* set buf *)
    buf =
      Which[
        GetInitializationsMode[ctx] === "MainIn" || GetInitializationsMode[ctx] === "MainOut",
          If[GetStorageType[ctx] === "Tile",
            "const auto " <> StringTrim[ToString[compToValue], GetGridPointIndex[ctx]]
            <> " = tl_" <> StringTrim[ToString[varname[[0]]]] <> subbuf <> ".ptr;"
            ,
            "const auto "
            <> StringTrim[ToString[compToValue], GetGridPointIndex[ctx]]
            <> " = gf_" <> StringTrim[ToString[varname[[0]]]] <> subbuf <> ";"
          ]
        ,
        GetInitializationsMode[ctx] === "Derivs",
          offset = fdorder - 1;
          If[GetStorageType[ctx] === "Tile",
            "const auto " <> StringTrim[ToString[compToValue], GetTilePointIndex[ctx]]
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
        GetInitializationsMode[ctx] === "Derivs",
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
        GetInitializationsMode[ctx] === "Temp",
          "auto " <> ToString[compToValue] <> ";"
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
    compToValue = (compname // ToValues) /. extrareplacerules;
    rhssToValue = ComputeRHSValue[ctx, coordinate, compname, extrareplacerules];
    PrintEquationByMode[ctx, compToValue, rhssToValue,
      (* MainOut formatter - standard assignment *)
      Function[{comp, rhs},
        PutAppend[CForm[comp], outputfile];
        Global`pr["="];
        PutAppend[CForm[rhs], outputfile];
        Global`pr[";\n"]
      ],
      (* Temp formatter - CarpetXGPU specific: const prefix, trimmed name *)
      Function[{comp, rhs},
        Global`pr["const " <> GetTempVariableType[ctx] <> " "];
        Global`pr[StringTrim[ToString[comp], GetGridPointIndex[ctx]]];
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
