(* ::Package:: *)

(* AMReX.wl, set up functions adapted to AMReX code *)

(* (c) Liwei Ji, 08/2025 *)


(******************************************************************************)
(*                    Finite difference stencil function                      *)
(******************************************************************************)

(* Function to get GF index name - GetGFIndexName is now in BackendCommon.wl *)

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
    (* Extract common component info using shared function *)
    {varlistindex, compToValue, varname, symmetry, len} =
      ExtractComponentInfo[varinfo, compname];

    (* set subbuf using shared function from BackendCommon *)
    subbuf = GetTensorIndexSubbuf[len, varlistindex];
    fdorder = GetDerivsOrder[];
    fdaccuracy = GetDerivsAccuracy[];

    (* set buf *)
    buf =
      Which[
        GetInitializationsMode[] === "MainIn" || GetInitializationsMode[] === "MainOut",
          "auto &" <> ToString[compToValue]
          <> " = " <> StringTrim[ToString[varname[[0]]]] <> subbuf <> ";"
        ,
        GetInitializationsMode[] === "Derivs",
          offset = fdorder - 1;
          If[GetStorageType[] === "Tile",
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
        GetInitializationsMode[] === "Temp",
          "const auto &" <> ToString[compToValue]
          <> " = " <> StringTrim[ToString[varname[[0]]]] <> subbuf <> ";"
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

PrintComponentEquation[coordinate_, compname_, extrareplacerules_] :=
  Module[{outputfile = GetOutputFile[], compToValue, rhssToValue},
    compToValue = (compname // ToValues) /. extrareplacerules;
    rhssToValue = ComputeRHSValue[coordinate, compname, extrareplacerules];
    PrintEquationByMode[compToValue, rhssToValue,
      (* MainOut formatter - standard assignment *)
      Function[{comp, rhs},
        PutAppend[CForm[comp], outputfile];
        Global`pr["="];
        PutAppend[CForm[rhs], outputfile];
        Global`pr[";\n"]
      ],
      (* Temp formatter - AMReX specific: const prefix, trimmed name *)
      Function[{comp, rhs},
        Global`pr["const " <> GetTempVariableType[] <> " "];
        Global`pr[StringTrim[ToString[comp], GetGridPointIndex[]]];
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
