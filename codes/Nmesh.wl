(* ::Package:: *)

(* Nmesh.wl, set up functions adapted to Nmesh code *)

(* (c) Liwei Ji, 01/2024 *)

GetInitialComp[varname_] :=
  Module[{initialcomp = ""},
    Do[
      If[Is3DAbstractIndex[varname[[icomp]]],
        initialcomp = initialcomp <> "x"
        ,
        initialcomp = initialcomp <> "t"
      ]
      ,
      {icomp, 1, Length[varname]}
    ];
    initialcomp
  ];

(*
    Print initialization of each component of a tensor
*)

PrintComponentInitialization[varinfo_, compname_] :=
  Module[{varlistindex, compToValue, varname, symmetry, len, buf},
    (* Extract common component info using shared function *)
    {varlistindex, compToValue, varname, symmetry, len} =
      ExtractComponentInfo[varinfo, compname];
    Which[
      GetInitializationsMode[] === "MainOut",
        buf =
          "double *" <> StringTrim[ToString[compToValue], GetGridPointIndex[]] <> " = Vard(node, Vind(vlr," <> ToString[GetProject[]] <> "->i_" <> StringTrim[ToString[varname[[0]]], (GetPrefixDt[] | GetSuffixUnprotected[])] <> GetInitialComp[varname] <>
            If[varlistindex == 0,
              ""
              ,
              "+" <> ToString[varlistindex]
            ] <> "));"
      ,
      GetInitializationsMode[] === "MainIn",
        buf =
          "double *" <> StringTrim[ToString[compToValue], GetGridPointIndex[]] <> " = Vard(node, Vind(vlu," <> ToString[GetProject[]] <> "->i_" <> StringTrim[ToString[varname[[0]]], GetSuffixUnprotected[]] <> GetInitialComp[varname] <>
            If[varlistindex == 0,
              ""
              ,
              "+" <> ToString[varlistindex]
            ] <> "));"
      ,
      GetInitializationsMode[] === "MoreInOut",
        buf =
          "double *" <> StringTrim[ToString[compToValue], GetGridPointIndex[]] <> " = Vard(node, i" <> StringTrim[ToString[varname[[0]]], GetSuffixUnprotected[]] <> GetInitialComp[varname] <>
            If[varlistindex == 0,
              ""
              ,
              "+" <> ToString[varlistindex]
            ] <> ");"
      ,
      GetInitializationsMode[] === "Temp",
        buf = "double " <> ToString[compToValue] <> ";"
      ,
      True,
        Throw @ Message[PrintComponentInitialization::EMode]
    ];
    pr[buf];
  ];

(* Error message PrintComponentInitialization::EMode is in BackendCommon.wl *)

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
    compToValue = compname // ToValues;
    rhssToValue = ComputeRHSValue[coordinate, compname, extrareplacerules];
    PrintEquationByMode[compToValue, rhssToValue,
      (* MainOut formatter - standard assignment *)
      Function[{comp, rhs},
        PutAppend[CForm[comp], outputfile];
        Global`pr["="];
        PutAppend[CForm[rhs], outputfile];
        Global`pr[";\n"]
      ]
      (* Temp uses default: no const prefix, CForm[compToValue] *)
    ]
  ];

Protect[PrintComponentEquation];

(*
    Write to files
*)

WriteToFile[GetOutputFile[]];
