(* ::Package:: *)

(* CarpetX.wl, set up functions adapted to CarpetX code *)

(* (c) Liwei Ji, 06/2024 *)

(*
    Print initialization of a tensor
*)

PrintListInitializations[varlist_?ListQ, storagetype_?StringQ, indextype_?StringQ] :=
  Module[{varname, symmetry, printname, buf},
    Do[
      {varname, symmetry, printname} = ParseVar[varlist[[ivar]]];
      buf = "const auto &tmp_" <> ToString[varname[[0]]] <> " = " <> storagetype <> ToString[varname[[0]]] <> "(mask, " <> indextype <> ");";
      pr[buf]
      ,
      {ivar, 1, Length[varlist]}
    ];
  ];

Protect[PrintListInitializations];

(*
    Print initialization of each component of a tensor
*)

PrintComponentInitialization[ctx_Association, varinfo_, compname_] :=
  Module[{varlistindex = GetMapComponentToVarlist[ctx][compname], compToValue = compname // ToValues, varname, symmetry, buf, subbuf, isGF3D2, isGF3D5},
    {varname, symmetry} = varinfo;
    (* set subbuf *)
    Which[
      GetTensorType[ctx] === "Scal",
        Which[
          Length[varname] == 0,
            subbuf = ""
          ,
          Length[varname] == 1,
            subbuf = "(" <> ToString[compname[[1]][[1]] - 1] <> ")"
          ,
          Length[varname] == 2,
            If[symmetry =!= Null,
              subbuf = "(" <> ToString[compname[[1]][[1]] - 1] <> "," <> ToString[compname[[2]][[1]] - 1] <> ")"
              ,
              subbuf = "(" <> ToString[compname[[1]][[1]] - 1] <> ")(" <> ToString[compname[[2]][[1]] - 1] <> ")"
            ]
          ,
          Length[varname] == 3,
            subbuf = "(" <> ToString[compname[[1]][[1]] - 1] <> ")(" <> ToString[compname[[2]][[1]] - 1] <> "," <> ToString[compname[[3]][[1]] - 1] <> ")"
          ,
          True,
            Throw @ Message[PrintComponentInitialization::EVarLength]
        ]
      ,
      GetTensorType[ctx] === "Vect",
        Which[
          Length[varname] == 1,
            subbuf = "(" <> ToString[compname[[1]][[1]] - 1] <> ")"
          ,
          Length[varname] == 2,
            subbuf = "(" <> ToString[compname[[2]][[1]] - 1] <> ")(" <> ToString[compname[[1]][[1]] - 1] <> ")"
          ,
          Length[varname] == 3,
            If[symmetry =!= Null,
              subbuf = "(" <> ToString[compname[[2]][[1]] - 1] <> "," <> ToString[compname[[3]][[1]] - 1] <> ")(" <> ToString[compname[[1]][[1]] - 1] <> ")"
              ,
              subbuf = "(" <> ToString[compname[[2]][[1]] - 1] <> ")(" <> ToString[compname[[3]][[1]] - 1] <> ")(" <> ToString[compname[[1]][[1]] - 1] <> ")"
            ]
          ,
          Length[varname] == 4,
            subbuf = "(" <> ToString[compname[[2]][[1]] - 1] <> ")(" <> ToString[compname[[3]][[1]] - 1] <> "," <> ToString[compname[[4]][[1]] - 1] <> ")(" <> ToString[compname[[1]][[1]] - 1] <> ")"
          ,
          True,
            Throw @ Message[PrintComponentInitialization::EVarLength]
        ]
      ,
      GetTensorType[ctx] === "Smat",
        Which[
          Length[varname] == 2,
            subbuf = "(" <> ToString[compname[[1]][[1]] - 1] <> "," <> ToString[compname[[2]][[1]] - 1] <> ")"
          ,
          Length[varname] == 3,
            subbuf = "(" <> ToString[compname[[3]][[1]] - 1] <> ")(" <> ToString[compname[[1]][[1]] - 1] <> "," <> ToString[compname[[2]][[1]] - 1] <> ")"
          ,
          Length[varname] == 4,
            subbuf = "(" <> ToString[compname[[3]][[1]] - 1] <> "," <> ToString[compname[[4]][[1]] - 1] <> ")(" <> ToString[compname[[1]][[1]] - 1] <> "," <> ToString[compname[[2]][[1]] - 1] <> ")"
          ,
          True,
            Throw @ Message[PrintComponentInitialization::EVarLength]
        ]
      ,
      True,
        Throw @ Message[PrintComponentInitialization::EMode]
    ];
    (* combine buf *)
    isGF3D2 = GetStorageType[ctx] === "GF";
    isGF3D5 = GetStorageType[ctx] === "Tile";
    buf =
      Which[
        GetInitializationsMode[ctx] === "MainOut" && isGF3D2,
          "const GF3D2<CCTK_REAL> &local_" <> StringTrim[ToString[compToValue], GetGridPointIndex[ctx]] <> " = gf_" <> StringTrim[ToString[varname[[0]]], GetSuffixUnprotected[ctx]] <> subbuf <> ";"
        ,
        GetInitializationsMode[ctx] === "MainIn" && isGF3D2,
          "const vreal " <> StringTrim[ToString[compToValue], GetGridPointIndex[ctx]] <> " = tmp_" <> StringTrim[ToString[varname[[0]]], GetSuffixUnprotected[ctx]] <> subbuf <> ";"
        ,
        GetInitializationsMode[ctx] === "MainIn" && isGF3D5,
          "const vreal " <> StringTrim[ToString[compToValue], GetGridPointIndex[ctx]] <> " = tmp_" <> StringTrim[ToString[varname[[0]]], GetSuffixUnprotected[ctx]] <> subbuf <> ";"
        ,
        GetInitializationsMode[ctx] === "Temp",
          buf = "vreal " <> ToString[compToValue] <> ";"
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
      (* MainOut formatter - CarpetX specific *)
      Function[{comp, rhs},
        Global`pr["local_" <> ToString[CForm[comp]] <> ".store(mask, index2, "];
        PutAppend[CForm[rhs], outputfile];
        Global`pr[");\n"]
      ]
    ]
  ];

Protect[PrintComponentEquation];

(******************************************************************************)

(*                                Write to files                              *)

(******************************************************************************)

WriteToFile[GetOutputFile[]];
