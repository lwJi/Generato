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

PrintComponentInitialization[varinfo_, compname_] :=
  Module[{varlistindex = GetMapComponentToVarlist[][compname], compToValue = compname // ToValues, varname, symmetry, buf, subbuf, isGF3D2, isGF3D5},
    {varname, symmetry} = varinfo;
    (* set subbuf *)
    Which[
      GetTensorType[] === "Scal",
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
      GetTensorType[] === "Vect",
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
      GetTensorType[] === "Smat",
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
    isGF3D2 = GetStorageType[] === "GF";
    isGF3D5 = GetStorageType[] === "Tile";
    buf =
      Which[
        GetInitMode[] === "MainOut" && isGF3D2,
          "const GF3D2<CCTK_REAL> &local_" <> StringTrim[ToString[compToValue], GetGridPointIndex[]] <> " = gf_" <> StringTrim[ToString[varname[[0]]], GetSuffixUnprotected[]] <> subbuf <> ";"
        ,
        GetInitMode[] === "MainIn" && isGF3D2,
          "const vreal " <> StringTrim[ToString[compToValue], GetGridPointIndex[]] <> " = tmp_" <> StringTrim[ToString[varname[[0]]], GetSuffixUnprotected[]] <> subbuf <> ";"
        ,
        GetInitMode[] === "MainIn" && isGF3D5,
          "const vreal " <> StringTrim[ToString[compToValue], GetGridPointIndex[]] <> " = tmp_" <> StringTrim[ToString[varname[[0]]], GetSuffixUnprotected[]] <> subbuf <> ";"
        ,
        GetInitMode[] === "Temp",
          buf = "vreal " <> ToString[compToValue] <> ";"
        ,
        True,
          Throw @ Message[PrintComponentInitialization::EMode]
      ];
    pr[buf];
  ];

PrintComponentInitialization::EMode = "PrintComponentInitialization mode unrecognized!";

PrintComponentInitialization::EVarLength = "PrintComponentInitialization variable's tensor type unsupported!";

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
      GetEqnMode[] === "NewVar",
        Module[{},
          Global`pr[GetTempVariableType[] <> " "];
          PutAppend[CForm[compToValue], outputfile];
          Global`pr["="];
          PutAppend[CForm[rhssToValue], outputfile];
          Global`pr[";\n"]
        ]
      ,
      GetEqnMode[] === "Main",
        Module[{},
          Global`pr["local_" <> ToString[CForm[compToValue]] <> ".store(mask, index2, "];
          PutAppend[CForm[rhssToValue], outputfile];
          Global`pr[");\n"]
        ]
      ,
      GetEqnMode[] === "AddToMain",
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

Protect[PrintComponentEquation];

(******************************************************************************)

(*                                Write to files                              *)

(******************************************************************************)

WriteToFile[GetOutputFile[]];
