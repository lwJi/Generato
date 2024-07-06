(* ::Package:: *)

(* CarpetX.wl, set up functions adapted to CarpetX code *)

(* (c) Liwei Ji, 06/2024 *)

(*
    Print initialization of each component of a tensor
*)

PrintComponentInitialization[varname_, compname_] :=
  Module[{varlistindex = GetMapComponentToVarlist[][compname], compToValue
     = compname // ToValues, buf, subbuf, isGF3D2, isGF3D5, isScal, isVect, isSmat},
    isScal = GetParseMode[PrintCompInitGF3D2] || GetParseMode[PrintCompInitGF3D5];
    isVect = GetParseMode[PrintCompInitVecGF3D2] || GetParseMode[PrintCompInitVecGF3D5];
    isSmat = GetParseMode[PrintCompInitSmatGF3D2] || GetParseMode[PrintCompInitSmatGF3D5];
    (* set subbuf *)
    Which[
      isScal,
        Which[
          Length[varname] == 0,
            subbuf = ""
          ,
          Length[varname] == 1,
            subbuf = "(" <> ToString[compname[[1]][[1]] - 1] <> ")"
          ,
          Length[varname] == 2,
            subbuf = "(" <> ToString[compname[[1]][[1]] - 1] <> ","
                         <> ToString[compname[[2]][[1]] - 1] <> ")"
          ,
          True,
            Throw @ Message[PrintComponentInitialization::EVarLength]
        ]
      ,
      isVect,
        Which[
          Length[varname] == 1,
            subbuf = "(" <> ToString[compname[[1]][[1]] - 1] <> ")"
          ,
          Length[varname] == 2,
            subbuf = "(" <> ToString[compname[[2]][[1]] - 1] <> ")"
                  <> "(" <> ToString[compname[[1]][[1]] - 1] <> ")"
          ,
          Length[varname] == 3,
            subbuf = "(" <> ToString[compname[[2]][[1]] - 1] <> ","
                         <> ToString[compname[[3]][[1]] - 1] <> ")"
                  <> "(" <> ToString[compname[[1]][[1]] - 1] <> ")"
          ,
          True,
            Throw @ Message[PrintComponentInitialization::EVarLength]
        ]
      ,
      isSmat,
        Which[
          Length[varname] == 2,
            subbuf = "(" <> ToString[compname[[1]][[1]] - 1] <> ","
                         <> ToString[compname[[2]][[1]] - 1] <> ")"
          ,
          Length[varname] == 3,
            subbuf = "(" <> ToString[compname[[3]][[1]] - 1] <> ")"
                  <> "(" <> ToString[compname[[1]][[1]] - 1] <> ","
                         <> ToString[compname[[2]][[1]] - 1] <> ")"
          ,
          Length[varname] == 4,
            subbuf = "(" <> ToString[compname[[3]][[1]] - 1] <> ","
                         <> ToString[compname[[4]][[1]] - 1] <> ")"
                  <> "(" <> ToString[compname[[1]][[1]] - 1] <> ","
                         <> ToString[compname[[2]][[1]] - 1] <> ")"
          ,
          True,
            Throw @ Message[PrintComponentInitialization::EVarLength]
        ]
      ,
      True,
        Throw @ Message[PrintComponentInitialization::EMode]
    ];
    (* combine buf *)
    isGF3D2 = GetParseMode[PrintCompInitGF3D2] || GetParseMode[PrintCompInitVecGF3D2] || GetParseMode[PrintCompInitSmatGF3D2];
    isGF3D5 = GetParseMode[PrintCompInitGF3D5] || GetParseMode[PrintCompInitVecGF3D5] || GetParseMode[PrintCompInitSmatGF3D5];
    buf =
      Which[
        isGF3D2 && GetParseMode[PrintCompInitMainOut],
          "const GF3D2<CCTK_REAL> &"
          <> StringTrim[ToString[compToValue], GetGridPointIndex[]]
          <> " = gf_" <> StringTrim[ToString[varname[[0]]], GetSuffixUnprotected[]]
          <> subbuf <> ";"
        ,
        isGF3D2 && GetParseMode[PrintCompInitMainIn],
          "const vreal &" <> StringTrim[ToString[compToValue], GetGridPointIndex[]]
          <> " = gf_" <> StringTrim[ToString[varname[[0]]], GetSuffixUnprotected[]]
          <> "(mask, index2)"
          <> subbuf <> ";"
        ,
        isGF3D5,
          "const vreal " <> StringTrim[ToString[compToValue], GetGridPointIndex[]]
          <> " = tl_" <> StringTrim[ToString[varname[[0]]], GetSuffixUnprotected[]]
          <> "(mask, index5)"
          <> subbuf <> ";"
        ,
        GetParseMode[PrintCompInitTemp],
          buf = "vreal " <> ToString[compToValue] <> ";"
        ,
        True,
          Throw @ Message[PrintComponentInitialization::EMode]
      ]
    ;
    pr[buf];
  ];

PrintComponentInitialization::EMode = "PrintComponentInitialization mode unrecognized!";

PrintComponentInitialization::EVarLength = "PrintComponentInitialization variable's tensor type unsupported!";

(*Protect[PrintComponentInitialization];*)

PrintComponentEquation[coordinate_, compname_] :=
  Module[{outputfile = GetOutputFile[], compToValue, rhssToValue},
    compToValue = compname // ToValues;
    rhssToValue =
      (compname /. {compname[[0]] -> RHSOf[compname[[0]], GetSuffixName[
        ]]}) //
      DummyToBasis[coordinate] //
      TraceBasisDummy //
      ToValues;
    If[GetSimplifyEquation[],
      rhssToValue = rhssToValue // Simplify
    ];
    Which[
      GetParseMode[PrintCompEQNNewVar],
        Module[{},
          Global`pr[GetTempVariableType[] <> " "];
          PutAppend[CForm[compToValue], outputfile];
          Global`pr["="];
          PutAppend[CForm[rhssToValue], outputfile];
          Global`pr[";\n"]
        ]
      ,
      GetParseMode[PrintCompEQNMain],
        Module[{},
          PutAppend[CForm[compToValue], outputfile];
          Global`pr["="];
          PutAppend[CForm[rhssToValue], outputfile];
          Global`pr[";\n"]
        ]
      ,
      GetParseMode[PrintCompEQNMainCarpetX],
        Module[{},
          Global`pr[ToString[CForm[compToValue]] <>".store(mask, index2, "];
          PutAppend[CForm[rhssToValue], outputfile];
          Global`pr[");\n"]
        ]
      ,
      GetParseMode[PrintCompEQNAddToMain],
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

(*
    Write to files
*)

Module[{outputfile = GetOutputFile[], filepointer},
  Print["Writing to \"", outputfile, "\"...\n"];
  If[FileExistsQ[outputfile],
    Print["\"", outputfile, "\" already exist, replacing it ...\n"];
    DeleteFile[outputfile]
  ];
  (* define pr *)
  filepointer = OpenAppend[outputfile];
  pr[x_:""] :=
    Module[{},
      If[x == "vreal ",
        WriteString[filepointer, x]
        ,
        WriteLine[filepointer, x]
      ]
    ];
  (* print first few lines *)
  pr["/* " <> FileNameTake[outputfile] <> " */"];
  pr[
    "/* Produced with Mathematica" <>
      If[GetPrintDate[],
        " on " <> DateString[{"Month", "/", "Day", "/", "Year"}]
        ,
        ""
      ] <> " */"
  ];
  pr[];
  $MainPrint[];
  pr[];
  pr["/* " <> FileNameTake[outputfile] <> " */"];
  Print["Done generating \"", outputfile, "\"\n"];
  Close[filepointer]
];
