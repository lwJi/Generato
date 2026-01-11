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

PrintComponentInitialization[ctx_Association, varinfo_, compname_] :=
  Module[{varlistindex = GetMapComponentToVarlist[ctx][compname], compToValue = compname // ToValues, varname, buf},
    varname = varinfo[[1]];
    Which[
      GetInitializationsMode[ctx] === "MainOut",
        buf =
          "double *" <> StringTrim[ToString[compToValue], GetGridPointIndex[ctx]] <> " = Vard(node, Vind(vlr," <> ToString[GetProject[ctx]] <> "->i_" <> StringTrim[ToString[varname[[0]]], (GetPrefixDt[ctx] | GetSuffixUnprotected[ctx])] <> GetInitialComp[varname] <>
            If[varlistindex == 0,
              ""
              ,
              "+" <> ToString[varlistindex]
            ] <> "));"
      ,
      GetInitializationsMode[ctx] === "MainIn",
        buf =
          "double *" <> StringTrim[ToString[compToValue], GetGridPointIndex[ctx]] <> " = Vard(node, Vind(vlu," <> ToString[GetProject[ctx]] <> "->i_" <> StringTrim[ToString[varname[[0]]], GetSuffixUnprotected[ctx]] <> GetInitialComp[varname] <>
            If[varlistindex == 0,
              ""
              ,
              "+" <> ToString[varlistindex]
            ] <> "));"
      ,
      GetInitializationsMode[ctx] === "MoreInOut",
        buf =
          "double *" <> StringTrim[ToString[compToValue], GetGridPointIndex[ctx]] <> " = Vard(node, i" <> StringTrim[ToString[varname[[0]]], GetSuffixUnprotected[ctx]] <> GetInitialComp[varname] <>
            If[varlistindex == 0,
              ""
              ,
              "+" <> ToString[varlistindex]
            ] <> ");"
      ,
      GetInitializationsMode[ctx] === "Temp",
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
      ]
      (* Temp uses default: no const prefix, CForm[compToValue] *)
    ]
  ];

(* Backwards compat: old 3-argument signature *)
PrintComponentEquation[coordinate_, compname_, extrareplacerules_] :=
  Module[{},
    SyncModeToContext[];
    PrintComponentEquation[$CurrentContext, coordinate, compname, extrareplacerules]
  ];

Protect[PrintComponentEquation];

(*
    Write to files
*)

Module[{outputfile = GetOutputFile[], filepointer},
  If[Environment["QUIET"] =!= "1", System`Print["Writing to \"", outputfile, "\"...\n"]];
  If[FileExistsQ[outputfile],
    If[Environment["QUIET"] =!= "1", System`Print["\"", outputfile, "\" already exists, replacing it ...\n"]];
    DeleteFile[outputfile]
  ];
  (* define pr *)
  filepointer = OpenAppend[outputfile];
  pr[x_:""] :=
    Module[{},
      If[x == "double ",
        WriteString[filepointer, x]
        ,
        WriteLine[filepointer, x]
      ]
    ];
  (* print first few lines *)
  pr["/* " <> FileNameTake[outputfile] <> " */"];
  pr[
    "/* Produced with Generato" <>
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
  If[Environment["QUIET"] =!= "1", System`Print["Done generating \"", outputfile, "\"\n"]];
  Close[filepointer]
];
