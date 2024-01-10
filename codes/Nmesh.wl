(* ::Package:: *)

(* Nmesh.wl, set up functions adapted to Nmesh code *)

(* (c) Liwei Ji, 01/2024 *)

GetInitialComp[varname_] :=
    Module[{initialcomp = ""},
        Do[
            If[is3DAbstractIndex[varname[[icomp]]],
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

PrintComponentInitialization[varname_, compname_] :=
    Module[{varlistindex = GetMapComponentToVarlist[][compname], compToValue
         = compname // ToValues, buf},
        Which[
            GetParseMode[PrintCompInitMainOut],
                buf =
                    "double *" <> StringTrim[ToString[compToValue], GetGridPointIndex[
                        ]] <> " = Vard(node, Vind(vlr," <> ToString[GetProjectName[]] <> "->i_"
                         <> StringTrim[ToString[varname[[0]]], (GetprefixDt[] | GetSuffixUnprotected[
                        ])] <> GetInitialComp[varname] <>
                        If[varlistindex == 0,
                            ""
                            ,
                            "+" <> ToString[varlistindex]
                        ] <> "));"
            ,
            GetParseMode[PrintCompInitMainIn],
                buf =
                    "double *" <> StringTrim[ToString[compToValue], GetGridPointIndex[
                        ]] <> " = Vard(node, Vind(vlu," <> ToString[GetProjectName[]] <> "->i_"
                         <> StringTrim[ToString[varname[[0]]], GetSuffixUnprotected[]] <> GetInitialComp[
                        varname] <>
                        If[varlistindex == 0,
                            ""
                            ,
                            "+" <> ToString[varlistindex]
                        ] <> "));"
            ,
            GetParseMode[PrintCompInitMoreInOut],
                buf =
                    "double *" <> StringTrim[ToString[compToValue], GetGridPointIndex[
                        ]] <> " = Vard(node, i" <> StringTrim[ToString[varname[[0]]], GetSuffixUnprotected[
                        ]] <> GetInitialComp[varname] <>
                        If[varlistindex == 0,
                            ""
                            ,
                            "+" <> ToString[varlistindex]
                        ] <> ");"
            ,
            GetParseMode[PrintCompInitTemp],
                buf = "double " <> ToString[compToValue] <> ";"
            ,
            True,
                Throw @ Message[PrintComponentInitialization::EMode]
        ];
        pr[buf];
    ];

PrintComponentInitialization::EMode = "PrintComponentInitialization mode unrecognized!";

(*Protect[PrintComponentInitialization];*)

(*
    Write to files
*)

Module[{outputFile = GetoutputFile[], filePointer},
    Print[];
    Print["============================================================"
        ];
    Print[" Writing to ", outputFile];
    Print["============================================================"
        ];
    If[FileExistsQ[outputFile],
        Print[outputFile, " already exist, replacing it ..."];
        DeleteFile[outputFile]
    ];
    Print[];
    (* define pr *)
    filePointer = OpenAppend[outputFile];
    pr[x_:""] :=
        Module[{},
            If[x == "double ",
                WriteString[filePointer, x]
                ,
                WriteLine[filePointer, x]
            ]
        ];
    (* print first few lines *)
    pr["/* " <> outputFile <> " */"];
    pr["/* (c) Liwei Ji " <> DateString[{"Month", "/", "Day", "/", "Year"
        }] <> " */"];
    pr["/* Produced with Mathematica */"];
    pr[];
    $headPart[];
    $bodyPart[];
    $endPart[];
    Print["Done generating ", outputFile, "\n"];
    Close[filePointer]
];
