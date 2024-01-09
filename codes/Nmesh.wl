(* ::Package:: *)

(* Nmesh.wl, set up functions adapted to Nmesh code *)

(* (c) Liwei Ji, 01/2024 *)

getInitialComp[varname_] :=
    Module[{initialComp = ""},
        Do[
            If[is3DAbstractIndex[varname[[compIndex]]],
                initialComp = initialComp <> "x"
                ,
                initialComp = initialComp <> "t"
            ]
            ,
            {compIndex, 1, Length[varname]}
        ];
        initialComp
    ];

(*
    Print initialization of each component of a tensor
*)

PrintComponentInitialization[varname_, compname_] :=
    Module[{varlistindex = GetMapComponentToVarlist[][compname], compToValue
         = compname // ToValues, buf},
        Which[
            StringMatchQ[mode, "print components initialization: vl_lhs using vl_index"
                ],
                buf = "double *" <> StringTrim[ToString[compToValue],
                     GetGridPointIndex[]] <> " = Vard(node, Vind(vlr," <> ToString[varlistindex
                    ] <> "));"
            ,
            StringMatchQ[mode, "print components initialization: vl_evo using vl_index"
                ],
                buf = "double *" <> StringTrim[ToString[compToValue],
                     GetGridPointIndex[]] <> " = Vard(node, Vind(vlu," <> ToString[varlistindex
                    ] <> "));"
            ,
            StringMatchQ[mode, "print components initialization: vl_lhs"
                ],
                buf =
                    "double *" <> StringTrim[ToString[compToValue], GetGridPointIndex[
                        ]] <> " = Vard(node, Vind(vlr," <> ToString[GetProjectName[]] <> "->i_"
                         <> StringTrim[ToString[varname[[0]]], (GetprefixDt[] | GetsuffixUnprotected[
                        ])] <> getInitialComp[varname] <>
                        If[varlistindex == 0,
                            ""
                            ,
                            "+" <> ToString[varlistindex]
                        ] <> "));"
            ,
            StringMatchQ[mode, "print components initialization: vl_evo"
                ],
                buf =
                    "double *" <> StringTrim[ToString[compToValue], GetGridPointIndex[
                        ]] <> " = Vard(node, Vind(vlu," <> ToString[GetProjectName[]] <> "->i_"
                         <> StringTrim[ToString[varname[[0]]], GetsuffixUnprotected[]] <> getInitialComp[
                        varname] <>
                        If[varlistindex == 0,
                            ""
                            ,
                            "+" <> ToString[varlistindex]
                        ] <> "));"
            ,
            StringMatchQ[mode, "print components initialization: more input/output"
                ],
                buf =
                    "double *" <> StringTrim[ToString[compToValue], GetGridPointIndex[
                        ]] <> " = Vard(node, i" <> StringTrim[ToString[varname[[0]]], GetsuffixUnprotected[
                        ]] <> getInitialComp[varname] <>
                        If[varlistindex == 0,
                            ""
                            ,
                            "+" <> ToString[varlistindex]
                        ] <> ");"
            ,
            StringMatchQ[mode, "print components initialization: temporary"
                ],
                buf = "double " <> StringTrim[ToString[compToValue], 
                    GetGridPointIndex[]] <> ";"
            ,
            True,
                Throw @ Message[PrintComponentInitialization::EMode, 
                    mode]
        ];
        pr[buf];
    ];

PrintComponentInitialization::EMode = "PrintComponentInitialization mode `1` unrecognized!";

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
