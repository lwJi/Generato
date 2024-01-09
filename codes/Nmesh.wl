(* ::Package:: *)

(* Nmesh.wl, set up functions adapted to Nmesh code *)

(* (c) Liwei Ji, 09/2022 *)

(* return the initial component expression of a tensor *)

getInitialComp[varName_] :=
    Module[{initialComp = ""},
        Do[
            If[is3DAbstractIndex[varName[[compIndex]]],
                initialComp = initialComp <> "x"
                ,
                initialComp = initialComp <> "t"
            ]
            ,
            {compIndex, 1, Length[varName]}
        ];
        initialComp
    ];

(*Protect[getInitialComp];*)

(* print components initialization: print initialization of each component of a tensor based on mode.
    mode choices ["vl_lhs using vl_index", "vl_evo using vl_index", "vl_lhs", "vl_evo", "more input/output"].
*)

PrintComponentInitialization[mode_?StringQ, varName_, compName_, gridPointIndex_
    ?StringQ] :=
    Module[{mapCtoV = GetmapComponentToVarlist[], projectName = GetprojectName[
        ], compToValue = compName // ToValues, varlistIndex, buf},
        varlistIndex = mapCtoV[[Position[mapCtoV, compName][[1, 1]], 
            2]];
        (* different modes *)
        Which[(* print output var initialization *)
            StringMatchQ[mode, "print components initialization: vl_lhs using vl_index"
                ],
                buf = "double *" <> StringTrim[ToString[compToValue],
                     gridPointIndex] <> " = Vard(node, Vind(vlr," <> ToString[varlistIndex
                    ] <> "));"
            ,
            (* print input var initialization *)StringMatchQ[mode, "print components initialization: vl_evo using vl_index"
                ],
                buf = "double *" <> StringTrim[ToString[compToValue],
                     gridPointIndex] <> " = Vard(node, Vind(vlu," <> ToString[varlistIndex
                    ] <> "));"
            ,
            (* print output var initialization using var indepedent index
                *)StringMatchQ[mode, "print components initialization: vl_lhs"],
                buf =
                    "double *" <> StringTrim[ToString[compToValue], gridPointIndex
                        ] <> " = Vard(node, Vind(vlr," <> ToString[projectName] <> "->i_" <> 
                        StringTrim[ToString[varName[[0]]], (GetprefixDt[] | GetsuffixUnprotected[
                        ])] <> getInitialComp[varName] <>
                        If[varlistIndex == 0,
                            ""
                            ,
                            "+" <> ToString[varlistIndex]
                        ] <> "));"
            ,
            (* print input var initialization using var independent index
                *)StringMatchQ[mode, "print components initialization: vl_evo"],
                buf =
                    "double *" <> StringTrim[ToString[compToValue], gridPointIndex
                        ] <> " = Vard(node, Vind(vlu," <> ToString[projectName] <> "->i_" <> 
                        StringTrim[ToString[varName[[0]]], GetsuffixUnprotected[]] <> getInitialComp[
                        varName] <>
                        If[varlistIndex == 0,
                            ""
                            ,
                            "+" <> ToString[varlistIndex]
                        ] <> "));"
            ,
            (* print more input var initialization *)StringMatchQ[mode,
                 "print components initialization: more input/output"],
                buf =
                    "double *" <> StringTrim[ToString[compToValue], gridPointIndex
                        ] <> " = Vard(node, i" <> StringTrim[ToString[varName[[0]]], GetsuffixUnprotected[
                        ]] <> getInitialComp[varName] <>
                        If[varlistIndex == 0,
                            ""
                            ,
                            "+" <> ToString[varlistIndex]
                        ] <> ");"
            ,
            (* print temp var initialization *)StringMatchQ[mode, "print components initialization: temporary"
                ],
                buf = "double " <> StringTrim[ToString[compToValue], 
                    gridPointIndex] <> ";"
            ,
            (* mode undefined *)True,
                Throw @ Message[PrintComponentInitialization::ErrorMode,
                     mode]
        ];
        pr[buf];
    ];

PrintComponentInitialization::ErrorMode = "Print component mode `1` unsupported yet!";

(*Protect[PrintComponentInitialization];*)

(* ============== *)

(* Write to files *)

(* ============== *)

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
