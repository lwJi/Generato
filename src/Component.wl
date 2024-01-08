(* ::Package:: *)

(* (c) Liwei Ji, 01/2024 *)

BeginPackage["Generato`Component`"];

Needs["Generato`Basic`"];

GetParseMode::usage = "GetParseMode[key] returns the mode correspond to the key";

SetParseMode::usage = "SetParseMode[key] add/update the mode correspond to the key";

GetMapComponentToVarlist::usage = "GetMapComponentToVarlist[] returns the map between tensor components and varlist indexes.";

SetProcessNewVarlist::usage = "SetProcessNewVarlist[True] update the Boolean variable specifying if we are processing a new varlist.";

ParseComponent::usage = "ParseComponent[varname, compindexlist, coordinate, suffixname] process a component.";

Begin["`Private`"];

(* Data *)

$ParseModeAssociation = <|SetComp -> False, PrintComp -> False, PrintCompInit
     -> False, PrintCompEQN -> False, PrintCompEQNTemp -> False, PrintCompEQNMain
     -> False, PrintCompEQNFlux -> False, PrintCompEQNAddToMain -> False|>;

$MapComponentToVarlist = <||>;(*store all varlist's map*)

$ProcessNewVarlist = True;

(* Function *)

GetParseMode[key_] :=
    Return[$ParseModeAssociation[key]];

Protect[GetParseMode];

SetParseMode[assoc_] :=
    Module[{},
        AppendTo[$ParseModeAssociation, assoc]
    ];

Protect[SetParseMode];

GetMapComponentToVarlist[] :=
    Return[$MapComponentToVarlist];

Protect[GetMapComponentToVarlist];

SetMapComponentToVarlist[map_] :=
    Module[{},
        $MapComponentToVarlist = map
    ];

GetProcessNewVarlist[] :=
    Return[$ProcessNewVarlist];

SetProcessNewVarlist[isnew_] :=
    Module[{},
        $ProcessNewVarlist = isnew
    ];

Protect[SetProcessNewVarlist];

ParseComponent[varname_, compindexlist_?ListQ, coordinate_, suffixname_
    ?StringQ] :=
    Module[{compname, exprname},
        compname = SetCompName[varname, compindexlist, coordinate];
        exprname = SetExprName[varname, compindexlist, addgpidx];
        If[is4DCompIndexListIn3DTensor[compindexlist, varname],
            If[isUp4DCompIndexListIn3DTensor[compindexlist, varname],
                
                ComponentValue[compname, 0]
            ];
            Continue[]
        ];
        Which[
            GetParseMode[SetComp],
                SetComponent[compname, exprname]
            ,
            GetParseMode[PrintComp],
                PrintComponent[coordinate, varname, compname, suffixname
                    ]
            ,
            True,
                Throw @ Message[ParseComponent::EMode]
        ]
    ];

ParseComponent::EMode = "ParseMode unrecognized!";

Protect[ParseComponent];

PrintComponent[coordinate_, varname_, compname_, suffixname_?StringQ] :=
    Module[{},
        Which[
            GetParseMode[PrintCompInit],
                Global`PrintComponentInitialization[varname, compname
                    ]
            ,
            GetParseMode[PrintCompEQN],
                PrintComponentEquation[coordinate, compname, suffixname
                    ]
            ,
            True,
                Throw @ Message[PrintComponent::EMode]
        ]
    ];

PrintComponent::EMode = "ParseMode unrecognized!";

PrintComponentEquation[coordinate_, compname_, suffixname_] :=
    Module[{outputfile = GetoutputFile[], compToValue, rhssToValue},
        compToValue = compname // ToValues;
        rhssToValue =
            (compname /. {compname[[0]] -> RHSOf[compname[[0]], suffixname
                ]}) //
            DummyToBasis[coordinate] //
            TraceBasisDummy //
            ToValues;
        If[GetSimplifyEquation[],
            rhssToValue = rhssToValue // Simplify
        ];
        Which[
            GetParseMode[Temp],
                Module[{},
                    Global`pr["double "];
                    PutAppend[CForm[compToValue], outputfile];
                    Global`pr["="];
                    PutAppend[CForm[rhssToValue], outputfile];
                    Global`pr[";\n"]
                ]
            ,
            GetParseMode[Main],
                Module[{},
                    PutAppend[CForm[compToValue], outputfile];
                    Global`pr["="];
                    PutAppend[CForm[rhssToValue], outputfile];
                    Global`pr[";\n"]
                ]
            ,
            GetParseMode[AddToMain],
                Module[{},
                    PutAppend[CForm[compToValue], outputfile];
                    Global`pr["+="];
                    PutAppend[CForm[rhssToValue], outputfile];
                    Global`pr[";\n"]
                ]
            ,
            GetParseMode[Flux],
                Module[{},
                    Global`pr["double "];
                    PutAppend[CForm[compToValue], outputfile];
                    Global`pr["="];
                    PutAppend[CForm[rhssToValue], outputfile];
                    Global`pr[";\n"]
                ]
            ,
            True,
                Throw @ Message[PrintComponentEquation::EMode]
        ]
    ];

PrintComponentEquation::EMode = "ParseMode unrecognized!";

(*
    1. Set components for tensors (How would print them in c/c++ code)
    2. Set global map between tensor component and varlist index ( $MapComponentToVarlist ):
        case1 (start at 'new varlist', in this case, user should be careful when they define varlist)
            {a00 a01 ... b00 b01 ...}, {e00 e01 ... f00 f01 ...}, ...
              0   1  ... ... ... ...     0   1  ... ... ... ...   ...
        case2 (start at 'new var')
            {a00 a01 ... b00 b01 ...}, {e00 e01 ... f00 f01 ...}, ...
              0   1  ...  0   1  ...     0   1  ...  0   1  ...   ...
*)

SetComponent[compname_, exprname_] :=
    Module[{varlistindex, mapCtoV = GetMapComponentToVarlist[]},
        If[Length[mapCtoV] == 0 || GetProcessNewVarlist[] || (StringMatchQ[
            mode, "set components: independent"] && (compname[[0]] =!= Last[mapCtoV
            ][[1, 0]])),
            varlistindex = 0
            ,
            varlistindex = Last[mapCtoV] + 1
        ];
        ComponentValue[compname, exprname];
        If[MemberQ[mapCtoV[[All, 1]], compname],
            PrintVerbose["Skip adding Component ", compname, " to Global VarList, since it already exist"
                ]
            ,
            AppendTo[mapCtoV, compname -> varlistindex];
            SetMapComponentToVarlist[mapCtoV];
            PrintVerbose["Add Component ", compname, " to Global VarList"
                ]
        ];
        SetProcessNewVarlist[False]
    ];

(*
    compname: component expr in Mathematica kernal, say Pi[{1,-cart},{2,-cart}] 
*)

SetCompName[varname_, compindexlist_, coordinate_] :=
    Module[{compname = varname[[0]][]},
        If[Length[compindexlist] > 0,(*not scalar*)
            Do[
                AppendTo[
                    compname
                    ,
                    {
                        compindexlist[[icomp]]
                        ,
                        If[DownIndexQ[varname[[icomp]]],
                            -coordinate
                            ,
                            coordinate
                        ]
                    }
                ]
                ,
                {icomp, 1, Length[compindexlist]}
            ]
        ];
        Return[compname]
    ];

(*
    exprname: component expr to be printed to C code, or lhs, say Pi12[[ijk]],
              ignore the information about covariant/contravariant
*)

SetExprName[varname_, compindexlist_, addgpidx_] :=
    Module[{exprname = StringTrim[ToString[varname[[0]]], GetSuffixUnprotected[
        ]]},
        If[Length[compindexlist] > 0, (*not scalar*)
            Do[exprname = exprname <> ToString @ compindexlist[[icomp
                ]], {icomp, 1, Length[compindexlist]}]
        ];
        exprname =
            If[addgpidx,
                ToExpression[exprname <> GetGridPointIndex[]]
                ,
                ToExpression[exprname]
            ];
        Return[exprname]
    ];

End[];

EndPackage[];
