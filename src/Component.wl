(* ::Package:: *)

(* (c) Liwei Ji, 01/2024 *)

BeginPackage["Generato`Component`"];

Needs["Generato`Basic`"];

GetMapComponentToVarlist::usage = "GetMapComponentToVarlist[] returns the map between tensor components and varlist indexes.";

SetProcessNewVarlist::usage="SetProcessNewVarlist[True] update the Boolean variable specifying if we are processing a new varlist.";

Begin["`Private`"];

(* Data *)

$MapComponentToVarlist = <||>;(*store all varlist's map*)

$ProcessNewVarlist = True;

(* Function *)

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

ParseComponent[varname_, compIndexList_?ListQ, coordinate_, addgpidx_
    ?BooleanQ, suffixname_?StringQ] :=
    Module[{compname, exprname},
        {compname, exprname} = SetNameArray[varname, compIndexList, coordinate,
             addgpidx];
        If[is4DCompIndexListIn3DTensor[compIndexList, varname],
            If[isUp4DCompIndexListIn3DTensor[compIndexList, varname],
                
                ComponentValue[compname, 0]
            ];
            Continue[]
        ];
        Which[
            StringMatchQ[mode, "set components*"],
                SetComponent[compname, exprname];
                PrintVerbose["Set Component ", compname, " for Tensor ",
                     varname[[0]]]
            ,
            StringMatchQ[mode, "print components*"],
                PrintComponent[mode, coordinate, varname, compname, suffixname
                    ];
                PrintVerbose["Print Component ", compname, " to C-file"
                    ]
            ,
            True,
                Throw @ Message[ParseComponent::EMode, mode]
        ]
    ];

ParseComponent::EMode = "Parse mode \"`1`\" undefined !";

PrintComponent[mode_?StringQ, coordinate_, varname_, compname_, suffixname_
    ?StringQ, cnd_?ListQ] :=
    Module[{},
        Which[
            StringMatchQ[mode, "print components initialization*"],
                Global`PrintComponentInitialization[mode, varname, compname,
                     GetGridPointIndex[]]
            ,
            StringMatchQ[mode, "print components equation*"],
                PrintComponentEquation[mode, coordinate, compname, {SuffixName
                     -> suffixname, ReplaceTerms -> cnd}]
            ,
            True,
                Throw @ Message[PrintComponent::ErrorMode, mode]
        ]
    ];

PrintComponent::ErrorMode = "Print mode `1` unsupported yet!";

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
            varlistindex = -1
            ,
            varlistindex = Last[mapCtoV]
        ];
        varlistindex = varlistindex + 1;
        (* set components *)
        ComponentValue[compname, exprname];
        (* if tensor component is already exist in the list or not *)
            
        If[MemberQ[mapCtoV[[All, 1]], compname],
            PrintVerbose["Skip adding Component ", compname, " to Global VarList, since it already exist"
                ]
            ,
            AppendTo[mapCtoV, {compname, varlistindex}];
            SetMapComponentToVarlist[mapCtoV];
            PrintVerbose["Add Component ", compname, " to Global VarList"
                ]
        ];
        (* update $ProcessNewVarlist *)
        SetProcessNewVarlist[False]
    ];

(*
    compname: component expr in Mathematica kernal, say Pi[{1,-cart},{2,-cart}] 
    exprname: component expr to be printed to C code, or lhs, say Pi12[[ijk]],
              ignore the information about covariant/contravariant
*)

SetNameArray[varname_, compindexlist_, coordinate_, addgpidx_] :=
    Module[{coordfull, compname, exprname},
        compname = varname[[0]][];
        exprname = StringTrim[ToString[varname[[0]]], GetSuffixUnprotected[
            ]];
        If[Length[compindexlist] > 0, (*not scalar*)
            Do[
                If[DownIndexQ[varname[[icomp]]],
                    coordfull = -coordinate
                    ,
                    coordfull = coordinate
                ];
                AppendTo[compname, {compindexlist[[icomp]], coordfull
                    }];
                exprname = exprname <> ToString @ compindexlist[[icomp
                    ]]
                ,
                {icomp, 1, Length[compindexlist]}
            ]
        ];
        exprname =
            If[addgpidx,
                ToExpression[exprname <> GetGridPointIndex[]]
                ,
                ToExpression[exprname]
            ];
        {compname, exprname}
    ];

End[];

EndPackage[];
