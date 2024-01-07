(* ::Package:: *)

(* (c) Liwei Ji, 01/2024 *)

BeginPackage["Generato`ParseVarlist`"];

Needs["Generato`Basic`"];

GetMapComponentToVarlist::usage = "GetMapComponentToVarlist[] returns the map between tensor components and varlist indexes.";

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

Options[ParseVarlist] = {ParseMode -> "", ChartName -> Null, GridPointIndex
     -> "[[ijk]]", SuffixName -> ""};

ParseVarlist[varlist_?ListQ, OptionsPattern[]] :=
    Module[{iMin, iMax = 3, var, varname, varLength, varWithSymmetry,
         varSymmetryName, varSymmetryIndexes, manipulateComponentValue, mode,
         chartname, gridpointindex, suffixname},
        {mode, chartname, gridpointindex, suffixname} = OptionValue[{
            ParseMode, ChartName, GridPointIndex, SuffixName}];
        manifold = $Manifolds[[1]];
        If[chartname == Null,
            chartname = ChartsOfManifold[manifold][[1]]
        ];
        (* set global parameters *)
        SetProcessNewVarlist[True];
        (* set temp parameters *)
        If[Getdim[] == 3,
            iMin = 1
            ,
            iMin = 0
        ];
        (* loop over varlist in the list *)
        Do[
            var = varlist[[iVar]];
            varname = var[[1]];(* say metricg[-a,-b] *)
            varLength = Length[var];
            (* if with symmetry *)
            varWithSymmetry = (varLength == 3) || (varLength == 2 && 
                (!StringQ[var[[2]]]));
            If[varWithSymmetry,
                varSymmetryName = var[[2]][[0]];
                varSymmetryIndexes = var[[2]][[1]]
            ];
            (* check if tensor defined yet *)
            If[!xTensorQ[varname[[0]]],
                (* tensor not exist, creat one *)
                If[StringMatchQ[mode, "set components*"],
                    DefineTensor[var];
                    PrintVerbose["Define Tensor ", varname[[0]]]
                    ,
                    Throw @ Message[ParseVarlist::ETensorNonExist, iVar,
                         varname, mode]
                ]
                ,
                If[!MemberQ[GetMapComponentToVarlist[][[All, 1, 0]], 
                    varname[[0]]],
                    Throw @ Message[ParseVarlist::ETensorExistOutside,
                         iVar, varname]
                ]
            ];
            (* set temp function *)
            manipulateComponentValue[compindexlist_] := ManipulateComponent[
                compindexlist, mode, chartname, varname, gridpointindex, suffixname, 
                cnd];
            (* consider different types of tensor *)
            Switch[Length[varname],
                (* ZERO INDEX CASE: *)0,
                    manipulateComponentValue[{}]
                ,
                (* ONE INDEX CASE: *)1,
                    Do[manipulateComponentValue[{ia}], {ia, iMin, iMax
                        }]
                ,
                (* TWO INDEXES CASE: *)2,
                    If[varWithSymmetry,
                        (* With Symmetry *)
                        Switch[varSymmetryName,
                            Symmetric,
                                Do[manipulateComponentValue[{ia, ib}],
                                     {ia, iMin, iMax}, {ib, ia, iMax}]
                            ,
                            Antisymmetric,
                                Do[manipulateComponentValue[{ia, ib}],
                                     {ia, iMin, iMax}, {ib, ia + 1, iMax}]
                            ,
                            _,
                                Throw @ Message[ParseVarlist::ESymmetryType,
                                     iVar, varname]
                        ];
                        varname //
                        ToBasis[chartname] //
                        ComponentArray //
                        ComponentValue
                        ,
                        (* Without Symmetry *)
                        Do[manipulateComponentValue[{ia, ib}], {ia, iMin,
                             iMax}, {ib, iMin, iMax}]
                    ]
                ,
                (* THREE INDEXES CASE *)3,
                    If[varWithSymmetry,
                        (* With Symmetry *)
                        Which[(* c(ab) or c[ab] *)
                            (varSymmetryIndexes[[1]] === varname[[2]]
                                ) && (varSymmetryIndexes[[2]] === varname[[3]]),
                                Switch[varSymmetryName,
                                    Symmetric,
                                        Do[manipulateComponentValue[{
                                            ic, ia, ib}], {ic, iMin, iMax}, {ia, iMin, iMax}, {ib, ia, iMax}]
                                    ,
                                    Antisymmetric,
                                        Do[manipulateComponentValue[{
                                            ic, ia, ib}], {ic, iMin, iMax}, {ia, iMin, iMax}, {ib, ia + 1, iMax}]
                                            
                                    ,
                                    _,
                                        Throw @ Message[ParseVarlist::ESymmetryType,
                                             iVar, varname]
                                ]
                            ,
                            (* (ab)c or [ab]c *)(varSymmetryIndexes[[
                                1]] === varname[[1]]) && (varSymmetryIndexes[[2]] === varname[[2]]),
                                Switch[varSymmetryName,
                                    Symmetric,
                                        Do[manipulateComponentValue[{
                                            ia, ib, ic}], {ia, iMin, iMax}, {ib, ia, iMax}, {ic, iMin, iMax}]
                                    ,
                                    Antisymmetric,
                                        Do[manipulateComponentValue[{
                                            ia, ib, ic}], {ia, iMin, iMax}, {ib, ia + 1, iMax}, {ic, iMin, iMax}]
                                            
                                    ,
                                    _,
                                        Throw @ Message[ParseVarlist::ESymmetryType,
                                             iVar, varname]
                                ]
                            ,
                            (* other three indexes cases *)True,
                                Throw @ Message[ParseVarlist::ESymmetryType,
                                     iVar, varname]
                        ];(* end of Which *)
                        varname //
                        ToBasis[chartname] //
                        ComponentArray //
                        ComponentValue
                        ,
                        (* Without Symmetry *)
                        Do[manipulateComponentValue[{ic, ia, ib}], {ic,
                             iMin, iMax}, {ia, iMin, iMax}, {ib, iMin, iMax}]
                    ]
                ,
                (* FOUR INDEXES CASE *)4,
                    If[varWithSymmetry,
                        (* With Symmetry *)
                        Which[(* cd(ab) or cd[ab] *)
                            (varSymmetryIndexes[[1]] === varname[[3]]
                                ) && (varSymmetryIndexes[[2]] === varname[[4]]),
                                Switch[varSymmetryName,
                                    Symmetric,
                                        Do[manipulateComponentValue[{
                                            ic, id, ia, ib}], {ic, iMin, iMax}, {id, iMin, iMax}, {ia, iMin, iMax
                                            }, {ib, ia, iMax}]
                                    ,
                                    Antisymmetric,
                                        Do[manipulateComponentValue[{
                                            ic, id, ia, ib}], {ic, iMin, iMax}, {id, iMin, iMax}, {ia, iMin, iMax
                                            }, {ib, ia + 1, iMax}]
                                    ,
                                    _,
                                        Throw @ Message[ParseVarlist::ESymmetryType,
                                             iVar, varname]
                                ]
                            ,
                            (* (ab)cd or [ab]cd *)(varSymmetryIndexes
                                [[1]] === varname[[1]]) && (varSymmetryIndexes[[2]] === varname[[2]]),
                                
                                Switch[varSymmetryName,
                                    Symmetric,
                                        Do[manipulateComponentValue[{
                                            ia, ib, ic, id}], {ia, iMin, iMax}, {ib, ia, iMax}, {ic, iMin, iMax},
                                             {id, iMin, iMax}]
                                    ,
                                    Antisymmetric,
                                        Do[manipulateComponentValue[{
                                            ia, ib, ic, id}], {ia, iMin, iMax}, {ib, ia + 1, iMax}, {ic, iMin, iMax
                                            }, {id, iMin, iMax}]
                                    ,
                                    _,
                                        Throw @ Message[ParseVarlist::ESymmetryType,
                                             iVar, varname]
                                ]
                            ,
                            (* c(ab)d or c[ab]d *)(varSymmetryIndexes
                                [[1]] === varname[[2]]) && (varSymmetryIndexes[[2]] === varname[[3]]),
                                
                                Switch[varSymmetryName,
                                    Symmetric,
                                        Do[manipulateComponentValue[{
                                            ic, ia, ib, id}], {ic, iMin, iMax}, {ia, iMin, iMax}, {ib, ia, iMax},
                                             {id, iMin, iMax}]
                                    ,
                                    Antisymmetric,
                                        Do[manipulateComponentValue[{
                                            ic, ia, ib, id}], {ic, iMin, iMax}, {ia, iMin, iMax}, {ib, ia + 1, iMax
                                            }, {id, iMin, iMax}]
                                    ,
                                    _,
                                        Throw @ Message[ParseVarlist::ESymmetryType,
                                             iVar, varname]
                                ]
                            ,
                            (* (cd)(ab) or [cd][ab] *)varSymmetryName
                                 == GenSet,
                                Which[
                                    (var[[2]][[1]] === Cycles[{1, 2}]
                                        ) && (var[[2]][[2]] === Cycles[{3, 4}]),
                                        Do[manipulateComponentValue[{
                                            ic, id, ia, ib}], {ic, iMin, iMax}, {id, ic, iMax}, {ia, iMin, iMax},
                                             {ib, ia, iMax}]
                                    ,
                                    (var[[2]][[1]] === -Cycles[{1, 2}
                                        ]) && (var[[2]][[2]] === -Cycles[{3, 4}]),
                                        Do[manipulateComponentValue[{
                                            ic, id, ia, ib}], {ic, iMin, iMax}, {id, ic + 1, iMax}, {ia, iMin, iMax
                                            }, {ib, ia + 1, iMax}]
                                    ,
                                    True,
                                        Throw @ Message[ParseVarlist::ESymmetryType,
                                             iVar, varname]
                                ]
                            ,
                            (* other four indexes cases *)True,
                                Throw @ Message[ParseVarlist::ESymmetryType,
                                     iVar, varname]
                        ]; (* end of Which *)
                        varname //
                        ToBasis[chartname] //
                        ComponentArray //
                        ComponentValue
                        ,
                        (* Without Symmetry *)
                        Do[manipulateComponentValue[{ic, id, ia, ib}],
                             {ic, iMin, iMax}, {id, iMin, iMax}, {ia, iMin, iMax}, {ib, iMin, iMax
                            }]
                    ]
                ,
                (* OTHER NUM OF INDEXES *)_,
                    Throw @ Message[ParseVarlist::ETensorType, iVar, 
                        varname]
            ]
            ,
            {iVar, 1, Length[varlist]}
        ];
    ];

ParseVarlist::ETensorNonExist = "Tensor of the `1`-th var, `2`, in varlist not exist and can't be defined since it's not in 'set components' mode (mode `3`). Please check that this type of tensor can be handled by xActToC!";

ParseVarlist::ETensorExistOutside = "Tensor of the `1`-th var, `2`, in varlist already exsit outside the global varlist, Please try a different name!";

ParseVarlist::ETensorType = "Tensor type of the `1`-th var, `2`, in varlist unsupported yet!";

ParseVarlist::ESymmetryType = "Symmetry type of the `1`-th var, `2`, in varlist unsupported yet!";

Protect[ParseVarlist];

(*
    compname: component expr in Mathematica kernal, say Pi[{1,-cart},{2,-cart}] 
    exprname: component expr to be printed to C code, or lhs, say Pi12[[ijk]],
              ignore the information about covariant/contravariant
*)

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

Protect[SetComponent];

SetNameArray[varname_, compindexlist_, mode_, coordinate_] :=
    Module[
        {coordfull, compname, exprname}
        ,
        (* initialize *)
        compname = varname[[0]][];
        exprname = StringTrim[ToString[varname[[0]]], GetsuffixUnprotected[
            ]];
        (* if not scalar, update names *)
        If[Length[compindexlist] > 0,
            Do[
                If[DownIndexQ[varname[[compIndex]]],
                    coordfull = -coordinate
                    ,
                    coordfull = coordinate
                ];
                AppendTo[compname, {compindexlist[[compIndex]], coordfull
                    }];
                exprname = exprname <> ToString @ compindexlist[[compIndex
                    ]]
                ,
                {compIndex, 1, Length[compindexlist]}
            ]
        ];
        (* if set component for temporary varlist or not *)
        If[StringMatchQ[mode, "set components: temporary"],
            exprname = ToExpression[exprname]
            ,
            exprname = ToExpression[exprname <> gridpointindex]
        ];
        (* return NameArray *)
        {compname, exprname}
    ];

Protect[SetNameArray];

End[];

EndPackage[];