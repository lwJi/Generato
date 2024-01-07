(* ::Package:: *)

(* (c) Liwei Ji, 01/2024 *)

BeginPackage["Generato`Varlist`"];

Needs["Generato`Basic`"];

Needs["Generato`Component`"];

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

Options[ParseVarlist] = {ParseMode -> "", ChartName -> Null, SuffixName
     -> ""};

ParseVarlist[varlist_?ListQ, OptionsPattern[]] :=
    Module[{iMin, iMax = 3, var, varname, varLength, varWithSymmetry,
         varSymmetryName, varSymmetryIndexes, manipulateComponentValue, mode,
         chartname, suffixname},
        {mode, chartname, suffixname} = OptionValue[{ParseMode, ChartName,
             SuffixName}];
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
            varname = var[[1]]; (* say metricg[-a,-b] *)
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
            manipulateComponentValue[compindexlist_] := ParseComponent[
                varname, compindexlist, chartname, mode, suffixname];
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
                        ];
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
                        ];
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

End[];

EndPackage[];
