(* ::Package:: *)

(* (c) Liwei Ji, 01/2024 *)

BeginPackage["Generato`Varlist`"];

Needs["Generato`Basic`"];

Needs["Generato`Component`"];

Begin["`Private`"];

(* Data *)

(* Function *)

ParseVarlist[varlist_?ListQ, chartname] :=
    Module[{iMin, iMax = 3, var, varname, varWithSymmetry, varSymmetryName,
         varSymmetryIndexes, parseComponentValue},
        manifold = $Manifolds[[1]];
        If[chartname == Null,
            chartname = ChartsOfManifold[manifold][[1]]
        ];
        If[DimOfManifold[manifold] == 3,
            iMin = 1
            ,
            iMin = 0
        ];
        SetProcessNewVarlist[True];
        (* loop over varlist in the list *)
        Do[
            var = varlist[[iVar]];
            varname = var[[1]]; (* say metricg[-a,-b] *)
            varWithSymmetry = (Length[var] == 3) || (Length[var] == 2
                 && (!StringQ[var[[2]]]));
            If[varWithSymmetry,
                varSymmetryName = var[[2]][[0]];
                varSymmetryIndexes = var[[2]][[1]]
            ];
            If[!xTensorQ[varname[[0]]],
                If[GetParseMode[SetComp],
                    DefineTensor[var]
                    ,
                    Throw @ Message[ParseVarlist::ETensorNonExist, iVar,
                         varname]
                ]
                ,
                If[!MemberQ[Keys[GetMapComponentToVarlist[]][[All, 0]],
                     varname[[0]]],
                    Throw @ Message[ParseVarlist::ETensorExistOutside,
                         iVar, varname]
                ]
            ];
            (* set temp function *)
            parseComponentValue[compindexlist_] := ParseComponent[varname,
                 compindexlist, chartname];
            (* consider different types of tensor *)
            Switch[Length[varname],
                (* ZERO INDEX CASE: *)0,
                    parseComponentValue[{}]
                ,
                (* ONE INDEX CASE: *)1,
                    Do[parseComponentValue[{ia}], {ia, iMin, iMax}]
                ,
                (* TWO INDEXES CASE: *)2,
                    If[varWithSymmetry,
                        (* With Symmetry *)
                        Switch[varSymmetryName,
                            Symmetric,
                                Do[parseComponentValue[{ia, ib}], {ia,
                                     iMin, iMax}, {ib, ia, iMax}]
                            ,
                            Antisymmetric,
                                Do[parseComponentValue[{ia, ib}], {ia,
                                     iMin, iMax}, {ib, ia + 1, iMax}]
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
                        Do[parseComponentValue[{ia, ib}], {ia, iMin, 
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
                                        Do[parseComponentValue[{ic, ia,
                                             ib}], {ic, iMin, iMax}, {ia, iMin, iMax}, {ib, ia, iMax}]
                                    ,
                                    Antisymmetric,
                                        Do[parseComponentValue[{ic, ia,
                                             ib}], {ic, iMin, iMax}, {ia, iMin, iMax}, {ib, ia + 1, iMax}]
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
                                        Do[parseComponentValue[{ia, ib,
                                             ic}], {ia, iMin, iMax}, {ib, ia, iMax}, {ic, iMin, iMax}]
                                    ,
                                    Antisymmetric,
                                        Do[parseComponentValue[{ia, ib,
                                             ic}], {ia, iMin, iMax}, {ib, ia + 1, iMax}, {ic, iMin, iMax}]
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
                        Do[parseComponentValue[{ic, ia, ib}], {ic, iMin,
                             iMax}, {ia, iMin, iMax}, {ib, iMin, iMax}]
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
                                        Do[parseComponentValue[{ic, id,
                                             ia, ib}], {ic, iMin, iMax}, {id, iMin, iMax}, {ia, iMin, iMax}, {ib,
                                             ia, iMax}]
                                    ,
                                    Antisymmetric,
                                        Do[parseComponentValue[{ic, id,
                                             ia, ib}], {ic, iMin, iMax}, {id, iMin, iMax}, {ia, iMin, iMax}, {ib,
                                             ia + 1, iMax}]
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
                                        Do[parseComponentValue[{ia, ib,
                                             ic, id}], {ia, iMin, iMax}, {ib, ia, iMax}, {ic, iMin, iMax}, {id, iMin,
                                             iMax}]
                                    ,
                                    Antisymmetric,
                                        Do[parseComponentValue[{ia, ib,
                                             ic, id}], {ia, iMin, iMax}, {ib, ia + 1, iMax}, {ic, iMin, iMax}, {id,
                                             iMin, iMax}]
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
                                        Do[parseComponentValue[{ic, ia,
                                             ib, id}], {ic, iMin, iMax}, {ia, iMin, iMax}, {ib, ia, iMax}, {id, iMin,
                                             iMax}]
                                    ,
                                    Antisymmetric,
                                        Do[parseComponentValue[{ic, ia,
                                             ib, id}], {ic, iMin, iMax}, {ia, iMin, iMax}, {ib, ia + 1, iMax}, {id,
                                             iMin, iMax}]
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
                                        Do[parseComponentValue[{ic, id,
                                             ia, ib}], {ic, iMin, iMax}, {id, ic, iMax}, {ia, iMin, iMax}, {ib, ia,
                                             iMax}]
                                    ,
                                    (var[[2]][[1]] === -Cycles[{1, 2}
                                        ]) && (var[[2]][[2]] === -Cycles[{3, 4}]),
                                        Do[parseComponentValue[{ic, id,
                                             ia, ib}], {ic, iMin, iMax}, {id, ic + 1, iMax}, {ia, iMin, iMax}, {ib,
                                             ia + 1, iMax}]
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
                        Do[parseComponentValue[{ic, id, ia, ib}], {ic,
                             iMin, iMax}, {id, iMin, iMax}, {ia, iMin, iMax}, {ib, iMin, iMax}]
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

ParseVarlist::ETensorNonExist = "Tensor of the `1`-th var, `2`, in varlist not exist and can't be defined since SetComp is false. Please check that this type of tensor can be handled by Generato!";

ParseVarlist::ETensorExistOutside = "Tensor of the `1`-th var, `2`, in varlist already exsit outside the global varlist, Please try a different name!";

ParseVarlist::ETensorType = "Tensor type of the `1`-th var, `2`, in varlist unsupported yet!";

ParseVarlist::ESymmetryType = "Symmetry type of the `1`-th var, `2`, in varlist unsupported yet!";

Protect[ParseVarlist];

End[];

EndPackage[];
