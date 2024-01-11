(* ::Package:: *)

(* (c) Liwei Ji, 01/2024 *)

BeginPackage["Generato`Varlist`"];

Needs["Generato`Basic`"];

Needs["Generato`Component`"];

ParseVarlist::usage = "ParseVarlist[varlist, chartname] process a varlist.";

Begin["`Private`"];

(* Data *)

(* Function *)

ParseVarlist[varlist_?ListQ, chartname_] :=
    Module[{iMin, iMax = 3, var, varname, symmetry, printname, symname,
         symindex, parseComponentValue},
        PrintVerbose["ParseVarlist..."];
        PrintVerbose["  Dim = ", GetDim[], ", Chart = ", chartname];
        PrintVerbose["  List: ", varlist];
        If[GetDim[] == 3,
            iMin = 1
            ,
            iMin = 0
        ];
        SetProcessNewVarlist[True];
        Do[
            var = varlist[[ivar]];
            {varname, symmetry, printname} = ParseVar[var];
            If[symmetry =!= Null,
                symname = symmetry[[0]];
                symindex = symmetry[[1]]
            ];
            If[!xTensorQ[varname[[0]]],
                If[GetParseMode[SetComp],
                    DefineTensor[varname, symmetry, printname]
                    ,
                    Throw @ Message[ParseVarlist::ETensorNonExist, ivar,
                         varname]
                ]
                ,
                If[!MemberQ[Keys[GetMapComponentToVarlist[]][[All, 0]],
                     varname[[0]]],
                    Throw @ Message[ParseVarlist::ETensorExistOutside,
                         ivar, varname]
                ]
            ];
            parseComponentValue[compindexlist_] := ParseComponent[varname,
                 compindexlist, chartname];
            Switch[Length[varname],
                0(* ZERO INDEX CASE: *),
                    parseComponentValue[{}]
                ,
                1(* ONE INDEX CASE: *),
                    Do[parseComponentValue[{ia}], {ia, iMin, iMax}]
                ,
                2(* TWO INDEXES CASE: *),
                    If[symmetry =!= Null,
                        Switch[symname,
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
                                     ivar, varname]
                        ];
                        varname //
                        ToBasis[chartname] //
                        ComponentArray //
                        ComponentValue
                        ,
                        Do[parseComponentValue[{ia, ib}], {ia, iMin, 
                            iMax}, {ib, iMin, iMax}]
                    ]
                ,
                3(* THREE INDEXES CASE *),
                    If[symmetry =!= Null,
                        Which[
                            (symindex[[1]] === varname[[2]]) && (symindex
                                [[2]] === varname[[3]])(*c(ab) or c[ab]*),
                                Switch[symname,
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
                                             ivar, varname]
                                ]
                            ,
                            (symindex[[1]] === varname[[1]]) && (symindex
                                [[2]] === varname[[2]])(*(ab)c or [ab]c*),
                                Switch[symname,
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
                                             ivar, varname]
                                ]
                            ,
                            True,
                                Throw @ Message[ParseVarlist::ESymmetryType,
                                     ivar, varname]
                        ];
                        varname //
                        ToBasis[chartname] //
                        ComponentArray //
                        ComponentValue
                        ,
                        Do[parseComponentValue[{ic, ia, ib}], {ic, iMin,
                             iMax}, {ia, iMin, iMax}, {ib, iMin, iMax}]
                    ]
                ,
                4(* FOUR INDEXES CASE *),
                    If[symmetry =!= Null,
                        Which[
                            symname == GenSet(*(cd)(ab) or [cd][ab]*),
                                
                                Which[
                                    (symmetry[[1]] === Cycles[{1, 2}]
                                        ) && (symmetry[[2]] === Cycles[{3, 4}]),
                                        Do[parseComponentValue[{ic, id,
                                             ia, ib}], {ic, iMin, iMax}, {id, ic, iMax}, {ia, iMin, iMax}, {ib, ia,
                                             iMax}]
                                    ,
                                    (symmetry[[1]] === -Cycles[{1, 2}
                                        ]) && (symmetry[[2]] === -Cycles[{3, 4}]),
                                        Do[parseComponentValue[{ic, id,
                                             ia, ib}], {ic, iMin, iMax}, {id, ic + 1, iMax}, {ia, iMin, iMax}, {ib,
                                             ia + 1, iMax}]
                                    ,
                                    True,
                                        Throw @ Message[ParseVarlist::ESymmetryType,
                                             ivar, varname]
                                ]
                            ,
                            (symindex[[1]] === varname[[3]]) && (symindex
                                [[2]] === varname[[4]])(*cd(ab) or cd[ab]*),
                                Switch[symname,
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
                                             ivar, varname]
                                ]
                            ,
                            (symindex[[1]] === varname[[1]]) && (symindex
                                [[2]] === varname[[2]])(*(ab)cd or [ab]cd*),
                                Switch[symname,
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
                                             ivar, varname]
                                ]
                            ,
                            (symindex[[1]] === varname[[2]]) && (symindex
                                [[2]] === varname[[3]])(*c(ab)d or c[ab]d*),
                                Switch[symname,
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
                                             ivar, varname]
                                ]
                            ,
                            True,
                                Throw @ Message[ParseVarlist::ESymmetryType,
                                     ivar, varname]
                        ];
                        varname //
                        ToBasis[chartname] //
                        ComponentArray //
                        ComponentValue
                        ,
                        Do[parseComponentValue[{ic, id, ia, ib}], {ic,
                             iMin, iMax}, {id, iMin, iMax}, {ia, iMin, iMax}, {ib, iMin, iMax}]
                    ]
                ,
                _(* OTHER NUM OF INDEXES *),
                    Throw @ Message[ParseVarlist::ETensorType, ivar, 
                        varname]
            ]
            ,
            {ivar, 1, Length[varlist]}
        ];
        PrintVerbose[];
    ];

ParseVarlist::ETensorNonExist = "Tensor of the `1`-th var, `2`, in varlist not exist and can't be defined since SetComp is false. Please check that this type of tensor can be handled by Generato!";

ParseVarlist::ETensorExistOutside = "Tensor of the `1`-th var, `2`, in varlist already exsit outside the global varlist, Please try a different name!";

ParseVarlist::ETensorType = "Tensor type of the `1`-th var, `2`, in varlist unsupported yet!";

ParseVarlist::ESymmetryType = "Symmetry type of the `1`-th var, `2`, in varlist unsupported yet!";

Protect[ParseVarlist];

DefineTensor[varname_, symmetry_, printname_] :=
    Module[{manifold = $Manifolds[[1]]},
        If[symmetry =!= Null && Length[printname] > 0,
            DefTensor[varname, manifold, symmetry, PrintAs -> printname
                ]
            ,
            If[symmetry =!= Null,
                DefTensor[varname, manifold, symmetry]
                ,
                If[Length[printname] > 0,
                    DefTensor[varname, manifold, PrintAs -> printname
                        ]
                    ,
                    DefTensor[varname, manifold]
                ]
            ]
        ]
    ];

ParseVar[var_] :=
    Module[{vfeature, varname = Null, symmetry = Null, printname = ""
        },
        Do[
            vfeature = var[[ifeature]];
            If[Head @ vfeature === Rule,
                printname = vfeature[[2]]
                ,
                If[Head @ Head @ vfeature === Symbol,
                    If[Length[vfeature] > 0 && AnyTrue[{Symmetric, Antisymmetric,
                         GenSet}, # === Head @ vfeature&],
                        symmetry = vfeature
                        ,
                        varname = vfeature
                    ]
                    ,
                    Throw @ Message[ParseVar::EVar, vfeature, var]
                ]
            ]
            ,
            {ifeature, 1, Length[var]}
        ];
        {varname, symmetry, printname}
    ];

ParseVar::EVar = "Var feature `1` in `2` unsupported yet!";

End[];

EndPackage[];
