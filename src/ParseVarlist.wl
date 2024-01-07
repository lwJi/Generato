(* ::Package:: *)

(* (c) Liwei Ji, 01/2024 *)

BeginPackage["Generato`ParseVarlist`"];

Needs["Generato`Basic`"];

GetMapComponentToVarlist::usage = "GetMapComponentToVarlist[] returns the map between tensor components and varlist indexes.";

Begin["`Private`"];

(* Data *)

$MapComponentToVarlist = <||>;

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
    Module[{iMin, iMax = 3, var, varName, varLength, varWithSymmetry,
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
            var = varlist[[iVar]];(* { metricg[-a,-b], Symmetric[{-a,-b}], "g" } 
                
                
                
                *)
            varName = var[[1]];(* say metricg[-a,-b] *)
            varLength = Length[var];(* var length: how many descriptions for var 
                
                
                
                *)
            (* if with symmetry *)
            varWithSymmetry = (varLength == 3) || (varLength == 2 && 
                (!StringQ[var[[2]]]));
            If[varWithSymmetry,
                varSymmetryName = var[[2]][[0]];
                varSymmetryIndexes = var[[2]][[1]]
            ];
            (* check if tensor defined yet *)
            If[!xTensorQ[varName[[0]]],
                (* tensor not exist, creat one *)
                If[StringMatchQ[mode, "set components*"],
                    DefineTensor[var];
                    PrintVerbose["Define Tensor ", varName[[0]]]
                    ,
                    Throw @ Message[ParseVarlist::ETensorNonExist, iVar,
                         varName, mode]
                ]
                ,
(* tensor exist already: if tensor name is outside the global varlist 
    
    
    
    
    
    
    
    
    
    
    
    
    
    *)
                If[!MemberQ[GetMapComponentToVarlist[][[All, 1, 0]], 
                    varName[[0]]],
                    Throw @ Message[ParseVarlist::ETensorExistOutside,
                         iVar, varName]
                ]
            ];
            (* set temp function *)
            manipulateComponentValue[compIndexList_] := ManipulateComponent[
                compIndexList, mode, chartname, varName, gridpointindex, suffixname, 
                cnd];
            (* consider different types of tensor *)
            Switch[Length[varName],
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
                                     iVar, varName]
                        ];
                        varName //
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
                            (varSymmetryIndexes[[1]] === varName[[2]]
                                ) && (varSymmetryIndexes[[2]] === varName[[3]]),
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
                                             iVar, varName]
                                ]
                            ,
                            (* (ab)c or [ab]c *)(varSymmetryIndexes[[
                                1]] === varName[[1]]) && (varSymmetryIndexes[[2]] === varName[[2]]),
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
                                             iVar, varName]
                                ]
                            ,
                            (* other three indexes cases *)True,
                                Throw @ Message[ParseVarlist::ESymmetryType,
                                     iVar, varName]
                        ];(* end of Which *)
                        varName //
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
                            (varSymmetryIndexes[[1]] === varName[[3]]
                                ) && (varSymmetryIndexes[[2]] === varName[[4]]),
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
                                             iVar, varName]
                                ]
                            ,
                            (* (ab)cd or [ab]cd *)(varSymmetryIndexes
                                [[1]] === varName[[1]]) && (varSymmetryIndexes[[2]] === varName[[2]]),
                                
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
                                             iVar, varName]
                                ]
                            ,
                            (* c(ab)d or c[ab]d *)(varSymmetryIndexes
                                [[1]] === varName[[2]]) && (varSymmetryIndexes[[2]] === varName[[3]]),
                                
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
                                             iVar, varName]
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
                                             iVar, varName]
                                ]
                            ,
                            (* other four indexes cases *)True,
                                Throw @ Message[ParseVarlist::ESymmetryType,
                                     iVar, varName]
                        ]; (* end of Which *)
                        varName //
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
                        varName]
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
    1. Set components for tensors (How would print them in c/c++ code)
    2. Set global map between tensor component and varlist index ( $MapComponentToVarlist ):
        case1 (start at 'new varlist', in this case, user should be careful when they define varlist)
            {a00 a01 ... b00 b01 ...}, {e00 e01 ... f00 f01 ...}, ...
              0   1  ... ... ... ...     0   1  ... ... ... ...   ...
        case2 (start at 'new var')
            {a00 a01 ... b00 b01 ...}, {e00 e01 ... f00 f01 ...}, ...
              0   1  ...  0   1  ...     0   1  ...  0   1  ...   ...
*)

SetComponentAndMapC2V[mode_, compName_, exprName_] :=
    Module[{varlistindex, mapCtoV = GetMapComponentToVarlist[]},
        If[Length[mapCtoV] == 0 || GetProcessNewVarlist[] || (StringMatchQ[
            mode, "set components: independent"] && (compName[[0]] =!= Last[mapCtoV
            ][[1, 0]])),
            varlistindex = -1
            ,
            varlistindex = Last[mapCtoV]
        ];
        varlistindex = varlistindex + 1;
        (* set components *)
        ComponentValue[compName, exprName];
        (* if tensor component is already exist in the list or not *)
            
        If[MemberQ[mapCtoV[[All, 1]], compName],
            PrintVerbose["Skip adding Component ", compName, " to Global VarList, since it already exist"
                ]
            ,
            AppendTo[mapCtoV, {compName, varlistindex}];
            SetMapComponentToVarlist[mapCtoV];
            PrintVerbose["Add Component ", compName, " to Global VarList"
                ]
        ];
        (* update $ProcessNewVarlist *)
        SetProcessNewVarlist[False]
    ];

Protect[SetComponentAndMapC2V];

End[];

EndPackage[];
