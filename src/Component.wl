(* ::Package:: *)

(* (c) Liwei Ji, 01/2024 *)

BeginPackage["Generato`Component`"];

Needs["Generato`Basic`"];

Needs["Generato`ParseMode`"];

Print["------------------------------------------------------------"];

Print["Package Generato`Component`, {2024, 1, 11}"];

Print["------------------------------------------------------------"];

GetMapComponentToVarlist::usage = "GetMapComponentToVarlist[] returns the map between tensor components and varlist indexes.";

SetProcessNewVarlist::usage = "SetProcessNewVarlist[True] updates the Boolean variable specifying if we are processing a new varlist.";

GetSimplifyEquation::usage = "GetSimplifyEquation[] returns the Boolean variable specifying if Simplify the equations.";

SetSimplifyEquation::usage = "SetSimplifyEquation[True] updates the Boolean variable specifying if Simplify the equations.";

GetUseLetterForTensorComponet::usage = "GetUseLetterForTensorComponet[] returns the Boolean variable specifying if use letter for tensor components (instead of number).";

SetUseLetterForTensorComponet::usage = "SetUseLetterForTensorComponet[True] updates the Boolean variable specifying if use letter for tensor components (instead of number).";

GetTempVariableType::usage = "GetTempVariableType[] returns the type of temporary variable.";

SetTempVariableType::usage = "SetTempVariableType[CCTK_REAL] updates the type of temporary variable.";

GetInterfaceWithNonCoordBasis::usage = "GetInterfaceWithNonCoordBasis[] returns the Boolean variable specifying if we are interfacing with non-coordinate basis.";

SetInterfaceWithNonCoordBasis::usage = "SetInterfaceWithNonCoordBasis[True] updates the Boolean variable specifying if we are interfacing with non-coordinate basis.";

GetSuffixName::usage = "GetSuffixName[] returns the suffix added to vars in the current list.";

SetSuffixName::usage = "SetSuffixName[suffix] updates the suffix added to vars in the current list.";

GetPrefixDt::usage = "GetPrefixDt[] return the prefix used for dt of vars.";

ParseComponent::usage = "ParseComponent[varinfo, compindexlist, coordinate] process a component.";

Is3DAbstractIndex::usage = "Is3DAbstractIndex[abstractindex] return True if the abstract index is after letter 'i' and only the first letter matters (say h <=> h1 <=> h2 ...).";

Begin["`Private`"];

(* Data *)

$MapComponentToVarlist = <||>;(*store all varlist's map*)

$ProcessNewVarlist = True;

$SimplifyEquation = True;

$UseLetterForTensorComponet = False;

$TempVariableType = "double";

$InterfaceWithNonCoordBasis = False;

$SuffixName = "";

$PrefixDt = "dt";

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

GetSimplifyEquation[] :=
  Return[$SimplifyEquation];

Protect[GetSimplifyEquation];

SetSimplifyEquation[simplify_] :=
  Module[{},
    $SimplifyEquation = simplify
  ];

Protect[SetSimplifyEquation];

GetUseLetterForTensorComponet[] :=
  Return[$UseLetterForTensorComponet];

Protect[GetUseLetterForTensorComponet];

SetUseLetterForTensorComponet[useletter_] :=
  Module[{},
    $UseLetterForTensorComponet = useletter
  ];

Protect[SetUseLetterForTensorComponet];

GetTempVariableType[] :=
  Return[$TempVariableType];

Protect[GetTempVariableType];

SetTempVariableType[type_] :=
  Module[{},
    $TempVariableType = type
  ];

Protect[SetTempVariableType];

GetInterfaceWithNonCoordBasis[] :=
  Return[$InterfaceWithNonCoordBasis];

Protect[GetInterfaceWithNonCoordBasis];

SetInterfaceWithNonCoordBasis[noncoordbasis_] :=
  Module[{},
    $InterfaceWithNonCoordBasis = noncoordbasis
  ];

Protect[SetInterfaceWithNonCoordBasis];

GetSuffixName[] :=
  Return[$SuffixName];

Protect[GetSuffixName];

SetSuffixName[suffix_] :=
  Module[{},
    $SuffixName = suffix
  ];

Protect[SetSuffixName];

GetPrefixDt[] :=
  Return[$PrefixDt];

Protect[GetPrefixDt];

SetPrefixDt[Prefix_] :=
  Module[{},
    $PrefixDt = Prefix
  ];

Protect[SetPrefixDt];

ParseComponent[varinfo_, compindexlist_?ListQ, coordinate_, extrareplacerules_?ListQ] :=
  Module[{compname, varname},
    PrintVerbose["  ParseComponent..."];
    varname = varinfo[[1]];
    compname = SetCompName[varname, compindexlist, coordinate];
    If[Is4DCompIndexListIn3DTensor[compindexlist, varname],
      If[IsUp4DCompIndexListIn3DTensor[compindexlist, varname],
        ComponentValue[compname, 0]
      ];
      Continue[]
    ];
    Which[
      GetParseMode[SetComp],
        SetComponent[compname, SetExprName[varname, compindexlist, coordinate]]
      ,
      GetParseMode[PrintComp],
        PrintComponent[coordinate, varinfo, compname, extrareplacerules]
      ,
      True,
        Throw @ Message[ParseComponent::EMode]
    ]
  ];

ParseComponent::EMode = "ParseMode unrecognized!";

Protect[ParseComponent];

PrintComponent[coordinate_, varinfo_, compname_, extrareplacerules_] :=
  Module[{},
    Which[
      GetParsePrintCompMode[Initializations],
        PrintVerbose["    PrintComponentInitialization ", compname, "..."];
        Global`PrintComponentInitialization[varinfo, compname]
      ,
      GetParsePrintCompMode[Equations],
        PrintVerbose["    PrintComponentEquation ", compname, "..."];
        Global`PrintComponentEquation[coordinate, compname, extrareplacerules]
      ,
      True,
        Throw @ Message[PrintComponent::EMode]
    ]
  ];

PrintComponent::EMode = "PrintMode unrecognized!";

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
    PrintVerbose["    SetComponent ", compname, "..."];
    If[Length[mapCtoV] == 0 || GetProcessNewVarlist[] || (GetParseSetCompMode[IndependentVarlistIndex] && (compname[[0]] =!= Last[Keys[mapCtoV]][[0]])),
      varlistindex = 0(*C convention*)
      ,
      varlistindex = Last[mapCtoV] + 1
    ];
    ComponentValue[compname, exprname];
    If[!MemberQ[Keys[mapCtoV], compname],
      AppendTo[mapCtoV, compname -> varlistindex];
      SetMapComponentToVarlist[mapCtoV]
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

SetExprName[varname_, compindexlist_, coordinate_] :=
  Module[{exprname, colist = {"t", "x", "y", "z"}},
    exprname = StringTrim[ToString[varname[[0]]], GetSuffixUnprotected[]];
    If[Length[compindexlist] > 0, (*not scalar, ignore up/down*)
      If[coordinate =!= GetDefaultChart[],
        exprname = exprname <> ToString[coordinate]
      ];
      Do[
        exprname =
          exprname <>
            If[GetUseLetterForTensorComponet[],
              colist[[compindexlist[[icomp]] + 1]]
              ,
              ToString @ compindexlist[[icomp]]
            ]
        ,
        {icomp, 1, Length[compindexlist]}
      ]
    ];
    exprname =
      If[GetParseSetCompMode[WithoutGridPointIndex] || (GetInterfaceWithNonCoordBasis[] && coordinate === GetDefaultChart[]),
        ToExpression[exprname]
        ,
        If[GetParseSetCompMode[UseTilePointIndex],
          ToExpression[exprname <> GetTilePointIndex[]]
          ,
          ToExpression[exprname <> GetGridPointIndex[]]
        ]
      ];
    Return[exprname]
  ];

Is3DAbstractIndex[idx_] :=
  Module[{},
    LetterNumber[StringPart[ToString[idx /. {-1 x_ :> x}], 1]] >= LetterNumber["i"]
  ];

Is4DCompIndexIn3DTensor[idx_, idxcomp_] :=
  Module[{},
    Is3DAbstractIndex[idx] && (idxcomp == 0)
  ];

Is4DCompIndexListIn3DTensor[idxcomplist_, varname_] :=
  Module[{is4Didxcomplist = False},
    If[Length[idxcomplist] > 0,
      Do[
        If[Is4DCompIndexIn3DTensor[varname[[icomp]], idxcomplist[[icomp]]],
          is4Didxcomplist = True
        ]
        ,
        {icomp, 1, Length[idxcomplist]}
      ]
    ];
    is4Didxcomplist
  ];

IsUp4DCompIndexListIn3DTensor[idxcomplist_, varname_] :=
  Module[
    {isup4Didxcomplist = False}
    ,
    (*is there a 0th up index in the component list*)
    If[Length[idxcomplist] > 0,
      Do[
        If[UpIndexQ[varname[[icomp]]] && Is4DCompIndexIn3DTensor[varname[[icomp]], idxcomplist[[icomp]]],
          isup4Didxcomplist = True
        ]
        ,
        {icomp, 1, Length[idxcomplist]}
      ]
    ];
    (*if there is also a 0th down index, skip this one*)
    (*we want skip all the components with 0th down index*)
    If[isup4Didxcomplist,
      Do[
        If[DownIndexQ[varname[[icomp]]] && Is4DCompIndexIn3DTensor[varname[[icomp]], idxcomplist[[icomp]]],
          isup4Didxcomplist = False
        ]
        ,
        {icomp, 1, Length[idxcomplist]}
      ]
    ];
    isup4Didxcomplist
  ];

End[];

EndPackage[];
