(* ::Package:: *)

(* (c) Liwei Ji, 01/2024 *)

BeginPackage["Generato`Component`", {"Generato`Context`"}];

Needs["Generato`Basic`"];

Needs["Generato`ParseMode`"];

If[Environment["QUIET"] =!= "1",
  System`Print["------------------------------------------------------------"];
  System`Print["Package Generato`Component`, {2024, 1, 11}"];
  System`Print["------------------------------------------------------------"];
];

GetMapComponentToVarlist::usage = "GetMapComponentToVarlist[] returns the Association mapping tensor components to varlist indices.";

SetMapComponentToVarlist::usage = "SetMapComponentToVarlist[map] sets the mapping in global state.";

GetProcessNewVarlist::usage = "GetProcessNewVarlist[] returns the ProcessNewVarlist flag.";

SetProcessNewVarlist::usage = "SetProcessNewVarlist[bool] sets whether the next varlist is treated as a new varlist for index numbering.";

GetSimplifyEquation::usage = "GetSimplifyEquation[] returns True if equations are simplified before output.";

SetSimplifyEquation::usage = "SetSimplifyEquation[bool] enables or disables simplification of equations before output.";

GetUseLetterForTensorComponent::usage = "GetUseLetterForTensorComponent[] returns True if letters are used for tensor component indices instead of numbers.";

SetUseLetterForTensorComponent::usage = "SetUseLetterForTensorComponent[bool] sets whether to use letters for tensor component indices instead of numbers.";

GetTempVariableType::usage = "GetTempVariableType[] returns the C type string used for temporary variables.";

SetTempVariableType::usage = "SetTempVariableType[type] sets the C type string used for temporary variables.";

GetInterfaceWithNonCoordBasis::usage = "GetInterfaceWithNonCoordBasis[] returns True if interfacing with non-coordinate basis tensors.";

SetInterfaceWithNonCoordBasis::usage = "SetInterfaceWithNonCoordBasis[bool] enables or disables interfacing with non-coordinate basis tensors.";

GetSuffixName::usage = "GetSuffixName[] returns the suffix appended to variable names in the current varlist.";

SetSuffixName::usage = "SetSuffixName[suffix] sets the suffix appended to variable names in the current varlist.";

GetPrefixDt::usage = "GetPrefixDt[] returns the prefix used for time derivatives of variables.";

SetPrefixDt::usage = "SetPrefixDt[prefix] sets the prefix used for time derivatives of variables.";

ParseComponent::usage = "ParseComponent[varinfo, compindexlist, coordinate, extrareplacerules] processes a single tensor component for setting or printing.";

Is3DAbstractIndex::usage = "Is3DAbstractIndex[index] returns True if the abstract index represents a 3D spatial index (first letter >= 'i').";

Begin["`Private`"];

(* Function *)

GetMapComponentToVarlist[] := $CurrentContext["MapComponentToVarlist"];

Protect[GetMapComponentToVarlist];

SetMapComponentToVarlist[map_] :=
  Module[{},
    If[!ValidateContextModeValue["MapComponentToVarlist", map],
      Throw @ Message[SetMapComponentToVarlist::EInvalidValue, map]
    ];
    $CurrentContext = Append[$CurrentContext, "MapComponentToVarlist" -> map]
  ];

SetMapComponentToVarlist::EInvalidValue = "Invalid MapComponentToVarlist value: `1`. Expected an Association.";

Protect[SetMapComponentToVarlist];

GetProcessNewVarlist[] := $CurrentContext["ProcessNewVarlist"];

Protect[GetProcessNewVarlist];

SetProcessNewVarlist[isnew_] :=
  Module[{},
    If[!ValidateContextModeValue["ProcessNewVarlist", isnew],
      Throw @ Message[SetProcessNewVarlist::EInvalidValue, isnew]
    ];
    $CurrentContext = Append[$CurrentContext, "ProcessNewVarlist" -> isnew]
  ];

SetProcessNewVarlist::EInvalidValue = "Invalid ProcessNewVarlist value: `1`. Expected True or False.";

Protect[SetProcessNewVarlist];

GetSimplifyEquation[] := $CurrentContext["SimplifyEquation"];

Protect[GetSimplifyEquation];

SetSimplifyEquation[simplify_] :=
  Module[{},
    If[!ValidateContextModeValue["SimplifyEquation", simplify],
      Throw @ Message[SetSimplifyEquation::EInvalidValue, simplify]
    ];
    $CurrentContext = Append[$CurrentContext, "SimplifyEquation" -> simplify]
  ];

SetSimplifyEquation::EInvalidValue = "Invalid SimplifyEquation value: `1`. Expected True or False.";

Protect[SetSimplifyEquation];

GetUseLetterForTensorComponent[] := $CurrentContext["UseLetterForTensorComponent"];

Protect[GetUseLetterForTensorComponent];

SetUseLetterForTensorComponent[useletter_] :=
  Module[{},
    If[!ValidateContextModeValue["UseLetterForTensorComponent", useletter],
      Throw @ Message[SetUseLetterForTensorComponent::EInvalidValue, useletter]
    ];
    $CurrentContext = Append[$CurrentContext, "UseLetterForTensorComponent" -> useletter]
  ];

SetUseLetterForTensorComponent::EInvalidValue = "Invalid UseLetterForTensorComponent value: `1`. Expected True or False.";

Protect[SetUseLetterForTensorComponent];

GetTempVariableType[] := $CurrentContext["TempVariableType"];

Protect[GetTempVariableType];

SetTempVariableType[type_] :=
  Module[{},
    If[!ValidateContextModeValue["TempVariableType", type],
      Throw @ Message[SetTempVariableType::EInvalidValue, type]
    ];
    $CurrentContext = Append[$CurrentContext, "TempVariableType" -> type]
  ];

SetTempVariableType::EInvalidValue = "Invalid TempVariableType value: `1`. Expected a String.";

Protect[SetTempVariableType];

GetInterfaceWithNonCoordBasis[] := $CurrentContext["InterfaceWithNonCoordBasis"];

Protect[GetInterfaceWithNonCoordBasis];

SetInterfaceWithNonCoordBasis[noncoordbasis_] :=
  Module[{},
    If[!ValidateContextModeValue["InterfaceWithNonCoordBasis", noncoordbasis],
      Throw @ Message[SetInterfaceWithNonCoordBasis::EInvalidValue, noncoordbasis]
    ];
    $CurrentContext = Append[$CurrentContext, "InterfaceWithNonCoordBasis" -> noncoordbasis]
  ];

SetInterfaceWithNonCoordBasis::EInvalidValue = "Invalid InterfaceWithNonCoordBasis value: `1`. Expected True or False.";

Protect[SetInterfaceWithNonCoordBasis];

GetSuffixName[] := $CurrentContext["SuffixName"];

Protect[GetSuffixName];

SetSuffixName[suffix_] :=
  Module[{},
    If[!ValidateContextModeValue["SuffixName", suffix],
      Throw @ Message[SetSuffixName::EInvalidValue, suffix]
    ];
    $CurrentContext = Append[$CurrentContext, "SuffixName" -> suffix]
  ];

SetSuffixName::EInvalidValue = "Invalid SuffixName value: `1`. Expected a String.";

Protect[SetSuffixName];

GetPrefixDt[] := $CurrentContext["PrefixDt"];

Protect[GetPrefixDt];

SetPrefixDt[prefix_] :=
  Module[{},
    If[!ValidateContextModeValue["PrefixDt", prefix],
      Throw @ Message[SetPrefixDt::EInvalidValue, prefix]
    ];
    $CurrentContext = Append[$CurrentContext, "PrefixDt" -> prefix]
  ];

SetPrefixDt::EInvalidValue = "Invalid PrefixDt value: `1`. Expected a String.";

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
      InSetCompPhase[],
        SetComponent[compname, SetExprName[varname, compindexlist, coordinate]]
      ,
      InPrintCompPhase[],
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
      InInitializationsMode[],
        PrintVerbose["    PrintComponentInitialization ", compname, "..."];
        Global`PrintComponentInitialization[varinfo, compname]
      ,
      InEquationsMode[],
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
    If[Length[mapCtoV] == 0 || GetProcessNewVarlist[] || (GetIndependentVarlistIndex[] && (compname[[0]] =!= Last[Keys[mapCtoV]][[0]])),
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
            If[GetUseLetterForTensorComponent[],
              colist[[compindexlist[[icomp]] + 1]]
              ,
              ToString @ compindexlist[[icomp]]
            ]
        ,
        {icomp, 1, Length[compindexlist]}
      ]
    ];
    exprname =
      If[GetWithoutGridPointIndex[] || (GetInterfaceWithNonCoordBasis[] && coordinate === GetDefaultChart[]),
        ToExpression[exprname]
        ,
        If[GetUseTilePointIndex[],
          ToExpression[exprname <> GetTilePointIndex[]]
          ,
          ToExpression[exprname <> GetGridPointIndex[]]
        ]
      ];
    Return[exprname]
  ];

Is3DAbstractIndex[idx_] :=
  Module[{},
    LetterNumber[StringPart[ToString[idx /. {Times[-1, x_] :> x}], 1]] >= LetterNumber["i"]
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
