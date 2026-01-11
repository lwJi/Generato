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

GetMapComponentToVarlist::usage = "GetMapComponentToVarlist[ctx] returns the Association mapping tensor components to varlist indices from context.\nGetMapComponentToVarlist[] returns the mapping from global state.";

SetMapComponentToVarlist::usage = "SetMapComponentToVarlist[ctx, map] returns new context with updated mapping.\nSetMapComponentToVarlist[map] sets the mapping in global state.";

GetProcessNewVarlist::usage = "GetProcessNewVarlist[ctx] returns ProcessNewVarlist flag from context.\nGetProcessNewVarlist[] returns the flag from global state.";

SetProcessNewVarlist::usage = "SetProcessNewVarlist[ctx, bool] returns new context with updated flag.\nSetProcessNewVarlist[bool] sets whether the next varlist is treated as a new varlist for index numbering.";

GetSimplifyEquation::usage = "GetSimplifyEquation[ctx] returns SimplifyEquation flag from context.\nGetSimplifyEquation[] returns True if equations are simplified before output.";

SetSimplifyEquation::usage = "SetSimplifyEquation[ctx, bool] returns new context with updated flag.\nSetSimplifyEquation[bool] enables or disables simplification of equations before output.";

GetUseLetterForTensorComponent::usage = "GetUseLetterForTensorComponent[ctx] returns UseLetterForTensorComponent flag from context.\nGetUseLetterForTensorComponent[] returns True if letters are used for tensor component indices instead of numbers.";

SetUseLetterForTensorComponent::usage = "SetUseLetterForTensorComponent[ctx, bool] returns new context with updated flag.\nSetUseLetterForTensorComponent[bool] sets whether to use letters for tensor component indices instead of numbers.";

GetTempVariableType::usage = "GetTempVariableType[ctx] returns TempVariableType from context.\nGetTempVariableType[] returns the C type string used for temporary variables.";

SetTempVariableType::usage = "SetTempVariableType[ctx, type] returns new context with updated type.\nSetTempVariableType[type] sets the C type string used for temporary variables.";

GetInterfaceWithNonCoordBasis::usage = "GetInterfaceWithNonCoordBasis[ctx] returns InterfaceWithNonCoordBasis flag from context.\nGetInterfaceWithNonCoordBasis[] returns True if interfacing with non-coordinate basis tensors.";

SetInterfaceWithNonCoordBasis::usage = "SetInterfaceWithNonCoordBasis[ctx, bool] returns new context with updated flag.\nSetInterfaceWithNonCoordBasis[bool] enables or disables interfacing with non-coordinate basis tensors.";

GetSuffixName::usage = "GetSuffixName[ctx] returns SuffixName from context.\nGetSuffixName[] returns the suffix appended to variable names in the current varlist.";

SetSuffixName::usage = "SetSuffixName[ctx, suffix] returns new context with updated suffix.\nSetSuffixName[suffix] sets the suffix appended to variable names in the current varlist.";

GetPrefixDt::usage = "GetPrefixDt[ctx] returns PrefixDt from context.\nGetPrefixDt[] returns the prefix used for time derivatives of variables.";

SetPrefixDt::usage = "SetPrefixDt[ctx, prefix] returns new context with updated prefix.\nSetPrefixDt[prefix] sets the prefix used for time derivatives of variables.";

ParseComponent::usage = "ParseComponent[varinfo, compindexlist, coordinate, extrareplacerules] processes a single tensor component for setting or printing.";

Is3DAbstractIndex::usage = "Is3DAbstractIndex[index] returns True if the abstract index represents a 3D spatial index (first letter >= 'i').";

Begin["`Private`"];

(* Function *)

(* Context-aware getter *)
GetMapComponentToVarlist[ctx_Association] := GetCtx[ctx, "MapComponentToVarlist"];

(* Global getter - reads from $CurrentContext *)
GetMapComponentToVarlist[] := $CurrentContext["MapComponentToVarlist"];

Protect[GetMapComponentToVarlist];

(* Context-aware setter - returns new context *)
SetMapComponentToVarlist[ctx_Association, map_] :=
  SetCtx[ctx, "MapComponentToVarlist", map];

(* Global setter - writes to $CurrentContext *)
SetMapComponentToVarlist[map_] :=
  Module[{},
    $CurrentContext = SetCtx[$CurrentContext, "MapComponentToVarlist", map]
  ];

Protect[SetMapComponentToVarlist];

(* Context-aware getter *)
GetProcessNewVarlist[ctx_Association] := GetCtx[ctx, "ProcessNewVarlist"];

(* Global getter - reads from $CurrentContext *)
GetProcessNewVarlist[] := $CurrentContext["ProcessNewVarlist"];

Protect[GetProcessNewVarlist];

(* Context-aware setter - returns new context *)
SetProcessNewVarlist[ctx_Association, isnew_] :=
  SetCtx[ctx, "ProcessNewVarlist", isnew];

(* Global setter - writes to $CurrentContext *)
SetProcessNewVarlist[isnew_] :=
  Module[{},
    $CurrentContext = SetCtx[$CurrentContext, "ProcessNewVarlist", isnew]
  ];

Protect[SetProcessNewVarlist];

(* Context-aware getter *)
GetSimplifyEquation[ctx_Association] := GetCtx[ctx, "SimplifyEquation"];

(* Global getter - reads from $CurrentContext *)
GetSimplifyEquation[] := $CurrentContext["SimplifyEquation"];

Protect[GetSimplifyEquation];

(* Context-aware setter - returns new context *)
SetSimplifyEquation[ctx_Association, simplify_] :=
  SetCtx[ctx, "SimplifyEquation", simplify];

(* Global setter - writes to $CurrentContext *)
SetSimplifyEquation[simplify_] :=
  Module[{},
    $CurrentContext = SetCtx[$CurrentContext, "SimplifyEquation", simplify]
  ];

Protect[SetSimplifyEquation];

(* Context-aware getter *)
GetUseLetterForTensorComponent[ctx_Association] := GetCtx[ctx, "UseLetterForTensorComponent"];

(* Global getter - reads from $CurrentContext *)
GetUseLetterForTensorComponent[] := $CurrentContext["UseLetterForTensorComponent"];

Protect[GetUseLetterForTensorComponent];

(* Context-aware setter - returns new context *)
SetUseLetterForTensorComponent[ctx_Association, useletter_] :=
  SetCtx[ctx, "UseLetterForTensorComponent", useletter];

(* Global setter - writes to $CurrentContext *)
SetUseLetterForTensorComponent[useletter_] :=
  Module[{},
    $CurrentContext = SetCtx[$CurrentContext, "UseLetterForTensorComponent", useletter]
  ];

Protect[SetUseLetterForTensorComponent];

(* Context-aware getter *)
GetTempVariableType[ctx_Association] := GetCtx[ctx, "TempVariableType"];

(* Global getter - reads from $CurrentContext *)
GetTempVariableType[] := $CurrentContext["TempVariableType"];

Protect[GetTempVariableType];

(* Context-aware setter - returns new context *)
SetTempVariableType[ctx_Association, type_] :=
  SetCtx[ctx, "TempVariableType", type];

(* Global setter - writes to $CurrentContext *)
SetTempVariableType[type_] :=
  Module[{},
    $CurrentContext = SetCtx[$CurrentContext, "TempVariableType", type]
  ];

Protect[SetTempVariableType];

(* Context-aware getter *)
GetInterfaceWithNonCoordBasis[ctx_Association] := GetCtx[ctx, "InterfaceWithNonCoordBasis"];

(* Global getter - reads from $CurrentContext *)
GetInterfaceWithNonCoordBasis[] := $CurrentContext["InterfaceWithNonCoordBasis"];

Protect[GetInterfaceWithNonCoordBasis];

(* Context-aware setter - returns new context *)
SetInterfaceWithNonCoordBasis[ctx_Association, noncoordbasis_] :=
  SetCtx[ctx, "InterfaceWithNonCoordBasis", noncoordbasis];

(* Global setter - writes to $CurrentContext *)
SetInterfaceWithNonCoordBasis[noncoordbasis_] :=
  Module[{},
    $CurrentContext = SetCtx[$CurrentContext, "InterfaceWithNonCoordBasis", noncoordbasis]
  ];

Protect[SetInterfaceWithNonCoordBasis];

(* Context-aware getter *)
GetSuffixName[ctx_Association] := GetCtx[ctx, "SuffixName"];

(* Global getter - reads from $CurrentContext *)
GetSuffixName[] := $CurrentContext["SuffixName"];

Protect[GetSuffixName];

(* Context-aware setter - returns new context *)
SetSuffixName[ctx_Association, suffix_] :=
  SetCtx[ctx, "SuffixName", suffix];

(* Global setter - writes to $CurrentContext *)
SetSuffixName[suffix_] :=
  Module[{},
    $CurrentContext = SetCtx[$CurrentContext, "SuffixName", suffix]
  ];

Protect[SetSuffixName];

(* Context-aware getter *)
GetPrefixDt[ctx_Association] := GetCtx[ctx, "PrefixDt"];

(* Global getter - reads from $CurrentContext *)
GetPrefixDt[] := $CurrentContext["PrefixDt"];

Protect[GetPrefixDt];

(* Context-aware setter - returns new context *)
SetPrefixDt[ctx_Association, prefix_] :=
  SetCtx[ctx, "PrefixDt", prefix];

(* Global setter - writes to $CurrentContext *)
SetPrefixDt[prefix_] :=
  Module[{},
    $CurrentContext = SetCtx[$CurrentContext, "PrefixDt", prefix]
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
        Global`PrintComponentInitialization[$CurrentContext, varinfo, compname]
      ,
      InEquationsMode[],
        PrintVerbose["    PrintComponentEquation ", compname, "..."];
        Global`PrintComponentEquation[$CurrentContext, coordinate, compname, extrareplacerules]
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
