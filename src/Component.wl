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

ParseComponent::usage = "ParseComponent[varinfo, compindexlist, coordinate, extrareplacerules] processes a single tensor component for setting or printing.";

Is3DAbstractIndex::usage = "Is3DAbstractIndex[index] returns True if the abstract index represents a 3D spatial index (first letter >= 'i').";

Begin["`Private`"];

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
    compname: component expr in Mathematica kernel, say Pi[{1,-cart},{2,-cart}]
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
