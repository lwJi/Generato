(* ::Package:: *)

(* (c) Liwei Ji, 01/2024 *)

BeginPackage["Generato`Component`"];

Needs["Generato`Basic`"];

Print["------------------------------------------------------------"];

Print["Package Generato`Component`, {2024, 1, 11}"];

Print["------------------------------------------------------------"];

GetParseMode::usage = "GetParseMode[key] returns the mode correspond to the key";

SetParseMode::usage = "SetParseMode[key] add/update the mode correspond to the key";

SetParseModeAllToFalse::usage = "SetParseModeAllToFalse[] set all the modes to false";

SetComp::usage = "ParseMode option.";

Protect[SetComp];

SetCompIndep::usage = "ParseMode option.";

SetCompNoGPIndex::usage = "ParseMode option.";

PrintComp::usage = "ParseMode option.";

Protect[PrintComp];

PrintCompInit::usage = "ParseMode option.";

PrintCompInitMainOut::usage = "ParseMode option.";

PrintCompInitMainIn::usage = "ParseMode option.";

PrintCompInitMoreInOut::usage = "ParseMode option.";

PrintCompInitTemp::usage = "ParseMode option.";

PrintCompInitGF3D2::usage = "ParseMode option.";

PrintCompInitGF3D5::usage = "ParseMode option.";

PrintCompInitVecGF3D2::usage = "ParseMode option.";

PrintCompInitVecGF3D5::usage = "ParseMode option.";

PrintCompInitSmatGF3D2::usage = "ParseMode option.";

PrintCompInitSmatGF3D5::usage = "ParseMode option.";

PrintCompEQN::usage = "ParseMode option.";

PrintCompEQNNewVar::usage = "ParseMode option.";

PrintCompEQNMain::usage = "ParseMode option.";

PrintCompEQNMainCarpetX::usage = "ParseMode option.";

PrintCompEQNAddToMain::usage = "ParseMode option.";

GetMapComponentToVarlist::usage = "GetMapComponentToVarlist[] returns the map between tensor components and varlist indexes.";

SetProcessNewVarlist::usage = "SetProcessNewVarlist[True] update the Boolean variable specifying if we are processing a new varlist.";

SetSimplifyEquation::usage = "SetSimplifyEquation[True] update the Boolean variable specifying if Simplify the equations.";

SetTempVariableType::usage = "SetTempVariableType[CCTK_REAL] update the type of temporary variable.";

GetSuffixName::usage = "GetSuffixName[] returns the suffix added to vars in the current list.";

SetSuffixName::usage = "SetSuffixName[suffix] update the suffix added to vars in the current list.";

GetPrefixDt::usage = "GetPrefixDt[] return the prefix used for dt of vars.";

ParseComponent::usage = "ParseComponent[varname, compindexlist, coordinate] process a component.";

Is3DAbstractIndex::usage = "Is3DAbstractIndex[abstractindex] return True if the abstract index is after letter 'i' and only the first letter matters (say h <=> h1 <=> h2 ...).";

Begin["`Private`"];

(* Data *)

$ParseModeAssociation = <||>;

$MapComponentToVarlist = <||>;(*store all varlist's map*)

$ProcessNewVarlist = True;

$SimplifyEquation = True;

$TempVariableType = "double";

$SuffixName = "";

$PrefixDt = "dt";

(* Function *)

GetParseMode[key_] :=
  Return[$ParseModeAssociation[key]];

Protect[GetParseMode];

SetParseMode[assoc_] :=
  Module[{},
    AppendTo[$ParseModeAssociation, assoc]
  ];

Protect[SetParseMode];

SetParseModeAllToFalse[] :=
  Module[{},
    AppendTo[$ParseModeAssociation, <|SetComp -> False, PrintComp -> False,
       SetCompIndep -> False, SetCompNoGPIndex -> False, PrintCompInit -> False,
       PrintCompInitMainOut -> False, PrintCompInitMainIn -> False, PrintCompInitMoreInOut
       -> False, PrintCompInitTemp -> False, PrintCompInitGF3D2 -> False, PrintCompInitGF3D5
       -> False, PrintCompInitVecGF3D2 -> False, PrintCompInitVecGF3D5 -> False,
       PrintCompInitSmatGF3D2 -> False, PrintCompInitSmatGF3D5 -> False, PrintCompEQN
       -> False, PrintCompEQNNewVar -> False,
       PrintCompEQNMain -> False,
       PrintCompEQNMainCarpetX -> False,
       PrintCompEQNAddToMain
       -> False|>]
  ];

Protect[SetParseModeAllToFalse];

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

SetSimplifyEquation[simplify_] :=
  Module[{},
    $SimplifyEquation = simplify
  ];

Protect[SetSimplifyEquation];

GetTempVariableType[] :=
  Return[$TempVariableType];

SetTempVariableType[type_] :=
  Module[{},
    $TempVariableType = type
  ];

Protect[SetTempVariableType];

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

ParseComponent[varname_, compindexlist_?ListQ, coordinate_] :=
  Module[{compname = SetCompName[varname, compindexlist, coordinate]},
    PrintVerbose["  ParseComponent..."];
    If[Is4DCompIndexListIn3DTensor[compindexlist, varname],
      If[IsUp4DCompIndexListIn3DTensor[compindexlist, varname],
        ComponentValue[compname, 0]
      ];
      Continue[]
    ];
    Which[
      GetParseMode[SetComp],
        SetComponent[compname, SetExprName[varname, compindexlist]]
      ,
      GetParseMode[PrintComp],
        PrintComponent[coordinate, varname, compname]
      ,
      True,
        Throw @ Message[ParseComponent::EMode]
    ]
  ];

ParseComponent::EMode = "ParseMode unrecognized!";

Protect[ParseComponent];

PrintComponent[coordinate_, varname_, compname_] :=
  Module[{},
    Which[
      GetParseMode[PrintCompInit],
        PrintVerbose["    PrintComponentInitialization ", compname, "..."
          ];
        Global`PrintComponentInitialization[varname, compname]
      ,
      GetParseMode[PrintCompEQN],
        PrintVerbose["    PrintComponentEquation ", compname, "..."];
        PrintComponentEquation[coordinate, compname]
      ,
      True,
        Throw @ Message[PrintComponent::EMode]
    ]
  ];

PrintComponent::EMode = "PrintMode unrecognized!";

PrintComponentEquation[coordinate_, compname_] :=
  Module[{outputfile = GetOutputFile[], compToValue, rhssToValue},
    compToValue = compname // ToValues;
    rhssToValue =
      (compname /. {compname[[0]] -> RHSOf[compname[[0]], GetSuffixName[
        ]]}) //
      DummyToBasis[coordinate] //
      TraceBasisDummy //
      ToValues;
    If[GetSimplifyEquation[],
      rhssToValue = rhssToValue // Simplify
    ];
    Which[
      GetParseMode[PrintCompEQNNewVar],
        Module[{},
          Global`pr[GetTempVariableType[] <> " "];
          PutAppend[CForm[compToValue], outputfile];
          Global`pr["="];
          PutAppend[CForm[rhssToValue], outputfile];
          Global`pr[";\n"]
        ]
      ,
      GetParseMode[PrintCompEQNMain],
        Module[{},
          PutAppend[CForm[compToValue], outputfile];
          Global`pr["="];
          PutAppend[CForm[rhssToValue], outputfile];
          Global`pr[";\n"]
        ]
      ,
      GetParseMode[PrintCompEQNMainCarpetX],
        Module[{},
          Global`pr[ToString[CForm[compToValue]] <>".store(mask, index2, "];
          PutAppend[CForm[rhssToValue], outputfile];
          Global`pr[");\n"]
        ]
      ,
      GetParseMode[PrintCompEQNAddToMain],
        Module[{},
          PutAppend[CForm[compToValue], outputfile];
          Global`pr["+="];
          PutAppend[CForm[rhssToValue], outputfile];
          Global`pr[";\n"]
        ]
      ,
      True,
        Throw @ Message[PrintComponentEquation::EMode]
    ]
  ];

PrintComponentEquation::EMode = "PrintEquationMode unrecognized!";

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
    If[Length[mapCtoV] == 0 || GetProcessNewVarlist[] || (GetParseMode[
      SetCompIndep] && (compname[[0]] =!= Last[Keys[mapCtoV]][[0]])),
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

SetExprName[varname_, compindexlist_] :=
  Module[{exprname = StringTrim[ToString[varname[[0]]], GetSuffixUnprotected[
    ]]},
    If[Length[compindexlist] > 0, (*not scalar, ignore up/down*)
      Do[exprname = exprname <> ToString @ compindexlist[[icomp]], {icomp,
         1, Length[compindexlist]}]
    ];
    exprname =
      If[GetParseMode[SetCompNoGPIndex],
        ToExpression[exprname]
        ,
        ToExpression[exprname <> GetGridPointIndex[]]
      ];
    Return[exprname]
  ];

Is3DAbstractIndex[idx_] :=
  Module[{},
    LetterNumber[StringPart[ToString[idx /. {-1 x_ :> x}], 1]] >= LetterNumber[
      "i"]
  ];

Is4DCompIndexIn3DTensor[idx_, idxcomp_] :=
  Module[{},
    Is3DAbstractIndex[idx] && (idxcomp == 0)
  ];

Is4DCompIndexListIn3DTensor[idxcomplist_, varname_] :=
  Module[{is4Didxcomplist = False},
    If[Length[idxcomplist] > 0,
      Do[
        If[Is4DCompIndexIn3DTensor[varname[[icomp]], idxcomplist[[icomp
          ]]],
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
        If[UpIndexQ[varname[[icomp]]] && Is4DCompIndexIn3DTensor[varname
          [[icomp]], idxcomplist[[icomp]]],
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
        If[DownIndexQ[varname[[icomp]]] && Is4DCompIndexIn3DTensor[varname
          [[icomp]], idxcomplist[[icomp]]],
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
