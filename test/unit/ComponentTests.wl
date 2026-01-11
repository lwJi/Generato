(* ::Package:: *)

(* ComponentTests.wl *)
(* Unit tests for Generato`Component module *)

If[Environment["QUIET"] =!= "1", Print["Loading ComponentTests.wl..."]];

(* Load Generato *)
Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"}]];

SetPVerbose[False];
SetPrintDate[False];

(* ========================================= *)
(* Test: Getter/Setter Functions *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    SetSimplifyEquation[True];
    GetSimplifyEquation[],
    True,
    TestID -> "GetSetSimplifyEquation-True"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    SetSimplifyEquation[False];
    GetSimplifyEquation[],
    False,
    TestID -> "GetSetSimplifyEquation-False"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    SetUseLetterForTensorComponent[True];
    GetUseLetterForTensorComponent[],
    True,
    TestID -> "GetSetUseLetterForTensorComponent-True"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    SetUseLetterForTensorComponent[False];
    GetUseLetterForTensorComponent[],
    False,
    TestID -> "GetSetUseLetterForTensorComponent-False"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    SetTempVariableType["CCTK_REAL"];
    GetTempVariableType[],
    "CCTK_REAL",
    TestID -> "GetSetTempVariableType-ReturnsValue"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    SetInterfaceWithNonCoordBasis[True];
    GetInterfaceWithNonCoordBasis[],
    True,
    TestID -> "GetSetInterfaceWithNonCoordBasis-ReturnsTrue"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    SetSuffixName["_rhs"];
    GetSuffixName[],
    "_rhs",
    TestID -> "GetSetSuffixName-ReturnsValue"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    SetSuffixName[""];
    GetSuffixName[],
    "",
    TestID -> "GetSetSuffixName-ReturnsEmpty"
  ]
];

(* ========================================= *)
(* Test: Is3DAbstractIndex *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* Index 'i' should be 3D (letter >= 'i') *)
    Is3DAbstractIndex[i],
    True,
    TestID -> "Is3DAbstractIndex-i"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* Index 'j' should be 3D *)
    Is3DAbstractIndex[j],
    True,
    TestID -> "Is3DAbstractIndex-j"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* Index 'a' should NOT be 3D (letter < 'i') *)
    Is3DAbstractIndex[a],
    False,
    TestID -> "Is3DAbstractIndex-a"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* Index 'h' should NOT be 3D (letter < 'i') *)
    Is3DAbstractIndex[h],
    False,
    TestID -> "Is3DAbstractIndex-h"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* Negative index -i should be 3D *)
    Is3DAbstractIndex[-i],
    True,
    TestID -> "Is3DAbstractIndex-NegativeI"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* Index 'i1' should be 3D (first letter is 'i') *)
    Is3DAbstractIndex[i1],
    True,
    TestID -> "Is3DAbstractIndex-i1"
  ]
];

(* ========================================= *)
(* Test: MapComponentToVarlist *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* GetMapComponentToVarlist should return an Association *)
    AssociationQ[GetMapComponentToVarlist[]],
    True,
    TestID -> "GetMapComponentToVarlist-ReturnsAssociation"
  ]
];

(* ========================================= *)
(* Test: GetPrefixDt *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* GetPrefixDt should return a string *)
    StringQ[GetPrefixDt[]],
    True,
    TestID -> "GetPrefixDt-ReturnsString"
  ]
];

(* ========================================= *)
(* Test: Context-Aware Getters *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* Context-aware GetMapComponentToVarlist should return default value *)
    Module[{ctx = CreateContext[]},
      GetMapComponentToVarlist[ctx]
    ],
    <||>,
    TestID -> "GetMapComponentToVarlist-Context-ReturnsDefault"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* Context-aware GetProcessNewVarlist should return default value *)
    Module[{ctx = CreateContext[]},
      GetProcessNewVarlist[ctx]
    ],
    True,
    TestID -> "GetProcessNewVarlist-Context-ReturnsDefault"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* Context-aware GetSimplifyEquation should return default value *)
    Module[{ctx = CreateContext[]},
      GetSimplifyEquation[ctx]
    ],
    True,
    TestID -> "GetSimplifyEquation-Context-ReturnsDefault"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* Context-aware GetTempVariableType should return default value *)
    Module[{ctx = CreateContext[]},
      GetTempVariableType[ctx]
    ],
    "double",
    TestID -> "GetTempVariableType-Context-ReturnsDefault"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* Context-aware GetSuffixName should return default value *)
    Module[{ctx = CreateContext[]},
      GetSuffixName[ctx]
    ],
    "",
    TestID -> "GetSuffixName-Context-ReturnsDefault"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* Context-aware GetPrefixDt should return default value *)
    Module[{ctx = CreateContext[]},
      GetPrefixDt[ctx]
    ],
    "dt",
    TestID -> "GetPrefixDt-Context-ReturnsDefault"
  ]
];

(* ========================================= *)
(* Test: Context-Aware Setters Return New Context *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* Context-aware SetSimplifyEquation should return new context *)
    Module[{ctx = CreateContext[], ctx2},
      ctx2 = SetSimplifyEquation[ctx, False];
      {GetSimplifyEquation[ctx], GetSimplifyEquation[ctx2]}
    ],
    {True, False},
    TestID -> "SetSimplifyEquation-Context-ReturnsNewContext"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* Context-aware SetTempVariableType should return new context *)
    Module[{ctx = CreateContext[], ctx2},
      ctx2 = SetTempVariableType[ctx, "CCTK_REAL"];
      {GetTempVariableType[ctx], GetTempVariableType[ctx2]}
    ],
    {"double", "CCTK_REAL"},
    TestID -> "SetTempVariableType-Context-ReturnsNewContext"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* Context-aware SetSuffixName should return new context *)
    Module[{ctx = CreateContext[], ctx2},
      ctx2 = SetSuffixName[ctx, "_rhs"];
      {GetSuffixName[ctx], GetSuffixName[ctx2]}
    ],
    {"", "_rhs"},
    TestID -> "SetSuffixName-Context-ReturnsNewContext"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* Context-aware SetProcessNewVarlist should return new context *)
    Module[{ctx = CreateContext[], ctx2},
      ctx2 = SetProcessNewVarlist[ctx, False];
      {GetProcessNewVarlist[ctx], GetProcessNewVarlist[ctx2]}
    ],
    {True, False},
    TestID -> "SetProcessNewVarlist-Context-ReturnsNewContext"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* Context-aware SetMapComponentToVarlist should return new context *)
    Module[{ctx = CreateContext[], ctx2, testMap = <|"test" -> 0|>},
      ctx2 = SetMapComponentToVarlist[ctx, testMap];
      {GetMapComponentToVarlist[ctx], GetMapComponentToVarlist[ctx2]}
    ],
    {<||>, <|"test" -> 0|>},
    TestID -> "SetMapComponentToVarlist-Context-ReturnsNewContext"
  ]
];

(* Reset state *)
SetSimplifyEquation[True];
SetUseLetterForTensorComponent[False];
SetTempVariableType["double"];
SetInterfaceWithNonCoordBasis[False];
SetSuffixName[""];

If[Environment["QUIET"] =!= "1", Print["ComponentTests.wl completed."]];
