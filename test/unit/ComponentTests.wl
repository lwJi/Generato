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

(* Reset state *)
SetSimplifyEquation[True];
SetUseLetterForTensorComponent[False];
SetTempVariableType["double"];
SetInterfaceWithNonCoordBasis[False];
SetSuffixName[""];

If[Environment["QUIET"] =!= "1", Print["ComponentTests.wl completed."]];
