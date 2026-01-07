(* ::Package:: *)

(* ComponentTests.wl *)
(* Unit tests for Generato`Component module *)

(* Load Generato *)
Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"}]];

SetPVerbose[False];
SetPrintDate[False];

(* ========================================= *)
(* Test: Getter/Setter Functions *)
(* ========================================= *)

VerificationTest[
  SetSimplifyEquation[True];
  GetSimplifyEquation[],
  True,
  TestID -> "GetSetSimplifyEquation-True"
];

VerificationTest[
  SetSimplifyEquation[False];
  GetSimplifyEquation[],
  False,
  TestID -> "GetSetSimplifyEquation-False"
];

VerificationTest[
  SetUseLetterForTensorComponent[True];
  GetUseLetterForTensorComponent[],
  True,
  TestID -> "GetSetUseLetterForTensorComponent-True"
];

VerificationTest[
  SetUseLetterForTensorComponent[False];
  GetUseLetterForTensorComponent[],
  False,
  TestID -> "GetSetUseLetterForTensorComponent-False"
];

VerificationTest[
  SetTempVariableType["CCTK_REAL"];
  GetTempVariableType[],
  "CCTK_REAL",
  TestID -> "GetSetTempVariableType"
];

VerificationTest[
  SetInterfaceWithNonCoordBasis[True];
  GetInterfaceWithNonCoordBasis[],
  True,
  TestID -> "GetSetInterfaceWithNonCoordBasis"
];

VerificationTest[
  SetSuffixName["_rhs"];
  GetSuffixName[],
  "_rhs",
  TestID -> "GetSetSuffixName"
];

VerificationTest[
  SetSuffixName[""];
  GetSuffixName[],
  "",
  TestID -> "GetSetSuffixName-Empty"
];

(* ========================================= *)
(* Test: Is3DAbstractIndex *)
(* ========================================= *)

VerificationTest[
  (* Index 'i' should be 3D (letter >= 'i') *)
  Is3DAbstractIndex[i],
  True,
  TestID -> "Is3DAbstractIndex-i"
];

VerificationTest[
  (* Index 'j' should be 3D *)
  Is3DAbstractIndex[j],
  True,
  TestID -> "Is3DAbstractIndex-j"
];

VerificationTest[
  (* Index 'a' should NOT be 3D (letter < 'i') *)
  Is3DAbstractIndex[a],
  False,
  TestID -> "Is3DAbstractIndex-a"
];

VerificationTest[
  (* Index 'h' should NOT be 3D (letter < 'i') *)
  Is3DAbstractIndex[h],
  False,
  TestID -> "Is3DAbstractIndex-h"
];

VerificationTest[
  (* Negative index -i should be 3D *)
  Is3DAbstractIndex[-i],
  True,
  TestID -> "Is3DAbstractIndex-NegativeI"
];

VerificationTest[
  (* Index 'i1' should be 3D (first letter is 'i') *)
  Is3DAbstractIndex[i1],
  True,
  TestID -> "Is3DAbstractIndex-i1"
];

(* ========================================= *)
(* Test: MapComponentToVarlist *)
(* ========================================= *)

VerificationTest[
  (* GetMapComponentToVarlist should return an Association *)
  AssociationQ[GetMapComponentToVarlist[]],
  True,
  TestID -> "GetMapComponentToVarlist-ReturnsAssociation"
];

(* ========================================= *)
(* Test: GetPrefixDt *)
(* ========================================= *)

VerificationTest[
  (* GetPrefixDt should return a string *)
  StringQ[GetPrefixDt[]],
  True,
  TestID -> "GetPrefixDt-ReturnsString"
];

(* Reset state *)
SetSimplifyEquation[True];
SetUseLetterForTensorComponent[False];
SetTempVariableType["double"];
SetInterfaceWithNonCoordBasis[False];
SetSuffixName[""];
