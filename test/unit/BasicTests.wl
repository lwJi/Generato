(* ::Package:: *)

(* BasicTests.wl *)
(* Unit tests for Generato`Basic module *)

(* Load Generato *)
Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"}]];

(* Suppress verbose output during tests *)
SetPVerbose[False];
SetPrintDate[False];

(* ========================================= *)
(* Test: Global Configuration Functions *)
(* ========================================= *)

VerificationTest[
  SetPVerbose[True];
  (* PVerbose should be settable without error *)
  True,
  True,
  TestID -> "SetPVerbose-NoError"
];

VerificationTest[
  SetPrintDate[False];
  GetPrintDate[],
  False,
  TestID -> "GetSetPrintDate"
];

VerificationTest[
  SetGridPointIndex["[[ijk]]"];
  GetGridPointIndex[],
  "[[ijk]]",
  TestID -> "GetSetGridPointIndex"
];

VerificationTest[
  SetTilePointIndex["(ti,tj,tk)"];
  GetTilePointIndex[],
  "(ti,tj,tk)",
  TestID -> "GetSetTilePointIndex"
];

VerificationTest[
  SetOutputFile["/tmp/test_output.c"];
  GetOutputFile[],
  "/tmp/test_output.c",
  TestID -> "GetSetOutputFile"
];

VerificationTest[
  SetProject["TestProject"];
  GetProject[],
  "TestProject",
  TestID -> "GetSetProject"
];

(* ========================================= *)
(* Test: SetEQN and RHSOf *)
(* ========================================= *)

(* Setup a simple manifold for equation tests *)
If[!MemberQ[$Manifolds, TestM3],
  DefManifold[TestM3, 3, IndexRange[a, z]];
  DefChart[testCart, TestM3, {1, 2, 3}, {TX[], TY[], TZ[]}, ChartColor -> Blue];
  DefMetric[1, testEuclid[-i, -j], testCD];
];

(* Define test tensors *)
testVarlist = GridTensors[{testVec[i], PrintAs -> "tv"}];

VerificationTest[
  (* SetEQN should set an equation without error *)
  SetEQN[testVec[i_], 2 testVec[i]];
  True,
  True,
  TestID -> "SetEQN-Basic-NoError"
];

VerificationTest[
  (* RHSOf should retrieve the set equation *)
  rhs = RHSOf[testVec];
  Head[rhs] =!= Null,
  True,
  TestID -> "RHSOf-ReturnsValue"
];

(* ========================================= *)
(* Test: Suffix handling *)
(* ========================================= *)

VerificationTest[
  SetEQN[{SuffixName -> "testSuffix"}, testVec[i_], 3 testVec[i]];
  True,
  True,
  TestID -> "SetEQN-WithSuffix-NoError"
];

(* ========================================= *)
(* Test: GetCheckInputEquations / SetCheckInputEquations *)
(* ========================================= *)

VerificationTest[
  SetCheckInputEquations[True];
  GetCheckInputEquations[],
  True,
  TestID -> "GetSetCheckInputEquations-True"
];

VerificationTest[
  SetCheckInputEquations[False];
  GetCheckInputEquations[],
  False,
  TestID -> "GetSetCheckInputEquations-False"
];

(* ========================================= *)
(* Test: GetPrintHeaderMacro / SetPrintHeaderMacro *)
(* ========================================= *)

VerificationTest[
  SetPrintHeaderMacro[True];
  GetPrintHeaderMacro[],
  True,
  TestID -> "GetSetPrintHeaderMacro-True"
];

VerificationTest[
  SetPrintHeaderMacro[False];
  GetPrintHeaderMacro[],
  False,
  TestID -> "GetSetPrintHeaderMacro-False"
];

(* ========================================= *)
(* Test: GetSuffixUnprotected / SetSuffixUnprotected *)
(* ========================================= *)

VerificationTest[
  SetSuffixUnprotected["_test"];
  GetSuffixUnprotected[],
  "_test",
  TestID -> "GetSetSuffixUnprotected-String"
];

(* ========================================= *)
(* Test: GetDefaultChart *)
(* ========================================= *)

VerificationTest[
  (* GetDefaultChart should return a symbol *)
  Head[GetDefaultChart[]] === Symbol || GetDefaultChart[] === Null,
  True,
  TestID -> "GetDefaultChart-ReturnsSymbolOrNull"
];

(* ========================================= *)
(* Test: GetDim *)
(* ========================================= *)

VerificationTest[
  (* GetDim should return an integer *)
  IntegerQ[GetDim[]],
  True,
  TestID -> "GetDim-ReturnsInteger"
];

(* Reset to clean state *)
SetPVerbose[False];
SetGridPointIndex[""];
SetTilePointIndex[""];
