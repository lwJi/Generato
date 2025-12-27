(* ::Package:: *)

(* BasicTests.wl *)
(* Unit tests for Generato`Basic module *)

Print["Loading BasicTests.wl..."];

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
DefManifold[TestM3, 3, IndexRange[a, z]];
DefChart[testCart, TestM3, {1, 2, 3}, {TX[], TY[], TZ[]}, ChartColor -> Blue];
DefMetric[1, testEuclid[-i, -j], testCD];

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

(* Reset to clean state *)
SetPVerbose[False];
SetGridPointIndex[""];
SetTilePointIndex[""];

Print["BasicTests.wl completed."];
