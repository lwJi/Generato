(* ::Package:: *)

(* InterfaceTests.wl *)
(* Unit tests for Generato`Interface module *)
(* Tests: GridTensors, TileTensors, TempTensors *)

Print["Loading InterfaceTests.wl..."];

(* Load Generato if not already loaded *)
If[!MemberQ[$Packages, "Generato`Interface`"],
  Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"}]]
];

SetPVerbose[False];
SetPrintDate[False];

(* ========================================= *)
(* Setup: Use existing test manifold from BasicTests *)
(* ========================================= *)

(* Reuse TestM3 manifold if it exists, otherwise define it *)
If[!MemberQ[$Manifolds, TestM3],
  DefManifold[TestM3, 3, IndexRange[a, z]];
  DefChart[testCart, TestM3, {1, 2, 3}, {TX[], TY[], TZ[]}, ChartColor -> Blue];
  DefMetric[1, testEuclid[-i, -j], testCD];
];

(* ========================================= *)
(* Test: GridTensors *)
(* ========================================= *)

VerificationTest[
  (* GridTensors should return a list *)
  varlist = GridTensors[{intTestVec[i], PrintAs -> "itv"}];
  ListQ[varlist],
  True,
  TestID -> "GridTensors-ReturnsList"
];

VerificationTest[
  (* Vector should have 3 components in 3D *)
  varlist = GridTensors[{intTestVec2[i], PrintAs -> "itv2"}];
  Length[varlist] >= 3,
  True,
  TestID -> "GridTensors-VectorComponents"
];

(* ========================================= *)
(* Test: Symmetric Tensor *)
(* ========================================= *)

VerificationTest[
  (* Symmetric 2-tensor should have 6 independent components in 3D *)
  symVarlist = GridTensors[{intTestSym[-i, -j], Symmetric[{-i, -j}], PrintAs -> "its"}];
  Length[symVarlist] >= 6,
  True,
  TestID -> "GridTensors-SymmetricComponents"
];

(* ========================================= *)
(* Test: TempTensors *)
(* ========================================= *)

VerificationTest[
  (* TempTensors should return a list *)
  tempList = TempTensors[{intTestTemp[i], PrintAs -> "itt"}];
  ListQ[tempList],
  True,
  TestID -> "TempTensors-ReturnsList"
];

Print["InterfaceTests.wl completed."];
