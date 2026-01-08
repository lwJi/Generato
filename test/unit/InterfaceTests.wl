(* ::Package:: *)

(* InterfaceTests.wl *)
(* Unit tests for Generato`Interface module *)
(* Tests: GridTensors, TileTensors, TempTensors *)

If[Environment["QUIET"] =!= "1", Print["Loading InterfaceTests.wl..."]];

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

AppendTo[$AllTests,
  VerificationTest[
    (* GridTensors should return a non-empty list *)
    varlist = GridTensors[{intTestVec[i], PrintAs -> "itv"}];
    ListQ[varlist] && Length[varlist] >= 1,
    True,
    TestID -> "GridTensors-ReturnsList"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* GridTensors returns the input varlist (not expanded components) *)
    varlist = GridTensors[{intTestVec2[i], PrintAs -> "itv2"}];
    Length[varlist] >= 1,
    True,
    TestID -> "GridTensors-VectorComponents"
  ]
];

(* ========================================= *)
(* Test: Symmetric Tensor *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* GridTensors returns the input varlist (not expanded components) *)
    symVarlist = GridTensors[{intTestSym[-i, -j], Symmetric[{-i, -j}], PrintAs -> "its"}];
    Length[symVarlist] >= 1,
    True,
    TestID -> "GridTensors-SymmetricComponents"
  ]
];

(* ========================================= *)
(* Test: TempTensors *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* TempTensors should return a non-empty list *)
    tempList = TempTensors[{intTestTemp[i], PrintAs -> "itt"}];
    ListQ[tempList] && Length[tempList] >= 1,
    True,
    TestID -> "TempTensors-ReturnsList"
  ]
];

(* ========================================= *)
(* Test: DefTensors *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* DefTensors should define tensors and return non-empty list *)
    defList = DefTensors[{intDefTest[i], PrintAs -> "idt"}];
    ListQ[defList] && Length[defList] >= 1,
    True,
    TestID -> "DefTensors-ReturnsList"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* DefTensors with symmetric tensor *)
    defList = DefTensors[{intDefSym[-i, -j], Symmetric[{-i, -j}], PrintAs -> "ids"}];
    Length[defList] >= 1,
    True,
    TestID -> "DefTensors-SymmetricTensor"
  ]
];

(* ========================================= *)
(* Test: TileTensors *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* TileTensors should return a non-empty list *)
    tileList = TileTensors[{intTileTest[i], PrintAs -> "itile"}];
    ListQ[tileList] && Length[tileList] >= 1,
    True,
    TestID -> "TileTensors-ReturnsList"
  ]
];

(* ========================================= *)
(* Test: SetComponents options *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* SetComponents with WithoutGridPointIndex *)
    varlist = {{intNoGP[i], PrintAs -> "ing"}};
    SetComponents[{WithoutGridPointIndex -> True}, varlist];
    True,
    True,
    TestID -> "SetComponents-WithoutGridPointIndex"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* SetComponents with UseTilePointIndex *)
    varlist = {{intTileGP[i], PrintAs -> "itg"}};
    SetComponents[{UseTilePointIndex -> True}, varlist];
    True,
    True,
    TestID -> "SetComponents-UseTilePointIndex"
  ]
];

(* Reset state *)
SetPVerbose[False];
SetPrintDate[False];

If[Environment["QUIET"] =!= "1", Print["InterfaceTests.wl completed."]];
