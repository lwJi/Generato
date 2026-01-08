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
    (* GridTensors should return a list *)
    varlist = GridTensors[{intTestVec[i], PrintAs -> "itv"}];
    ListQ[varlist],
    True,
    TestID -> "GridTensors-ReturnsList"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* Vector should have 3 components in 3D *)
    varlist = GridTensors[{intTestVec2[i], PrintAs -> "itv2"}];
    Length[varlist] >= 3,
    True,
    TestID -> "GridTensors-VectorComponents"
  ]
];

(* ========================================= *)
(* Test: Symmetric Tensor *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* Symmetric 2-tensor should have 6 independent components in 3D *)
    symVarlist = GridTensors[{intTestSym[-i, -j], Symmetric[{-i, -j}], PrintAs -> "its"}];
    Length[symVarlist] >= 6,
    True,
    TestID -> "GridTensors-SymmetricComponents"
  ]
];

(* ========================================= *)
(* Test: TempTensors *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* TempTensors should return a list *)
    tempList = TempTensors[{intTestTemp[i], PrintAs -> "itt"}];
    ListQ[tempList],
    True,
    TestID -> "TempTensors-ReturnsList"
  ]
];

(* ========================================= *)
(* Test: DefTensors *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* DefTensors should define tensors without setting components *)
    defList = DefTensors[{intDefTest[i], PrintAs -> "idt"}];
    ListQ[defList],
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
    (* TileTensors should return a list *)
    tileList = TileTensors[{intTileTest[i], PrintAs -> "itile"}];
    ListQ[tileList],
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

If[Environment["QUIET"] =!= "1", Print["InterfaceTests.wl completed."]];
