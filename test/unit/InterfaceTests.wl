(* ::Package:: *)

(* InterfaceTests.wl *)
(* Unit tests for Generato`Interface module *)
(* Tests: DefTensors, GridTensors, TileTensors, TempTensors, SetComponents, PrintEquations, PrintInitializations *)

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
(* Test: DefTensors *)
(* ========================================= *)

VerificationTest[
  (* DefTensors should return a list *)
  result = DefTensors[{intDefScalar[], PrintAs -> "ids"}];
  ListQ[result],
  True,
  TestID -> "DefTensors-ReturnsList"
];

VerificationTest[
  (* DefTensors should define tensor without error *)
  DefTensors[{intDefVec[i], PrintAs -> "idv"}];
  True,
  True,
  TestID -> "DefTensors-NoError"
];

VerificationTest[
  (* DefTensors with symmetric tensor *)
  DefTensors[{intDefSym[-i, -j], Symmetric[{-i, -j}], PrintAs -> "idsym"}];
  True,
  True,
  TestID -> "DefTensors-Symmetric-NoError"
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

VerificationTest[
  (* Scalar tensor *)
  scalarVarlist = GridTensors[{intTestScalar[], PrintAs -> "its"}];
  Length[scalarVarlist] >= 1,
  True,
  TestID -> "GridTensors-Scalar"
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
(* Test: TileTensors *)
(* ========================================= *)

VerificationTest[
  (* TileTensors should return a list *)
  tileList = TileTensors[{intTestTile[i], PrintAs -> "itt"}];
  ListQ[tileList],
  True,
  TestID -> "TileTensors-ReturnsList"
];

VerificationTest[
  (* TileTensors vector should have 3 components *)
  tileList = TileTensors[{intTestTile2[i], PrintAs -> "itt2"}];
  Length[tileList] >= 3,
  True,
  TestID -> "TileTensors-VectorComponents"
];

VerificationTest[
  (* TileTensors with symmetric tensor *)
  tileSym = TileTensors[{intTestTileSym[-i, -j], Symmetric[{-i, -j}], PrintAs -> "itts"}];
  Length[tileSym] >= 6,
  True,
  TestID -> "TileTensors-SymmetricComponents"
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

VerificationTest[
  (* TempTensors vector should have 3 components *)
  tempList = TempTensors[{intTestTemp2[i], PrintAs -> "itt2"}];
  Length[tempList] >= 3,
  True,
  TestID -> "TempTensors-VectorComponents"
];

VerificationTest[
  (* TempTensors with scalar *)
  tempScalar = TempTensors[{intTestTempScalar[], PrintAs -> "itts"}];
  Length[tempScalar] >= 1,
  True,
  TestID -> "TempTensors-Scalar"
];

(* ========================================= *)
(* Test: SetComponents with options *)
(* ========================================= *)

VerificationTest[
  (* SetComponents should work without options *)
  testVarlist = {{intSetCompVec[i], PrintAs -> "iscv"}};
  SetComponents[testVarlist];
  True,
  True,
  TestID -> "SetComponents-NoOptions"
];

VerificationTest[
  (* SetComponents with WithoutGridPointIndex *)
  testVarlist2 = {{intSetCompVec2[i], PrintAs -> "iscv2"}};
  SetComponents[{WithoutGridPointIndex -> True}, testVarlist2];
  True,
  True,
  TestID -> "SetComponents-WithoutGridPointIndex"
];

VerificationTest[
  (* SetComponents with UseTilePointIndex *)
  testVarlist3 = {{intSetCompVec3[i], PrintAs -> "iscv3"}};
  SetComponents[{UseTilePointIndex -> True}, testVarlist3];
  True,
  True,
  TestID -> "SetComponents-UseTilePointIndex"
];

VerificationTest[
  (* SetComponents with IndependentIndexForEachVar *)
  testVarlist4 = {{intSetCompVec4[i], PrintAs -> "iscv4"}};
  SetComponents[{IndependentIndexForEachVar -> False}, testVarlist4];
  True,
  True,
  TestID -> "SetComponents-IndependentIndexForEachVar"
];

(* ========================================= *)
(* Test: PrintEquations modes *)
(* ========================================= *)

(* Setup test tensors and equations for PrintEquations tests *)
peqVarlist = GridTensors[{intPEQVec[i], PrintAs -> "peqv"}];
SetEQN[intPEQVec[i_], 2 intPEQVec[i]];

VerificationTest[
  (* PrintEquations with Mode -> "Main" should not error *)
  PrintEquations[{Mode -> "Main"}, peqVarlist];
  True,
  True,
  TestID -> "PrintEquations-ModeMain"
];

VerificationTest[
  (* PrintEquations with Mode -> "Temp" should not error *)
  peqVarlist2 = TempTensors[{intPEQTemp[i], PrintAs -> "peqt"}];
  SetEQN[intPEQTemp[i_], intPEQVec[i]];
  PrintEquations[{Mode -> "Temp"}, peqVarlist2];
  True,
  True,
  TestID -> "PrintEquations-ModeTemp"
];

VerificationTest[
  (* PrintEquations with Mode -> "AddToMain" should not error *)
  peqVarlist3 = GridTensors[{intPEQVec3[i], PrintAs -> "peqv3"}];
  SetEQN[intPEQVec3[i_], 3 intPEQVec[i]];
  PrintEquations[{Mode -> "AddToMain"}, peqVarlist3];
  True,
  True,
  TestID -> "PrintEquations-ModeAddToMain"
];

VerificationTest[
  (* PrintEquations with SuffixName option *)
  peqVarlist4 = GridTensors[{intPEQVec4[i], PrintAs -> "peqv4"}];
  SetEQN[{SuffixName -> "testSuffix"}, intPEQVec4[i_], intPEQVec[i]];
  PrintEquations[{SuffixName -> "testSuffix", Mode -> "Main"}, peqVarlist4];
  True,
  True,
  TestID -> "PrintEquations-WithSuffixName"
];

(* ========================================= *)
(* Test: PrintInitializations modes *)
(* ========================================= *)

pinitVarlist = GridTensors[{intPInitVec[i], PrintAs -> "pinv"}];

VerificationTest[
  (* PrintInitializations with Mode -> "Temp" *)
  PrintInitializations[{Mode -> "Temp"}, pinitVarlist];
  True,
  True,
  TestID -> "PrintInitializations-ModeTemp"
];

VerificationTest[
  (* PrintInitializations with Mode -> "MainOut" *)
  PrintInitializations[{Mode -> "MainOut"}, pinitVarlist];
  True,
  True,
  TestID -> "PrintInitializations-ModeMainOut"
];

VerificationTest[
  (* PrintInitializations with Mode -> "MainIn" *)
  PrintInitializations[{Mode -> "MainIn"}, pinitVarlist];
  True,
  True,
  TestID -> "PrintInitializations-ModeMainIn"
];

VerificationTest[
  (* PrintInitializations with Mode -> "MoreInOut" *)
  PrintInitializations[{Mode -> "MoreInOut"}, pinitVarlist];
  True,
  True,
  TestID -> "PrintInitializations-ModeMoreInOut"
];

(* ========================================= *)
(* Test: PrintInitializations TensorType options *)
(* ========================================= *)

VerificationTest[
  (* PrintInitializations with TensorType -> "Scal" *)
  pinitScalar = GridTensors[{intPInitScal[], PrintAs -> "pins"}];
  PrintInitializations[{Mode -> "Temp", TensorType -> "Scal"}, pinitScalar];
  True,
  True,
  TestID -> "PrintInitializations-TensorTypeScal"
];

VerificationTest[
  (* PrintInitializations with TensorType -> "Vect" *)
  PrintInitializations[{Mode -> "Temp", TensorType -> "Vect"}, pinitVarlist];
  True,
  True,
  TestID -> "PrintInitializations-TensorTypeVect"
];

VerificationTest[
  (* PrintInitializations with TensorType -> "Smat" *)
  pinitSym = GridTensors[{intPInitSym[-i, -j], Symmetric[{-i, -j}], PrintAs -> "pinsym"}];
  PrintInitializations[{Mode -> "Temp", TensorType -> "Smat"}, pinitSym];
  True,
  True,
  TestID -> "PrintInitializations-TensorTypeSmat"
];

(* ========================================= *)
(* Test: PrintInitializations StorageType options *)
(* ========================================= *)

VerificationTest[
  (* PrintInitializations with StorageType -> "GF" *)
  PrintInitializations[{Mode -> "Temp", StorageType -> "GF"}, pinitVarlist];
  True,
  True,
  TestID -> "PrintInitializations-StorageTypeGF"
];

VerificationTest[
  (* PrintInitializations with StorageType -> "Tile" *)
  pinitTile = TileTensors[{intPInitTileVec[i], PrintAs -> "pintv"}];
  PrintInitializations[{Mode -> "Temp", StorageType -> "Tile"}, pinitTile];
  True,
  True,
  TestID -> "PrintInitializations-StorageTypeTile"
];

(* ========================================= *)
(* Test: PrintInitializations Derivs mode *)
(* ========================================= *)

VerificationTest[
  (* PrintInitializations with Mode -> "Derivs" and order/accuracy options *)
  PrintInitializations[{Mode -> "Derivs", DerivsOrder -> 1, AccuracyOrder -> 4}, pinitVarlist];
  True,
  True,
  TestID -> "PrintInitializations-ModeDerivsOrder1"
];

VerificationTest[
  (* PrintInitializations with Mode -> "Derivs" and second order *)
  PrintInitializations[{Mode -> "Derivs", DerivsOrder -> 2, AccuracyOrder -> 4}, pinitVarlist];
  True,
  True,
  TestID -> "PrintInitializations-ModeDerivsOrder2"
];

Print["InterfaceTests.wl completed."];
