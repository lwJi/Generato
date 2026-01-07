(* ::Package:: *)

(* VarlistTests.wl *)
(* Unit tests for Generato`Varlist module *)
(* Tests: ParseVar, DefineTensor *)

If[Environment["QUIET"] =!= "1", Print["Loading VarlistTests.wl..."]];

(* Load Generato *)
Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"}]];

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
(* Test: ParseVar - Basic Cases *)
(* ========================================= *)

VerificationTest[
  (* ParseVar with vector and PrintAs *)
  {varname, symmetry, printname} = ParseVar[{varTestVec[i], PrintAs -> "vtv"}];
  varname === varTestVec[i],
  True,
  TestID -> "ParseVar-VectorVarname"
];

VerificationTest[
  (* ParseVar extracts PrintAs value *)
  {varname, symmetry, printname} = ParseVar[{varTestVec2[i], PrintAs -> "vtv2"}];
  printname,
  "vtv2",
  TestID -> "ParseVar-PrintAs"
];

VerificationTest[
  (* ParseVar with no symmetry returns Null *)
  {varname, symmetry, printname} = ParseVar[{varTestVec3[i], PrintAs -> "vtv3"}];
  symmetry,
  Null,
  TestID -> "ParseVar-NoSymmetry"
];

(* ========================================= *)
(* Test: ParseVar - Symmetric Tensors *)
(* ========================================= *)

VerificationTest[
  (* ParseVar with Symmetric 2-tensor *)
  {varname, symmetry, printname} = ParseVar[{varTestSym[-i, -j], Symmetric[{-i, -j}], PrintAs -> "vts"}];
  Head[symmetry] === Symmetric,
  True,
  TestID -> "ParseVar-SymmetricHead"
];

VerificationTest[
  (* ParseVar extracts symmetric indices *)
  {varname, symmetry, printname} = ParseVar[{varTestSym2[-i, -j], Symmetric[{-i, -j}], PrintAs -> "vts2"}];
  symmetry[[1]],
  {-i, -j},
  TestID -> "ParseVar-SymmetricIndices"
];

(* ========================================= *)
(* Test: ParseVar - Antisymmetric Tensors *)
(* ========================================= *)

VerificationTest[
  (* ParseVar with Antisymmetric 2-tensor *)
  {varname, symmetry, printname} = ParseVar[{varTestAnti[-i, -j], Antisymmetric[{-i, -j}], PrintAs -> "vta"}];
  Head[symmetry] === Antisymmetric,
  True,
  TestID -> "ParseVar-AntisymmetricHead"
];

(* ========================================= *)
(* Test: ParseVar - Scalar *)
(* ========================================= *)

VerificationTest[
  (* ParseVar with scalar (no indices) *)
  {varname, symmetry, printname} = ParseVar[{varTestScalar[], PrintAs -> "vsc"}];
  Length[varname],
  0,
  TestID -> "ParseVar-ScalarLength"
];

(* ========================================= *)
(* Test: ParseVar - Order Independence *)
(* ========================================= *)

VerificationTest[
  (* ParseVar handles different ordering: PrintAs first *)
  {varname, symmetry, printname} = ParseVar[{PrintAs -> "order1", varTestOrder1[i]}];
  printname,
  "order1",
  TestID -> "ParseVar-OrderIndependence-PrintAsFirst"
];

VerificationTest[
  (* ParseVar handles different ordering: symmetry in middle *)
  {varname, symmetry, printname} = ParseVar[{varTestOrder2[-i, -j], Symmetric[{-i, -j}], PrintAs -> "order2"}];
  {Head[symmetry], printname},
  {Symmetric, "order2"},
  TestID -> "ParseVar-OrderIndependence-SymmetryMiddle"
];

(* ========================================= *)
(* Test: DefineTensor *)
(* ========================================= *)

VerificationTest[
  (* DefineTensor creates a valid xTensor *)
  DefineTensor[varDefTest1[i], Null, "vdt1"];
  xTensorQ[varDefTest1],
  True,
  TestID -> "DefineTensor-CreatesXTensor"
];

VerificationTest[
  (* DefineTensor with symmetry *)
  DefineTensor[varDefTest2[-i, -j], Symmetric[{-i, -j}], "vdt2"];
  xTensorQ[varDefTest2],
  True,
  TestID -> "DefineTensor-WithSymmetry"
];

VerificationTest[
  (* DefineTensor without PrintAs (empty string) *)
  DefineTensor[varDefTest3[i], Null, ""];
  xTensorQ[varDefTest3],
  True,
  TestID -> "DefineTensor-NoPrintAs"
];

(* ========================================= *)
(* Test: ParseVarlist *)
(* ========================================= *)

VerificationTest[
  (* ParseVarlist should process a list of variable definitions *)
  (* This is a basic smoke test - full functionality tested via GridTensors *)
  varlist = {{parseVarlistTest[i], PrintAs -> "pvt"}};
  SetParseMode[{SetComp -> True, PrintComp -> False}];
  ParseVarlist[varlist, testCart];
  True,
  True,
  TestID -> "ParseVarlist-BasicSmoke"
];

If[Environment["QUIET"] =!= "1", Print["VarlistTests.wl completed."]];
