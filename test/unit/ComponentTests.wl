(* ::Package:: *)

(* ComponentTests.wl *)
(* Unit tests for Generato`Component module: the enumeration of components, binding and the records, *)
(* for what a backend reads from the records: the index layout and the variable a relation names, *)
(* and for the print calls, which hand the backend of a write one statement per component *)
(* The store of the records lasts as long as the kernel, like the xAct tensors, so each test uses heads of its own *)

If[Environment["QUIET"] =!= "1", Print["Loading ComponentTests.wl..."]];

(* Load Generato *)
Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"}]];

(* Helper to reset context to defaults *)
ResetContext[] := ($CurrentContext = $ContextDefaults);

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
(* Test: IndependentIndices - counts and order per rank and symmetry *)
(* ========================================= *)

(*
  Each case is checked against the loop nest it replaces: slots iterate in
  order, first slot outermost, and the second slot of a pair starts at its
  partner's value, or one above it.
*)
Scan[
  Function[case,
    With[{id = "IndependentIndices-" <> case[[1]], rank = case[[2]], pairs = case[[3]], count = case[[4]], expected = case[[5]]},
      AppendTo[$AllTests,
        VerificationTest[
          With[{indices = IndependentIndices[rank, pairs, {1, 3}]},
            {Length[indices], indices === expected}
          ],
          {count, True},
          TestID -> id
        ]
      ]
    ]
  ],
  {
    {"Rank0", 0, {}, 1, {{}}},
    {"Rank1", 1, {}, 3, Table[{ia}, {ia, 1, 3}]},
    {"Rank2-NoSymmetry", 2, {}, 9, Flatten[Table[{ia, ib}, {ia, 1, 3}, {ib, 1, 3}], 1]},
    {"Rank2-Symmetric", 2, {{1, 2, 1}}, 6, Flatten[Table[{ia, ib}, {ia, 1, 3}, {ib, ia, 3}], 1]},
    {"Rank2-Antisymmetric", 2, {{1, 2, -1}}, 3, Flatten[Table[{ia, ib}, {ia, 1, 3}, {ib, ia + 1, 3}], 1]},
    {"Rank3-NoSymmetry", 3, {}, 27, Flatten[Table[{ic, ia, ib}, {ic, 1, 3}, {ia, 1, 3}, {ib, 1, 3}], 2]},
    {"Rank3-c(ab)", 3, {{2, 3, 1}}, 18, Flatten[Table[{ic, ia, ib}, {ic, 1, 3}, {ia, 1, 3}, {ib, ia, 3}], 2]},
    {"Rank3-c[ab]", 3, {{2, 3, -1}}, 9, Flatten[Table[{ic, ia, ib}, {ic, 1, 3}, {ia, 1, 3}, {ib, ia + 1, 3}], 2]},
    {"Rank3-(ab)c", 3, {{1, 2, 1}}, 18, Flatten[Table[{ia, ib, ic}, {ia, 1, 3}, {ib, ia, 3}, {ic, 1, 3}], 2]},
    {"Rank3-[ab]c", 3, {{1, 2, -1}}, 9, Flatten[Table[{ia, ib, ic}, {ia, 1, 3}, {ib, ia + 1, 3}, {ic, 1, 3}], 2]},
    {"Rank4-NoSymmetry", 4, {}, 81, Flatten[Table[{ic, id, ia, ib}, {ic, 1, 3}, {id, 1, 3}, {ia, 1, 3}, {ib, 1, 3}], 3]},
    {"Rank4-(cd)(ab)", 4, {{1, 2, 1}, {3, 4, 1}}, 36, Flatten[Table[{ic, id, ia, ib}, {ic, 1, 3}, {id, ic, 3}, {ia, 1, 3}, {ib, ia, 3}], 3]},
    {"Rank4-[cd][ab]", 4, {{1, 2, -1}, {3, 4, -1}}, 9, Flatten[Table[{ic, id, ia, ib}, {ic, 1, 3}, {id, ic + 1, 3}, {ia, 1, 3}, {ib, ia + 1, 3}], 3]},
    {"Rank4-cd(ab)", 4, {{3, 4, 1}}, 54, Flatten[Table[{ic, id, ia, ib}, {ic, 1, 3}, {id, 1, 3}, {ia, 1, 3}, {ib, ia, 3}], 3]},
    {"Rank4-cd[ab]", 4, {{3, 4, -1}}, 27, Flatten[Table[{ic, id, ia, ib}, {ic, 1, 3}, {id, 1, 3}, {ia, 1, 3}, {ib, ia + 1, 3}], 3]},
    {"Rank4-(ab)cd", 4, {{1, 2, 1}}, 54, Flatten[Table[{ia, ib, ic, id}, {ia, 1, 3}, {ib, ia, 3}, {ic, 1, 3}, {id, 1, 3}], 3]},
    {"Rank4-[ab]cd", 4, {{1, 2, -1}}, 27, Flatten[Table[{ia, ib, ic, id}, {ia, 1, 3}, {ib, ia + 1, 3}, {ic, 1, 3}, {id, 1, 3}], 3]},
    {"Rank4-c(ab)d", 4, {{2, 3, 1}}, 54, Flatten[Table[{ic, ia, ib, id}, {ic, 1, 3}, {ia, 1, 3}, {ib, ia, 3}, {id, 1, 3}], 3]},
    {"Rank4-c[ab]d", 4, {{2, 3, -1}}, 27, Flatten[Table[{ic, ia, ib, id}, {ic, 1, 3}, {ia, 1, 3}, {ib, ia + 1, 3}, {id, 1, 3}], 3]}
  }
];

AppendTo[$AllTests,
  VerificationTest[
    (* in four dimensions the range starts at 0 *)
    IndependentIndices[2, {{1, 2, 1}}, {0, 3}],
    {{0, 0}, {0, 1}, {0, 2}, {0, 3}, {1, 1}, {1, 2}, {1, 3}, {2, 2}, {2, 3}, {3, 3}},
    TestID -> "IndependentIndices-Rank2-Symmetric-4D"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a pair need not be of neighbouring slots *)
    IndependentIndices[3, {{1, 3, 1}}, {1, 2}],
    {{1, 1, 1}, {1, 1, 2}, {1, 2, 1}, {1, 2, 2}, {2, 1, 2}, {2, 2, 2}},
    TestID -> "IndependentIndices-Rank3-SeparatedPair"
  ]
];

(* ========================================= *)
(* Setup: bound variables on the test manifold from BasicTests *)
(* ========================================= *)

(* Reuse TestM3 manifold if it exists, otherwise define it *)
If[!MemberQ[$Manifolds, TestM3],
  DefManifold[TestM3, 3, IndexRange[a, z]];
  DefChart[testCart, TestM3, {1, 2, 3}, {TX[], TY[], TZ[]}, ChartColor -> Blue];
  DefMetric[1, testEuclid[-i, -j], testCD];
];

ResetContext[];
SetPVerbose[False];
SetPrintDate[False];
SetGridPointIndex["[[ijk]]"];
SetTilePointIndex["[[tI]]"];

compTestGridVarlist =
  GridTensors[
    {compTestS[], PrintAs -> "s"},
    {compTestM[-i, -j], Symmetric[{-i, -j}], PrintAs -> "m"},
    {compTestPi$Upt[i]}
  ];

compTestTileVarlist = TileTensors[{compTestT[i], PrintAs -> "t"}];

compTestTempVarlist = TempTensors[{compTestV[i], PrintAs -> "v"}];

(* The fields of the component records of a bound variable, with "Expr" as text *)
compTestFields[head_, field_] := #[field]& /@ VariableRecord[head]["Components"];

compTestExprs[head_] := ToString /@ compTestFields[head, "Expr"];

(* ========================================= *)
(* Test: binding - the record of a bound variable *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* a binding returns its var specs, so a varlist stays a plain list *)
    compTestGridVarlist,
    {{compTestS[], PrintAs -> "s"}, {compTestM[-i, -j], Symmetric[{-i, -j}], PrintAs -> "m"}, {compTestPi$Upt[i]}},
    TestID -> "Bind-ReturnsVarSpecs"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the record holds what the declaration states, and the storage of the binding *)
    KeyDrop[VariableRecord[compTestM], "Components"],
    <|"Head" -> compTestM, "Name" -> "compTestM", "Slots" -> {-i, -j}, "Symmetry" -> Symmetric[{-i, -j}], "Pairs" -> {{1, 2, 1}}, "PrintAs" -> "m", "DerivativeOf" -> None, "TimeDerivativeOf" -> None, "Storage" -> "Grid"|>,
    TestID -> "Bind-Record-DeclaredFactsAndStorage"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* binding extends the record of the declaration: there is one record, and one store *)
    {
      DeclaredVariable[compTestM] === VariableRecord[compTestM],
      KeyDrop[DeclaredVariable[compTestM], {"Storage", "Components"}] === ParseVar[compTestGridVarlist[[2]]],
      Keys[DeclaredVariable[compTestM]]
    },
    {True, True, {"Head", "Name", "Slots", "Symmetry", "Pairs", "PrintAs", "DerivativeOf", "TimeDerivativeOf", "Storage", "Components"}},
    TestID -> "Bind-ExtendsTheDeclaredRecord"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    {VariableRecord[compTestS]["Storage"], VariableRecord[compTestT]["Storage"], VariableRecord[compTestV]["Storage"]},
    {"Grid", "Tile", "Temp"},
    TestID -> "Bind-Record-StoragePerBinding"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a var spec resolves through its head, whatever else it states *)
    {
      VariableRecord[compTestGridVarlist[[2]]] === VariableRecord[compTestM],
      VariableRecord[{compTestM[-i, -j]}] === VariableRecord[compTestM]
    },
    {True, True},
    TestID -> "VariableRecord-VarSpecResolvesThroughHead"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* one record per independent component, in the order of the enumeration *)
    {compTestFields[compTestM, "Indices"], compTestFields[compTestM, "Comp"]},
    {
      {{1, 1}, {1, 2}, {1, 3}, {2, 2}, {2, 3}, {3, 3}},
      {
        compTestM[{1, -testCart}, {1, -testCart}], compTestM[{1, -testCart}, {2, -testCart}], compTestM[{1, -testCart}, {3, -testCart}],
        compTestM[{2, -testCart}, {2, -testCart}], compTestM[{2, -testCart}, {3, -testCart}], compTestM[{3, -testCart}, {3, -testCart}]
      }
    },
    TestID -> "Bind-Components-IndicesAndComp"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a slot is the position in the variable's own list, counted from 0 *)
    {compTestFields[compTestS, "Slot"], compTestFields[compTestM, "Slot"], compTestFields[compTestPi$Upt, "Slot"]},
    {{0}, {0, 1, 2, 3, 4, 5}, {0, 1, 2}},
    TestID -> "Bind-Components-Slots"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* "Name" is the bare C identifier; an up slot is named by its number like a down slot *)
    {compTestFields[compTestS, "Name"], compTestFields[compTestM, "Name"], compTestFields[compTestT, "Name"], compTestFields[compTestPi$Upt, "Name"]},
    {
      {"compTestS"},
      {"compTestM11", "compTestM12", "compTestM13", "compTestM22", "compTestM23", "compTestM33"},
      {"compTestT1", "compTestT2", "compTestT3"},
      {"compTestPi1", "compTestPi2", "compTestPi3"}
    },
    TestID -> "Bind-Components-Name"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* "Expr" carries the point index of the storage, and a temporary carries none *)
    {compTestExprs[compTestS], compTestExprs[compTestM][[{1, 6}]], compTestExprs[compTestT], compTestFields[compTestV, "Expr"]},
    {
      {"compTestS[[ijk]]"},
      {"compTestM11[[ijk]]", "compTestM33[[ijk]]"},
      {"compTestT1[[tI]]", "compTestT2[[tI]]", "compTestT3[[tI]]"},
      {compTestV1, compTestV2, compTestV3}
    },
    TestID -> "Bind-Components-ExprPerStorage"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* xAct holds a copy of the records: an independent component, and its mirror, give the "Expr" *)
    ToString /@ {
      compTestM[{1, -testCart}, {2, -testCart}] // ToValues,
      compTestM[{2, -testCart}, {1, -testCart}] // ToValues,
      compTestV[{3, testCart}] // ToValues
    },
    {"compTestM12[[ijk]]", "compTestM12[[ijk]]", "compTestV3"},
    TestID -> "Bind-WritesXActComponentRules"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* components may be named by letters *)
    ResetContext[];
    SetUseLetterForTensorComponent[True];
    TempTensors[{compTestL[-i, -j], Antisymmetric[{-i, -j}]}];
    SetUseLetterForTensorComponent[False];
    compTestFields[compTestL, "Name"],
    {"compTestLxy", "compTestLxz", "compTestLyz"},
    TestID -> "Bind-Components-NameWithLetters"
  ]
];

(* ========================================= *)
(* Test: binding - what is an error *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* a variable is bound once, and the first binding stays *)
    {
      Catch[
        TempTensors[{compTestM[-i, -j], Symmetric[{-i, -j}], PrintAs -> "m"}];
        "NoThrow",
        GeneratoFailure
      ],
      VariableRecord[compTestM]["Storage"],
      Length[VariableRecord[compTestM]["Components"]]
    },
    {$Failed, "Grid", 6},
    {TempTensors::EBound},
    TestID -> "Bind-BoundHead-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a declared variable cannot be bound yet: its record stays as declared, without storage *)
    DefTensors[{compTestD[i]}];
    {
      Catch[
        GridTensors[{compTestD[i]}];
        "NoThrow",
        GeneratoFailure
      ],
      DeclaredVariable[compTestD]
    },
    {
      $Failed,
      <|"Head" -> compTestD, "Name" -> "compTestD", "Slots" -> {i}, "Symmetry" -> None, "Pairs" -> {}, "PrintAs" -> None, "DerivativeOf" -> None, "TimeDerivativeOf" -> None|>
    },
    {DefineTensor::EExists},
    TestID -> "Bind-ExistingTensor-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a var spec whose head is not bound has no record *)
    Catch[
      VariableRecord[{compTestUnbound[i], PrintAs -> "u"}];
      "NoThrow",
      GeneratoFailure
    ],
    $Failed,
    {VariableRecord::EUnbound},
    TestID -> "VariableRecord-UnboundVarSpec-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* when input equations are only checked, a binding declares and binds nothing *)
    ResetContext[];
    SetCheckInputEquations[True];
    compTestCheckVarlist = GridTensors[{compTestC[i]}];
    SetCheckInputEquations[False];
    {
      compTestCheckVarlist,
      xTensorQ[compTestC],
      KeyExistsQ[DeclaredVariable[compTestC], "Storage"],
      Catch[VariableRecord[compTestC]; "NoThrow", GeneratoFailure]
    },
    {{{compTestC[i]}}, True, False, $Failed},
    {VariableRecord::EUnbound},
    TestID -> "Bind-CheckInputEquations-DeclaresOnly"
  ]
];

(* ========================================= *)
(* Test: binding - the 4D-in-3D skip *)
(* ========================================= *)

(*
  On a 4D manifold a slot with a spatial letter has no component 0. The
  default manifold is the first one defined, so the 4D one is put first for
  the binding only.
*)
If[!MemberQ[$Manifolds, TestM4],
  DefManifold[TestM4, 4, {a4, b4, c4, d4, i4, j4, k4, l4}];
  DefChart[testCart4, TestM4, {0, 1, 2, 3}, {TT4[], TX4[], TY4[], TZ4[]}, ChartColor -> Red];
];

ResetContext[];
SetPVerbose[False];
SetPrintDate[False];

Block[{$Manifolds = {TestM4}},
  compTest4DVarlist =
    TempTensors[
      {compTestG4[-a4, -b4], Symmetric[{-a4, -b4}]},
      {compTestB4[i4]},
      {compTestK4[-i4, -j4], Symmetric[{-i4, -j4}]},
      {compTestH4[-k4, -a4]},
      {compTestU4[i4, -k4]}
    ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* spacetime letters keep every component: 10 of a symmetric pair, from {0, 0} *)
    compTestFields[compTestG4, "Indices"],
    {{0, 0}, {0, 1}, {0, 2}, {0, 3}, {1, 1}, {1, 2}, {1, 3}, {2, 2}, {2, 3}, {3, 3}},
    TestID -> "Bind-4D-SpacetimeLettersKeepAll"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a skipped component gets no record, and the slots count the records that are left *)
    {
      compTestFields[compTestB4, "Indices"],
      compTestFields[compTestB4, "Slot"],
      compTestFields[compTestK4, "Indices"],
      compTestFields[compTestK4, "Slot"]
    },
    {
      {{1}, {2}, {3}},
      {0, 1, 2},
      {{1, 1}, {1, 2}, {1, 3}, {2, 2}, {2, 3}, {3, 3}},
      {0, 1, 2, 3, 4, 5}
    },
    TestID -> "Bind-4D-SpatialLettersSkipZero"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* only the slot with a spatial letter is restricted *)
    {compTestFields[compTestH4, "Indices"], compTestFields[compTestH4, "Name"][[{1, 12}]]},
    {
      {{1, 0}, {1, 1}, {1, 2}, {1, 3}, {2, 0}, {2, 1}, {2, 2}, {2, 3}, {3, 0}, {3, 1}, {3, 2}, {3, 3}},
      {"compTestH410", "compTestH433"}
    },
    TestID -> "Bind-4D-MixedLetters"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a skipped component is the number 0 when its 0 is on an up slot and on no down slot *)
    {
      compTestB4[{0, testCart4}] // ToValues,
      compTestU4[{0, testCart4}, {2, -testCart4}] // ToValues,
      compTestU4[{0, testCart4}, {0, -testCart4}] // ToValues,
      compTestU4[{2, testCart4}, {0, -testCart4}] // ToValues,
      compTestK4[{0, -testCart4}, {1, -testCart4}] // ToValues
    },
    {
      0,
      0,
      compTestU4[{0, testCart4}, {0, -testCart4}],
      compTestU4[{2, testCart4}, {0, -testCart4}],
      compTestK4[{0, -testCart4}, {1, -testCart4}]
    },
    TestID -> "Bind-4D-SkippedUpComponentIsZero"
  ]
];

(* ========================================= *)
(* Test: the records reach a backend *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* a component in xAct's notation resolves through its head to its record *)
    MapAt[ToString, ComponentRecord[compTestM[{2, -testCart}, {3, -testCart}]], "Expr"],
    <|"Comp" -> compTestM[{2, -testCart}, {3, -testCart}], "Indices" -> {2, 3}, "Slot" -> 4, "Name" -> "compTestM23", "Expr" -> "compTestM23[[ijk]]"|>,
    TestID -> "ComponentRecord-ReadsTheRecord"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a record is that of a bound variable once a binding has given it storage: the record of a declaration *)
    (* has none yet, and a head that is not declared has no record *)
    {
      BoundQ[DeclaredVariable[compTestM]],
      BoundQ[ParseVar[{compTestNotDeclared[i]}]],
      BoundQ[DeclaredVariable[compTestNotDeclared]]
    },
    {True, False, False},
    TestID -> "BoundQ-StorageMakesARecordBound"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the mirror of a symmetric pair has no record *)
    Catch[
      ComponentRecord[compTestM[{3, -testCart}, {2, -testCart}]];
      "NoThrow",
      GeneratoFailure
    ],
    $Failed,
    {ComponentRecord::EComponent},
    TestID -> "ComponentRecord-DependentComponent-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a variable has records in the chart it was bound in and in no other; *)
    (* the component of a scalar carries no chart, so any chart has it *)
    {
      RequireChart[VariableRecord[compTestM], First[VariableRecord[compTestM]["Components"]], testCart],
      Catch[
        RequireChart[VariableRecord[compTestM], First[VariableRecord[compTestM]["Components"]], compTestOtherChart];
        "NoThrow",
        GeneratoFailure
      ],
      RequireChart[VariableRecord[compTestS], First[VariableRecord[compTestS]["Components"]], compTestOtherChart]
    },
    {Null, $Failed, Null},
    {RequireChart::EChart},
    TestID -> "RequireChart-OtherChart-Stops"
  ]
];

(* A backend whose one formatter keeps each statement it is handed, and prints its mode and component *)
compTestStatements = {};

compTestKeep = Function[statement, AppendTo[compTestStatements, statement]; {statement["Mode"] <> " " <> statement["Component", "Name"]}];

DefineBackend["compTestKeep", <|
  "Load" -> <|
    "In" -> <|All -> compTestKeep|>,
    "Out" -> <|All -> compTestKeep|>,
    "Derivs" -> <|All -> compTestKeep|>,
    "Temp" -> <|All -> compTestKeep|>,
    "MoreInOut" -> <|All -> compTestKeep|>
  |>,
  "Store" -> <|"Out" -> <|All -> compTestKeep|>, "AddTo" -> <|All -> compTestKeep|>, "Temp" -> <|All -> compTestKeep|>|>
|>];

(* A backend with the stores codes/CarpetXGPU.wl states, and no load *)
DefineBackend["compTestConst", <|"Store" -> <|"Temp" -> <|All -> DeclareAssign|>|>|>];

(*
  compTestWrite - evaluates body as the main content of a write through a
  backend, and returns the lines it printed: the file without its header
  and its footer.
*)
SetAttributes[compTestWrite, HoldRest];

compTestWrite[backend_String, body_] :=
  Module[{file = FileNameJoin[{$TemporaryDirectory, "generato_component_write.hxx"}], lines},
    compTestStatements = {};
    SetPrintHeaderMacro[False];
    SetMainPrint[body];
    WriteToFile[backend, file];
    lines = StringSplit[ReadString[file], "\n", All];
    DeleteFile[file];
    lines[[4 ;; -4]]
  ];

(* ========================================= *)
(* Test: a load print call hands the backend of the write one Load statement per component *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* one statement per component record, in the order of the varlist; each is rendered and its lines are printed *)
    ResetContext[];
    {
      compTestWrite["compTestKeep", PrintInitializations[{Mode -> "MainIn"}, Take[compTestGridVarlist, 2]]],
      #["Component"]& /@ compTestStatements,
      #["Variable"]& /@ compTestStatements,
      Union[KeyTake[#, {"Kind", "Mode", "Base"}]& /@ compTestStatements]
    },
    {
      {"In compTestS", "In compTestM11", "In compTestM12", "In compTestM13", "In compTestM22", "In compTestM23", "In compTestM33"},
      Join[VariableRecord[compTestS]["Components"], VariableRecord[compTestM]["Components"]],
      Join[{VariableRecord[compTestS]}, ConstantArray[VariableRecord[compTestM], 6]],
      {<|"Kind" -> "Load", "Mode" -> "In", "Base" -> None|>}
    },
    TestID -> "PrintInitializations-OneLoadStatementPerComponent"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a Load statement has these keys and no other: it has no right-hand side, *)
    (* and its "Config" is a copy of the user configuration *)
    ResetContext[];
    SetProject["compTestProject"];
    compTestWrite["compTestKeep", PrintInitializations[{Mode -> "MainIn"}, Take[compTestGridVarlist, 1]]];
    {
      Keys[First[compTestStatements]],
      First[compTestStatements]["Config"] === GetConfiguration[],
      First[compTestStatements]["Config", "Project"]
    },
    {{"Kind", "Mode", "Variable", "Component", "Base", "Config"}, True, "compTestProject"},
    TestID -> "PrintInitializations-StatementIsSelfContained"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the Mode and the StorageType of the call give the mode of the statement and the storage of its variable, *)
    (* whatever the binding: "GF" is the StorageType unstated, and "Temp" the Mode unstated *)
    ResetContext[];
    compTestWrite["compTestKeep",
      PrintInitializations[{Mode -> "MainIn"}, compTestTileVarlist[[{1}]]];
      PrintInitializations[{Mode -> "MainIn", StorageType -> "GF"}, Take[compTestGridVarlist, 1]];
      PrintInitializations[{Mode -> "MainOut"}, Take[compTestGridVarlist, 1]];
      PrintInitializations[{Mode -> "MainIn", StorageType -> "Tile"}, Take[compTestGridVarlist, 1]];
      PrintInitializations[{Mode -> "MainOut", StorageType -> "Tile"}, Take[compTestGridVarlist, 1]];
      PrintInitializations[{Mode -> "Derivs"}, Take[compTestGridVarlist, 1]];
      PrintInitializations[{Mode -> "Derivs", StorageType -> "Tile"}, Take[compTestGridVarlist, 1]];
      PrintInitializations[{Mode -> "Temp"}, Take[compTestGridVarlist, 1]];
      PrintInitializations[{Mode -> "Temp", StorageType -> "Tile"}, Take[compTestGridVarlist, 1]];
      PrintInitializations[{Mode -> "MoreInOut"}, Take[compTestGridVarlist, 1]];
      PrintInitializations[{Mode -> "MoreInOut", StorageType -> "Tile"}, Take[compTestGridVarlist, 1]];
      PrintInitializations[Take[compTestGridVarlist, 1]]
    ];
    {#["Mode"], #["Variable", "Storage"]}& /@ compTestStatements,
    {
      {"In", "Grid"}, {"In", "Grid"}, {"In", "Grid"},
      {"In", "Grid"},
      {"Out", "Grid"},
      {"In", "Tile"},
      {"Out", "Tile"},
      {"Derivs", "Temp"},
      {"In", "Tile"},
      {"Temp", "Temp"},
      {"Temp", "Temp"},
      {"MoreInOut", "Grid"},
      {"MoreInOut", "Grid"},
      {"Temp", "Temp"}
    },
    TestID -> "PrintInitializations-ModeAndStorageOfALoadStatement"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the storage is written over the statement's copy of the variable record: *)
    (* nothing else of the copy differs, and the record binding wrote is left as it is *)
    ResetContext[];
    compTestWrite["compTestKeep", PrintInitializations[{Mode -> "MainIn", StorageType -> "Tile"}, Take[compTestGridVarlist, {2}]]];
    {
      Union[#["Variable", "Storage"]& /@ compTestStatements],
      VariableRecord[compTestM]["Storage"],
      Union[#["Variable"]& /@ compTestStatements] === {ReplacePart[VariableRecord[compTestM], "Storage" -> "Tile"]},
      Keys[First[compTestStatements]["Variable"]] === Keys[VariableRecord[compTestM]]
    },
    {{"Tile"}, "Grid", True, True},
    TestID -> "PrintInitializations-StorageOfTheCallNotOfTheBinding"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a Mode or a StorageType the call does not take stops, whatever the varlist and before a backend is asked for *)
    ResetContext[];
    {
      Catch[PrintInitializations[{Mode -> "Main"}, {}]; "NoThrow", GeneratoFailure],
      Catch[
        compTestWrite["compTestKeep", PrintInitializations[{Mode -> "MainIn", StorageType -> "Grid"}, compTestTempVarlist]];
        "NoThrow",
        GeneratoFailure
      ],
      compTestStatements
    },
    {$Failed, $Failed, {}},
    {PrintInitializations::EMode, PrintInitializations::EMode},
    TestID -> "PrintInitializations-UnknownModeOrStorageType-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* None is the value of no mode and of no storage type: it is not taken either *)
    ResetContext[];
    {
      Catch[PrintInitializations[{Mode -> None}, {}]; "NoThrow", GeneratoFailure],
      Catch[PrintInitializations[{Mode -> "MainIn", StorageType -> None}, {}]; "NoThrow", GeneratoFailure]
    },
    {$Failed, $Failed},
    {PrintInitializations::EMode, PrintInitializations::EMode},
    TestID -> "PrintInitializations-ModeOrStorageTypeNone-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a print call belongs in the main content of a write: outside one there is no backend to print through *)
    ResetContext[];
    Catch[PrintInitializations[{Mode -> "MainIn"}, Take[compTestGridVarlist, 1]]; "NoThrow", GeneratoFailure],
    $Failed,
    {CurrentBackend::ENoWrite},
    TestID -> "PrintInitializations-OutsideAWrite-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a load the backend of the write has no key for stops: the keys of a table are what the backend accepts *)
    ResetContext[];
    Catch[
      compTestWrite["compTestConst", PrintInitializations[{Mode -> "MainIn"}, Take[compTestGridVarlist, 1]]];
      "NoThrow",
      GeneratoFailure
    ],
    $Failed,
    {Render::EKey},
    TestID -> "PrintInitializations-NoLoadInTheBackend-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a print call on a var that is not bound stops before anything is printed *)
    ResetContext[];
    {
      Catch[
        compTestWrite["compTestKeep", PrintInitializations[{Mode -> "MainIn"}, {{compTestUnbound[i], PrintAs -> "u"}}]];
        "NoThrow",
        GeneratoFailure
      ],
      compTestStatements
    },
    {$Failed, {}},
    {VariableRecord::EUnbound},
    TestID -> "PrintCalls-UnboundVar-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* an initialization in another chart stops at the first variable that has no record there *)
    ResetContext[];
    {
      Catch[
        compTestWrite["compTestKeep", PrintInitializations[{Mode -> "MainIn", ChartName -> compTestOtherChart}, Take[compTestGridVarlist, 2]]];
        "NoThrow",
        GeneratoFailure
      ],
      #["Component", "Name"]& /@ compTestStatements
    },
    {$Failed, {"compTestS"}},
    {RequireChart::EChart},
    TestID -> "PrintInitializations-OtherChart-Stops"
  ]
];

(* ========================================= *)
(* Test: an equation print call hands the backend of the write one Store statement per component *)
(* ========================================= *)

SetEQN[compTestS[], 2 compTestS[]];

SetEQN[compTestV[i_], compTestS[] compTestT[i]];

SetEQN[{SuffixName -> "alt"}, compTestV[i_], 2 compTestT[i]];

SetEQN[compTestT[i_], compTestS[] compTestV[i]];

SetEQN[compTestM[i_, j_], compTestS[] compTestM[i, j]];

AppendTo[$AllTests,
  VerificationTest[
    (* one statement per component record, in order; each is rendered and its lines are printed *)
    ResetContext[];
    {
      compTestWrite["compTestKeep", PrintEquations[{Mode -> "Temp"}, compTestTempVarlist]],
      #["Component"]& /@ compTestStatements,
      Union[KeyTake[#, {"Kind", "Mode", "Variable", "Base"}]& /@ compTestStatements],
      CText[#["RHS"]]& /@ compTestStatements
    },
    {
      {"Temp compTestV1", "Temp compTestV2", "Temp compTestV3"},
      VariableRecord[compTestV]["Components"],
      {<|"Kind" -> "Store", "Mode" -> "Temp", "Variable" -> VariableRecord[compTestV], "Base" -> None|>},
      {"compTestS[ijk]*compTestT1[tI]", "compTestS[ijk]*compTestT2[tI]", "compTestS[ijk]*compTestT3[tI]"}
    },
    TestID -> "PrintEquations-OneStoreStatementPerComponent"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a statement has these keys and no other, and its "Config" is a copy of the user configuration *)
    ResetContext[];
    SetTempVariableType["vreal"];
    SetProject["compTestProject"];
    compTestWrite["compTestKeep", PrintEquations[{Mode -> "Temp"}, compTestTempVarlist]];
    {
      Keys[First[compTestStatements]],
      First[compTestStatements]["Config"] === GetConfiguration[],
      KeyTake[First[compTestStatements]["Config"], {"TempVariableType", "Project", "GridPointIndex"}],
      Keys[First[compTestStatements]["Config"]]
    },
    {
      {"Kind", "Mode", "Variable", "Component", "Base", "RHS", "Config"},
      True,
      <|"TempVariableType" -> "vreal", "Project" -> "compTestProject", "GridPointIndex" -> ""|>,
      {
        "CheckInputEquations", "PVerbose", "QuietMode", "PrintDate", "PrintHeaderMacro", "GridPointIndex", "TilePointIndex",
        "SuffixUnprotected", "Project", "SimplifyEquation", "UseLetterForTensorComponent", "TempVariableType",
        "InterfaceWithNonCoordBasis", "PrefixDt"
      }
    },
    TestID -> "PrintEquations-StatementIsSelfContained"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the modes of the print call are the modes of a Store statement, and "MainOut" is the one unstated *)
    ResetContext[];
    compTestWrite["compTestKeep",
      PrintEquations[{Mode -> "Temp"}, compTestTileVarlist];
      PrintEquations[{Mode -> "MainOut"}, compTestTileVarlist];
      PrintEquations[{Mode -> "AddToMainOut"}, compTestTileVarlist];
      PrintEquations[Take[compTestGridVarlist, 1]]
    ],
    {
      "Temp compTestT1", "Temp compTestT2", "Temp compTestT3",
      "Out compTestT1", "Out compTestT2", "Out compTestT3",
      "AddTo compTestT1", "AddTo compTestT2", "AddTo compTestT3",
      "Out compTestS"
    },
    TestID -> "PrintEquations-ModesOfAStoreStatement"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a mode that is none of the three stops, whatever the varlist and before a backend is asked for *)
    ResetContext[];
    {
      Catch[PrintEquations[{Mode -> "Main"}, {}]; "NoThrow", GeneratoFailure],
      Catch[compTestWrite["compTestKeep", PrintEquations[{Mode -> "Main"}, compTestTempVarlist]]; "NoThrow", GeneratoFailure],
      compTestStatements
    },
    {$Failed, $Failed, {}},
    {PrintEquations::EMode, PrintEquations::EMode},
    TestID -> "PrintEquations-UnknownMode-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a print call belongs in the main content of a write: outside one there is no backend to print through *)
    ResetContext[];
    Catch[PrintEquations[{Mode -> "Temp"}, compTestTempVarlist]; "NoThrow", GeneratoFailure],
    $Failed,
    {CurrentBackend::ENoWrite},
    TestID -> "PrintEquations-OutsideAWrite-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* SuffixName selects the equation set under that name. It is an argument: the SuffixName of the *)
    (* context is neither written nor read by the print call *)
    ResetContext[];
    SetSuffixName["compTestKept"];
    compTestWrite["compTestKeep",
      PrintEquations[{Mode -> "Temp", SuffixName -> "alt"}, compTestTempVarlist];
      PrintEquations[{Mode -> "Temp"}, Take[compTestTempVarlist, 1]]
    ];
    {CText[#["RHS"]]& /@ compTestStatements[[{1, 4}]], GetSuffixName[]},
    {{"2*compTestT1[tI]", "compTestS[ijk]*compTestT1[tI]"}, "compTestKept"},
    TestID -> "PrintEquations-SuffixNameIsAnArgument"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a SuffixName that is not a string stops *)
    ResetContext[];
    Catch[compTestWrite["compTestKeep", PrintEquations[{Mode -> "Temp", SuffixName -> compTestNotAString}, compTestTempVarlist]]; "NoThrow", GeneratoFailure],
    $Failed,
    {PrintEquations::ESuffixName},
    TestID -> "PrintEquations-SuffixNameNotAString-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* ExtraReplaceRules rewrite the right-hand side and what the equation assigns to, the "Expr" of the *)
    (* statement's copy of the component. The "Name" is not rewritten, and neither is the bound record *)
    ResetContext[];
    compTestWrite["compTestKeep",
      PrintEquations[{Mode -> "Temp", ExtraReplaceRules -> {compTestV1 -> compTestRenamed, compTestS[[ijk]] -> compTestOne}}, compTestTempVarlist]
    ];
    {
      #["Component", "Expr"]& /@ compTestStatements,
      #["Component", "Name"]& /@ compTestStatements,
      CText[#["RHS"]]& /@ compTestStatements,
      compTestFields[compTestV, "Expr"]
    },
    {
      {compTestRenamed, compTestV2, compTestV3},
      {"compTestV1", "compTestV2", "compTestV3"},
      {"compTestOne*compTestT1[tI]", "compTestOne*compTestT2[tI]", "compTestOne*compTestT3[tI]"},
      {compTestV1, compTestV2, compTestV3}
    },
    TestID -> "PrintEquations-ExtraReplaceRules"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* an equation in another chart has no record to print, and nothing of the varlist is printed *)
    ResetContext[];
    {
      Catch[
        compTestWrite["compTestKeep", PrintEquations[{Mode -> "MainOut", ChartName -> compTestOtherChart}, {compTestGridVarlist[[2]]}]];
        "NoThrow",
        GeneratoFailure
      ],
      compTestStatements
    },
    {$Failed, {}},
    {RequireChart::EChart},
    TestID -> "PrintEquations-OtherChart-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the component of a scalar carries no chart, so any chart prints it *)
    ResetContext[];
    {
      compTestWrite["compTestKeep", PrintEquations[{Mode -> "MainOut", ChartName -> compTestOtherChart}, {compTestGridVarlist[[1]]}]],
      CText[#["RHS"]]& /@ compTestStatements
    },
    {{"Out compTestS"}, {"2*compTestS[ijk]"}},
    TestID -> "PrintEquations-OtherChart-Scalar"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* through the formatters a backend builds on, the lines are the one-token-a-line layout of the goldens: *)
    (* a temporary is declared by its bare name, and an assignment refers to the storage *)
    ResetContext[];
    SetTempVariableType["vreal"];
    compTestWrite["compTestConst",
      pr["{"];
      PrintEquations[{Mode -> "Temp"}, Take[compTestTempVarlist, 1]];
      PrintEquations[{Mode -> "MainOut"}, Take[compTestGridVarlist, 1]];
      PrintEquations[{Mode -> "AddToMainOut"}, Take[compTestGridVarlist, 1]];
      pr["}"]
    ],
    {
      "{",
      "const vreal", "compTestV1", "=", "compTestS[ijk]*compTestT1[tI]", ";", "",
      "const vreal", "compTestV2", "=", "compTestS[ijk]*compTestT2[tI]", ";", "",
      "const vreal", "compTestV3", "=", "compTestS[ijk]*compTestT3[tI]", ";", "",
      "compTestS[ijk]", "=", "2*compTestS[ijk]", ";", "",
      "compTestS[ijk]", "+=", "2*compTestS[ijk]", ";", "",
      "}"
    },
    TestID -> "PrintEquations-LinesThroughTheSharedFormatters"
  ]
];

(* ========================================= *)
(* Test: one component, the way a script prints components in its own order *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* the mode and the suffix name are the keys in force, and the component is the one named *)
    ResetContext[];
    compTestWrite["compTestKeep",
      WithMode[{"Phase" -> "PrintComp", "PrintCompType" -> "Equations", "EquationsMode" -> "MainOut"},
        PrintComponentEquation[testCart, compTestM[{2, -testCart}, {3, -testCart}], {}];
        PrintComponentEquation[testCart, compTestM[{1, -testCart}, {1, -testCart}], {}]
      ];
      SetSuffixName["alt"];
      WithMode[{"EquationsMode" -> "Temp"},
        PrintComponentEquation[testCart, compTestV[{3, testCart}], {compTestT3[[tI]] -> compTestThree}]
      ];
      SetSuffixName[""]
    ];
    {
      #["Mode"]& /@ compTestStatements,
      #["Component"]& /@ compTestStatements,
      #["Variable"]& /@ compTestStatements,
      CText[#["RHS"]]& /@ compTestStatements
    },
    {
      {"Out", "Out", "Temp"},
      {
        ComponentRecord[compTestM[{2, -testCart}, {3, -testCart}]],
        ComponentRecord[compTestM[{1, -testCart}, {1, -testCart}]],
        ComponentRecord[compTestV[{3, testCart}]]
      },
      {VariableRecord[compTestM], VariableRecord[compTestM], VariableRecord[compTestV]},
      {"compTestM23[ijk]*compTestS[ijk]", "compTestM11[ijk]*compTestS[ijk]", "2*compTestThree"}
    },
    TestID -> "PrintComponentEquation-OneStatementForTheNamedComponent"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* with no equation mode in force there is nothing to print the component as *)
    ResetContext[];
    Catch[
      compTestWrite["compTestKeep", PrintComponentEquation[testCart, compTestV[{3, testCart}], {}]];
      "NoThrow",
      GeneratoFailure
    ],
    $Failed,
    {PrintComponentEquation::EMode},
    TestID -> "PrintComponentEquation-NoMode-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the mirror of a symmetric pair is not an independent component, and a head that is not bound has none *)
    ResetContext[];
    {
      Catch[
        compTestWrite["compTestKeep",
          WithMode[{"EquationsMode" -> "MainOut"}, PrintComponentEquation[testCart, compTestM[{3, -testCart}, {2, -testCart}], {}]]
        ];
        "NoThrow",
        GeneratoFailure
      ],
      Catch[
        compTestWrite["compTestKeep",
          WithMode[{"EquationsMode" -> "MainOut"}, PrintComponentEquation[testCart, compTestUnbound[{1, testCart}], {}]]
        ];
        "NoThrow",
        GeneratoFailure
      ],
      compTestStatements
    },
    {$Failed, $Failed, {}},
    {ComponentRecord::EComponent, VariableRecord::EUnbound},
    TestID -> "PrintComponentEquation-NotAnIndependentComponent-Stops"
  ]
];

(* ========================================= *)
(* Test: a declared relation reaches a backend *)
(* ========================================= *)

ResetContext[];
SetPVerbose[False];
SetPrintDate[False];
SetGridPointIndex["[[ijk]]"];

(* A scalar, a symmetric matrix and a vector, with their first and second derivatives: *)
(* the shapes of test/regression/CarpetXGPU/testTileDerivs.wl *)
compTestBaseVarlist =
  GridTensors[
    {compTestW[]},
    {compTestG[-i, -j], Symmetric[{-i, -j}]},
    {compTestB[i]}
  ];

compTestDVarlist =
  TileTensors[
    {compTestDW[-k], DerivativeOf -> compTestW},
    {compTestDG[-k, -i, -j], Symmetric[{-i, -j}], DerivativeOf -> compTestG},
    {compTestDB[-k, i], DerivativeOf -> compTestB}
  ];

compTestDDVarlist =
  TileTensors[
    {compTestDDW[-i, -j], Symmetric[{-i, -j}], DerivativeOf -> compTestW},
    {compTestDDG[-l, -m, -i, -j], GenSet[Cycles[{1, 2}], Cycles[{3, 4}]], DerivativeOf -> compTestG},
    {compTestDDB[-i, -j, k], Symmetric[{-i, -j}], DerivativeOf -> compTestB}
  ];

compTestDtVarlist = GridTensors[{compTestDtB[i], TimeDerivativeOf -> compTestB}];

(* A derivative declared without the symmetry of its variable, and one of a variable that is not bound *)
compTestOddVarlist = TempTensors[{compTestDGn[-k, -i, -j], DerivativeOf -> compTestG}];

DefTensors[{compTestUB[i]}];

compTestDUBVarlist = TempTensors[{compTestDUB[-k, i], DerivativeOf -> compTestUB}];

ResetContext[];

(* The index of each component of a bound variable, as a print call would have it *)
compTestSuffixes[head_] :=
  With[{variable = VariableRecord[head]},
    IndexSuffix[variable, #, BaseRecord[variable]]& /@ variable["Components"]
  ];

(* The slots 0 to count - 1 as one index, and as the two indices [v mod n][v div n] *)
compTestOneIndex[count_] := Table["[" <> ToString[v] <> "]", {v, 0, count - 1}];

compTestTwoIndices[count_, n_] := Table["[" <> ToString[Mod[v, n]] <> "][" <> ToString[Quotient[v, n]] <> "]", {v, 0, count - 1}];

AppendTo[$AllTests,
  VerificationTest[
    (* without a relation a scalar has no index, and any other variable the slot *)
    {compTestSuffixes[compTestW], compTestSuffixes[compTestB], compTestSuffixes[compTestG]},
    {{""}, compTestOneIndex[3], compTestOneIndex[6]},
    TestID -> "IndexSuffix-NoRelation"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* first derivatives of a scalar, a symmetric matrix and a vector: N is 1, 6 and 3 *)
    {compTestSuffixes[compTestDW], compTestSuffixes[compTestDG], compTestSuffixes[compTestDB]},
    {compTestOneIndex[3], compTestTwoIndices[18, 6], compTestTwoIndices[9, 3]},
    TestID -> "IndexSuffix-FirstDerivatives"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* second derivatives of the same three *)
    {compTestSuffixes[compTestDDW], compTestSuffixes[compTestDDG], compTestSuffixes[compTestDDB]},
    {compTestOneIndex[6], compTestTwoIndices[36, 6], compTestTwoIndices[18, 3]},
    TestID -> "IndexSuffix-SecondDerivatives"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the index is [component of the differentiated variable][derivative]: *)
    (* d_2 G_13 is component 2 of G in direction 1, and d_2 d_3 B^1 is component 0 of B in the pair 4 *)
    {
      IndexSuffix[VariableRecord[compTestDG], ComponentRecord[compTestDG[{2, -testCart}, {1, -testCart}, {3, -testCart}]], VariableRecord[compTestG]],
      IndexSuffix[VariableRecord[compTestDDB], ComponentRecord[compTestDDB[{2, -testCart}, {3, -testCart}, {1, testCart}]], VariableRecord[compTestB]]
    },
    {"[2][1]", "[0][4]"},
    TestID -> "IndexSuffix-ComponentThenDerivative"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a time derivative has the components of its variable and is laid out like it *)
    compTestSuffixes[compTestDtB],
    compTestOneIndex[3],
    TestID -> "IndexSuffix-TimeDerivative"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a relation is followed to the record of the bound variable it names *)
    {
      BaseRecord[VariableRecord[compTestG]],
      BaseRecord[VariableRecord[compTestDG]] === VariableRecord[compTestG],
      BaseRecord[VariableRecord[compTestDDW]] === VariableRecord[compTestW],
      BaseRecord[VariableRecord[compTestDtB]] === VariableRecord[compTestB]
    },
    {None, True, True, True},
    TestID -> "BaseRecord-FollowsTheRelation"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the variable a relation names must be bound when the relation is read *)
    Catch[
      BaseRecord[VariableRecord[compTestDUB]];
      "NoThrow",
      GeneratoFailure
    ],
    $Failed,
    {VariableRecord::EUnbound},
    TestID -> "BaseRecord-UnboundBase-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a derivative component differentiates the component at its trailing indices *)
    {
      BaseComponent[ComponentRecord[compTestDG[{2, -testCart}, {1, -testCart}, {3, -testCart}]], VariableRecord[compTestG]]["Name"],
      BaseComponent[ComponentRecord[compTestDDB[{2, -testCart}, {3, -testCart}, {1, testCart}]], VariableRecord[compTestB]]["Name"],
      BaseComponent[ComponentRecord[compTestDDW[{1, -testCart}, {2, -testCart}]], VariableRecord[compTestW]]["Name"]
    },
    {"compTestG13", "compTestB1", "compTestW"},
    TestID -> "BaseComponent-TrailingIndices"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a derivative without the symmetry of its variable has components the variable has none for *)
    {
      BaseComponent[ComponentRecord[compTestDGn[{1, -testCart}, {1, -testCart}, {2, -testCart}]], VariableRecord[compTestG]]["Name"],
      Catch[
        BaseComponent[ComponentRecord[compTestDGn[{1, -testCart}, {2, -testCart}, {1, -testCart}]], VariableRecord[compTestG]];
        "NoThrow",
        GeneratoFailure
      ]
    },
    {"compTestG12", $Failed},
    {BaseComponent::EComponent},
    TestID -> "BaseComponent-NoSuchComponent-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a load that reads a relation stops at a variable that does not declare it *)
    {
      RequireRelation[VariableRecord[compTestDG], "DerivativeOf", "Derivs"],
      Catch[
        RequireRelation[VariableRecord[compTestG], "DerivativeOf", "Derivs"];
        "NoThrow",
        GeneratoFailure
      ],
      Catch[
        RequireRelation[VariableRecord[compTestDG], "TimeDerivativeOf", "MainOut"];
        "NoThrow",
        GeneratoFailure
      ]
    },
    {Null, $Failed, $Failed},
    {RequireRelation::EMissing, RequireRelation::EMissing},
    TestID -> "RequireRelation-MissingRelation-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a Load statement carries the record a relation names; the options that stated the layout are accepted *)
    ResetContext[];
    compTestWrite["compTestKeep",
      PrintInitializations[{Mode -> "Derivs", TensorType -> "Vect", DerivsOrder -> 1, DerivsAccuracy -> 4}, Take[compTestDVarlist, 1]];
      PrintInitializations[{Mode -> "MainOut"}, compTestDtVarlist]
    ];
    {
      {#["Mode"], #["Component", "Name"], #["Base", "Head"]}& /@ compTestStatements,
      Union[#["Base"]& /@ compTestStatements] === Sort[{VariableRecord[compTestW], VariableRecord[compTestB]}]
    },
    {
      {
        {"Derivs", "compTestDW1", compTestW}, {"Derivs", "compTestDW2", compTestW}, {"Derivs", "compTestDW3", compTestW},
        {"Out", "compTestDtB1", compTestB}, {"Out", "compTestDtB2", compTestB}, {"Out", "compTestDtB3", compTestB}
      },
      True
    },
    TestID -> "PrintCalls-HandTheBaseRecord"
  ]
];

(* A backend whose loads read a relation and say so, as codes/CarpetXGPU.wl and codes/Nmesh.wl do *)
DefineBackend["compTestRelation", <|
  "Load" -> <|
    "Derivs" -> <|"Temp" -> Requires["DerivativeOf", compTestKeep]|>,
    "Out" -> <|"Grid" -> Requires["TimeDerivativeOf", compTestKeep]|>
  |>
|>];

AppendTo[$AllTests,
  VerificationTest[
    (* a load whose formatter reads a relation prints a variable that declares it, *)
    (* and stops at one that does not before its formatter is handed anything *)
    ResetContext[];
    {
      compTestWrite["compTestRelation",
        PrintInitializations[{Mode -> "Derivs"}, Take[compTestDVarlist, 1]];
        PrintInitializations[{Mode -> "MainOut"}, compTestDtVarlist]
      ],
      Catch[
        compTestWrite["compTestRelation", PrintInitializations[{Mode -> "Derivs"}, compTestTileVarlist]];
        "NoThrow",
        GeneratoFailure
      ],
      Catch[
        compTestWrite["compTestRelation", PrintInitializations[{Mode -> "MainOut"}, Take[compTestDVarlist, 1]]];
        "NoThrow",
        GeneratoFailure
      ],
      compTestStatements
    },
    {
      {"Derivs compTestDW1", "Derivs compTestDW2", "Derivs compTestDW3", "Out compTestDtB1", "Out compTestDtB2", "Out compTestDtB3"},
      $Failed,
      $Failed,
      {}
    },
    {RequireRelation::EMissing, RequireRelation::EMissing},
    TestID -> "PrintInitializations-RequiredRelationMissing-Stops"
  ]
];

SetEQN[compTestDtB[i_], compTestB[i]];

SetEQN[compTestDUB[i_, j_], compTestDB[i, j]];

AppendTo[$AllTests,
  VerificationTest[
    (* a Store statement carries the record a relation names, as a load is handed it; *)
    (* so the variable the relation names must be bound when its derivative is stored *)
    ResetContext[];
    {
      compTestWrite["compTestKeep", PrintEquations[{Mode -> "MainOut"}, compTestDtVarlist]],
      Union[#["Base"]& /@ compTestStatements] === {VariableRecord[compTestB]},
      Catch[
        compTestWrite["compTestKeep", PrintEquations[{Mode -> "Temp"}, compTestDUBVarlist]];
        "NoThrow",
        GeneratoFailure
      ],
      compTestStatements
    },
    {{"Out compTestDtB1", "Out compTestDtB2", "Out compTestDtB3"}, True, $Failed, {}},
    {VariableRecord::EUnbound},
    TestID -> "PrintEquations-StatementCarriesTheBaseRecord"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* TensorType "Vect" or "Smat" on a variable that declares no DerivativeOf stops the call before it prints: *)
    (* the first var of the list declares one and is not printed either *)
    ResetContext[];
    {
      Catch[
        compTestWrite["compTestKeep",
          PrintInitializations[{Mode -> "Derivs", StorageType -> "Tile", TensorType -> "Vect"}, Join[Take[compTestDVarlist, 1], compTestTileVarlist]]
        ];
        "NoThrow",
        GeneratoFailure
      ],
      Catch[
        compTestWrite["compTestKeep", PrintInitializations[{Mode -> "MainIn", TensorType -> "Smat"}, Take[compTestGridVarlist, {2}]]];
        "NoThrow",
        GeneratoFailure
      ],
      compTestStatements
    },
    {$Failed, $Failed, {}},
    {PrintInitializations::ETensorType, PrintInitializations::ETensorType},
    TestID -> "PrintInitializations-TensorTypeWithoutDerivativeOf-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* nothing else checks the value of TensorType, so any value but "Scal" is a stated layout: *)
    (* a mistyped one stops at a variable that declares no DerivativeOf, and is not read at one that does *)
    ResetContext[];
    {
      Catch[
        compTestWrite["compTestKeep", PrintInitializations[{Mode -> "MainIn", TensorType -> "Vec"}, Take[compTestGridVarlist, {2}]]];
        "NoThrow",
        GeneratoFailure
      ],
      compTestStatements,
      compTestWrite["compTestKeep", PrintInitializations[{Mode -> "Derivs", StorageType -> "Tile", TensorType -> "Vec"}, Take[compTestDVarlist, 1]]]
    },
    {$Failed, {}, {"In compTestDW1", "In compTestDW2", "In compTestDW3"}},
    {PrintInitializations::ETensorType},
    TestID -> "PrintInitializations-TensorTypeMistyped"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a variable that declares no DerivativeOf is printed when TensorType is "Scal" or is not stated, *)
    (* and one that declares it is printed whatever TensorType says *)
    ResetContext[];
    compTestWrite["compTestKeep",
      PrintInitializations[{Mode -> "Derivs", StorageType -> "Tile"}, compTestTileVarlist];
      PrintInitializations[{Mode -> "Derivs", StorageType -> "Tile", TensorType -> "Scal"}, compTestTileVarlist];
      PrintInitializations[{Mode -> "Derivs", StorageType -> "Tile", TensorType -> "Smat"}, Take[compTestDDVarlist, 1]]
    ],
    {
      "In compTestT1", "In compTestT2", "In compTestT3",
      "In compTestT1", "In compTestT2", "In compTestT3",
      "In compTestDDW11", "In compTestDDW12", "In compTestDDW13", "In compTestDDW22", "In compTestDDW23", "In compTestDDW33"
    },
    TestID -> "PrintInitializations-TensorType-AcceptedOtherwise"
  ]
];

(* Reset state *)
ResetContext[];

If[Environment["QUIET"] =!= "1", Print["ComponentTests.wl completed."]];
