(* ::Package:: *)

(* VariableTests.wl *)
(* Unit tests for Generato`Variable`: a var spec becomes a variable record and an xAct tensor *)
(* Tests: ParseVar, DefineTensor, DefTensors, DeclaredVariable, RecordBinding, and the relations DerivativeOf and TimeDerivativeOf *)
(* The store of the records lasts as long as the kernel, like the xAct tensors, so each test uses heads of its own *)

If[Environment["QUIET"] =!= "1", Print["Loading VariableTests.wl..."]];

(* Load Generato *)
Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"}]];

(* Helper to reset context to defaults *)
ResetContext[] := ($CurrentContext = $ContextDefaults);

ResetContext[];
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
(* Test: ParseVar - the fields of a variable record *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* a vector with a print name *)
    ParseVar[{varTestVec[i], PrintAs -> "vtv"}],
    <|"Head" -> varTestVec, "Name" -> "varTestVec", "Slots" -> {i}, "Symmetry" -> None, "Pairs" -> {}, "PrintAs" -> "vtv", "DerivativeOf" -> None, "TimeDerivativeOf" -> None|>,
    TestID -> "ParseVar-Vector-Record"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a scalar has no slots, and a field the spec does not state is None *)
    ParseVar[{varTestScalar[]}],
    <|"Head" -> varTestScalar, "Name" -> "varTestScalar", "Slots" -> {}, "Symmetry" -> None, "Pairs" -> {}, "PrintAs" -> None, "DerivativeOf" -> None, "TimeDerivativeOf" -> None|>,
    TestID -> "ParseVar-Scalar-Record"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the symmetry is kept as stated, and normalised to a pair of slots *)
    ParseVar[{varTestSym[-i, -j], Symmetric[{-i, -j}], PrintAs -> "vts"}],
    <|"Head" -> varTestSym, "Name" -> "varTestSym", "Slots" -> {-i, -j}, "Symmetry" -> Symmetric[{-i, -j}], "Pairs" -> {{1, 2, 1}}, "PrintAs" -> "vts", "DerivativeOf" -> None, "TimeDerivativeOf" -> None|>,
    TestID -> "ParseVar-Symmetric-Record"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the features of a var spec come in any order *)
    ParseVar[{PrintAs -> "order", Symmetric[{-i, -j}], varTestOrder[-i, -j]}],
    <|"Head" -> varTestOrder, "Name" -> "varTestOrder", "Slots" -> {-i, -j}, "Symmetry" -> Symmetric[{-i, -j}], "Pairs" -> {{1, 2, 1}}, "PrintAs" -> "order", "DerivativeOf" -> None, "TimeDerivativeOf" -> None|>,
    TestID -> "ParseVar-OrderIndependence"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the name is the head without the suffix that keeps it apart from a system symbol *)
    ResetContext[];
    ParseVar[{varTestPi$Upt[-i, -j], Symmetric[{-i, -j}]}]["Name"],
    "varTestPi",
    TestID -> "ParseVar-Name-WithoutUnprotectedSuffix"
  ]
];

(* ========================================= *)
(* Test: ParseVar - a declared relation *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* a rule is told apart by its key: DerivativeOf names the variable a derivative is taken of *)
    ParseVar[{varTestD[-k, -i, -j], Symmetric[{-i, -j}], DerivativeOf -> varTestBase, PrintAs -> "d"}],
    <|"Head" -> varTestD, "Name" -> "varTestD", "Slots" -> {-k, -i, -j}, "Symmetry" -> Symmetric[{-i, -j}], "Pairs" -> {{2, 3, 1}}, "PrintAs" -> "d",
      "DerivativeOf" -> varTestBase, "TimeDerivativeOf" -> None|>,
    TestID -> "ParseVar-DerivativeOf-Record"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* TimeDerivativeOf names the variable a time derivative is taken of; the relation is a rule like the print name, in any place *)
    ParseVar[{TimeDerivativeOf -> varTestBase, varTestDt[-i, -j], Symmetric[{-i, -j}]}],
    <|"Head" -> varTestDt, "Name" -> "varTestDt", "Slots" -> {-i, -j}, "Symmetry" -> Symmetric[{-i, -j}], "Pairs" -> {{1, 2, 1}}, "PrintAs" -> None,
      "DerivativeOf" -> None, "TimeDerivativeOf" -> varTestBase|>,
    TestID -> "ParseVar-TimeDerivativeOf-Record"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a relation names a variable by its head *)
    Catch[
      ParseVar[{varTestD[-k], DerivativeOf -> "varTestBase"}];
      "NoThrow",
      GeneratoFailure
    ],
    $Failed,
    {ParseVar::EKey},
    TestID -> "ParseVar-RelationNotAHead-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a var states at most one relation *)
    Catch[
      ParseVar[{varTestD[-k], DerivativeOf -> varTestBase, TimeDerivativeOf -> varTestBase}];
      "NoThrow",
      GeneratoFailure
    ],
    $Failed,
    {ParseVar::EShape},
    TestID -> "ParseVar-TwoRelations-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* and at most one print name *)
    Catch[
      ParseVar[{varTestD[-k], PrintAs -> "d", PrintAs -> "e"}];
      "NoThrow",
      GeneratoFailure
    ],
    $Failed,
    {ParseVar::EShape},
    TestID -> "ParseVar-TwoPrintNames-Stops"
  ]
];

(* ========================================= *)
(* Test: ParseVar - a symmetry as pairs of slots *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    {
      ParseVar[{varTestPairs[-i, -j], Antisymmetric[{-i, -j}]}]["Pairs"],
      (* c(ab) and (ab)c *)
      ParseVar[{varTestPairs[-k, -i, -j], Symmetric[{-i, -j}]}]["Pairs"],
      ParseVar[{varTestPairs[-i, -j, k], Symmetric[{-i, -j}]}]["Pairs"],
      (* cd(ab), (ab)cd and c(ab)d *)
      ParseVar[{varTestPairs[-k, -l, -i, -j], Symmetric[{-i, -j}]}]["Pairs"],
      ParseVar[{varTestPairs[-i, -j, -k, -l], Symmetric[{-i, -j}]}]["Pairs"],
      ParseVar[{varTestPairs[-k, -i, -j, -l], Antisymmetric[{-i, -j}]}]["Pairs"],
      (* (cd)(ab) and [cd][ab] *)
      ParseVar[{varTestPairs[-k, -l, -i, -j], GenSet[Cycles[{1, 2}], Cycles[{3, 4}]]}]["Pairs"],
      ParseVar[{varTestPairs[-k, -l, -i, -j], GenSet[-Cycles[{1, 2}], -Cycles[{3, 4}]]}]["Pairs"]
    },
    {
      {{1, 2, -1}},
      {{2, 3, 1}},
      {{1, 2, 1}},
      {{3, 4, 1}},
      {{1, 2, 1}},
      {{2, 3, -1}},
      {{1, 2, 1}, {3, 4, 1}},
      {{1, 2, -1}, {3, 4, -1}}
    },
    TestID -> "ParseVar-Pairs-PerRankAndSymmetry"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* three indices are not a pair *)
    Catch[
      ParseVar[{varTestPairs[-i, -j, -k], Symmetric[{-i, -j, -k}]}];
      "NoThrow",
      GeneratoFailure
    ],
    $Failed,
    {ParseVar::ESymmetryType},
    TestID -> "ParseVar-Symmetry-NotAPair-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the indices of a symmetry must be slots of the tensor *)
    Catch[
      ParseVar[{varTestPairs[-i, -j], Symmetric[{-k, -l}]}];
      "NoThrow",
      GeneratoFailure
    ],
    $Failed,
    {ParseVar::ESymmetryType},
    TestID -> "ParseVar-Symmetry-NotSlots-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* pairs that share a slot are not disjoint *)
    Catch[
      ParseVar[{varTestPairs[-i, -j, -k], GenSet[Cycles[{1, 2}], Cycles[{2, 3}]]}];
      "NoThrow",
      GeneratoFailure
    ],
    $Failed,
    {ParseVar::ESymmetryType},
    TestID -> "ParseVar-Symmetry-SharedSlot-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* rank 5 and above stays unsupported *)
    Catch[
      ParseVar[{varTestRank5[-i, -j, -k, -l, -m]}];
      "NoThrow",
      GeneratoFailure
    ],
    $Failed,
    {ParseVar::ETensorType},
    TestID -> "ParseVar-Rank5-Stops"
  ]
];

(* ========================================= *)
(* Test: ParseVar - what is not a var spec *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* a rule is told apart by its key: an unknown key is an error, not a print name *)
    Catch[
      ParseVar[{varTestVec[i], PrintName -> "vtv"}];
      "NoThrow",
      GeneratoFailure
    ],
    $Failed,
    {ParseVar::EKey},
    TestID -> "ParseVar-UnknownRuleKey-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the print name is a string *)
    Catch[
      ParseVar[{varTestVec[i], PrintAs -> vtv}];
      "NoThrow",
      GeneratoFailure
    ],
    $Failed,
    {ParseVar::EKey},
    TestID -> "ParseVar-PrintAsNotAString-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* indices written as two groups are not a tensor *)
    Catch[
      ParseVar[{varTestVec[i][j], PrintAs -> "vtv"}];
      "NoThrow",
      GeneratoFailure
    ],
    $Failed,
    {ParseVar::EVar},
    TestID -> "ParseVar-UnsupportedFeature-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a var names one tensor *)
    Catch[
      ParseVar[{varTestVec[i], varTestVec2[j]}];
      "NoThrow",
      GeneratoFailure
    ],
    $Failed,
    {ParseVar::EShape},
    TestID -> "ParseVar-TwoTensors-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a var is a list *)
    Catch[
      ParseVar[varTestVec[i]];
      "NoThrow",
      GeneratoFailure
    ],
    $Failed,
    {ParseVar::EShape},
    TestID -> "ParseVar-NotAList-Stops"
  ]
];

(* ========================================= *)
(* Test: DefineTensor *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* DefineTensor gives xAct the tensor, stores the record and returns it *)
    {
      DefineTensor[ParseVar[{varDefTest1[i], PrintAs -> "vdt1"}]] === ParseVar[{varDefTest1[i], PrintAs -> "vdt1"}],
      xTensorQ[varDefTest1],
      PrintAs[varDefTest1],
      DeclaredVariable[varDefTest1] === ParseVar[{varDefTest1[i], PrintAs -> "vdt1"}]
    },
    {True, True, "vdt1", True},
    TestID -> "DefineTensor-CreatesXTensor"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the symmetry reaches xAct as the spec states it *)
    DefineTensor[ParseVar[{varDefTest2[-i, -j], Symmetric[{-i, -j}], PrintAs -> "vdt2"}]];
    {xTensorQ[varDefTest2], varDefTest2[-j, -i] // ToCanonical},
    {True, varDefTest2[-i, -j]},
    TestID -> "DefineTensor-WithSymmetry"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* no print name, and an empty one, leave xAct its own *)
    DefineTensor[ParseVar[{varDefTest3[i]}]];
    DefineTensor[ParseVar[{varDefTest4[i], PrintAs -> ""}]];
    {xTensorQ[varDefTest3], PrintAs[varDefTest3], PrintAs[varDefTest4]},
    {True, "varDefTest3", "varDefTest4"},
    TestID -> "DefineTensor-NoPrintAs"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a variable is declared once, and the record of the first declaration stays *)
    {
      Catch[
        DefineTensor[ParseVar[{varDefTest1[i]}]];
        "NoThrow",
        GeneratoFailure
      ],
      DeclaredVariable[varDefTest1]["PrintAs"]
    },
    {$Failed, "vdt1"},
    {DefineTensor::EExists},
    TestID -> "DefineTensor-ExistingTensor-Stops"
  ]
];

(* ========================================= *)
(* Test: DefineTensor - a relation is checked where it is declared *)
(* ========================================= *)

DefTensors[{varRelScalar[]}, {varRelVec[i]}, {varRelSym[-i, -j], Symmetric[{-i, -j}]}];

AppendTo[$AllTests,
  VerificationTest[
    (* a derivative has more slots than the variable it differentiates, at any order; a time derivative has as many *)
    DefTensors[
      {varRelDScalar[-k], DerivativeOf -> varRelScalar},
      {varRelDDScalar[-i, -j], Symmetric[{-i, -j}], DerivativeOf -> varRelScalar},
      {varRelDSym[-k, -i, -j], Symmetric[{-i, -j}], DerivativeOf -> varRelSym},
      {varRelDtSym[-i, -j], Symmetric[{-i, -j}], TimeDerivativeOf -> varRelSym}
    ];
    {
      DeclaredVariable[varRelDScalar]["DerivativeOf"],
      DeclaredVariable[varRelDDScalar]["DerivativeOf"],
      DeclaredVariable[varRelDSym]["DerivativeOf"],
      DeclaredVariable[varRelDSym]["TimeDerivativeOf"],
      DeclaredVariable[varRelDtSym]["DerivativeOf"],
      DeclaredVariable[varRelDtSym]["TimeDerivativeOf"]
    },
    {varRelScalar, varRelScalar, varRelSym, None, None, varRelSym},
    TestID -> "DefineTensor-Relation-Declared"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the variable a relation names must be declared already; the var that fails is not declared *)
    {
      Catch[
        DefTensors[{varRelDNever[-k], DerivativeOf -> varRelNever}];
        "NoThrow",
        GeneratoFailure
      ],
      xTensorQ[varRelDNever],
      DeclaredVariable[varRelDNever]
    },
    {$Failed, False, Missing["NotDeclared", varRelDNever]},
    {DefineTensor::EBase},
    TestID -> "DefineTensor-DerivativeOf-UndeclaredBase-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the same holds for a time derivative *)
    Catch[
      DefTensors[{varRelDtNever[i], TimeDerivativeOf -> varRelNever}];
      "NoThrow",
      GeneratoFailure
    ],
    $Failed,
    {DefineTensor::EBase},
    TestID -> "DefineTensor-TimeDerivativeOf-UndeclaredBase-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a variable is not its own derivative: it is not declared while its var is read *)
    Catch[
      DefTensors[{varRelSelf[-k], DerivativeOf -> varRelSelf}];
      "NoThrow",
      GeneratoFailure
    ],
    $Failed,
    {DefineTensor::EBase},
    TestID -> "DefineTensor-DerivativeOf-Itself-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a derivative of a variable of equal rank has no derivative slot *)
    {
      Catch[
        DefTensors[{varRelDEqual[-k], DerivativeOf -> varRelVec}];
        "NoThrow",
        GeneratoFailure
      ],
      xTensorQ[varRelDEqual]
    },
    {$Failed, False},
    {DefineTensor::EDerivativeOf},
    TestID -> "DefineTensor-DerivativeOf-EqualRank-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* nor has a derivative of a variable of higher rank *)
    Catch[
      DefTensors[{varRelDHigher[-k], DerivativeOf -> varRelSym}];
      "NoThrow",
      GeneratoFailure
    ],
    $Failed,
    {DefineTensor::EDerivativeOf},
    TestID -> "DefineTensor-DerivativeOf-HigherRank-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a time derivative has the slots of its variable: a lower and a higher rank do not fit *)
    {
      Catch[
        DefTensors[{varRelDtLower[i], TimeDerivativeOf -> varRelSym}];
        "NoThrow",
        GeneratoFailure
      ],
      Catch[
        DefTensors[{varRelDtHigher[-k, i], TimeDerivativeOf -> varRelVec}];
        "NoThrow",
        GeneratoFailure
      ]
    },
    {$Failed, $Failed},
    {DefineTensor::ETimeDerivativeOf, DefineTensor::ETimeDerivativeOf},
    TestID -> "DefineTensor-TimeDerivativeOf-OtherRank-Stops"
  ]
];

(* ========================================= *)
(* Test: DefTensors *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* DefTensors declares each var and returns the var specs *)
    {
      DefTensors[{varDefsTest1[i], PrintAs -> "a"}, {varDefsTest2[-i, -j], Symmetric[{-i, -j}]}],
      xTensorQ[varDefsTest1],
      xTensorQ[varDefsTest2]
    },
    {
      {{varDefsTest1[i], PrintAs -> "a"}, {varDefsTest2[-i, -j], Symmetric[{-i, -j}]}},
      True,
      True
    },
    TestID -> "DefTensors-DeclaresAndReturnsSpecs"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a declared variable has a record: the facts of its declaration, and no storage *)
    {
      DeclaredVariable[varDefsTest1],
      DeclaredVariable[varDefsTest2] === ParseVar[{varDefsTest2[-i, -j], Symmetric[{-i, -j}]}]
    },
    {
      <|"Head" -> varDefsTest1, "Name" -> "varDefsTest1", "Slots" -> {i}, "Symmetry" -> None, "Pairs" -> {}, "PrintAs" -> "a", "DerivativeOf" -> None, "TimeDerivativeOf" -> None|>,
      True
    },
    TestID -> "DefTensors-DeclaredHasARecordWithoutStorage"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a declared variable is not bound: a print call has nothing to read *)
    Catch[
      VariableRecord[varDefsTest1];
      "NoThrow",
      GeneratoFailure
    ],
    $Failed,
    {VariableRecord::EUnbound},
    TestID -> "DefTensors-DeclaredIsNotBound"
  ]
];

(* ========================================= *)
(* Test: the store of the records *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* a head that was never declared has no record; that is an answer, not a failure *)
    DeclaredVariable[varStoreTestNever],
    Missing["NotDeclared", varStoreTestNever],
    TestID -> "DeclaredVariable-NotDeclared-Missing"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* RecordBinding adds storage and components to the record the declaration wrote *)
    DefTensors[{varStoreTest1[i], PrintAs -> "s1"}];
    RecordBinding[varStoreTest1, "Temp", {<|"Slot" -> 0|>}];
    DeclaredVariable[varStoreTest1],
    <|"Head" -> varStoreTest1, "Name" -> "varStoreTest1", "Slots" -> {i}, "Symmetry" -> None, "Pairs" -> {}, "PrintAs" -> "s1", "DerivativeOf" -> None, "TimeDerivativeOf" -> None,
      "Storage" -> "Temp", "Components" -> {<|"Slot" -> 0|>}|>,
    TestID -> "RecordBinding-ExtendsTheDeclaredRecord"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* storage is given once, and the first stays *)
    {
      Catch[
        RecordBinding[varStoreTest1, "Grid", {}];
        "NoThrow",
        GeneratoFailure
      ],
      DeclaredVariable[varStoreTest1]["Storage"]
    },
    {$Failed, "Temp"},
    {RecordBinding::EState},
    TestID -> "RecordBinding-BoundHead-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a head that is not declared has no record to extend, and gets none *)
    {
      Catch[
        RecordBinding[varStoreTestNever, "Grid", {}];
        "NoThrow",
        GeneratoFailure
      ],
      DeclaredVariable[varStoreTestNever]
    },
    {$Failed, Missing["NotDeclared", varStoreTestNever]},
    {RecordBinding::EState},
    TestID -> "RecordBinding-NotDeclared-Stops"
  ]
];

(* ========================================= *)
(* Cleanup *)
(* ========================================= *)

ResetContext[];

If[Environment["QUIET"] =!= "1", Print["VariableTests.wl completed."]];
