(* ::Package:: *)

(* ComponentTests.wl *)
(* Unit tests for Generato`Component module: the enumeration of components, binding and the records, *)
(* and for what a backend reads from the records: the index layout and the variable a relation names *)
(* The store of the records lasts as long as the kernel, like the xAct tensors, so each test uses heads of its own *)

If[Environment["QUIET"] =!= "1", Print["Loading ComponentTests.wl..."]];

(* Load Generato *)
Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"}]];

SetPVerbose[False];
SetPrintDate[False];

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
(* Setup: bound variables on the test manifold of the unit tests *)
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
(* Test: binding - a variable that is declared already *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* a variable that is declared already is bound without being declared again: binding adds the storage *)
    (* and the component records to the record the declaration wrote, and xAct gets its copy of them *)
    ResetContext[];
    compTestDeclaredVars = DefTensors[{compTestD[i], PrintAs -> "d"}];
    compTestDeclaredRecord = DeclaredVariable[compTestD];
    {
      GridTensors[{compTestD[i], PrintAs -> "d"}],
      KeyDrop[VariableRecord[compTestD], {"Storage", "Components"}] === compTestDeclaredRecord,
      VariableRecord[compTestD]["Storage"],
      compTestFields[compTestD, "Name"],
      ToString[compTestD[{2, testCart}] // ToValues]
    },
    {{{compTestD[i], PrintAs -> "d"}}, True, "Grid", {"compTestD1", "compTestD2", "compTestD3"}, "compTestD2"},
    TestID -> "Bind-DeclaredHead-BindsWithoutDeclaring"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a binding takes its var specs as one list, which is how the list a declaration returned is bound. *)
    (* It returns the list, and parts of one list may be bound to different storage *)
    compTestModuleVars = DefTensors[{compTestMA[], PrintAs -> "a"}, {compTestMB[i]}, {compTestMC[-i, -j], Symmetric[{-i, -j}]}];
    {
      TileTensors[Take[compTestModuleVars, 2]] === Take[compTestModuleVars, 2],
      TempTensors[Drop[compTestModuleVars, 2]] === Drop[compTestModuleVars, 2],
      VariableRecord[#]["Storage"]& /@ {compTestMA, compTestMB, compTestMC},
      Length[VariableRecord[#]["Components"]]& /@ {compTestMA, compTestMB, compTestMC}
    },
    {True, True, {"Tile", "Tile", "Temp"}, {1, 3, 6}},
    TestID -> "Bind-ListOfVarSpecs-BindsADeclaration"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* one list of var specs that are not declared yet is declared and bound, like the var specs one by one *)
    {
      GridTensors[{{compTestLA[]}, {compTestLB[i]}}],
      VariableRecord[#]["Storage"]& /@ {compTestLA, compTestLB}
    },
    {{{compTestLA[]}, {compTestLB[i]}}, {"Grid", "Grid"}},
    TestID -> "Bind-ListOfVarSpecs-DeclaresAndBinds"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a declared variable is bound once like any other: a second binding stops, and the first stays *)
    {
      Catch[
        TempTensors[compTestDeclaredVars];
        "NoThrow",
        GeneratoFailure
      ],
      VariableRecord[compTestD]["Storage"]
    },
    {$Failed, "Grid"},
    {TempTensors::EBound},
    TestID -> "Bind-DeclaredHead-BoundTwice-Stops"
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
    (* a tensor defined outside Generato has no record, and cannot be bound *)
    DefTensor[compTestOutside[i], TestM3];
    {
      Catch[
        GridTensors[{compTestOutside[i]}];
        "NoThrow",
        GeneratoFailure
      ],
      DeclaredVariable[compTestOutside]
    },
    {$Failed, Missing["NotDeclared", compTestOutside]},
    {DefineTensor::EExists},
    TestID -> "Bind-TensorDefinedOutside-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a var that states a declared variable differently is not bound: the declaration stands, without storage *)
    DefTensors[{compTestDF[-i, -j], Symmetric[{-i, -j}]}];
    {
      Catch[
        GridTensors[{compTestDF[-i, -j]}];
        "NoThrow",
        GeneratoFailure
      ],
      Catch[
        GridTensors[{compTestDF[-i, -j], Symmetric[{-i, -j}], PrintAs -> "other"}];
        "NoThrow",
        GeneratoFailure
      ],
      DeclaredVariable[compTestDF]
    },
    {
      $Failed,
      $Failed,
      <|"Head" -> compTestDF, "Name" -> "compTestDF", "Slots" -> {-i, -j}, "Symmetry" -> Symmetric[{-i, -j}], "Pairs" -> {{1, 2, 1}}, "PrintAs" -> None, "DerivativeOf" -> None, "TimeDerivativeOf" -> None|>
    },
    {GridTensors::EDeclared, GridTensors::EDeclared},
    TestID -> "Bind-DeclaredHead-StatedDifferently-Stops"
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

AppendTo[$AllTests,
  VerificationTest[
    (* and a variable that is declared already is left as it is, declared and not bound *)
    ResetContext[];
    compTestCheckVars = DefTensors[{compTestCD[i]}];
    SetCheckInputEquations[True];
    compTestCheckResult = TileTensors[compTestCheckVars];
    SetCheckInputEquations[False];
    {compTestCheckResult === compTestCheckVars, BoundQ[DeclaredVariable[compTestCD]]},
    {True, False},
    TestID -> "Bind-CheckInputEquations-DeclaredHeadStaysUnbound"
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
    (* a component is found among the records of its variable by its "Comp". The mirror of a symmetric pair *)
    (* and the tensor with its abstract indices are the "Comp" of no record, and looking for them stops nothing *)
    {
      FindComponent[VariableRecord[compTestM], compTestM[{2, -testCart}, {3, -testCart}]] === ComponentRecord[compTestM[{2, -testCart}, {3, -testCart}]],
      FindComponent[VariableRecord[compTestM], compTestM[{3, -testCart}, {2, -testCart}]],
      FindComponent[VariableRecord[compTestM], compTestM[-i, -j]]
    },
    {True, Missing["NotIndependent", compTestM[{3, -testCart}, {2, -testCart}]], Missing["NotIndependent", compTestM[-i, -j]]},
    TestID -> "FindComponent-RecordOrMissing"
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

(* The index of each component of a bound variable, as a formatter is handed what it reads *)
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

(* Reset state *)
ResetContext[];

If[Environment["QUIET"] =!= "1", Print["ComponentTests.wl completed."]];
