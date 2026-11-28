(* ::Package:: *)

(* ComponentTests.wl *)
(* Unit tests for Generato`Component module: the enumeration of components, binding and the records *)
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
    <|"Head" -> compTestM, "Name" -> "compTestM", "Slots" -> {-i, -j}, "Symmetry" -> Symmetric[{-i, -j}], "Pairs" -> {{1, 2, 1}}, "PrintAs" -> "m", "Storage" -> "Grid"|>,
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
    {True, True, {"Head", "Name", "Slots", "Symmetry", "Pairs", "PrintAs", "Storage", "Components"}},
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
      <|"Head" -> compTestD, "Name" -> "compTestD", "Slots" -> {i}, "Symmetry" -> None, "Pairs" -> {}, "PrintAs" -> None|>
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
    (* ExtractComponentInfo reads its tuple from the record: slot, C expression, tensor, symmetry, rank *)
    MapAt[ToString, ExtractComponentInfo[Null, compTestM[{2, -testCart}, {3, -testCart}]], 2],
    {4, "compTestM23[[ijk]]", compTestM[-i, -j], Symmetric[{-i, -j}], 2},
    TestID -> "ExtractComponentInfo-ReadsTheRecord"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the mirror of a symmetric pair has no record *)
    Catch[
      ExtractComponentInfo[Null, compTestM[{3, -testCart}, {2, -testCart}]];
      "NoThrow",
      GeneratoFailure
    ],
    $Failed,
    {ExtractComponentInfo::EComponent},
    TestID -> "ExtractComponentInfo-DependentComponent-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a print call walks the records of its varlist, in order, and hands each to the backend *)
    ResetContext[];
    Block[{PrintComponentInitialization, PrintComponentEquation, compTestPrinted = {}},
      PrintComponentInitialization[varinfo_, compname_] := AppendTo[compTestPrinted, {"Init", GetInitializationsMode[], varinfo, compname}];
      PrintComponentEquation[coordinate_, compname_, rules_] := AppendTo[compTestPrinted, {"Eqn", GetEquationsMode[], coordinate, compname, rules}];
      PrintInitializations[{Mode -> "MainIn"}, Take[compTestGridVarlist, 2]];
      PrintEquations[{Mode -> "Temp", ExtraReplaceRules -> {compTestX -> 1}}, compTestTempVarlist];
      compTestPrinted
    ],
    {
      {"Init", "MainIn", {compTestS[], None}, compTestS[]},
      {"Init", "MainIn", {compTestM[-i, -j], Symmetric[{-i, -j}]}, compTestM[{1, -testCart}, {1, -testCart}]},
      {"Init", "MainIn", {compTestM[-i, -j], Symmetric[{-i, -j}]}, compTestM[{1, -testCart}, {2, -testCart}]},
      {"Init", "MainIn", {compTestM[-i, -j], Symmetric[{-i, -j}]}, compTestM[{1, -testCart}, {3, -testCart}]},
      {"Init", "MainIn", {compTestM[-i, -j], Symmetric[{-i, -j}]}, compTestM[{2, -testCart}, {2, -testCart}]},
      {"Init", "MainIn", {compTestM[-i, -j], Symmetric[{-i, -j}]}, compTestM[{2, -testCart}, {3, -testCart}]},
      {"Init", "MainIn", {compTestM[-i, -j], Symmetric[{-i, -j}]}, compTestM[{3, -testCart}, {3, -testCart}]},
      {"Eqn", "Temp", testCart, compTestV[{1, testCart}], {compTestX -> 1}},
      {"Eqn", "Temp", testCart, compTestV[{2, testCart}], {compTestX -> 1}},
      {"Eqn", "Temp", testCart, compTestV[{3, testCart}], {compTestX -> 1}}
    },
    TestID -> "PrintCalls-WalkRecordsInOrder"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a print call on a var that is not bound stops before anything is printed *)
    ResetContext[];
    Catch[
      PrintInitializations[{Mode -> "MainIn"}, {{compTestUnbound[i], PrintAs -> "u"}}];
      "NoThrow",
      GeneratoFailure
    ],
    $Failed,
    {VariableRecord::EUnbound},
    TestID -> "PrintCalls-UnboundVar-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* outside a print call there is no mode to print a component in *)
    ResetContext[];
    Catch[
      PrintComponent[testCart, VariableRecord[compTestM], First[VariableRecord[compTestM]["Components"]], {}];
      "NoThrow",
      GeneratoFailure
    ],
    $Failed,
    {PrintComponent::EMode},
    TestID -> "PrintComponent-NoMode-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a variable has records in the chart it was bound in and in no other *)
    ResetContext[];
    Catch[
      PrintEquations[{Mode -> "MainOut", ChartName -> compTestOtherChart}, {compTestGridVarlist[[2]]}];
      "NoThrow",
      GeneratoFailure
    ],
    $Failed,
    {PrintComponent::EChart},
    TestID -> "PrintCalls-OtherChart-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the component of a scalar carries no chart, so any chart prints it *)
    ResetContext[];
    Block[{PrintComponentEquation, compTestPrinted = {}},
      PrintComponentEquation[coordinate_, compname_, rules_] := AppendTo[compTestPrinted, {coordinate, compname}];
      PrintEquations[{Mode -> "MainOut", ChartName -> compTestOtherChart}, {compTestGridVarlist[[1]]}];
      compTestPrinted
    ],
    {{compTestOtherChart, compTestS[]}},
    TestID -> "PrintCalls-OtherChart-Scalar"
  ]
];

(* Reset state *)
ResetContext[];

If[Environment["QUIET"] =!= "1", Print["ComponentTests.wl completed."]];
