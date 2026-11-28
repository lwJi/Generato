(* ::Package:: *)

(* EquationTests.wl *)
(* Unit tests for Generato`Equation module: an equation is set on a tensor, under a suffix name or without one, *)
(* and its right-hand side is evaluated for one component *)
(* The equations last as long as the kernel, like the xAct tensors, so each test uses heads of its own *)

If[Environment["QUIET"] =!= "1", Print["Loading EquationTests.wl..."]];

(* Load Generato which includes Equation *)
If[!MemberQ[$Packages, "Generato`Equation`"],
  Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"}]]
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

(* Define test tensors *)
testVarlist = GridTensors[{testVec[i], PrintAs -> "tv"}];

eqnTestVarlist = GridTensors[{eqnTestU[i]}, {eqnTestR[i]}, {eqnTestNone[i]}];

(* ========================================= *)
(* Test: SetEQN and RHSOf *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* SetEQN should set an equation without error *)
    SetEQN[testVec[i_], 2 testVec[i]];
    True,
    True,
    TestID -> "SetEQN-Basic-NoError"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* RHSOf should return a symbol referencing the tensor's RHS *)
    rhs = RHSOf[testVec];
    Head[rhs] === Symbol && StringContainsQ[ToString[rhs], "RHS"],
    True,
    TestID -> "RHSOf-ReturnsValue"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the symbol is named from the head and the suffix name, and three arguments are not understood *)
    {
      SymbolName[RHSOf[eqnTestR]],
      SymbolName[RHSOf[eqnTestR, "alt"]],
      Catch[RHSOf[eqnTestR, "alt", "more"]; "NoThrow", GeneratoFailure]
    },
    {"eqnTestR$RHS", "eqnTestRalt$RHS", $Failed},
    {RHSOf::Eargs},
    TestID -> "RHSOf-NamedFromHeadAndSuffix"
  ]
];

(* ========================================= *)
(* Test: Suffix handling *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    SetEQN[{SuffixName -> "testSuffix"}, testVec[i_], 3 testVec[i]];
    True,
    True,
    TestID -> "SetEQN-WithSuffix-NoError"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* SuffixName is one symbol: the option SetEQN sets an equation under is the one a Store selects it by *)
    {Context[SuffixName], Names["Generato`*`SuffixName"], Names["Generato`*`Private`SuffixName"]},
    {"Generato`Equation`", {"SuffixName"}, {}},
    TestID -> "SuffixName-IsOneSymbol"
  ]
];

(* ========================================= *)
(* Test: ComputeRHSValue *)
(* ========================================= *)

SetEQN[eqnTestR[i_], 2 eqnTestU[i]];

SetEQN[{SuffixName -> "alt"}, eqnTestR[i_], 3 eqnTestU[i]];

SetEQNDelayed[{SuffixName -> "delayed"}, eqnTestR[i_], If[i[[1]] == 1, 0, eqnTestU[i]]];

AppendTo[$AllTests,
  VerificationTest[
    (* the right-hand side of one component, as values: the suffix name selects the equation, *)
    (* a delayed one is evaluated for the component, and the replace rules are applied last *)
    ResetContext[];
    ToString[#, InputForm]& /@ {
      ComputeRHSValue[testCart, eqnTestR[{1, testCart}], "", {}],
      ComputeRHSValue[testCart, eqnTestR[{2, testCart}], "alt", {}],
      ComputeRHSValue[testCart, eqnTestR[{1, testCart}], "delayed", {}],
      ComputeRHSValue[testCart, eqnTestR[{3, testCart}], "delayed", {}],
      ComputeRHSValue[testCart, eqnTestR[{1, testCart}], "", {eqnTestU1 -> eqnTestRenamed}]
    },
    {"2*eqnTestU1", "3*eqnTestU2", "0", "eqnTestU3", "2*eqnTestRenamed"},
    TestID -> "ComputeRHSValue-OneComponent"
  ]
];

(* ========================================= *)
(* Test: an equation that cannot be set *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* a right-hand side with a term that is not defined stops, unless it is not to be checked *)
    {
      Catch[SetEQN[eqnTestNone[i_], eqnTestUndefined[i] + eqnTestU[i]]; "NoThrow", GeneratoFailure],
      Catch[SetEQN[{CheckRHS -> False, SuffixName -> "unchecked"}, eqnTestNone[i_], eqnTestUndefined[i]]; "NoThrow", GeneratoFailure] =!= $Failed
    },
    {$Failed, True},
    {IsDefined::ETerm, Generato`Equation`Private`SetEQNDetail::Evarrhs},
    TestID -> "SetEQN-UndefinedTerm-Stops"
  ]
];

(* ========================================= *)
(* Cleanup *)
(* ========================================= *)

ResetContext[];

If[Environment["QUIET"] =!= "1", Print["EquationTests.wl completed."]];
