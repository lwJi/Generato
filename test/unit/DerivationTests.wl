(* ::Package:: *)

(* DerivationTests.wl *)
(* Unit tests for Generato`Derivation module *)

If[Environment["QUIET"] =!= "1", Print["Loading DerivationTests.wl..."]];

(* Load the Derivation package *)
Needs["Generato`Derivation`", FileNameJoin[{Environment["GENERATO"], "src/Derivation.wl"}]];

(* ========================================= *)
(* Run tests with suppressed Print output *)
(* (TestEQN prints "FAILED!" when testing abort behavior - this is expected) *)
(* ========================================= *)

Block[{System`Print},

  (* Test: TestEQN with True condition *)
  test1 = VerificationTest[
    TestEQN[True, "TrueCondition"];
    True,
    True,
    TestID -> "TestEQN-TrueCondition-NoAbort"
  ];

  test2 = VerificationTest[
    TestEQN[True, ""];
    True,
    True,
    TestID -> "TestEQN-TrueCondition-EmptyLabel"
  ];

  (* Test: TestEQN with False condition - should abort *)
  test3 = VerificationTest[
    result = CheckAbort[
      TestEQN[False, "FalseCondition"];
      "did-not-abort",
      "aborted"
    ];
    result,
    "aborted",
    TestID -> "TestEQN-FalseCondition-Aborts"
  ];

  test4 = VerificationTest[
    CheckAbort[TestEQN[False, "Test"], "caught"];
    TestEQN[True, "Recovery"];
    True,
    True,
    TestID -> "TestEQN-RecoveryAfterAbort"
  ];

];

(* ========================================= *)
(* Report test results *)
(* ========================================= *)

$DerivationTestResults = {test1, test2, test3, test4};
$DerivationTestReport = TestReport[$DerivationTestResults];

If[Environment["QUIET"] =!= "1",
  Print["DerivationTests.wl: ",
    $DerivationTestReport["TestsSucceededCount"], "/",
    $DerivationTestReport["TestsSucceededCount"] + $DerivationTestReport["TestsFailedCount"],
    " tests passed"]
];

(* Exit with error code if any tests failed (when run standalone) *)
If[$DerivationTestReport["TestsFailedCount"] > 0,
  If[Environment["QUIET"] =!= "1",
    Print["Failed tests: ", $DerivationTestReport["TestsFailedWrappers"][[All, "TestID"]]]
  ];
  Exit[1]
];
