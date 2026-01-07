(* ::Package:: *)

(* DerivationTests.wl *)
(* Unit tests for Generato`Derivation module *)

If[Environment["QUIET"] =!= "1", Print["Loading DerivationTests.wl..."]];

(* Load the Derivation package *)
Needs["Generato`Derivation`", FileNameJoin[{Environment["GENERATO"], "src/Derivation.wl"}]];

(* ========================================= *)
(* Test: TestEQN with True condition *)
(* ========================================= *)

VerificationTest[
  (* TestEQN should not abort when condition is True *)
  TestEQN[True, "TrueCondition"];
  True,
  True,
  TestID -> "TestEQN-TrueCondition-NoAbort"
];

VerificationTest[
  (* TestEQN with empty label *)
  TestEQN[True, ""];
  True,
  True,
  TestID -> "TestEQN-TrueCondition-EmptyLabel"
];

(* ========================================= *)
(* Test: TestEQN with False condition *)
(* Should abort - use CheckAbort to catch *)
(* ========================================= *)

VerificationTest[
  (* TestEQN should abort when condition is False *)
  result = CheckAbort[
    TestEQN[False, "FalseCondition"];
    "did-not-abort",
    "aborted"
  ];
  result,
  "aborted",
  TestID -> "TestEQN-FalseCondition-Aborts"
];

VerificationTest[
  (* Verify we can continue after catching the abort *)
  CheckAbort[TestEQN[False, "Test"], "caught"];
  TestEQN[True, "Recovery"];
  True,
  True,
  TestID -> "TestEQN-RecoveryAfterAbort"
];

If[Environment["QUIET"] =!= "1", Print["DerivationTests.wl completed."]];
