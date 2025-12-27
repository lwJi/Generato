(* ::Package:: *)

(* AllTests.wl *)
(* Master test runner for Generato *)
(* Usage: wolframscript -f test/AllTests.wl *)

$TestDir = DirectoryName[$InputFileName];

Print[""];
Print["========================================"];
Print["  Generato Test Suite"];
Print["========================================"];
Print[""];

(* Collect all verification tests *)
$AllTests = {};

(* ========================================= *)
(* PHASE 1: Unit Tests *)
(* ========================================= *)

Print["--- Unit Tests ---"];
Print[""];

unitTestFiles = FileNames["*.wl", FileNameJoin[{$TestDir, "unit"}]];

If[Length[unitTestFiles] > 0,
  Do[
    Print["Running: ", FileNameTake[file]];
    Get[file],
    {file, unitTestFiles}
  ],
  Print["No unit test files found in test/unit/"]
];

Print[""];

(* Generate test report if VerificationTests were run *)
If[Length[$AllTests] > 0,
  Print["Generating TestReport..."];
  report = TestReport[$AllTests];
  Print["Tests Passed: ", report["TestsSucceededCount"], "/", report["TestsSucceededCount"] + report["TestsFailedCount"]];
  Print[""];
];

(* ========================================= *)
(* PHASE 2: Golden File Regression Tests *)
(* ========================================= *)

Print["--- Regression Tests (Golden Files) ---"];
Print[""];

(* Load and run golden file comparisons *)
Get[FileNameJoin[{$TestDir, "regression", "compare_golden.wl"}]];

(* Note: compare_golden.wl will call Exit[] with appropriate status *)
