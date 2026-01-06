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

(* Load test cases from config file *)
$ConfigFile = FileNameJoin[{$TestDir, "test_cases.txt"}];
$TestCases = Select[
  StringSplit[#, ":"] & /@ Import[$ConfigFile, "Lines"],
  Length[#] == 3 && !StringStartsQ[#[[1]], "#"] &
];

(* Generate outputs for each test case *)
Print["Generating test outputs..."];
Print[""];

$GenerationFailed = False;
Do[
  {backend, testName, ext} = testCase;
  testFile = FileNameJoin[{$TestDir, backend, testName <> ".wl"}];

  If[FileExistsQ[testFile],
    Print["  Generating: ", backend, "/", testName, ".wl"];
    (* Run Generato from the test directory *)
    result = Run["cd " <> FileNameJoin[{$TestDir, backend}] <> " && \"$GENERATO/Generato\" " <> testName <> ".wl 2>&1"];
    If[result != 0,
      Print["    FAILED to generate ", testName, ext];
      $GenerationFailed = True;
    ],
    Print["  SKIP: ", testFile, " not found"];
  ],
  {testCase, $TestCases}
];

Print[""];

If[$GenerationFailed,
  Print["ERROR: Some outputs failed to generate"];
  Exit[1]
];

(* Load golden file comparison module *)
Get[FileNameJoin[{$TestDir, "regression", "compare_golden.wl"}]];

(* Run golden file comparisons *)
$RegressionResult = GoldenTest`RunGoldenTests[];

(* Cleanup generated output files *)
Print[""];
Print["Cleaning up generated files..."];
Do[
  {backend, testName, ext} = testCase;
  outputFile = FileNameJoin[{$TestDir, backend, testName <> ext}];
  If[FileExistsQ[outputFile],
    DeleteFile[outputFile];
  ],
  {testCase, $TestCases}
];

(* Exit with appropriate status *)
If[$RegressionResult === $Failed,
  Exit[1],
  Exit[0]
];
