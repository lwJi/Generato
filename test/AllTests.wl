(* ::Package:: *)

(* AllTests.wl *)
(* Master test runner for Generato *)
(* Usage: wolframscript -script test/AllTests.wl [--verbose] [--unit-only] [--generate] *)

$TestDir = DirectoryName[$InputFileName];

(* Quiet mode support - use --verbose flag to enable verbose output *)
$QuietMode = !MemberQ[$CommandLine, "--verbose"];
(* Unit-only mode - use --unit-only flag to skip regression tests *)
$UnitOnlyMode = MemberQ[$CommandLine, "--unit-only"];
(* Generate mode - use --generate flag to update golden files *)
$GenerateMode = MemberQ[$CommandLine, "--generate"];
QuietPrint[args___] := If[!$QuietMode, Print[args]];
(* Suppress all Print output during package loading in quiet mode *)
QuietGet[file_] := Block[{Print}, Get[file]];

(* Load shared test configuration *)
Get[FileNameJoin[{$TestDir, "TestConfig.wl"}]];

(* Phase tracking for quiet mode *)
$PhaseSuccess = True;

RunPhase[phaseName_String, phaseCode_] := Module[{},
  $PhaseSuccess = True;
  (* Evaluate the phase code - $PhaseSuccess is set by the phase itself on failure *)
  phaseCode;
  If[$QuietMode,
    If[$PhaseSuccess,
      Print["\033[0;32mPASS: " <> phaseName <> "\033[0m"],
      Print["\033[0;31mFAIL: " <> phaseName <> "\033[0m"]
    ]
  ];
  $PhaseSuccess
];
SetAttributes[RunPhase, HoldAll];

QuietPrint[""];
QuietPrint["========================================"];
QuietPrint["  Generato Test Suite"];
QuietPrint["========================================"];
QuietPrint[""];

(* Collect all verification tests *)
$AllTests = {};

(* ========================================= *)
(* PHASE 1: Unit Tests *)
(* ========================================= *)

RunUnitTests[] := Module[{unitTestFiles, report},
  QuietPrint["--- Unit Tests ---"];
  QuietPrint[""];

  unitTestFiles = FileNames["*.wl", FileNameJoin[{$TestDir, "unit"}]];

  If[Length[unitTestFiles] > 0,
    Do[
      QuietPrint["Running: ", FileNameTake[file]];
      QuietGet[file],
      {file, unitTestFiles}
    ],
    QuietPrint["No unit test files found in test/unit/"]
  ];

  QuietPrint[""];

  (* Generate test report if VerificationTests were run *)
  If[Length[$AllTests] > 0,
    QuietPrint["Generating TestReport..."];
    report = TestReport[$AllTests];
    QuietPrint["Tests Passed: ", report["TestsSucceededCount"], "/", report["TestsSucceededCount"] + report["TestsFailedCount"]];
    QuietPrint[""];
    (* Mark phase as failed if any tests failed *)
    If[report["TestsFailedCount"] > 0,
      $PhaseSuccess = False;
      (* Print detailed failure information - always show errors even in quiet mode *)
      (* Flatten all failed test categories (WrongResults, WithMessages, WithErrors) *)
      Module[{failedAssocs, allFailedTests},
        failedAssocs = Values[report["TestsFailed"]];
        allFailedTests = Flatten[Values /@ failedAssocs];
        Print[""];
        Print["=== UNIT TEST FAILURES ==="];
        Print[""];
        Do[
          Module[{testResult, testID, actual, expected, outcome},
            testResult = allFailedTests[[i]];
            testID = testResult["TestID"];
            outcome = testResult["Outcome"];
            actual = testResult["ActualOutput"];
            expected = testResult["ExpectedOutput"];
            Print["FAILED: ", testID];
            Print["  Outcome:  ", outcome];
            Print["  Expected: ", expected];
            Print["  Actual:   ", actual];
            Print[""];
          ],
          {i, Length[allFailedTests]}
        ];
      ];
    ];
  ];
];

(* ========================================= *)
(* PHASE 2: Golden File Regression Tests *)
(* ========================================= *)

(* Load test cases from shared config *)
$TestCases = TestConfig`LoadTestCases[];

(* Validate shell input to prevent injection *)
ValidShellInput[str_String] := StringMatchQ[str, RegularExpression["^[a-zA-Z0-9_./\\-]+$"]];

(* Validate golden files exist before running tests *)
ValidateGoldenFiles[] := Module[{backend, testName, ext, goldenFile, missing},
  missing = {};
  Do[
    {backend, testName, ext} = testCase;
    goldenFile = FileNameJoin[{$TestDir, "golden", backend, testName <> ext <> ".golden"}];
    If[!FileExistsQ[goldenFile],
      AppendTo[missing, goldenFile];
    ],
    {testCase, $TestCases}
  ];
  If[Length[missing] > 0,
    QuietPrint["WARNING: Missing golden files:"];
    Do[QuietPrint["  ", f], {f, missing}];
  ];
  missing
];

(* Generate golden files from current outputs *)
GenerateGoldenFiles[] := Module[{backend, testName, ext, testFile, result, outputFile, goldenFile, goldenDir, quietPrefix},
  Print["--- Generating Golden Files ---"];
  Print[""];

  quietPrefix = "QUIET=1 ";

  Do[
    {backend, testName, ext} = testCase;
    testFile = FileNameJoin[{$TestDir, backend, testName <> ".wl"}];
    outputFile = FileNameJoin[{$TestDir, backend, testName <> ext}];
    goldenFile = FileNameJoin[{$TestDir, "golden", backend, testName <> ext <> ".golden"}];
    goldenDir = DirectoryName[goldenFile];

    If[FileExistsQ[testFile],
      (* Validate inputs before shell execution *)
      If[!ValidShellInput[backend] || !ValidShellInput[testName],
        Print["ERROR: Invalid characters in backend or testName"];
        Continue[];
      ];

      Print["  Generating: ", backend, "/", testName, ".wl"];
      result = Run["cd " <> FileNameJoin[{$TestDir, backend}] <> " && " <> quietPrefix <> "\"$GENERATO/Generato\" " <> testName <> ".wl 2>&1"];
      If[result != 0,
        Print["    \033[0;31mFAILED\033[0m to generate ", testName, ext];
        Continue[];
      ];

      (* Create golden directory if needed *)
      If[!DirectoryQ[goldenDir],
        CreateDirectory[goldenDir];
      ];

      (* Copy output to golden file *)
      If[FileExistsQ[outputFile],
        CopyFile[outputFile, goldenFile, OverwriteTarget -> True];
        Print["    \033[0;32mUpdated\033[0m: ", goldenFile];
        (* Clean up generated file *)
        DeleteFile[outputFile];
      ],
      Print["  SKIP: ", testFile, " not found"];
    ],
    {testCase, $TestCases}
  ];

  Print[""];
  Print["Golden file generation complete."];
];

RunRegressionTests[] := Module[{backend, testName, ext, testFile, result, outputFile, quietPrefix, missingGolden},
  QuietPrint["--- Regression Tests (Golden Files) ---"];
  QuietPrint[""];

  (* Validate golden files first *)
  missingGolden = ValidateGoldenFiles[];
  If[Length[missingGolden] > 0,
    QuietPrint["Run with --generate to create missing golden files"];
    QuietPrint[""];
  ];

  (* Generate outputs for each test case *)
  QuietPrint["Generating test outputs..."];
  QuietPrint[""];

  (* Always suppress package loading messages in subprocess *)
  quietPrefix = "QUIET=1 ";

  $GenerationFailed = False;
  Do[
    {backend, testName, ext} = testCase;
    testFile = FileNameJoin[{$TestDir, backend, testName <> ".wl"}];

    If[FileExistsQ[testFile],
      (* Validate inputs before shell execution *)
      If[!ValidShellInput[backend] || !ValidShellInput[testName],
        QuietPrint["ERROR: Invalid characters in backend or testName"];
        $GenerationFailed = True;
        Continue[];
      ];

      QuietPrint["  Generating: ", backend, "/", testName, ".wl"];
      (* Run Generato from the test directory, passing QUIET mode *)
      result = Run["cd " <> FileNameJoin[{$TestDir, backend}] <> " && " <> quietPrefix <> "\"$GENERATO/Generato\" " <> testName <> ".wl 2>&1"];
      If[result != 0,
        QuietPrint["    FAILED to generate ", testName, ext];
        $GenerationFailed = True;
      ],
      QuietPrint["  SKIP: ", testFile, " not found"];
    ],
    {testCase, $TestCases}
  ];

  QuietPrint[""];

  If[$GenerationFailed,
    QuietPrint["ERROR: Some outputs failed to generate"];
    $PhaseSuccess = False;
    Return[];
  ];

  (* Load golden file comparison module *)
  QuietGet[FileNameJoin[{$TestDir, "regression", "compare_golden.wl"}]];

  (* Run golden file comparisons *)
  $RegressionResult = GoldenTest`RunGoldenTests[];

  (* Cleanup generated output files *)
  QuietPrint[""];
  QuietPrint["Cleaning up generated files..."];
  Do[
    {backend, testName, ext} = testCase;
    outputFile = FileNameJoin[{$TestDir, backend, testName <> ext}];
    If[FileExistsQ[outputFile],
      Quiet[
        DeleteFile[outputFile],
        DeleteFile::fdnfnd  (* Suppress "file doesn't exist" which is fine *)
      ];
      If[FileExistsQ[outputFile],
        QuietPrint["WARNING: Failed to delete ", outputFile];
      ];
    ],
    {testCase, $TestCases}
  ];

  (* Mark phase as failed if regression detected *)
  If[$RegressionResult === $Failed,
    $PhaseSuccess = False;
  ];
];

(* Run the test phases or generate golden files *)
If[$GenerateMode,
  (* Generate mode: update golden files *)
  GenerateGoldenFiles[];
  Exit[0],
  (* Test mode: run unit and regression tests *)
  $UnitTestsSuccess = RunPhase["unit", RunUnitTests[]];
  $RegressionTestsSuccess = If[$UnitOnlyMode,
    True,
    RunPhase["regression", RunRegressionTests[]]
  ];

  (* Exit with appropriate status *)
  If[!$UnitTestsSuccess || !$RegressionTestsSuccess,
    Exit[1],
    Exit[0]
  ]
];
