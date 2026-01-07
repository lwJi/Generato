(* ::Package:: *)

(* AllTests.wl *)
(* Master test runner for Generato *)
(* Usage: wolframscript -script test/AllTests.wl [-quiet] *)

$TestDir = DirectoryName[$InputFileName];

(* Parse command line for quiet mode *)
$QuietMode = MemberQ[$ScriptCommandLine, "-quiet"];

(* Conditional print - only prints when not in quiet mode *)
QPrint[args___] := If[!$QuietMode, Print[args]];

(* Failure message collection for quiet mode output *)
$FailureMessages = {};
AppendFailure[msg_String] := AppendTo[$FailureMessages, msg];

QPrint[""];
QPrint["========================================"];
QPrint["  Generato Test Suite"];
QPrint["========================================"];
QPrint[""];

(* Collect all verification tests *)
$AllTests = {};

(* ========================================= *)
(* PHASE 1: Unit Tests *)
(* ========================================= *)

QPrint["--- Unit Tests ---"];
QPrint[""];

unitTestFiles = FileNames["*.wl", FileNameJoin[{$TestDir, "unit"}]];

If[Length[unitTestFiles] > 0,
  Do[
    QPrint["Running: ", FileNameTake[file]];
    (* In quiet mode, suppress Print output from unit tests *)
    If[$QuietMode,
      Block[{Print = Null &}, Get[file]],
      Get[file]
    ],
    {file, unitTestFiles}
  ],
  QPrint["No unit test files found in test/unit/"]
];

QPrint[""];

(* Generate test report if VerificationTests were run *)
If[Length[$AllTests] > 0,
  QPrint["Generating TestReport..."];
  report = TestReport[$AllTests];
  QPrint["Tests Passed: ", report["TestsSucceededCount"], "/", report["TestsSucceededCount"] + report["TestsFailedCount"]];
  QPrint[""];
];

(* ========================================= *)
(* PHASE 2: Golden File Regression Tests *)
(* ========================================= *)

QPrint["--- Regression Tests (Golden Files) ---"];
QPrint[""];

(* Load test cases from config file *)
$ConfigFile = FileNameJoin[{$TestDir, "test_cases.txt"}];
$TestCases = Select[
  StringSplit[#, ":"] & /@ Import[$ConfigFile, "Lines"],
  Length[#] == 3 && !StringStartsQ[#[[1]], "#"] &
];

(* Validate shell input to prevent injection *)
ValidShellInput[str_String] := StringMatchQ[str, RegularExpression["^[a-zA-Z0-9_./\\-]+$"]];

(* Generate outputs for each test case *)
QPrint["Generating test outputs..."];
QPrint[""];

$GenerationFailed = False;
Do[
  {backend, testName, ext} = testCase;
  testFile = FileNameJoin[{$TestDir, backend, testName <> ".wl"}];

  If[FileExistsQ[testFile],
    (* Validate inputs before shell execution *)
    If[!ValidShellInput[backend] || !ValidShellInput[testName],
      QPrint["  ERROR: Invalid characters in backend or testName"];
      AppendFailure["FAILED: Invalid characters in " <> backend <> "/" <> testName];
      $GenerationFailed = True;
      Continue[];
    ];

    QPrint["  Generating: ", backend, "/", testName, ".wl"];
    (* Run Generato from the test directory *)
    (* In quiet mode, suppress subprocess output *)
    result = If[$QuietMode,
      Run["cd " <> FileNameJoin[{$TestDir, backend}] <> " && \"$GENERATO/Generato\" " <> testName <> ".wl > /dev/null 2>&1"],
      Run["cd " <> FileNameJoin[{$TestDir, backend}] <> " && \"$GENERATO/Generato\" " <> testName <> ".wl 2>&1"]
    ];
    If[result != 0,
      QPrint["    FAILED to generate ", testName, ext];
      AppendFailure["FAILED to generate " <> testName <> ext];
      $GenerationFailed = True;
    ],
    QPrint["  SKIP: ", testFile, " not found"];
  ],
  {testCase, $TestCases}
];

QPrint[""];

If[$GenerationFailed,
  QPrint["ERROR: Some outputs failed to generate"];
  If[$QuietMode,
    Print["✗ tests"];
    Print /@ $FailureMessages;
  ];
  Exit[1]
];

(* Load golden file comparison module *)
Get[FileNameJoin[{$TestDir, "regression", "compare_golden.wl"}]];

(* Run golden file comparisons *)
$RegressionResult = GoldenTest`RunGoldenTests[];

(* Cleanup generated output files *)
QPrint[""];
QPrint["Cleaning up generated files..."];
Do[
  {backend, testName, ext} = testCase;
  outputFile = FileNameJoin[{$TestDir, backend, testName <> ext}];
  If[FileExistsQ[outputFile],
    Check[
      DeleteFile[outputFile],
      QPrint["  Warning: Failed to delete ", outputFile]
    ];
  ],
  {testCase, $TestCases}
];

(* Exit with appropriate status *)
If[$RegressionResult === $Failed,
  If[$QuietMode,
    Print["✗ tests"];
    Print /@ $FailureMessages;
  ];
  Exit[1],
  If[$QuietMode,
    Print["✓ tests"];
  ];
  Exit[0]
];
