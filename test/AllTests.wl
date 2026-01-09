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

(* ========================================= *)
(* Formatting Helpers for Beautified Output  *)
(* ========================================= *)

(* ASCII box characters *)
$BoxLine = "+==========================================+";
$BoxSide = "|";

(* Status tags with ANSI colors *)
$TagPass = "\033[0;32m[PASS]\033[0m";
$TagFail = "\033[0;31m[FAIL]\033[0m";
$TagRun  = "\033[0;33m[RUN ]\033[0m";
$TagWarn = "\033[0;33m[WARN]\033[0m";
$TagSkip = "\033[0;36m[SKIP]\033[0m";

(* Section header helper - creates "-- Title ----..." *)
PrintSection[title_String] := Module[{dashes, totalWidth},
  totalWidth = 50;
  dashes = StringJoin[Table["-", {Max[1, totalWidth - StringLength[title] - 4]}]];
  QuietPrint[""];
  QuietPrint["-- ", title, " ", dashes]
];

(* Centered box line helper *)
PrintBoxLine[text_String] := Module[{padding, leftPad, rightPad, boxWidth},
  boxWidth = 40;
  padding = boxWidth - StringLength[text];
  leftPad = Floor[padding/2];
  rightPad = Ceiling[padding/2];
  QuietPrint[$BoxSide, StringJoin[Table[" ", {leftPad}]], text, StringJoin[Table[" ", {rightPad}]], $BoxSide]
];

(* Print the header box *)
PrintHeader[] := Module[{},
  QuietPrint[""];
  QuietPrint[$BoxLine];
  PrintBoxLine["Generato Test Suite"];
  PrintBoxLine["--verbose mode"];
  QuietPrint[$BoxLine];
  QuietPrint[""]
];

(* Print final result box - handles ANSI color codes in text *)
PrintResultBoxLine[text_String, visibleLen_Integer] := Module[{padding, leftPad, rightPad, boxWidth},
  boxWidth = 40;
  padding = boxWidth - visibleLen;
  leftPad = Floor[padding/2];
  rightPad = Ceiling[padding/2];
  QuietPrint[$BoxSide, StringJoin[Table[" ", {leftPad}]], text, StringJoin[Table[" ", {rightPad}]], $BoxSide]
];

PrintResultBox[passed_] := Module[{},
  QuietPrint[""];
  QuietPrint[$BoxLine];
  If[passed,
    PrintResultBoxLine["\033[0;32mALL TESTS PASSED\033[0m", 15],  (* "ALL TESTS PASSED" = 15 chars *)
    PrintResultBoxLine["\033[0;31mSOME TESTS FAILED\033[0m", 17]  (* "SOME TESTS FAILED" = 17 chars *)
  ];
  QuietPrint[$BoxLine];
  QuietPrint[""]
];

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

PrintHeader[];

(* Collect all verification tests *)
$AllTests = {};

(* ========================================= *)
(* PHASE 1: Unit Tests *)
(* ========================================= *)

RunUnitTests[] := Module[{unitTestFiles, report},
  PrintSection["Unit Tests"];
  QuietPrint[""];

  unitTestFiles = FileNames["*.wl", FileNameJoin[{$TestDir, "unit"}]];

  If[Length[unitTestFiles] > 0,
    Do[
      QuietPrint["  ", $TagRun, " ", FileNameTake[file]];
      QuietGet[file];
      QuietPrint["  ", $TagPass, " ", FileNameTake[file]],
      {file, unitTestFiles}
    ],
    QuietPrint["  ", $TagWarn, " No unit test files found in test/unit/"]
  ];

  QuietPrint[""];

  (* Generate test report if VerificationTests were run *)
  If[Length[$AllTests] > 0,
    report = TestReport[$AllTests];
    QuietPrint["  Unit Tests Summary: ", report["TestsSucceededCount"], "/", report["TestsSucceededCount"] + report["TestsFailedCount"], " passed"];
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
        Print["-- Failures ", StringJoin[Table["-", {38}]]];
        Print[""];
        Do[
          Module[{testResult, testID, actual, expected, outcome},
            testResult = allFailedTests[[i]];
            testID = testResult["TestID"];
            outcome = testResult["Outcome"];
            actual = testResult["ActualOutput"];
            expected = testResult["ExpectedOutput"];
            Print["  ", $TagFail, " ", testID];
            Print["         Outcome:  ", outcome];
            Print["         Expected: ", expected];
            Print["         Actual:   ", actual];
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

(* Abort if test configuration failed to load *)
If[$TestCases === $Failed,
  Print["FATAL: Failed to load test configuration"];
  Exit[1]
];

(* Validate shell input to prevent injection *)
ValidShellInput[str_String] := StringMatchQ[str, RegularExpression["^[a-zA-Z0-9_./\\-]+$"]];

(* Validate golden files exist before running tests *)
ValidateGoldenFiles[] := Module[{backend, testName, ext, goldenFile, missing},
  missing = {};
  Do[
    {backend, testName, ext} = testCase;
    goldenFile = FileNameJoin[{$TestDir, "regression", "golden", backend, testName <> ext <> ".golden"}];
    If[!FileExistsQ[goldenFile],
      AppendTo[missing, goldenFile];
    ],
    {testCase, $TestCases}
  ];
  If[Length[missing] > 0,
    QuietPrint["  ", $TagWarn, " Missing golden files:"];
    Do[QuietPrint["           ", FileNameTake[f]], {f, missing}];
  ];
  missing
];

(* Generate golden files from current outputs *)
GenerateGoldenFiles[] := Module[{backend, testName, ext, testFile, result, outputFile, goldenFile, goldenDir, quietPrefix},
  Print[""];
  Print["-- Generating Golden Files ", StringJoin[Table["-", {28}]]];
  Print[""];

  quietPrefix = "QUIET=1 ";

  Do[
    {backend, testName, ext} = testCase;
    testFile = FileNameJoin[{$TestDir, "regression", backend, testName <> ".wl"}];
    outputFile = FileNameJoin[{$TestDir, "regression", backend, testName <> ext}];
    goldenFile = FileNameJoin[{$TestDir, "regression", "golden", backend, testName <> ext <> ".golden"}];
    goldenDir = DirectoryName[goldenFile];

    If[FileExistsQ[testFile],
      (* Validate inputs before shell execution *)
      If[!ValidShellInput[backend] || !ValidShellInput[testName],
        Print["  ", $TagFail, " Invalid characters in backend or testName"];
        Continue[];
      ];

      Print["  ", $TagRun, " ", backend, "/", testName, ".wl"];
      result = Run["cd " <> FileNameJoin[{$TestDir, "regression", backend}] <> " && " <> quietPrefix <> "\"$GENERATO/Generato\" " <> testName <> ".wl 2>&1"];
      If[result != 0,
        Print["  ", $TagFail, " Failed to generate ", testName, ext];
        Continue[];
      ];

      (* Create golden directory if needed *)
      If[!DirectoryQ[goldenDir],
        CreateDirectory[goldenDir];
      ];

      (* Copy output to golden file *)
      If[FileExistsQ[outputFile],
        CopyFile[outputFile, goldenFile, OverwriteTarget -> True];
        Print["  ", $TagPass, " Updated: ", FileNameTake[goldenFile]];
        (* Clean up generated file *)
        DeleteFile[outputFile];
      ],
      Print["  ", $TagSkip, " ", testFile, " not found"];
    ],
    {testCase, $TestCases}
  ];

  Print[""];
  Print["  Golden file generation complete."];
  Print[""];
];

RunRegressionTests[] := Module[{backend, testName, ext, testFile, result, outputFile, quietPrefix, missingGolden},
  PrintSection["Regression Tests (Golden Files)"];
  QuietPrint[""];

  (* Validate golden files first *)
  missingGolden = ValidateGoldenFiles[];
  If[Length[missingGolden] > 0,
    QuietPrint["           Run with --generate to create missing golden files"];
    QuietPrint[""];
  ];

  (* Generate outputs for each test case *)
  QuietPrint["  Generating test outputs..."];
  QuietPrint[""];

  (* Always suppress package loading messages in subprocess *)
  quietPrefix = "QUIET=1 ";

  $GenerationFailed = False;
  Do[
    {backend, testName, ext} = testCase;
    testFile = FileNameJoin[{$TestDir, "regression", backend, testName <> ".wl"}];

    If[FileExistsQ[testFile],
      (* Validate inputs before shell execution *)
      If[!ValidShellInput[backend] || !ValidShellInput[testName],
        QuietPrint["  ", $TagFail, " Invalid characters in backend or testName"];
        $GenerationFailed = True;
        Continue[];
      ];

      QuietPrint["  ", $TagRun, " ", backend, "/", testName, ".wl"];
      (* Run Generato from the test directory, passing QUIET mode *)
      result = Run["cd " <> FileNameJoin[{$TestDir, "regression", backend}] <> " && " <> quietPrefix <> "\"$GENERATO/Generato\" " <> testName <> ".wl 2>&1"];
      If[result != 0,
        QuietPrint["  ", $TagFail, " Failed to generate ", testName, ext];
        $GenerationFailed = True,
        QuietPrint["  ", $TagPass, " ", backend, "/", testName, ".wl"]
      ],
      QuietPrint["  ", $TagSkip, " ", testFile, " not found"];
    ],
    {testCase, $TestCases}
  ];

  QuietPrint[""];

  If[$GenerationFailed,
    QuietPrint["  ", $TagFail, " Some outputs failed to generate"];
    $PhaseSuccess = False;
    Return[];
  ];

  (* Load golden file comparison module *)
  QuietGet[FileNameJoin[{$TestDir, "compare_golden.wl"}]];

  (* Run golden file comparisons *)
  $RegressionResult = GoldenTest`RunGoldenTests[];

  (* Cleanup generated output files *)
  QuietPrint[""];
  QuietPrint["  Cleaning up generated files..."];
  Do[
    {backend, testName, ext} = testCase;
    outputFile = FileNameJoin[{$TestDir, "regression", backend, testName <> ext}];
    If[FileExistsQ[outputFile],
      Quiet[
        DeleteFile[outputFile],
        DeleteFile::fdnfnd  (* Suppress "file doesn't exist" which is fine *)
      ];
      If[FileExistsQ[outputFile],
        QuietPrint["  ", $TagWarn, " Failed to delete ", outputFile];
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

  (* Print final result box *)
  PrintResultBox[$UnitTestsSuccess && $RegressionTestsSuccess];

  (* Exit with appropriate status *)
  If[!$UnitTestsSuccess || !$RegressionTestsSuccess,
    Exit[1],
    Exit[0]
  ]
];
