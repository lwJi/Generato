(* ::Package:: *)

(* AllTests.wl *)
(* Master test runner for Generato *)
(* Usage: wolframscript -f test/AllTests.wl *)

$TestDir = DirectoryName[$InputFileName];

(* Quiet mode support *)
$QuietMode = (Environment["QUIET"] === "1");
QuietPrint[args___] := If[!$QuietMode, Print[args]];
(* Suppress all Print output during package loading in quiet mode *)
QuietGet[file_] := If[$QuietMode, Block[{Print}, Get[file]], Get[file]];

(* Phase tracking for quiet mode *)
$PhaseSuccess = True;

RunPhase[phaseName_String, phaseCode_] := Module[{},
  $PhaseSuccess = True;
  Check[phaseCode, $PhaseSuccess = False];
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
    ];
  ];
];

(* ========================================= *)
(* PHASE 2: Golden File Regression Tests *)
(* ========================================= *)

(* Load test cases from config file *)
$ConfigFile = FileNameJoin[{$TestDir, "test_cases.txt"}];
$TestCases = Select[
  StringSplit[#, ":"] & /@ Import[$ConfigFile, "Lines"],
  Length[#] == 3 && !StringStartsQ[#[[1]], "#"] &
];

(* Validate shell input to prevent injection *)
ValidShellInput[str_String] := StringMatchQ[str, RegularExpression["^[a-zA-Z0-9_./\\-]+$"]];

RunRegressionTests[] := Module[{backend, testName, ext, testFile, result, outputFile, quietPrefix},
  QuietPrint["--- Regression Tests (Golden Files) ---"];
  QuietPrint[""];

  (* Generate outputs for each test case *)
  QuietPrint["Generating test outputs..."];
  QuietPrint[""];

  (* Pass QUIET to subprocess if in quiet mode *)
  quietPrefix = If[$QuietMode, "QUIET=1 ", ""];

  $GenerationFailed = False;
  Do[
    {backend, testName, ext} = testCase;
    testFile = FileNameJoin[{$TestDir, backend, testName <> ".wl"}];

    If[FileExistsQ[testFile],
      (* Validate inputs before shell execution *)
      If[!ValidShellInput[backend] || !ValidShellInput[testName],
        QuietPrint["  ERROR: Invalid characters in backend or testName"];
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
      Check[
        DeleteFile[outputFile],
        QuietPrint["  Warning: Failed to delete ", outputFile]
      ];
    ],
    {testCase, $TestCases}
  ];

  (* Mark phase as failed if regression detected *)
  If[$RegressionResult === $Failed,
    $PhaseSuccess = False;
  ];
];

(* Run the test phases *)
$UnitTestsSuccess = RunPhase["unit", RunUnitTests[]];
$RegressionTestsSuccess = RunPhase["regression", RunRegressionTests[]];

(* Exit with appropriate status *)
If[!$UnitTestsSuccess || !$RegressionTestsSuccess,
  Exit[1],
  Exit[0]
];
