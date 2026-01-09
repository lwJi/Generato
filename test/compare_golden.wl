(* ::Package:: *)

(* compare_golden.wl *)
(* Golden file comparison for regression testing *)
(* (c) Generato Test Suite *)

BeginPackage["GoldenTest`"];

CompareGoldenFile::usage = "CompareGoldenFile[currentFile, goldenFile] compares a generated file against its golden reference.";
RunGoldenTests::usage = "RunGoldenTests[] runs all golden file comparisons and returns a summary.";

Begin["`Private`"];

(* Load shared test configuration *)
Get[FileNameJoin[{DirectoryName[$InputFileName], "TestConfig.wl"}]];

(* Use QuietPrint if available (from AllTests.wl), otherwise use Print *)
GoldenPrint[args___] := If[ValueQ[Global`$QuietMode] && Global`$QuietMode === True,
  Null,  (* Suppress output in quiet mode *)
  Print[args]
];

$TestDir = TestConfig`GetTestDir[];
$GoldenDir = FileNameJoin[{$TestDir, "regression", "golden"}];

(* Compare two files, return True if identical *)
CompareGoldenFile[currentFile_String, goldenFile_String] := Module[
  {current, golden, result},

  If[!FileExistsQ[currentFile],
    GoldenPrint["FAIL: Current file not found: ", currentFile];
    Return[$Failed]
  ];

  If[!FileExistsQ[goldenFile],
    GoldenPrint["FAIL: Golden file not found: ", goldenFile];
    Return[$Failed]
  ];

  current = Import[currentFile, "Text"];
  golden = Import[goldenFile, "Text"];

  If[current === golden,
    GoldenPrint["PASS: ", FileNameTake[currentFile]];
    True,
    GoldenPrint["FAIL: ", FileNameTake[currentFile], " differs from golden"];
    GoldenPrint["  Current: ", StringLength[current], " chars"];
    GoldenPrint["  Golden:  ", StringLength[golden], " chars"];
    (* Always print failure details even in quiet mode *)
    If[ValueQ[Global`$QuietMode] && Global`$QuietMode === True,
      Print["  FAIL: ", FileNameTake[currentFile], " (", StringLength[current], " vs ", StringLength[golden], " chars)"]
    ];
    $Failed
  ]
];

(* Run all golden file comparisons *)
RunGoldenTests[] := Module[
  {results, passed, failed, backend, testName, ext, currentFile, goldenFile},

  GoldenPrint["=== Golden File Regression Tests ==="];
  GoldenPrint[""];

  results = Table[
    {backend, testName, ext} = testCase;
    currentFile = FileNameJoin[{$TestDir, "regression", backend, testName <> ext}];
    goldenFile = FileNameJoin[{$GoldenDir, backend, testName <> ext <> ".golden"}];
    CompareGoldenFile[currentFile, goldenFile],
    {testCase, TestConfig`LoadTestCases[]}
  ];

  passed = Count[results, True];
  failed = Count[results, $Failed];

  GoldenPrint[""];
  GoldenPrint["=== Summary ==="];
  GoldenPrint["Passed: ", passed, "/", Length[results]];
  GoldenPrint["Failed: ", failed, "/", Length[results]];

  If[failed > 0,
    GoldenPrint[""];
    GoldenPrint["REGRESSION DETECTED"];
    $Failed,
    GoldenPrint[""];
    GoldenPrint["ALL TESTS PASSED"];
    True
  ]
];

End[];
EndPackage[];
