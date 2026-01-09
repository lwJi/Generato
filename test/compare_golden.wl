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

(* Status tags with ANSI colors - access from Global if available, else define locally *)
$TagPass = If[ValueQ[Global`$TagPass], Global`$TagPass, "\033[0;32m[PASS]\033[0m"];
$TagFail = If[ValueQ[Global`$TagFail], Global`$TagFail, "\033[0;31m[FAIL]\033[0m"];

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
    GoldenPrint["  ", $TagFail, " Current file not found: ", currentFile];
    Return[$Failed]
  ];

  If[!FileExistsQ[goldenFile],
    GoldenPrint["  ", $TagFail, " Golden file not found: ", goldenFile];
    Return[$Failed]
  ];

  current = Import[currentFile, "Text"];
  golden = Import[goldenFile, "Text"];

  If[current === golden,
    GoldenPrint["  ", $TagPass, " ", FileNameTake[currentFile]];
    True,
    GoldenPrint["  ", $TagFail, " ", FileNameTake[currentFile], " differs from golden"];
    GoldenPrint["         Current: ", StringLength[current], " chars"];
    GoldenPrint["         Golden:  ", StringLength[golden], " chars"];
    (* Always print failure details even in quiet mode *)
    If[ValueQ[Global`$QuietMode] && Global`$QuietMode === True,
      Print["  ", $TagFail, " ", FileNameTake[currentFile], " (", StringLength[current], " vs ", StringLength[golden], " chars)"]
    ];
    $Failed
  ]
];

(* Run all golden file comparisons *)
RunGoldenTests[] := Module[
  {results, passed, failed, backend, testName, ext, currentFile, goldenFile},

  GoldenPrint[""];
  GoldenPrint["  Comparing against golden files..."];
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
  GoldenPrint["  Golden Tests Summary: ", passed, "/", Length[results], " passed"];

  If[failed > 0,
    GoldenPrint[""];
    GoldenPrint["  ", $TagFail, " REGRESSION DETECTED"];
    $Failed,
    True
  ]
];

End[];
EndPackage[];
