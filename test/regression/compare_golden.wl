(* ::Package:: *)

(* compare_golden.wl *)
(* Golden file comparison for regression testing *)
(* (c) Generato Test Suite *)

BeginPackage["GoldenTest`"];

CompareGoldenFile::usage = "CompareGoldenFile[currentFile, goldenFile] compares a generated file against its golden reference.";
RunGoldenTests::usage = "RunGoldenTests[] runs all golden file comparisons and returns a summary.";

Begin["`Private`"];

(* Use QuietPrint if available (from AllTests.wl), otherwise use Print *)
GoldenPrint[args___] := If[ValueQ[Global`$QuietMode] && Global`$QuietMode === True,
  Null,  (* Suppress output in quiet mode *)
  Print[args]
];

$TestDir = DirectoryName[$InputFileName];
$GoldenDir = FileNameJoin[{$TestDir, "..", "golden"}];

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
    $Failed
  ]
];

(* Load test cases from shared config file *)
$ConfigFile = FileNameJoin[{$TestDir, "..", "test_cases.txt"}];
$TestCases = Select[
  StringSplit[#, ":"] & /@ Import[$ConfigFile, "Lines"],
  Length[#] == 3 && !StringStartsQ[#[[1]], "#"] &
];

(* Run all golden file comparisons *)
RunGoldenTests[] := Module[
  {results, passed, failed, backend, testName, ext, currentFile, goldenFile},

  GoldenPrint["=== Golden File Regression Tests ==="];
  GoldenPrint[""];

  results = Table[
    {backend, testName, ext} = testCase;
    currentFile = FileNameJoin[{$TestDir, "..", backend, testName <> ext}];
    goldenFile = FileNameJoin[{$GoldenDir, backend, testName <> ext <> ".golden"}];
    CompareGoldenFile[currentFile, goldenFile],
    {testCase, $TestCases}
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
    If[Length[$ScriptCommandLine] > 0 && MemberQ[StringContainsQ[#, "compare_golden.wl"] & /@ $ScriptCommandLine, True],
      Exit[1],
      $Failed
    ],
    GoldenPrint[""];
    GoldenPrint["ALL TESTS PASSED"];
    If[Length[$ScriptCommandLine] > 0 && MemberQ[StringContainsQ[#, "compare_golden.wl"] & /@ $ScriptCommandLine, True],
      Exit[0],
      True
    ]
  ]
];

End[];
EndPackage[];

(* Run tests only when executed directly (not when loaded via Get[]) *)
If[Length[$ScriptCommandLine] > 0 && MemberQ[StringContainsQ[#, "compare_golden.wl"] & /@ $ScriptCommandLine, True],
  RunGoldenTests[]
];
