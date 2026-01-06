(* ::Package:: *)

(* compare_golden.wl *)
(* Golden file comparison for regression testing *)
(* (c) Generato Test Suite *)

BeginPackage["GoldenTest`"];

CompareGoldenFile::usage = "CompareGoldenFile[currentFile, goldenFile] compares a generated file against its golden reference.";
RunGoldenTests::usage = "RunGoldenTests[] runs all golden file comparisons and returns a summary.";

Begin["`Private`"];

$TestDir = DirectoryName[$InputFileName];
$GoldenDir = FileNameJoin[{$TestDir, "..", "golden"}];

(* Compare two files, return True if identical *)
CompareGoldenFile[currentFile_String, goldenFile_String] := Module[
  {current, golden, result},

  If[!FileExistsQ[currentFile],
    Print["FAIL: Current file not found: ", currentFile];
    Return[$Failed]
  ];

  If[!FileExistsQ[goldenFile],
    Print["FAIL: Golden file not found: ", goldenFile];
    Return[$Failed]
  ];

  current = Import[currentFile, "Text"];
  golden = Import[goldenFile, "Text"];

  If[current === golden,
    Print["PASS: ", FileNameTake[currentFile]];
    True,
    Print["FAIL: ", FileNameTake[currentFile], " differs from golden"];
    Print["  Current: ", StringLength[current], " chars"];
    Print["  Golden:  ", StringLength[golden], " chars"];
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

  Print["=== Golden File Regression Tests ==="];
  Print[""];

  results = Table[
    {backend, testName, ext} = testCase;
    currentFile = FileNameJoin[{$TestDir, "..", backend, testName <> ext}];
    goldenFile = FileNameJoin[{$GoldenDir, backend, testName <> ext <> ".golden"}];
    CompareGoldenFile[currentFile, goldenFile],
    {testCase, $TestCases}
  ];

  passed = Count[results, True];
  failed = Count[results, $Failed];

  Print[""];
  Print["=== Summary ==="];
  Print["Passed: ", passed, "/", Length[results]];
  Print["Failed: ", failed, "/", Length[results]];

  If[failed > 0,
    Print[""];
    Print["REGRESSION DETECTED"];
    If[Length[$ScriptCommandLine] > 0 && MemberQ[StringContainsQ[#, "compare_golden.wl"] & /@ $ScriptCommandLine, True],
      Exit[1],
      $Failed
    ],
    Print[""];
    Print["ALL TESTS PASSED"];
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
