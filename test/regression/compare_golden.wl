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

(* Check if quiet mode is set by parent (AllTests.wl) *)
$LocalQuietMode = If[ValueQ[Global`$QuietMode], Global`$QuietMode, False];

(* Conditional print - only prints when not in quiet mode *)
QPrint[args___] := If[!$LocalQuietMode, Print[args]];

(* Add failure to parent's failure list if available *)
AddFailure[msg_String] := If[ValueQ[Global`$FailureMessages],
  AppendTo[Global`$FailureMessages, msg]
];

(* Compare two files, return True if identical *)
CompareGoldenFile[currentFile_String, goldenFile_String] := Module[
  {current, golden, result, failMsg},

  If[!FileExistsQ[currentFile],
    failMsg = "FAIL: Current file not found: " <> currentFile;
    QPrint[failMsg];
    AddFailure[failMsg];
    Return[$Failed]
  ];

  If[!FileExistsQ[goldenFile],
    failMsg = "FAIL: Golden file not found: " <> goldenFile;
    QPrint[failMsg];
    AddFailure[failMsg];
    Return[$Failed]
  ];

  current = Import[currentFile, "Text"];
  golden = Import[goldenFile, "Text"];

  If[current === golden,
    QPrint["PASS: ", FileNameTake[currentFile]];
    True,
    failMsg = "FAIL: " <> FileNameTake[currentFile] <> " differs from golden";
    QPrint[failMsg];
    QPrint["  Current: ", StringLength[current], " chars"];
    QPrint["  Golden:  ", StringLength[golden], " chars"];
    AddFailure[failMsg];
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

  QPrint["=== Golden File Regression Tests ==="];
  QPrint[""];

  results = Table[
    {backend, testName, ext} = testCase;
    currentFile = FileNameJoin[{$TestDir, "..", backend, testName <> ext}];
    goldenFile = FileNameJoin[{$GoldenDir, backend, testName <> ext <> ".golden"}];
    CompareGoldenFile[currentFile, goldenFile],
    {testCase, $TestCases}
  ];

  passed = Count[results, True];
  failed = Count[results, $Failed];

  QPrint[""];
  QPrint["=== Summary ==="];
  QPrint["Passed: ", passed, "/", Length[results]];
  QPrint["Failed: ", failed, "/", Length[results]];

  If[failed > 0,
    QPrint[""];
    QPrint["REGRESSION DETECTED"];
    If[Length[$ScriptCommandLine] > 0 && MemberQ[StringContainsQ[#, "compare_golden.wl"] & /@ $ScriptCommandLine, True],
      Exit[1],
      $Failed
    ],
    QPrint[""];
    QPrint["ALL TESTS PASSED"];
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
