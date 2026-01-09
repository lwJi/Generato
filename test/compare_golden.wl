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

(* Find the first N differing lines between two strings *)
FindDifferingLines[current_String, golden_String, maxDiffs_:5] := Module[
  {currentLines, goldenLines, maxLen, diffs, i},
  currentLines = StringSplit[current, "\n"];
  goldenLines = StringSplit[golden, "\n"];
  maxLen = Max[Length[currentLines], Length[goldenLines]];
  diffs = {};
  For[i = 1, i <= maxLen && Length[diffs] < maxDiffs, i++,
    Module[{currLine, goldLine},
      currLine = If[i <= Length[currentLines], currentLines[[i]], "<missing>"];
      goldLine = If[i <= Length[goldenLines], goldenLines[[i]], "<missing>"];
      If[currLine =!= goldLine,
        AppendTo[diffs, {i, goldLine, currLine}]
      ]
    ]
  ];
  diffs
];

(* Print diff details - always prints (not affected by quiet mode) *)
PrintDiffDetails[diffs_List, totalCurrentLines_Integer, totalGoldenLines_Integer] := Module[
  {},
  Print[""];
  Do[
    Module[{lineNum, expected, actual},
      {lineNum, expected, actual} = diff;
      Print["         Line ", lineNum, ":"];
      Print["           - ", StringTake[expected, UpTo[80]]];
      Print["           + ", StringTake[actual, UpTo[80]]];
    ],
    {diff, diffs}
  ];
  (* Show if there are more differences *)
  If[Length[diffs] >= 5,
    Print["         ... (showing first 5 differences)"]
  ];
];

(* Compare two files, return True if identical *)
CompareGoldenFile[currentFile_String, goldenFile_String] := Module[
  {current, golden, diffs, currentLines, goldenLines},

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
    (* Files differ - show details *)
    currentLines = Length[StringSplit[current, "\n"]];
    goldenLines = Length[StringSplit[golden, "\n"]];

    (* Always print failure header *)
    Print["  ", $TagFail, " ", FileNameTake[currentFile], " differs from golden"];
    Print["         Current: ", StringLength[current], " chars (", currentLines, " lines)"];
    Print["         Golden:  ", StringLength[golden], " chars (", goldenLines, " lines)"];

    (* Find and display differing lines *)
    diffs = FindDifferingLines[current, golden, 5];
    PrintDiffDetails[diffs, currentLines, goldenLines];

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
