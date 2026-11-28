(* ::Package:: *)

(* TestConfig.wl *)
(* Shared test configuration for Generato test suite *)

BeginPackage["TestConfig`"];

GetTestDir::usage = "GetTestDir[] returns the base test directory path.";
LoadTestCases::usage = "LoadTestCases[] returns the cases of test_cases.txt that must succeed, each as {backend, test, extension}.";
LoadFailingTestCases::usage = "LoadFailingTestCases[] returns the cases of test_cases.txt marked \"fails\", each as {backend, test, extension}.";

Begin["`Private`"];

$TestDir = DirectoryName[$InputFileName];

GetTestDir[] := $TestDir;

(* A case is backend:test:extension, and one that must fail carries a fourth field, "fails" *)
ReadTestCases[] := Module[{configFile, lines, parsed, entries, valid},
  configFile = FileNameJoin[{$TestDir, "test_cases.txt"}];

  (* Check if config file exists *)
  If[!FileExistsQ[configFile],
    Print["ERROR: test_cases.txt not found at ", configFile];
    Print["ERROR: Test suite cannot proceed without test configuration"];
    Return[$Failed]
  ];

  lines = Import[configFile, "Lines"];
  parsed = StringSplit[#, ":"] & /@ lines;

  (* Drop empty lines and comments *)
  entries = Select[parsed, Length[#] > 0 && !StringStartsQ[#[[1]], "#"] &];

  (* Filter valid entries (3 parts, or 4 parts ending in "fails") *)
  valid = Select[entries, MatchQ[#, {_, _, _} | {_, _, _, "fails"}] &];

  (* Warn about malformed lines (non-comments with wrong format) *)
  If[Length[valid] < Length[entries],
    Print["WARNING: Skipped ", Length[entries] - Length[valid], " malformed line(s) in test_cases.txt"]
  ];

  valid
];

LoadTestCases[] := Module[{cases = ReadTestCases[]},
  If[cases === $Failed, $Failed, Select[cases, Length[#] == 3 &]]
];

LoadFailingTestCases[] := Module[{cases = ReadTestCases[]},
  If[cases === $Failed, $Failed, Most /@ Select[cases, Length[#] == 4 &]]
];

End[];
EndPackage[];
