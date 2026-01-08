(* ::Package:: *)

(* TestConfig.wl *)
(* Shared test configuration for Generato test suite *)

BeginPackage["TestConfig`"];

GetTestDir::usage = "GetTestDir[] returns the base test directory path.";
LoadTestCases::usage = "LoadTestCases[] returns the list of test cases from test_cases.txt.";

Begin["`Private`"];

$TestDir = DirectoryName[$InputFileName];

GetTestDir[] := $TestDir;

LoadTestCases[] := Module[{configFile, lines, parsed, valid, skipped},
  configFile = FileNameJoin[{$TestDir, "test_cases.txt"}];

  (* Check if config file exists *)
  If[!FileExistsQ[configFile],
    Print["ERROR: test_cases.txt not found at ", configFile];
    Print["ERROR: Test suite cannot proceed without test configuration"];
    Return[$Failed]
  ];

  lines = Import[configFile, "Lines"];
  parsed = StringSplit[#, ":"] & /@ lines;

  (* Filter valid entries (3 parts, not comments) *)
  valid = Select[parsed, Length[#] == 3 && !StringStartsQ[#[[1]], "#"] &];

  (* Warn about malformed lines (non-comments with wrong format) *)
  skipped = Select[parsed, Length[#] != 3 && Length[#] > 0 && !StringStartsQ[#[[1]], "#"] &];
  If[Length[skipped] > 0,
    Print["WARNING: Skipped ", Length[skipped], " malformed line(s) in test_cases.txt"]
  ];

  valid
];

End[];
EndPackage[];
