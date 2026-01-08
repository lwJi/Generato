(* ::Package:: *)

(* TestConfig.wl *)
(* Shared test configuration for Generato test suite *)

BeginPackage["TestConfig`"];

GetTestDir::usage = "GetTestDir[] returns the base test directory path.";
LoadTestCases::usage = "LoadTestCases[] returns the list of test cases from test_cases.txt.";

Begin["`Private`"];

$TestDir = DirectoryName[$InputFileName];

GetTestDir[] := $TestDir;

LoadTestCases[] := Module[{configFile},
  configFile = FileNameJoin[{$TestDir, "test_cases.txt"}];
  Select[
    StringSplit[#, ":"] & /@ Import[configFile, "Lines"],
    Length[#] == 3 && !StringStartsQ[#[[1]], "#"] &
  ]
];

End[];
EndPackage[];
