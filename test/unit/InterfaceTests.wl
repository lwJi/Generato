(* ::Package:: *)

(* InterfaceTests.wl *)
(* Unit tests for Generato`Interface module *)

If[Environment["QUIET"] =!= "1", Print["Loading InterfaceTests.wl..."]];

(* Load Generato *)
Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"}]];

SetPVerbose[False];
SetPrintDate[False];

(* Helper to reset context to defaults *)
ResetContext[] := ($CurrentContext = $ContextDefaults);

(* ========================================= *)
(* Test: SuffixName restoration in PrintEquations *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    (* Set initial SuffixName *)
    SetSuffixName["original"];
    (* PrintEquations with empty varlist should restore SuffixName *)
    PrintEquations[{SuffixName -> "temp"}, {}];
    (* Verify original value is restored *)
    GetSuffixName[],
    "original",
    TestID -> "Interface-PrintEquations-SuffixName-Restored"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    ResetContext[];
    (* Set initial SuffixName *)
    SetSuffixName["original"];
    (* PrintEquations without SuffixName option should also restore *)
    PrintEquations[{}];
    (* Verify original value is restored *)
    GetSuffixName[],
    "original",
    TestID -> "Interface-PrintEquations-SuffixName-Restored-NoOption"
  ]
];

(* ========================================= *)
(* Cleanup *)
(* ========================================= *)

ResetContext[];

If[Environment["QUIET"] =!= "1", Print["InterfaceTests.wl completed."]];
