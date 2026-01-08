(* ::Package:: *)

(* WritefileTests.wl *)
(* Unit tests for Generato`Writefile module *)

If[Environment["QUIET"] =!= "1", Print["Loading WritefileTests.wl..."]];

(* Load Generato which includes Writefile *)
If[!MemberQ[$Packages, "Generato`Writefile`"],
  Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"}]]
];

SetPVerbose[False];
SetPrintDate[False];

(* ========================================= *)
(* Test: SetMainPrint / GetMainPrint *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* SetMainPrint should store content *)
    SetMainPrint[Print["test content"]];
    GetMainPrint[] =!= Null,
    True,
    TestID -> "SetMainPrint-StoresContent"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* SetMainPrint has HoldAll - content should not evaluate until GetMainPrint *)
    testVar = 0;
    SetMainPrint[testVar = 42];
    beforeGet = testVar;
    GetMainPrint[];
    afterGet = testVar;
    {beforeGet, afterGet},
    {0, 42},
    TestID -> "SetMainPrint-HoldAllBehavior"
  ]
];

(* ========================================= *)
(* Test: ReplaceGFIndexName *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* Create a temp file, apply replacement, verify result *)
    tempFile = FileNameJoin[{$TemporaryDirectory, "test_replace_gf.txt"}];
    Export[tempFile, "gf_var[[ijk]] = value;", "Text"];
    ReplaceGFIndexName[tempFile, "[[ijk]]" -> "[i][j][k]"];
    result = Import[tempFile, "Text"];
    DeleteFile[tempFile];
    StringContainsQ[result, "[i][j][k]"],
    True,
    TestID -> "ReplaceGFIndexName-AppliesReplacement"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* Multiple replacements *)
    tempFile = FileNameJoin[{$TemporaryDirectory, "test_replace_multi.txt"}];
    Export[tempFile, "a[[ijk]] + b[[ijk]]", "Text"];
    ReplaceGFIndexName[tempFile, "[[ijk]]" -> ""];
    result = Import[tempFile, "Text"];
    DeleteFile[tempFile];
    result,
    "a + b\n",
    TestID -> "ReplaceGFIndexName-MultipleReplacements"
  ]
];

(* ========================================= *)
(* Test: WriteToFile *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* WriteToFile should create a file with header and footer *)
    tempFile = FileNameJoin[{$TemporaryDirectory, "test_write.hxx"}];
    SetPrintHeaderMacro[True];
    SetMainPrint[Global`pr["// test content"]];
    WriteToFile[tempFile];
    result = Import[tempFile, "Text"];
    DeleteFile[tempFile];
    (* Check for header comment and macro guards *)
    StringContainsQ[result, "/* test_write.hxx */"] &&
    StringContainsQ[result, "#ifndef TEST_WRITE_HXX"] &&
    StringContainsQ[result, "// test content"],
    True,
    TestID -> "WriteToFile-CreatesFileWithHeader"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* WriteToFile without header macro *)
    tempFile = FileNameJoin[{$TemporaryDirectory, "test_write_nomacro.hxx"}];
    SetPrintHeaderMacro[False];
    SetMainPrint[Global`pr["// content only"]];
    WriteToFile[tempFile];
    result = Import[tempFile, "Text"];
    DeleteFile[tempFile];
    (* Should have comment but no macro guards *)
    StringContainsQ[result, "/* test_write_nomacro.hxx */"] &&
    !StringContainsQ[result, "#ifndef"],
    True,
    TestID -> "WriteToFile-NoHeaderMacro"
  ]
];

If[Environment["QUIET"] =!= "1", Print["WritefileTests.wl completed."]];
