(* ::Package:: *)

(* WritefileTests.wl *)
(* Unit tests for Generato`Writefile module *)

Print["Loading WritefileTests.wl..."];

(* Load Generato *)
Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"}]];

(* Suppress verbose output during tests *)
SetPVerbose[False];
SetPrintDate[False];

(* ========================================= *)
(* Test: SetMainPrint and GetMainPrint *)
(* ========================================= *)

VerificationTest[
  (* SetMainPrint should set content without error *)
  SetMainPrint[Print["Test content"]];
  True,
  True,
  TestID -> "SetMainPrint-NoError"
];

VerificationTest[
  (* GetMainPrint should return something (not Null after setting) *)
  SetMainPrint[1 + 1];
  result = GetMainPrint[];
  result === 2,
  True,
  TestID -> "GetMainPrint-ReturnsSetValue"
];

VerificationTest[
  (* SetMainPrint with string content *)
  SetMainPrint["Hello World"];
  GetMainPrint[],
  "Hello World",
  TestID -> "SetMainPrint-StringContent"
];

VerificationTest[
  (* SetMainPrint with complex expression *)
  SetMainPrint[{1, 2, 3}];
  GetMainPrint[],
  {1, 2, 3},
  TestID -> "SetMainPrint-ListContent"
];

(* ========================================= *)
(* Test: WriteToFile *)
(* ========================================= *)

VerificationTest[
  (* WriteToFile should create a file *)
  testOutputFile = FileNameJoin[{$TemporaryDirectory, "generato_test_output.hxx"}];
  If[FileExistsQ[testOutputFile], DeleteFile[testOutputFile]];
  SetMainPrint[Global`pr["// Test line"]];
  WriteToFile[testOutputFile];
  FileExistsQ[testOutputFile],
  True,
  TestID -> "WriteToFile-CreatesFile"
];

VerificationTest[
  (* Written file should contain the header comment *)
  testOutputFile = FileNameJoin[{$TemporaryDirectory, "generato_test_output.hxx"}];
  content = Import[testOutputFile, "Text"];
  StringContainsQ[content, "generato_test_output.hxx"],
  True,
  TestID -> "WriteToFile-ContainsFilename"
];

VerificationTest[
  (* Written file should contain "Produced with Generato" *)
  testOutputFile = FileNameJoin[{$TemporaryDirectory, "generato_test_output.hxx"}];
  content = Import[testOutputFile, "Text"];
  StringContainsQ[content, "Produced with Generato"],
  True,
  TestID -> "WriteToFile-ContainsGeneratoHeader"
];

VerificationTest[
  (* Written file should contain user content *)
  testOutputFile = FileNameJoin[{$TemporaryDirectory, "generato_test_output.hxx"}];
  content = Import[testOutputFile, "Text"];
  StringContainsQ[content, "Test line"],
  True,
  TestID -> "WriteToFile-ContainsUserContent"
];

(* Test with header macro enabled *)
VerificationTest[
  (* WriteToFile with header macro should include #ifndef guards *)
  testOutputFile2 = FileNameJoin[{$TemporaryDirectory, "generato_test_macro.hxx"}];
  If[FileExistsQ[testOutputFile2], DeleteFile[testOutputFile2]];
  SetPrintHeaderMacro[True];
  SetMainPrint[Global`pr["// Macro test"]];
  WriteToFile[testOutputFile2];
  content = Import[testOutputFile2, "Text"];
  StringContainsQ[content, "#ifndef GENERATO_TEST_MACRO_HXX"],
  True,
  TestID -> "WriteToFile-HeaderMacroIfndef"
];

VerificationTest[
  (* Header macro should include #define *)
  testOutputFile2 = FileNameJoin[{$TemporaryDirectory, "generato_test_macro.hxx"}];
  content = Import[testOutputFile2, "Text"];
  StringContainsQ[content, "#define GENERATO_TEST_MACRO_HXX"],
  True,
  TestID -> "WriteToFile-HeaderMacroDefine"
];

VerificationTest[
  (* Header macro should include #endif *)
  testOutputFile2 = FileNameJoin[{$TemporaryDirectory, "generato_test_macro.hxx"}];
  content = Import[testOutputFile2, "Text"];
  StringContainsQ[content, "#endif"],
  True,
  TestID -> "WriteToFile-HeaderMacroEndif"
];

(* Reset header macro setting *)
SetPrintHeaderMacro[False];

(* ========================================= *)
(* Test: ReplaceGFIndexName *)
(* ========================================= *)

VerificationTest[
  (* ReplaceGFIndexName should perform string replacement in file *)
  testOutputFile3 = FileNameJoin[{$TemporaryDirectory, "generato_test_replace.txt"}];
  Export[testOutputFile3, "old_name = value;\n", "Text"];
  ReplaceGFIndexName[testOutputFile3, "old_name" -> "new_name"];
  content = Import[testOutputFile3, "Text"];
  StringContainsQ[content, "new_name"],
  True,
  TestID -> "ReplaceGFIndexName-ReplacesString"
];

VerificationTest[
  (* ReplaceGFIndexName should remove old string *)
  testOutputFile3 = FileNameJoin[{$TemporaryDirectory, "generato_test_replace.txt"}];
  content = Import[testOutputFile3, "Text"];
  !StringContainsQ[content, "old_name"],
  True,
  TestID -> "ReplaceGFIndexName-RemovesOldString"
];

VerificationTest[
  (* ReplaceGFIndexName with multiple replacements *)
  testOutputFile4 = FileNameJoin[{$TemporaryDirectory, "generato_test_multi.txt"}];
  Export[testOutputFile4, "alpha = 1; beta = 2;\n", "Text"];
  ReplaceGFIndexName[testOutputFile4, {"alpha" -> "ALPHA", "beta" -> "BETA"}];
  content = Import[testOutputFile4, "Text"];
  StringContainsQ[content, "ALPHA"] && StringContainsQ[content, "BETA"],
  True,
  TestID -> "ReplaceGFIndexName-MultipleReplacements"
];

(* Cleanup temporary files *)
If[FileExistsQ[FileNameJoin[{$TemporaryDirectory, "generato_test_output.hxx"}]],
  DeleteFile[FileNameJoin[{$TemporaryDirectory, "generato_test_output.hxx"}]]
];
If[FileExistsQ[FileNameJoin[{$TemporaryDirectory, "generato_test_macro.hxx"}]],
  DeleteFile[FileNameJoin[{$TemporaryDirectory, "generato_test_macro.hxx"}]]
];
If[FileExistsQ[FileNameJoin[{$TemporaryDirectory, "generato_test_replace.txt"}]],
  DeleteFile[FileNameJoin[{$TemporaryDirectory, "generato_test_replace.txt"}]]
];
If[FileExistsQ[FileNameJoin[{$TemporaryDirectory, "generato_test_multi.txt"}]],
  DeleteFile[FileNameJoin[{$TemporaryDirectory, "generato_test_multi.txt"}]]
];

Print["WritefileTests.wl completed."];
