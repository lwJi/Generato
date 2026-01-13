(* ::Package:: *)

(* WritefileTests.wl *)
(* Unit tests for Generato`Writefile module *)

If[Environment["QUIET"] =!= "1", Print["Loading WritefileTests.wl..."]];

(* Load Generato which includes Writefile *)
If[!MemberQ[$Packages, "Generato`Writefile`"],
  Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"}]]
];

$CurrentContext = SetPVerbose[$CurrentContext, False];
$CurrentContext = SetPrintDate[$CurrentContext, False];

(* ========================================= *)
(* Test: Context-aware SetMainPrint / GetMainPrint *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* Context-aware SetMainPrint should store content in context *)
    ctx = CreateContext[];
    ctx2 = SetMainPrint[ctx, Print["context test"]];
    GetMainPrint[ctx2] =!= Null,
    True,
    TestID -> "SetMainPrint-Context-StoresContent"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* Context-aware SetMainPrint returns new context, original unchanged *)
    ctx = CreateContext[];
    ctx2 = SetMainPrint[ctx, Print["new content"]];
    {GetMainPrint[ctx], GetMainPrint[ctx2] =!= Null},
    {Null, True},
    TestID -> "SetMainPrint-Context-ImmutableOriginal"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* Context-aware SetMainPrint holds content *)
    ctx = CreateContext[];
    ctx2 = SetMainPrint[ctx, testContextVar = 99];
    (* Content should be wrapped in Hold *)
    Head[GetMainPrint[ctx2]],
    Hold,
    TestID -> "SetMainPrint-Context-HoldsContent"
  ]
];

(* ========================================= *)
(* Test: SetMainPrint HoldAll behavior *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* SetMainPrint has HoldAll - content should not evaluate until released *)
    testVar = 0;
    ctx = CreateContext[];
    ctx2 = SetMainPrint[ctx, testVar = 42];
    beforeRelease = testVar;
    (* Release the held content *)
    mainPrint = GetMainPrint[ctx2];
    If[Head[mainPrint] === Hold,
      ReleaseHold[mainPrint]
    ];
    afterRelease = testVar;
    {beforeRelease, afterRelease},
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
    (* Multiple replacements - Import["Text"] strips trailing newlines *)
    tempFile = FileNameJoin[{$TemporaryDirectory, "test_replace_multi.txt"}];
    Export[tempFile, "a[[ijk]] + b[[ijk]]", "Text"];
    ReplaceGFIndexName[tempFile, "[[ijk]]" -> ""];
    result = Import[tempFile, "Text"];
    DeleteFile[tempFile];
    result,
    "a + b",
    TestID -> "ReplaceGFIndexName-MultipleReplacements"
  ]
];

(* ========================================= *)
(* Test: Context-aware WriteToFile *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* Context-aware WriteToFile should create a file with header and footer *)
    tempFile = FileNameJoin[{$TemporaryDirectory, "test_write.hxx"}];
    ctx = CreateContext[];
    ctx = SetCtx[ctx, "PrintHeaderMacro", True];
    ctx = SetCtx[ctx, "PrintDate", False];
    ctx = SetMainPrint[ctx, Global`pr["// test content"]];
    WriteToFile[ctx, tempFile];
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
    (* Context-aware WriteToFile without header macro *)
    tempFile = FileNameJoin[{$TemporaryDirectory, "test_write_nomacro.hxx"}];
    ctx = CreateContext[];
    ctx = SetCtx[ctx, "PrintHeaderMacro", False];
    ctx = SetCtx[ctx, "PrintDate", False];
    ctx = SetMainPrint[ctx, Global`pr["// content only"]];
    WriteToFile[ctx, tempFile];
    result = Import[tempFile, "Text"];
    DeleteFile[tempFile];
    (* Should have comment but no macro guards *)
    StringContainsQ[result, "/* test_write_nomacro.hxx */"] &&
    !StringContainsQ[result, "#ifndef"],
    True,
    TestID -> "WriteToFile-NoHeaderMacro"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* Context-aware WriteToFile should create a file with header and footer *)
    tempFile = FileNameJoin[{$TemporaryDirectory, "test_ctx_write.hxx"}];
    ctx = CreateContext[];
    ctx = SetCtx[ctx, "PrintHeaderMacro", True];
    ctx = SetCtx[ctx, "PrintDate", False];
    ctx = SetMainPrint[ctx, Global`pr["// context test content"]];
    WriteToFile[ctx, tempFile];
    result = Import[tempFile, "Text"];
    DeleteFile[tempFile];
    (* Check for header comment and macro guards *)
    StringContainsQ[result, "/* test_ctx_write.hxx */"] &&
    StringContainsQ[result, "#ifndef TEST_CTX_WRITE_HXX"] &&
    StringContainsQ[result, "// context test content"],
    True,
    TestID -> "WriteToFile-Context-CreatesFileWithHeader"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* Context-aware WriteToFile without header macro *)
    tempFile = FileNameJoin[{$TemporaryDirectory, "test_ctx_nomacro.hxx"}];
    ctx = CreateContext[];
    ctx = SetCtx[ctx, "PrintHeaderMacro", False];
    ctx = SetCtx[ctx, "PrintDate", False];
    ctx = SetMainPrint[ctx, Global`pr["// context only"]];
    WriteToFile[ctx, tempFile];
    result = Import[tempFile, "Text"];
    DeleteFile[tempFile];
    (* Should have comment but no macro guards *)
    StringContainsQ[result, "/* test_ctx_nomacro.hxx */"] &&
    !StringContainsQ[result, "#ifndef"],
    True,
    TestID -> "WriteToFile-Context-NoHeaderMacro"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* Context-aware WriteToFile evaluates held content *)
    tempFile = FileNameJoin[{$TemporaryDirectory, "test_ctx_held.hxx"}];
    ctx = CreateContext[];
    ctx = SetCtx[ctx, "PrintHeaderMacro", False];
    ctx = SetCtx[ctx, "PrintDate", False];
    (* Use SetMainPrint which wraps in Hold *)
    ctx = SetMainPrint[ctx, Global`pr["// held content evaluated"]];
    WriteToFile[ctx, tempFile];
    result = Import[tempFile, "Text"];
    DeleteFile[tempFile];
    StringContainsQ[result, "// held content evaluated"],
    True,
    TestID -> "WriteToFile-Context-EvaluatesHeldContent"
  ]
];

If[Environment["QUIET"] =!= "1", Print["WritefileTests.wl completed."]];
