(* ::Package:: *)

(* EmitTests.wl *)
(* Unit tests for Generato`Emit module *)

If[Environment["QUIET"] =!= "1", Print["Loading EmitTests.wl..."]];

(* Load Generato which includes Emit *)
If[!MemberQ[$Packages, "Generato`Emit`"],
  Needs["xAct`xCoba`", FileNameJoin[{Environment["GENERATO"], "src/Generato.wl"}]]
];

SetPVerbose[False];
SetPrintDate[False];

(* Helper to reset context to defaults *)
ResetContext[] := ($CurrentContext = $ContextDefaults);

(* The text PutAppend writes for expr, which CText reproduces less the final newline *)
PutAppendText[expr_] :=
  Module[{file = FileNameJoin[{$TemporaryDirectory, "generato_emit_putappend.txt"}], text},
    If[FileExistsQ[file], DeleteFile[file]];
    PutAppend[CForm[expr], file];
    text = ReadString[file];
    DeleteFile[file];
    text
  ];

(* A sum long enough to be wrapped several times *)
emitLongSum = Sum[k Symbol["emitC" <> ToString[k]]^2, {k, 1, 40}];

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
(* Test: CText *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* a short expression is one line, with no newline at its end *)
    CText[emitA emitB + emitD],
    "emitA*emitB + emitD",
    TestID -> "CText-ShortExpression"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a Part expression prints as a C subscript, as a point index does *)
    CText[emitA[[emitIjk]]],
    "emitA[emitIjk]",
    TestID -> "CText-PartExpression"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a long expression is broken over lines shorter than 78 characters,
       and a continuation line is indented by two spaces *)
    Module[{textLines = StringSplit[CText[emitLongSum], "\n"]},
      {
        Length[textLines] > 3,
        Max[StringLength /@ textLines] < 78,
        AllTrue[Rest[textLines], StringStartsQ[#, "  "] &],
        StringEndsQ[CText[emitLongSum], "\n"]
      }
    ],
    {True, True, True, False},
    TestID -> "CText-WrapsLongExpression"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* CText is the text PutAppend[CForm[expr]] writes, less its final newline *)
    Map[
      CText[#] <> "\n" === PutAppendText[#] &,
      {
        emitA emitB + emitD,
        emitLongSum,
        Expand[(emitA + emitB + emitD)^6],
        emitA[[emitIjk]]^2 / (3 emitB[[emitIjk]]) - 0.5 Sqrt[emitD],
        emitF @@ Table[Symbol["emitArg" <> ToString[k]], {k, 1, 30}]
      }
    ],
    {True, True, True, True, True},
    TestID -> "CText-MatchesPutAppend"
  ]
];

(* ========================================= *)
(* Test: WriteToFile *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* WriteToFile should create a file with header and footer *)
    ResetContext[];
    tempFile = FileNameJoin[{$TemporaryDirectory, "test_write.hxx"}];
    SetPrintHeaderMacro[True];
    SetPrintDate[False];
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
    ResetContext[];
    tempFile = FileNameJoin[{$TemporaryDirectory, "test_write_nomacro.hxx"}];
    SetPrintHeaderMacro[False];
    SetPrintDate[False];
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

AppendTo[$AllTests,
  VerificationTest[
    (* every pr call is a line; whitespace that ends a line is stripped,
       inside a multi-line entry too, and leading whitespace is kept *)
    ResetContext[];
    tempFile = FileNameJoin[{$TemporaryDirectory, "test_write_strip.hxx"}];
    SetPrintHeaderMacro[False];
    SetPrintDate[False];
    SetMainPrint[
      Global`pr["const double "];
      Global`pr["x \t"];
      Global`pr["="];
      Global`pr["a +  \n  b"];
      Global`pr[";\n"];
      Global`pr["  indented"];
      Global`pr["   "]
    ];
    WriteToFile[tempFile];
    result = ReadString[tempFile];
    DeleteFile[tempFile];
    result,
    "/* test_write_strip.hxx */\n" <>
    "/* Produced with Generato */\n" <>
    "\n" <>
    "const double\n" <>
    "x\n" <>
    "=\n" <>
    "a +\n" <>
    "  b\n" <>
    ";\n" <>
    "\n" <>
    "  indented\n" <>
    "\n" <>
    "\n" <>
    "/* test_write_strip.hxx */\n",
    TestID -> "WriteToFile-StripsTrailingWhitespacePerLine"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a wrapped right-hand side keeps its line breaks and its indent;
       only the space CText leaves before each break is stripped *)
    ResetContext[];
    tempFile = FileNameJoin[{$TemporaryDirectory, "test_write_wrapped.hxx"}];
    SetPrintHeaderMacro[False];
    SetPrintDate[False];
    SetMainPrint[Global`pr[CText[emitLongSum]]];
    WriteToFile[tempFile];
    result = ReadString[tempFile];
    DeleteFile[tempFile];
    {
      StringContainsQ[CText[emitLongSum], " \n  "],
      StringContainsQ[result, "\n\n" <> StringReplace[CText[emitLongSum], " \n" -> "\n"] <> "\n\n"]
    },
    {True, True},
    TestID -> "WriteToFile-KeepsWrappedLines"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the file is written once: an earlier file at the path is replaced, not appended to *)
    ResetContext[];
    tempFile = FileNameJoin[{$TemporaryDirectory, "test_write_twice.hxx"}];
    SetPrintHeaderMacro[False];
    SetPrintDate[False];
    SetMainPrint[Global`pr["// first"]];
    WriteToFile[tempFile];
    SetMainPrint[Global`pr["// second"]];
    WriteToFile[tempFile];
    result = ReadString[tempFile];
    DeleteFile[tempFile];
    {StringContainsQ[result, "// first"], StringCount[result, "/* test_write_twice.hxx */"]},
    {False, 2},
    TestID -> "WriteToFile-ReplacesExistingFile"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* nothing is written when the main content throws *)
    ResetContext[];
    tempFile = FileNameJoin[{$TemporaryDirectory, "test_write_throw.hxx"}];
    If[FileExistsQ[tempFile], DeleteFile[tempFile]];
    SetPrintHeaderMacro[True];
    SetPrintDate[False];
    SetMainPrint[
      Global`pr["// before"];
      Throw["stopped", emitTestTag];
      Global`pr["// after"]
    ];
    caught = Catch[WriteToFile[tempFile], emitTestTag];
    {caught, FileExistsQ[tempFile]},
    {"stopped", False},
    TestID -> "WriteToFile-NothingWrittenWhenBodyThrows"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a throw without a tag in the main content, which the Import of a backend
       file would swallow, is raised again through Stop; nothing is written *)
    ResetContext[];
    tempFile = FileNameJoin[{$TemporaryDirectory, "test_write_untagged.hxx"}];
    If[FileExistsQ[tempFile], DeleteFile[tempFile]];
    SetPrintHeaderMacro[True];
    SetPrintDate[False];
    SetMainPrint[
      Global`pr["// before"];
      Throw["stopped"];
      Global`pr["// after"]
    ];
    caught = Catch[WriteToFile[tempFile]; "NoThrow", GeneratoFailure];
    {caught, FileExistsQ[tempFile]},
    {$Failed, False},
    {WriteToFile::EThrow},
    TestID -> "WriteToFile-UntaggedThrowInBodyStops"
  ]
];

If[Environment["QUIET"] =!= "1", Print["EmitTests.wl completed."]];
