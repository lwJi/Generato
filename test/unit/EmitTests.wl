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

(* A sum long enough to be wrapped several times *)
emitLongSum = Sum[k Symbol["emitC" <> ToString[k]]^2, {k, 1, 40}];

(* A backend to write through: the tests of the writer state nothing of their own *)
DefineBackend["emitTest", <||>];

(* ========================================= *)
(* Test: Emit *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* a file is its header, the lines it was given and its footer; the name of the file is returned *)
    ResetContext[];
    tempFile = FileNameJoin[{$TemporaryDirectory, "test_emit.hxx"}];
    SetPrintHeaderMacro[True];
    SetPrintDate[False];
    {
      Emit[tempFile, {"// first", "", "// second"}] === tempFile,
      result = ReadString[tempFile];
      DeleteFile[tempFile];
      result
    },
    {
      True,
      "/* test_emit.hxx */\n" <>
      "/* Produced with Generato */\n" <>
      "\n" <>
      "#ifndef TEST_EMIT_HXX\n" <>
      "#define TEST_EMIT_HXX\n" <>
      "\n" <>
      "// first\n" <>
      "\n" <>
      "// second\n" <>
      "\n" <>
      "#endif // #ifndef TEST_EMIT_HXX\n" <>
      "\n" <>
      "/* test_emit.hxx */\n"
    },
    TestID -> "Emit-HeaderLinesFooter"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* without the header macro, and with no lines, the file is its header and its footer; *)
    (* the second line is the one Driver.wl knows a generated file by *)
    ResetContext[];
    tempFile = FileNameJoin[{$TemporaryDirectory, "test_emit_empty.c"}];
    SetPrintHeaderMacro[False];
    SetPrintDate[False];
    Emit[tempFile, {}];
    result = ReadString[tempFile];
    DeleteFile[tempFile];
    result,
    "/* test_emit_empty.c */\n" <>
    "/* Produced with Generato */\n" <>
    "\n" <>
    "\n" <>
    "/* test_emit_empty.c */\n",
    TestID -> "Emit-NoLinesNoHeaderMacro"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the whitespace that ends a line is stripped, inside an entry of several lines too; *)
    (* an earlier file at the path is replaced, not appended to *)
    ResetContext[];
    tempFile = FileNameJoin[{$TemporaryDirectory, "test_emit_strip.hxx"}];
    SetPrintHeaderMacro[False];
    SetPrintDate[False];
    Emit[tempFile, {"// earlier"}];
    Emit[tempFile, {"const double ", "x \t", "=", "a +  \n  b", ";", "", "  indented"}];
    result = ReadString[tempFile];
    DeleteFile[tempFile];
    result,
    "/* test_emit_strip.hxx */\n" <>
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
    "/* test_emit_strip.hxx */\n",
    TestID -> "Emit-StripsTrailingWhitespaceAndReplaces"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the date is in the second line when it is asked for, after the text Driver.wl looks for *)
    ResetContext[];
    tempFile = FileNameJoin[{$TemporaryDirectory, "test_emit_date.hxx"}];
    SetPrintDate[True];
    Emit[tempFile, {}];
    result = StringSplit[ReadString[tempFile], "\n"][[2]];
    DeleteFile[tempFile];
    SetPrintDate[False];
    StringMatchQ[result, "/* Produced with Generato on " ~~ DigitCharacter.. ~~ "/" ~~ DigitCharacter.. ~~ "/" ~~ DigitCharacter.. ~~ " */"],
    True,
    TestID -> "Emit-DateInTheSecondLine"
  ]
];

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
(* Test: WriteToFile *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* WriteToFile should create a file with header and footer *)
    ResetContext[];
    tempFile = FileNameJoin[{$TemporaryDirectory, "test_write.hxx"}];
    SetPrintHeaderMacro[True];
    SetPrintDate[False];
    SetMainPrint[pr["// test content"]];
    WriteToFile["emitTest", tempFile];
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
    SetMainPrint[pr["// content only"]];
    WriteToFile["emitTest", tempFile];
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
      pr["const double "];
      pr["x \t"];
      pr["="];
      pr["a +  \n  b"];
      pr[";\n"];
      pr["  indented"];
      pr["   "]
    ];
    WriteToFile["emitTest", tempFile];
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
    SetMainPrint[pr[CText[emitLongSum]]];
    WriteToFile["emitTest", tempFile];
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
    SetMainPrint[pr["// first"]];
    WriteToFile["emitTest", tempFile];
    SetMainPrint[pr["// second"]];
    WriteToFile["emitTest", tempFile];
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
      pr["// before"];
      Throw["stopped", emitTestTag];
      pr["// after"]
    ];
    caught = Catch[WriteToFile["emitTest", tempFile], emitTestTag];
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
      pr["// before"];
      Throw["stopped"];
      pr["// after"]
    ];
    caught = Catch[WriteToFile["emitTest", tempFile]; "NoThrow", GeneratoFailure];
    {caught, FileExistsQ[tempFile]},
    {$Failed, False},
    {WriteToFile::EThrow},
    TestID -> "WriteToFile-UntaggedThrowInBodyStops"
  ]
];

(* ========================================= *)
(* Test: pr *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* pr is a symbol of the package, so the pr a script writes is the one the write reads *)
    {Context[pr], Names["Global`pr"]},
    {"Generato`Emit`", {}},
    TestID -> "pr-IsAPackageSymbol"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* pr[] is an empty line, and the lines are in the order of the calls *)
    ResetContext[];
    tempFile = FileNameJoin[{$TemporaryDirectory, "test_write_pr.hxx"}];
    SetPrintHeaderMacro[False];
    SetPrintDate[False];
    SetMainPrint[
      pr["// first"];
      pr[];
      pr["// second"]
    ];
    WriteToFile["emitTest", tempFile];
    result = ReadString[tempFile];
    DeleteFile[tempFile];
    result,
    "/* test_write_pr.hxx */\n" <>
    "/* Produced with Generato */\n" <>
    "\n" <>
    "// first\n" <>
    "\n" <>
    "// second\n" <>
    "\n" <>
    "/* test_write_pr.hxx */\n",
    TestID -> "pr-AddsALineToTheWrite"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* outside a write there is nothing to add a line to: the call stops, it is not inert *)
    {
      Catch[pr["// lost"]; "NoThrow", GeneratoFailure],
      Catch[pr[]; "NoThrow", GeneratoFailure]
    },
    {$Failed, $Failed},
    {pr::ENoWrite, pr::ENoWrite},
    TestID -> "pr-OutsideAWrite-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the lines of a write are let go when it ends, and when its main content stops *)
    ResetContext[];
    tempFile = FileNameJoin[{$TemporaryDirectory, "test_write_pr_stop.hxx"}];
    If[FileExistsQ[tempFile], DeleteFile[tempFile]];
    SetPrintDate[False];
    SetMainPrint[pr["// before"]; Throw["stopped", emitTestTag]];
    {
      Catch[WriteToFile["emitTest", tempFile], emitTestTag],
      Catch[pr["// after"]; "NoThrow", GeneratoFailure],
      FileExistsQ[tempFile]
    },
    {"stopped", $Failed, False},
    {pr::ENoWrite},
    TestID -> "pr-LetGoWhenTheWriteStops"
  ]
];

(* ========================================= *)
(* Test: the backend of a write *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* the table of the named backend is held while the main content is evaluated, and let go after *)
    ResetContext[];
    tempFile = FileNameJoin[{$TemporaryDirectory, "test_write_backend.hxx"}];
    SetPrintHeaderMacro[False];
    SetPrintDate[False];
    SetMainPrint[emitTable = CurrentBackend[]];
    WriteToFile["emitTest", tempFile];
    DeleteFile[tempFile];
    {
      emitTable === ResolveBackend["emitTest"],
      Catch[CurrentBackend[]; "NoThrow", GeneratoFailure]
    },
    {True, $Failed},
    {CurrentBackend::ENoWrite},
    TestID -> "WriteToFile-HoldsTheBackendForTheWrite"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the table is let go when the main content stops, too *)
    ResetContext[];
    tempFile = FileNameJoin[{$TemporaryDirectory, "test_write_backend_stop.hxx"}];
    If[FileExistsQ[tempFile], DeleteFile[tempFile]];
    SetPrintDate[False];
    SetMainPrint[Throw["stopped", emitTestTag]];
    {
      Catch[WriteToFile["emitTest", tempFile], emitTestTag],
      Catch[CurrentBackend[]; "NoThrow", GeneratoFailure]
    },
    {"stopped", $Failed},
    {CurrentBackend::ENoWrite},
    TestID -> "WriteToFile-LetsTheBackendGoWhenBodyStops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a backend that is not registered stops the write before the main content is evaluated *)
    ResetContext[];
    tempFile = FileNameJoin[{$TemporaryDirectory, "test_write_nobackend.hxx"}];
    If[FileExistsQ[tempFile], DeleteFile[tempFile]];
    SetPrintDate[False];
    emitReached = False;
    SetMainPrint[emitReached = True];
    {
      Catch[WriteToFile["emitTestNotRegistered", tempFile]; "NoThrow", GeneratoFailure],
      emitReached,
      FileExistsQ[tempFile]
    },
    {$Failed, False, False},
    {ResolveBackend::EUnknown},
    TestID -> "WriteToFile-UnregisteredBackend-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* the call a backend file used to end with, without a backend, stops and writes nothing *)
    ResetContext[];
    tempFile = FileNameJoin[{$TemporaryDirectory, "test_write_oldform.hxx"}];
    If[FileExistsQ[tempFile], DeleteFile[tempFile]];
    SetMainPrint[pr["// content"]];
    {
      Catch[WriteToFile[tempFile]; "NoThrow", GeneratoFailure],
      FileExistsQ[tempFile]
    },
    {$Failed, False},
    {WriteToFile::EBackend},
    TestID -> "WriteToFile-WithoutABackend-Stops"
  ]
];

(* ========================================= *)
(* Test: RequireMainPrintWritten *)
(* ========================================= *)

AppendTo[$AllTests,
  VerificationTest[
    (* a script that set no main content has nothing to be written *)
    ResetContext[];
    Catch[RequireMainPrintWritten[]; "NoThrow", GeneratoFailure],
    "NoThrow",
    TestID -> "RequireMainPrintWritten-NoMainPrint-Passes"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a main content that no WriteToFile wrote stops the run: the script that ends by importing a backend file *)
    ResetContext[];
    SetMainPrint[pr["// never written"]];
    Catch[RequireMainPrintWritten[]; "NoThrow", GeneratoFailure],
    $Failed,
    {RequireMainPrintWritten::EUnwritten},
    TestID -> "RequireMainPrintWritten-UnwrittenMainPrint-Stops"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* a main content WriteToFile wrote passes, and one set after that write does not *)
    ResetContext[];
    tempFile = FileNameJoin[{$TemporaryDirectory, "test_require_written.hxx"}];
    SetMainPrint[pr["// written"]];
    WriteToFile["emitTest", tempFile];
    DeleteFile[tempFile];
    {
      Catch[RequireMainPrintWritten[]; "NoThrow", GeneratoFailure],
      SetMainPrint[pr["// set after the write"]];
      Catch[RequireMainPrintWritten[]; "NoThrow", GeneratoFailure]
    },
    {"NoThrow", $Failed},
    {RequireMainPrintWritten::EUnwritten},
    TestID -> "RequireMainPrintWritten-WrittenMainPrint-Passes"
  ]
];

If[Environment["QUIET"] =!= "1", Print["EmitTests.wl completed."]];
