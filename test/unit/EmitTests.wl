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
    SetMainPrint[Global`pr["// test content"]];
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
    SetMainPrint[Global`pr["// content only"]];
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
      Global`pr["const double "];
      Global`pr["x \t"];
      Global`pr["="];
      Global`pr["a +  \n  b"];
      Global`pr[";\n"];
      Global`pr["  indented"];
      Global`pr["   "]
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
    SetMainPrint[Global`pr[CText[emitLongSum]]];
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
    SetMainPrint[Global`pr["// first"]];
    WriteToFile["emitTest", tempFile];
    SetMainPrint[Global`pr["// second"]];
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
      Global`pr["// before"];
      Throw["stopped", emitTestTag];
      Global`pr["// after"]
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
      Global`pr["// before"];
      Throw["stopped"];
      Global`pr["// after"]
    ];
    caught = Catch[WriteToFile["emitTest", tempFile]; "NoThrow", GeneratoFailure];
    {caught, FileExistsQ[tempFile]},
    {$Failed, False},
    {WriteToFile::EThrow},
    TestID -> "WriteToFile-UntaggedThrowInBodyStops"
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
    SetMainPrint[Global`pr["// content"]];
    {
      Catch[WriteToFile[tempFile]; "NoThrow", GeneratoFailure],
      FileExistsQ[tempFile]
    },
    {$Failed, False},
    {WriteToFile::EBackend},
    TestID -> "WriteToFile-WithoutABackend-Stops"
  ]
];

If[Environment["QUIET"] =!= "1", Print["EmitTests.wl completed."]];
