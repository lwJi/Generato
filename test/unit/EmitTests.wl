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

(* A sum long enough to be wrapped several times *)
emitLongSum = Sum[k Symbol["emitC" <> ToString[k]]^2, {k, 1, 40}];

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

AppendTo[$AllTests,
  VerificationTest[
    (* a wrapped right-hand side keeps its line breaks and its indent;
       only the space CText leaves before each break is stripped *)
    ResetContext[];
    tempFile = FileNameJoin[{$TemporaryDirectory, "test_emit_wrapped.hxx"}];
    SetPrintHeaderMacro[False];
    SetPrintDate[False];
    Emit[tempFile, {CText[emitLongSum]}];
    result = ReadString[tempFile];
    DeleteFile[tempFile];
    {
      StringContainsQ[CText[emitLongSum], " \n  "],
      StringContainsQ[result, "\n\n" <> StringReplace[CText[emitLongSum], " \n" -> "\n"] <> "\n\n"]
    },
    {True, True},
    TestID -> "Emit-KeepsWrappedLines"
  ]
];

AppendTo[$AllTests,
  VerificationTest[
    (* Emit is the one write of the package: nothing else in it writes, holds a line or evaluates a main content *)
    Names["Generato`Emit`*"],
    {"Emit"},
    TestID -> "Emit-IsTheOneWrite"
  ]
];

(* ========================================= *)
(* Cleanup *)
(* ========================================= *)

ResetContext[];

If[Environment["QUIET"] =!= "1", Print["EmitTests.wl completed."]];
