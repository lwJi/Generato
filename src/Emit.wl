(* ::Package:: *)

(* Generato`Emit`, build the output in memory and write it once *)

(* (c) Liwei Ji, 11/2026 *)

BeginPackage["Generato`Emit`", {"Generato`Context`"}];

If[Environment["QUIET"] =!= "1",
  System`Print["------------------------------------------------------------"];
  System`Print["Package Generato`Emit`, {2026, 11, 27}"];
  System`Print["------------------------------------------------------------"];
];

CText::usage = "CText[expr] returns expr as C text, wrapped at 78 columns with no trailing newline.";

WriteToFile::usage = "WriteToFile[filename] builds the header, the main content and the footer in memory and writes the file once, after the main content has finished.";

Begin["`Private`"];

(*
  CText - the one helper that turns an expression into C text.
  The goldens hold right-hand sides in the layout Put gives CForm[expr]: a
  long expression is broken over lines no longer than 77 characters, and a
  continuation line is indented by two spaces. InputForm at PageWidth 78
  gives the same text, and the newline that ends it is removed.
*)
CText[expr_] :=
  Module[{text = ToString[CForm[expr], InputForm, PageWidth -> 78]},
    If[StringEndsQ[text, "\n"], StringDrop[text, -1], text]
  ];

Protect[CText];

(*
  StripTrailingWhitespace - removes the whitespace that ends each line of text.
  A buffer entry may hold several lines (a wrapped right-hand side, or ";\n"),
  so the text is split again after the join.
*)
StripTrailingWhitespace[text_String] :=
  StringRiffle[
    StringDelete[StringSplit[text, "\n", All], RegularExpression["[[:space:]]+$"]],
    "\n"
  ];

(*
  WriteToFile - the one writer.
  Global`pr appends to a buffer that lives for this call only. The file is
  opened after the main content has finished, so a run that stops inside it
  writes nothing.
*)
WriteToFile[outputfile_String] :=
  Module[{lines = {}, headerGuard, stream},
    If[Environment["QUIET"] =!= "1",
      System`Print["Writing to \"", outputfile, "\"...\n"]
    ];
    headerGuard =
      StringReplace[
        ToUpperCase[FileNameTake[outputfile]],
        Except[LetterCharacter | DigitCharacter] -> "_"
      ];
    Block[{Global`pr},
      Global`pr[x_:""] := (AppendTo[lines, x];);
      (* print first few lines *)
      Global`pr["/* " <> FileNameTake[outputfile] <> " */"];
      Global`pr[
        "/* Produced with Generato" <>
          If[GetPrintDate[],
            " on " <> DateString[{"Month", "/", "Day", "/", "Year"}]
            ,
            ""
          ] <> " */"
      ];
      Global`pr[];
      If[GetPrintHeaderMacro[],
        Global`pr["#ifndef " <> headerGuard];
        Global`pr["#define " <> headerGuard];
        Global`pr[]
      ];
      (* GetMainPrint[] evaluates the held main print content *)
      GetMainPrint[];
      Global`pr[];
      If[GetPrintHeaderMacro[],
        Global`pr["#endif // #ifndef " <> headerGuard];
        Global`pr[]
      ];
      Global`pr["/* " <> FileNameTake[outputfile] <> " */"];
    ];
    If[FileExistsQ[outputfile] && Environment["QUIET"] =!= "1",
      System`Print["\"", outputfile, "\" already exists, replacing it ...\n"]
    ];
    stream = OpenWrite[outputfile];
    WriteString[stream, StripTrailingWhitespace[StringRiffle[lines, "\n"]], "\n"];
    Close[stream];
    If[Environment["QUIET"] =!= "1",
      System`Print["Done generating \"", outputfile, "\"\n"]
    ];
    outputfile
  ];

Protect[WriteToFile];

End[];

EndPackage[];
