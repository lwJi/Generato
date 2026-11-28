(* ::Package:: *)

(* Generato`Emit`, the one write: header, lines and footer, written once *)

(* (c) Liwei Ji, 11/2026 *)

BeginPackage["Generato`Emit`", {"Generato`Context`"}];

If[!GetQuietMode[],
  System`Print["------------------------------------------------------------"];
  System`Print["Package Generato`Emit`, {2026, 11, 27}"];
  System`Print["------------------------------------------------------------"];
];

Emit::usage = "Emit[filename, lines] writes the file once: the header, the lines and the footer, with the whitespace that ends a line removed. It returns filename. Nothing else writes output, so a file exists only when its last line has been built.";

Begin["`Private`"];

(*
  StripTrailingWhitespace - removes the whitespace that ends each line of text.
  An entry of the lines may hold several lines, as a wrapped right-hand side
  does, so the text is split again after the join.
*)
StripTrailingWhitespace[text_String] :=
  StringRiffle[
    StringDelete[StringSplit[text, "\n", All], RegularExpression["[[:space:]]+$"]],
    "\n"
  ];

(*
  Emit - the one writer.
  A file is its header, the lines it was given and its footer. Nothing is
  opened before the text is whole, so a run that stops while its lines are
  being built writes nothing. The second line is how Driver.wl knows a
  generated file.
*)
Emit[outputfile_String, lines_List] :=
  Module[{name = FileNameTake[outputfile], macro = GetPrintHeaderMacro[], headerGuard, text, stream},
    If[!GetQuietMode[],
      System`Print["Writing to \"", outputfile, "\"...\n"]
    ];
    headerGuard =
      StringReplace[
        ToUpperCase[name],
        Except[LetterCharacter | DigitCharacter] -> "_"
      ];
    text =
      StripTrailingWhitespace[
        StringRiffle[
          Join[
            {
              "/* " <> name <> " */",
              "/* Produced with Generato" <>
                If[GetPrintDate[],
                  " on " <> DateString[{"Month", "/", "Day", "/", "Year"}]
                  ,
                  ""
                ] <> " */",
              ""
            },
            If[macro, {"#ifndef " <> headerGuard, "#define " <> headerGuard, ""}, {}],
            lines,
            {""},
            If[macro, {"#endif // #ifndef " <> headerGuard, ""}, {}],
            {"/* " <> name <> " */"}
          ],
          "\n"
        ]
      ];
    If[FileExistsQ[outputfile] && !GetQuietMode[],
      System`Print["\"", outputfile, "\" already exists, replacing it ...\n"]
    ];
    stream = OpenWrite[outputfile];
    WriteString[stream, text, "\n"];
    Close[stream];
    If[!GetQuietMode[],
      System`Print["Done generating \"", outputfile, "\"\n"]
    ];
    outputfile
  ];

Protect[Emit];

End[];

EndPackage[];
