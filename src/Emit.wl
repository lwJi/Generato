(* ::Package:: *)

(* Generato`Emit`, the one write: header, lines and footer, written once *)

(* (c) Liwei Ji, 11/2026 *)

BeginPackage["Generato`Emit`", {"Generato`Failure`", "Generato`Context`", "Generato`Backend`"}];

If[Environment["QUIET"] =!= "1",
  System`Print["------------------------------------------------------------"];
  System`Print["Package Generato`Emit`, {2026, 11, 27}"];
  System`Print["------------------------------------------------------------"];
];

Emit::usage = "Emit[filename, lines] writes the file once: the header, the lines and the footer, with the whitespace that ends a line removed. It returns filename. Nothing else writes output, so a file exists only when its last line has been built.";

WriteToFile::usage = "WriteToFile[backend, filename] resolves the table of the backend, evaluates the main content, and hands the lines it printed to Emit, which writes the file once the main content has finished. WriteToFile[filename], without a backend, stops the run.";

RequireMainPrintWritten::usage = "RequireMainPrintWritten[] stops the run when the script set a main content with SetMainPrint and no WriteToFile wrote it. Driver.wl calls it when a script has run to its end.";

CurrentBackend::usage ="CurrentBackend[] returns the backend table of the write under way: the one WriteToFile resolved from the name it was given. It stops the run when no write is under way.";

pr::usage = "pr[line] adds a line to the output of the write under way, and pr[] an empty line. A script calls it in its main content, which WriteToFile evaluates. It stops the run when no write is under way.";

Begin["`Private`"];

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

(* ThrownText - a thrown value as text; it may be a whole expression, so only its start is kept. *)
ThrownText[value_] :=
  Module[{text = ToString[value, InputForm]},
    If[StringLength[text] > 200, StringTake[text, 200] <> " ...", text]
  ];

(*
  The state of the write under way: the table of its backend and the lines
  built so far, both None when there is none. WriteToFile is their one
  writer: it holds them for as long as its call lasts, so nothing of a write
  is left when it ends, however it ends. CurrentBackend is how a print call
  in the main content reads the table, and pr is how a line is added.
*)
$WriteBackend = None;

$WriteLines = None;

CurrentBackend[] :=
  If[$WriteBackend === None,
    Stop[CurrentBackend::ENoWrite]
    ,
    $WriteBackend
  ];

CurrentBackend::ENoWrite = "No write is under way, so there is no backend to print through. A print call belongs in the main content, which WriteToFile evaluates.";

Protect[CurrentBackend];

(*
  pr - one more line of the output.
  It is a symbol of this package, so the pr a script writes after it has
  loaded Generato is this one. Outside a write there is nothing to add the
  line to: left inert, the line would be lost and the run would report
  success.
*)
pr[line_:""] :=
  If[$WriteLines === None,
    Stop[pr::ENoWrite]
    ,
    AppendTo[$WriteLines, line];
  ];

pr::ENoWrite = "No write is under way, so there is no output to add a line to. pr belongs in the main content, which WriteToFile evaluates.";

Protect[pr];

(*
  Emit - the one writer.
  A file is its header, the lines it was given and its footer. Nothing is
  opened before the text is whole, so a run that stops while its lines are
  being built writes nothing. The second line is how Driver.wl knows a
  generated file.
*)
Emit[outputfile_String, lines_List] :=
  Module[{name = FileNameTake[outputfile], macro = GetPrintHeaderMacro[], headerGuard, text, stream},
    If[Environment["QUIET"] =!= "1",
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
    If[FileExistsQ[outputfile] && Environment["QUIET"] =!= "1",
      System`Print["\"", outputfile, "\" already exists, replacing it ...\n"]
    ];
    stream = OpenWrite[outputfile];
    WriteString[stream, text, "\n"];
    Close[stream];
    If[Environment["QUIET"] =!= "1",
      System`Print["Done generating \"", outputfile, "\"\n"]
    ];
    outputfile
  ];

Protect[Emit];

(*
  WriteToFile - the write of a script that holds its main content, started
  by a backend file that ends in a call of it. No file in codes/ does: each
  is one DefineBackend call, and a script hands its program to Generate.
  The backend is resolved once, before anything is built, and its table is
  held for this call only, as are the lines pr adds. Emit is handed the
  lines after the main content has finished, so a run that stops inside it
  writes nothing.
*)
WriteToFile[backend_String, outputfile_String] :=
  Module[{table = ResolveBackend[backend], lines, ending},
    lines =
      Block[{$WriteBackend = table, $WriteLines = {}},
        (* GetMainPrint[] evaluates the held main print content.
           A throw without a tag raised in it, which is how an xAct error ends,
           would not reach Driver.wl: the Import that loads the backend file
           returns the thrown value and the script carries on. So it is caught
           here and raised again through Stop, whose tag Import lets pass. *)
        ending = Catch[GetMainPrint[]; finished];
        If[ending =!= finished,
          Stop[WriteToFile::EThrow, ThrownText[ending]]
        ];
        $WriteLines
      ];
    Emit[outputfile, lines];
    $WrittenMainPrint = $CurrentContext["MainPrint"];
    outputfile
  ];

(*
  A call without a backend is the form a backend file ended with before a
  backend was a registered table. Left undefined it would be inert: the
  script would run to its end, write nothing and report success.
*)
WriteToFile[outputfile_String] :=
  Stop[WriteToFile::EBackend, outputfile];

WriteToFile::EThrow = "The main content stopped at an uncaught Throw[`1`].";

WriteToFile::EBackend = "WriteToFile[\"`1`\"] names no backend. A backend file is one DefineBackend[name, table] call, and a script hands its program to Generate[name, file, program].";

Protect[WriteToFile];

(*
  RequireMainPrintWritten - the end of a script that holds its main content.
  A backend file that is one DefineBackend call starts no write, so a script
  that sets its main content and ends by importing such a file would run to
  its end, write nothing and report success. $WrittenMainPrint is the main
  content the last WriteToFile wrote, and WriteToFile is its one writer; a
  main content that is set and is not that one was never written. This goes
  with WriteToFile and SetMainPrint.
*)
$WrittenMainPrint = None;

RequireMainPrintWritten[] :=
  With[{mainPrint = $CurrentContext["MainPrint"]},
    If[mainPrint =!= Null && mainPrint =!= $WrittenMainPrint,
      Stop[RequireMainPrintWritten::EUnwritten]
    ];
  ];

RequireMainPrintWritten::EUnwritten = "The script set its main content with SetMainPrint and nothing wrote it: a backend file in codes/ that is one DefineBackend call no longer starts the write. Pass the program as a list to Generate[backend, file, program], which replaces SetMainPrint, SetOutputFile and the Import of the backend file.";

Protect[RequireMainPrintWritten];

End[];

EndPackage[];
