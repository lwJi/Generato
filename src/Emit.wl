(* ::Package:: *)

(* Generato`Emit`, build the output in memory and write it once *)

(* (c) Liwei Ji, 11/2026 *)

BeginPackage["Generato`Emit`", {"Generato`Failure`", "Generato`Context`", "Generato`Backend`"}];

If[Environment["QUIET"] =!= "1",
  System`Print["------------------------------------------------------------"];
  System`Print["Package Generato`Emit`, {2026, 11, 27}"];
  System`Print["------------------------------------------------------------"];
];

WriteToFile::usage = "WriteToFile[backend, filename] resolves the table of the registered backend, builds the header, the main content and the footer in memory and writes the file once, after the main content has finished. WriteToFile[filename], without a backend, stops the run.";

CurrentBackend::usage = "CurrentBackend[] returns the backend table of the write under way: the one WriteToFile resolved from the name it was given. It stops the run when no write is under way.";

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
  WriteToFile - the one writer.
  The backend is resolved once, before anything is built, and its table is
  held for this call only, as are the lines pr adds. The file is opened
  after the main content has finished, so a run that stops inside it writes
  nothing.
*)
WriteToFile[backend_String, outputfile_String] :=
  Module[{table = ResolveBackend[backend], headerGuard, text, stream, ending},
    If[Environment["QUIET"] =!= "1",
      System`Print["Writing to \"", outputfile, "\"...\n"]
    ];
    headerGuard =
      StringReplace[
        ToUpperCase[FileNameTake[outputfile]],
        Except[LetterCharacter | DigitCharacter] -> "_"
      ];
    Block[{$WriteBackend = table, $WriteLines = {}},
      (* print first few lines; Driver.wl knows a generated file by the second *)
      pr["/* " <> FileNameTake[outputfile] <> " */"];
      pr[
        "/* Produced with Generato" <>
          If[GetPrintDate[],
            " on " <> DateString[{"Month", "/", "Day", "/", "Year"}]
            ,
            ""
          ] <> " */"
      ];
      pr[];
      If[GetPrintHeaderMacro[],
        pr["#ifndef " <> headerGuard];
        pr["#define " <> headerGuard];
        pr[]
      ];
      (* GetMainPrint[] evaluates the held main print content.
         A throw without a tag raised in it, which is how an xAct error ends,
         would not reach Driver.wl: the Import that loads the backend file
         returns the thrown value and the script carries on. So it is caught
         here and raised again through Stop, whose tag Import lets pass. *)
      ending = Catch[GetMainPrint[]; finished];
      If[ending =!= finished,
        Stop[WriteToFile::EThrow, ThrownText[ending]]
      ];
      pr[];
      If[GetPrintHeaderMacro[],
        pr["#endif // #ifndef " <> headerGuard];
        pr[]
      ];
      pr["/* " <> FileNameTake[outputfile] <> " */"];
      text = StripTrailingWhitespace[StringRiffle[$WriteLines, "\n"]]
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

(*
  A call without a backend is the form a backend file ended with before a
  backend was a registered table. Left undefined it would be inert: the
  script would run to its end, write nothing and report success.
*)
WriteToFile[outputfile_String] :=
  Stop[WriteToFile::EBackend, outputfile];

WriteToFile::EThrow = "The main content stopped at an uncaught Throw[`1`].";

WriteToFile::EBackend = "WriteToFile[\"`1`\"] names no backend. A backend file registers its table with DefineBackend[name, table] and ends with WriteToFile[name, GetOutputFile[]].";

Protect[WriteToFile];

End[];

EndPackage[];
