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
  The backend table of the write under way, or None. WriteToFile is its one
  writer: it holds the table for as long as the write lasts, and
  CurrentBackend is how a print call in the main content reads it.
*)
$WriteBackend = None;

CurrentBackend[] :=
  If[$WriteBackend === None,
    Stop[CurrentBackend::ENoWrite]
    ,
    $WriteBackend
  ];

CurrentBackend::ENoWrite = "No write is under way, so there is no backend to print through. A print call belongs in the main content, which WriteToFile evaluates.";

Protect[CurrentBackend];

(*
  WriteToFile - the one writer.
  The backend is resolved once, before anything is built, and its table is
  held for this call only. Global`pr appends to a buffer that lives for this
  call only. The file is opened after the main content has finished, so a
  run that stops inside it writes nothing.
*)
WriteToFile[backend_String, outputfile_String] :=
  Module[{table = ResolveBackend[backend], lines = {}, headerGuard, stream, ending},
    If[Environment["QUIET"] =!= "1",
      System`Print["Writing to \"", outputfile, "\"...\n"]
    ];
    headerGuard =
      StringReplace[
        ToUpperCase[FileNameTake[outputfile]],
        Except[LetterCharacter | DigitCharacter] -> "_"
      ];
    Block[{$WriteBackend = table, Global`pr},
      Global`pr[x_:""] := (AppendTo[lines, x];);
      (* print first few lines; Driver.wl knows a generated file by the second *)
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
      (* GetMainPrint[] evaluates the held main print content.
         A throw without a tag raised in it, which is how an xAct error ends,
         would not reach Driver.wl: the Import that loads the backend file
         returns the thrown value and the script carries on. So it is caught
         here and raised again through Stop, whose tag Import lets pass. *)
      ending = Catch[GetMainPrint[]; finished];
      If[ending =!= finished,
        Stop[WriteToFile::EThrow, ThrownText[ending]]
      ];
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
